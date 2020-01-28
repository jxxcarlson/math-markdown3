module Markdown.Parse exposing
    ( toMDBlockTree, searchAST, sourceMap, getLeadingTextFromAST
    , MDBlock(..), MDBlockWithId(..), BlockContent(..), Id
    , project, stringFromId, idOfBlock, projectedStringOfBlockContent, stringOfMDBlockTree
    , equalContent, equalIds, getId, idFromString, stringFromId
    )

{-| The purpose of this module is to parse a Document,
that is, a string, into an abstract syntax tree (AST)
which can then be further transformed or passed on
to a rendering function. The AST is a rose tree
of `MDBlockWithId` — short for "Markdown Blocks."

See the documentation at the head of module `Markdown.ElmWithId` for
the rationale for this module.


## Create or use AST

@docs toMDBlockTree, searchAST, sourceMap, getLeadingTextFromAST


## Types

@docs MDBlock, MDBlockWithId, BlockContent, Id


## Tools

@docs equal, project, stringFromId, idOfBlock, projectedStringOfBlockContent, stringOfMDBlockTree

-}

import BlockType exposing (BalancedType(..), BlockType(..), Line, MarkdownType(..))
import Dict exposing (Dict)
import HTree
import MDInline exposing (MDInline(..))
import Markdown.Option exposing (Option(..))
import Parser exposing ((|.), (|=), Parser, int, succeed, symbol)
import Prefix
import Tree exposing (Tree)



-- TYPES --


{-| A Parse is defined as follows:

    type Parse
        = Parse BlockType Level Content

    type alias Level =
        Int

    type alias Content =
        String

-}
type Block
    = Block Id BlockType Level Content


trimBalancedBlock : Block -> Block
trimBalancedBlock (Block id bt lev content) =
    Block id bt lev (String.trim content)


{-| Used to generate Ids of Html elements and to
implement differential rendering. The first
Int is a version number, incremented after each
edit. The second is a an integer representing
position in a traversal of the tree of blocks
obtained by parsing the text.
-}
type alias Id =
    ( Int, Int )


typeOfBlock : Block -> BlockType
typeOfBlock (Block _ bt _ _) =
    bt


{-| An MBlockWithId is like na MDBlock,
except that it has an

    Id : (Int, Int)

which should be thought of as

    ( version, id )

where the id is unique to each block.

-}
type MDBlockWithId
    = MDBlockWithId Id BlockType Level BlockContent


{-| An MDBlock (Markdown block) carries

    - the type of the block
    - its level
    - its unparsed content

-}
type MDBlock
    = MDBlock BlockType Level BlockContent


{-| Project an MDBlockWithId to an MDBlock
by omitting its id
-}
project : MDBlockWithId -> MDBlock
project (MDBlockWithId _ bt lev content) =
    MDBlock bt lev content


{-| Return a string representing the content of a block
if it is of type `T`. Otherwise, return the empty string.
-}
projectedStringOfBlockContent : BlockContent -> String
projectedStringOfBlockContent blockContent =
    case blockContent of
        M mmInline ->
            ""

        T str ->
            str


{-| Return the id of a block
-}
idOfBlock : MDBlockWithId -> Id
idOfBlock (MDBlockWithId id _ _ _) =
    id


{-| Return true of the blocks have the same id
-}
equalIds : MDBlockWithId -> MDBlockWithId -> Bool
equalIds a b =
    idOfBlock a == idOfBlock b



--{-| Check for equality of
--
--    - blockType
--    - level
--    - content
--
--ignoring the id.
--
---}
--slowerEqual : MDBlockWithId -> MDBlockWithId -> Bool
--slowerEqual (MDBlockWithId _ bt1 l1 c1) (MDBlockWithId _ bt2 l2 c2) =
--    bt1 == bt2 && l1 == l2 && c1 == c2


{-| Check for equality of

    - blockType
    - level
    - content

ignoring the id.

-}
equalContent : MDBlockWithId -> MDBlockWithId -> Bool
equalContent (MDBlockWithId _ bt1 l1 c1) (MDBlockWithId _ bt2 l2 c2) =
    if (l1 - l2) == 0 then
        -- && bt1 == bt2 && c1 == c2
        case bt1 of
            BalancedBlock balanced1 ->
                case bt2 of
                    BalancedBlock balanced2 ->
                        if balanced1 == balanced2 then
                            case c1 of
                                T a ->
                                    case c2 of
                                        T b ->
                                            a == b

                                        _ ->
                                            False

                                M a ->
                                    case c2 of
                                        M b ->
                                            a == b

                                        _ ->
                                            False

                        else
                            False

                    MarkdownBlock _ ->
                        False

            MarkdownBlock markdown1 ->
                case bt2 of
                    MarkdownBlock markdown2 ->
                        if markdown1 == markdown2 then
                            case c1 of
                                T a ->
                                    case c2 of
                                        T b ->
                                            a == b

                                        _ ->
                                            False

                                M a ->
                                    case c2 of
                                        M b ->
                                            a == b

                                        _ ->
                                            False

                        else
                            False

                    BalancedBlock _ ->
                        False

    else
        False


{-| The type of a parsed Block
-}
type BlockContent
    = M MDInline
    | T String


type alias Level =
    Int


type alias Content =
    String


type alias Line =
    String


type alias Document =
    String



-- THE FINITE-STATE MACHINE --


type FSM
    = FSM State (List Block) Register


type State
    = Start
    | InBlock Block
    | Error


getCurrentBlock : FSM -> Maybe Block
getCurrentBlock (FSM state _ _) =
    case state of
        InBlock block ->
            Just block

        _ ->
            Nothing


getCurrentBlockType : FSM -> Maybe BlockType
getCurrentBlockType (FSM state _ _) =
    case state of
        InBlock block ->
            Just (typeOfBlock block)

        _ ->
            Nothing


getTopOfBlockTypeStack : FSM -> Maybe BlockType
getTopOfBlockTypeStack (FSM _ _ register) =
    List.head register.blockTypeStack


{-| The register collects information
needed to number list items and (with
blocStack and level) to parse tables.
For functions that use the level field,
search for functions which contain
Register in their type signature.
-}
type alias Register =
    { id : Id
    , itemIndex1 : Int
    , itemIndex2 : Int
    , itemIndex3 : Int
    , itemIndex4 : Int
    , level : Int
    , blockStack : List Block
    , blockTypeStack : List BlockType
    }


pushBlockStack : Block -> Register -> Register
pushBlockStack block register =
    { register | blockStack = block :: register.blockStack }


topOfBlockStack : Register -> Maybe Block
topOfBlockStack register =
    List.head register.blockStack


popBlockStack : Register -> ( Maybe Block, Register )
popBlockStack register =
    ( List.head register.blockStack, { register | blockStack = List.tail register.blockStack |> Maybe.withDefault [] } )


clearBlockStack : Register -> Register
clearBlockStack register =
    { register | blockStack = [] }


emptyRegister : Register
emptyRegister =
    { id = ( 0, 0 )
    , itemIndex1 = 0
    , itemIndex2 = 0
    , itemIndex3 = 0
    , itemIndex4 = 0
    , level = 0
    , blockStack = []
    , blockTypeStack = []
    }


{-| `parseToBlockTree` runs the FSM to parse the input into
a list of Blocks. The machine is flushed to obtain
the last block and the level of elements is incremented
so that only the Root block has level 0. Finally,
a three of Blocks in constructed using the level information.

    parseToTree  "- One\nsome stuff\n- Two\nMore stuff"
    -->    Tree (Parse (MarkdownBlock Plain) 0 "*") [
    -->      Tree (Parse (MarkdownBlock UListItem) 1 ("- One\nsome stuff\n")) []
    -->      ,Tree (Parse (MarkdownBlock UListItem) 1 ("- Two\nMore stuff\n")) []
    -->    ]

-}
toBlockTree : Option -> Document -> Tree Block
toBlockTree option document =
    document
        |> splitIntoLines
        |> runFSM option
        |> flush
        |> List.map (changeLevel 1)
        |> HTree.fromList rootBlock blockLevel


parseTableRow : Level -> Line -> List Block
parseTableRow level line =
    line
        |> String.split "|"
        |> List.map String.trim
        |> List.filter (\s -> s /= "")
        |> List.map (\s -> Block ( -1, -1 ) (MarkdownBlock TableCell) level s)


changeLevel : Int -> Block -> Block
changeLevel k (Block id_ bt_ level_ content_) =
    Block id_ bt_ (level_ + k) content_


{-| Parse a string using a Markdown flavor option, returning the AST.
Example:

    Parse.toMDBlockTree 1 Extended "This **is** a test."
    --> Tree (MDBlockWithId (0,1)
    -->    (MarkdownBlock Root) 0 (M (Paragraph [
    -->       Line [OrdinaryText "DOCUMENT"]]))) [
    -->          Tree (MDBlockWithId (1,1)
    -->            (MarkdownBlock Plain) 1 (M (Paragraph [
    -->                Line [  OrdinaryText ("This ")
    -->              , BoldText ("is ")
    -->              , OrdinaryText ("a test.")]
    -->              , Line []
    -->            ])))
    -->      []]
    --> : Tree.Tree Parse.MDBlockWithId

-}
toMDBlockTree : Int -> Option -> Document -> Tree MDBlockWithId
toMDBlockTree version option document =
    document
        |> toBlockTree option
        |> Tree.map (selectParser option)
        |> Tree.indexedMap (\idx block -> setBlockIndex version idx block)


setBlockIndex : Int -> Int -> MDBlockWithId -> MDBlockWithId
setBlockIndex version idx (MDBlockWithId id bt lev blockContent) =
    MDBlockWithId ( idx, version ) bt lev blockContent


selectParser : Option -> (Block -> MDBlockWithId)
selectParser option ((Block id bt level_ content_) as block) =
    case option of
        Standard ->
            standardMDParser option block

        Extended ->
            extendedMDParser option block

        ExtendedMath ->
            extendedMathMDParser option block


extendedMathMDParser : Option -> Block -> MDBlockWithId
extendedMathMDParser option_ (Block id bt level_ content_) =
    case bt of
        MarkdownBlock mt ->
            case mt of
                Poetry ->
                    MDBlockWithId id (MarkdownBlock mt) level_ (M (Stanza content_))

                _ ->
                    MDBlockWithId id (MarkdownBlock mt) level_ (M (MDInline.parse option_ content_))

        BalancedBlock (DisplayCode lang) ->
            MDBlockWithId id (BalancedBlock (DisplayCode lang)) level_ (T content_)

        BalancedBlock Verbatim ->
            MDBlockWithId id (BalancedBlock Verbatim) level_ (T content_)

        BalancedBlock DisplayMath ->
            MDBlockWithId id (BalancedBlock DisplayMath) level_ (T content_)


extendedMDParser : Option -> Block -> MDBlockWithId
extendedMDParser option_ (Block id bt level_ content_) =
    case bt of
        MarkdownBlock mt ->
            case mt of
                Poetry ->
                    MDBlockWithId id (MarkdownBlock mt) level_ (M (Stanza content_))

                _ ->
                    MDBlockWithId id (MarkdownBlock mt) level_ (M (MDInline.parse option_ content_))

        BalancedBlock (DisplayCode lang) ->
            MDBlockWithId id (BalancedBlock (DisplayCode lang)) level_ (T content_)

        BalancedBlock Verbatim ->
            MDBlockWithId id (BalancedBlock Verbatim) level_ (T content_)

        _ ->
            MDBlockWithId id (MarkdownBlock Plain) level_ (M (MDInline.parse option_ content_))


standardMDParser : Option -> Block -> MDBlockWithId
standardMDParser option_ (Block id bt level_ content_) =
    case bt of
        MarkdownBlock mt ->
            MDBlockWithId id (MarkdownBlock mt) level_ (M (MDInline.parse option_ content_))

        BalancedBlock (DisplayCode lang) ->
            MDBlockWithId id (BalancedBlock (DisplayCode lang)) level_ (T content_)

        _ ->
            MDBlockWithId id (MarkdownBlock Plain) level_ (M (MDInline.parse option_ content_))



-- THE FINITE STATE MACHINE --


{-| runFSM runs a function

     folder : String -> FSM -> FSM

over the input, a list of strings, to run
the finite-state-machine, thereby accumulating
the parse result, a List Parse.

Recall that

    type FSM
      = FSM State (List Parse) Register

    runFSM  Standard "1. A\nxx\n   6. u\nyy\n4.  B"
    --> FSM (InBlock (Parse (MarkdownBlock (OListItem 2)) 0 (" B\n")))
    -->   [
    -->       Parse (MarkdownBlock (OListItem 1)) 1 "u\nyy\n"
    -->     , Parse (MarkdownBlock (OListItem 1)) 0 "A\nxx\n"
    -->   ]
    -->   { itemIndex1 = 2, itemIndex2 = 0, itemIndex3 = 0, itemIndex4 = 0 }

-}
runFSM : Option -> List Line -> FSM
runFSM option lines =
    let
        folder : String -> FSM -> FSM
        folder =
            \line fsm -> nextState option line fsm
    in
    List.foldl folder initialFSM lines



-- FINITE STATE MACHINE: NEXT STATE FUNCTION --


nextState : Option -> Line -> FSM -> FSM
nextState option line ((FSM state blocks register) as fsm_) =
    let
        fsm =
            handleRegister fsm_
    in
    case stateOfFSM fsm of
        Start ->
            nextStateAtStart option line fsm

        InBlock _ ->
            nextStateInBlock option line fsm

        Error ->
            fsm


handleRegister : FSM -> FSM
handleRegister ((FSM state blocks register) as fsm) =
    case topOfBlockStack register of
        Nothing ->
            fsm

        Just block ->
            -- Pop the blockStack the new item is not a table row
            case typeOfState state of
                Just (MarkdownBlock TableRow) ->
                    fsm

                _ ->
                    let
                        tableBlock : Block
                        tableBlock =
                            Block ( -1, -1 ) (MarkdownBlock Table) 0 "tableRoot"

                        rowBlock : Block
                        rowBlock =
                            Block ( -1, -1 ) (MarkdownBlock TableRow) 1 "row"

                        tableData : List Block
                        tableData =
                            List.reverse register.blockStack
                                |> (\x -> x ++ [ rowBlock, tableBlock ])
                                |> List.map editBlock

                        newBlocks : List Block
                        newBlocks =
                            -- NOTE: the below is a very bad solution!!
                            List.filter (\(Block _ _ _ content) -> content /= "deleteMe") blocks
                    in
                    FSM Start (tableData ++ newBlocks) (clearBlockStack register)


editBlock : Block -> Block
editBlock ((Block id bt lev content) as block) =
    if bt == MarkdownBlock TableRow && content == "row" then
        Block id bt lev ""

    else
        block


nextStateAtStart : Option -> Line -> FSM -> FSM
nextStateAtStart option line ((FSM state blocks register) as fsm) =
    case BlockType.get option line of
        ( _, Nothing ) ->
            FSM Error blocks register

        -- add line
        ( level, Just blockType ) ->
            let
                ( newBlockType, newRegister ) =
                    updateRegisterAndBlockType blockType level register

                newLine =
                    removePrefix blockType line
            in
            if
                newBlockType
                    == MarkdownBlock TableRow
                    && newBlockTypeIsDifferent newBlockType state
            then
                handleTableStart blockType level line state blocks register

            else if lineIsNotBlank line then
                FSM (InBlock (Block ( -1, -1 ) newBlockType level newLine)) blocks newRegister

            else
                fsm


currentIdOfFSM : FSM -> Id
currentIdOfFSM (FSM _ _ register) =
    register.id


{-| Used in parsing tables
-}
newBlockTypeIsDifferent : BlockType -> State -> Bool
newBlockTypeIsDifferent blockType state =
    case state of
        InBlock currentBlock ->
            typeOfBlock currentBlock /= blockType

        _ ->
            False


nextStateInBlock : Option -> Line -> FSM -> FSM
nextStateInBlock option line ((FSM state_ blocks_ register) as fsm) =
    case BlockType.get option line of
        ( _, Nothing ) ->
            FSM Error (blockListOfFSM fsm) register

        ( level, Just blockType ) ->
            -- process balanced block
            if isBalanced line (getTopOfBlockTypeStack fsm) blockType then
                processBalancedBlock blockType line fsm
                -- add markDown block d

            else if BlockType.isMarkDown blockType then
                processMarkDownBlock option level blockType line fsm

            else
                fsm


isBalanced : String -> Maybe BlockType -> BlockType -> Bool
isBalanced str mbt bt2 =
    case mbt of
        Nothing ->
            case bt2 of
                BalancedBlock _ ->
                    True

                MarkdownBlock _ ->
                    False

        Just bt1 ->
            case ( bt1, bt2, String.trimLeft str == "```\n" ) of
                ( BalancedBlock (DisplayCode _), MarkdownBlock _, False ) ->
                    False

                ( BalancedBlock (DisplayCode _), MarkdownBlock _, True ) ->
                    True

                ( BalancedBlock (DisplayCode _), _, _ ) ->
                    False

                ( _, BalancedBlock _, _ ) ->
                    True

                ( _, MarkdownBlock _, _ ) ->
                    False


processMarkDownBlock : Option -> Level -> BlockType -> Line -> FSM -> FSM
processMarkDownBlock option level blockTypeOfLine line ((FSM state blocks register) as fsm) =
    case state of
        -- add current block to block list and
        -- start new block with the current line and lineType
        InBlock ((Block id typeOfCurrentBlock levelOfCurrentBlock _) as currentBlock) ->
            if BlockType.isBalanced typeOfCurrentBlock then
                -- add line to current balanced block
                addLineToFSM line fsm

            else if blockTypeOfLine == MarkdownBlock Blank then
                -- start new block
                FSM Start (adjustLevel currentBlock :: blocks) register

            else if
                (blockTypeOfLine == MarkdownBlock Plain)
                    && (typeOfCurrentBlock /= MarkdownBlock TableRow)
                    && lineIsNotBlank line
            then
                -- continue, add content to current block
                addLineToFSM line fsm

            else if blockTypeOfLine == MarkdownBlock TableRow then
                handleTableRow blockTypeOfLine level line state blocks register

            else
                addNewMarkdownBlock option currentBlock line fsm

        _ ->
            fsm


lineIsNotBlank : Line -> Bool
lineIsNotBlank line =
    String.trim line /= ""


handleTableRow : BlockType -> Level -> Line -> State -> List Block -> Register -> FSM
handleTableRow blockTypeOfLine level line state blocks register =
    if newBlockTypeIsDifferent blockTypeOfLine state then
        handleTableStart blockTypeOfLine level line state blocks register

    else
        handleInnerTableRow blockTypeOfLine level line state blocks register


handleTableStart : BlockType -> Level -> Line -> State -> List Block -> Register -> FSM
handleTableStart blockTypeOfLine level line state blocks register =
    case state of
        Start ->
            FSM state blocks register

        Error ->
            FSM state blocks register

        InBlock currentBlock ->
            let
                tableBlock : Block
                tableBlock =
                    Block ( -1, -1 ) (MarkdownBlock Table) level "tableRoot"

                rowBlock : Block
                rowBlock =
                    Block ( -1, -1 ) blockTypeOfLine (level + 1) "row"

                childrenOfNewBlock =
                    parseTableRow (level + 2) line

                newRow =
                    childrenOfNewBlock ++ [ rowBlock ]
            in
            FSM (InBlock rowBlock)
                blocks
                { register | level = register.level + 0, blockStack = newRow }


handleInnerTableRow : BlockType -> Level -> Line -> State -> List Block -> Register -> FSM
handleInnerTableRow blockTypeOfLine level line state blocks register =
    case state of
        Start ->
            FSM state blocks register

        Error ->
            FSM state blocks register

        InBlock currentBlock ->
            let
                rowBlock : Block
                rowBlock =
                    Block ( -1, -1 ) blockTypeOfLine (level + 1) "row"

                childrenOfNewBlock =
                    parseTableRow (level + 2) line

                tableMarker : Block
                tableMarker =
                    Block ( -1, -1 ) (MarkdownBlock TableRow) (level + 1) "deleteMe"

                newRow =
                    childrenOfNewBlock ++ [ rowBlock ]
            in
            FSM (InBlock tableMarker) blocks { register | blockStack = register.blockStack ++ newRow }


processBalancedBlock : BlockType -> Line -> FSM -> FSM
processBalancedBlock blockType line ((FSM state_ blocks_ register) as fsm) =
    -- the currently processed block should be closed and a new one opened
    if Just blockType == typeOfState (stateOfFSM fsm) then
        case stateOfFSM fsm of
            InBlock block_ ->
                let
                    line_ =
                        removePrefix blockType line

                    block__ =
                        -- NOTE: the case analysis is needed to preserve the integrity of verbatim blocks
                        case blockType of
                            BalancedBlock Verbatim ->
                                block_

                            _ ->
                                trimBalancedBlock block_
                in
                FSM Start (addLineToBlock line_ block__ :: blocks_) register

            _ ->
                fsm
        -- open balanced block

    else
        case stateOfFSM fsm of
            InBlock block_ ->
                let
                    line_ =
                        if String.trimLeft line == "```\n" then
                            "\n"

                        else
                            line

                    block__ =
                        trimBalancedBlock block_
                in
                FSM (InBlock (Block register.id blockType (BlockType.level line_) line_)) (block__ :: blocks_) { register | blockTypeStack = List.drop 1 register.blockTypeStack }

            -- YYY
            _ ->
                fsm



-- FINITE STATE MACHINE: HELPER FUNCTIONS FOR THE UPDATE FUNCTION


{-|

1.  add the current block the block list ;
    (2) replace the current block by a new one derived from the
    current line and use that line to update the register

-}
addNewMarkdownBlock : Option -> Block -> Line -> FSM -> FSM
addNewMarkdownBlock option ((Block id typeOfCurrentBlock levelOfCurrentBlock _) as currentBlock) line ((FSM state blocks register) as fsm) =
    case BlockType.get option line of
        ( _, Nothing ) ->
            fsm

        ( level, Just newBlockType_ ) ->
            let
                ( newBlockType, newRegister ) =
                    updateRegisterAndBlockType newBlockType_ level register

                newLine =
                    removePrefix typeOfCurrentBlock line

                newBlock =
                    Block id newBlockType level (removePrefix newBlockType newLine)
            in
            FSM (InBlock newBlock) (adjustLevel currentBlock :: blocks) newRegister


removePrefix : BlockType -> Line -> Line
removePrefix blockType line_ =
    let
        p =
            BlockType.prefixOfBlockType blockType line_
    in
    String.replace p "" line_


{-| Recall that lines are stripped of leading space
-}
adjustLevel : Block -> Block
adjustLevel ((Block id blockType level content) as block) =
    if blockType == MarkdownBlock Plain then
        let
            newLevel =
                BlockType.level content
        in
        Block id blockType newLevel content

    else
        block


updateRegisterAndBlockType : BlockType -> Int -> Register -> ( BlockType, Register )
updateRegisterAndBlockType blockType level_ register =
    if BlockType.isOListItem blockType then
        let
            ( index, newRegister ) =
                incrementRegisterLevel level_ register

            newBlockType =
                MarkdownBlock (OListItem index)
        in
        ( newBlockType, newRegister )

    else if BlockType.isCode blockType then
        ( blockType, { register | blockTypeStack = blockType :: register.blockTypeStack } )

    else
        ( blockType, emptyRegister )


incrementRegisterLevel : Int -> Register -> ( Int, Register )
incrementRegisterLevel level register =
    case level + 1 of
        1 ->
            ( register.itemIndex1 + 1
            , { register
                | itemIndex1 = register.itemIndex1 + 1
                , itemIndex2 = 0
                , itemIndex3 = 0
                , itemIndex4 = 0
              }
            )

        2 ->
            ( register.itemIndex2 + 1
            , { register
                | itemIndex2 = register.itemIndex2 + 1
                , itemIndex3 = 0
                , itemIndex4 = 0
              }
            )

        3 ->
            ( register.itemIndex3 + 1
            , { register
                | itemIndex3 = register.itemIndex3 + 1
                , itemIndex4 = 0
              }
            )

        4 ->
            ( register.itemIndex4 + 1, { register | itemIndex4 = register.itemIndex4 + 1 } )

        _ ->
            ( 0, register )


addLineToFSM : Line -> FSM -> FSM
addLineToFSM line (FSM state_ blocks_ register) =
    case state_ of
        Start ->
            FSM state_ blocks_ register

        Error ->
            FSM state_ blocks_ register

        InBlock _ ->
            case List.head register.blockStack of
                Nothing ->
                    FSM (addLineToState line state_) blocks_ register

                Just block ->
                    FSM (addLineToState line state_) (block :: blocks_) { register | blockStack = List.drop 1 register.blockStack }


addLineToState : Line -> State -> State
addLineToState line state_ =
    case state_ of
        Start ->
            Start

        Error ->
            Error

        InBlock block_ ->
            InBlock (addLineToBlock line block_)


addLineToBlock : Line -> Block -> Block
addLineToBlock line (Block id blockType_ level_ content_) =
    Block id blockType_ level_ (content_ ++ line)



-- FINITE STATE MACHINE: HELPER FUNCTIONS --


blockLevel : Block -> Int
blockLevel (Block _ _ k _) =
    k


typeOfState : State -> Maybe BlockType
typeOfState s =
    case s of
        Start ->
            Nothing

        InBlock b ->
            Just (typeOfBlock b)

        Error ->
            Nothing


rootBlock =
    Block ( 0, 0 ) (MarkdownBlock Root) 0 "DOCUMENT"


flush : FSM -> List Block
flush fsm =
    case stateOfFSM fsm of
        Start ->
            List.reverse (blockListOfFSM fsm)

        Error ->
            List.reverse (blockListOfFSM fsm)

        InBlock b ->
            List.reverse (b :: blockListOfFSM fsm)


stateOfFSM : FSM -> State
stateOfFSM (FSM state_ _ _) =
    state_


blockListOfFSM : FSM -> List Block
blockListOfFSM (FSM _ blockList_ _) =
    blockList_


splitIntoLines : String -> List Line
splitIntoLines str =
    str
        |> String.lines
        |> List.map
            (\l -> l ++ "\n")


initialFSM : FSM
initialFSM =
    FSM Start [] emptyRegister



-- STRING FUNCTIONS: WERE USED TO DEBUG DURING DEVELOPMENT --


toAnnotatedStringList : List Block -> List ( Int, String )
toAnnotatedStringList blocks =
    List.map (\(Block _ _ level content) -> ( level, content )) blocks


stringOfFSM : FSM -> String
stringOfFSM fsm =
    fsm
        |> flush
        |> List.map stringOfBlock
        |> String.join "\n\n"


fsmToPairs : FSM -> List ( Int, String )
fsmToPairs fsm =
    fsm |> flush |> toAnnotatedStringList


pairsListToPairsTree : List ( Int, String ) -> Tree ( Int, String )
pairsListToPairsTree lst =
    lst |> HTree.fromList ( -1, "ROOT" ) Tuple.first


pairsTreeToString : Tree ( Int, String ) -> String
pairsTreeToString tree =
    HTree.toOutline Tuple.second tree


fsmToPairsTree : FSM -> Tree ( Int, String )
fsmToPairsTree fsm =
    fsm |> fsmToPairs |> HTree.fromList ( -1, "ROOT" ) Tuple.first


stringOfBlockTree : Tree Block -> String
stringOfBlockTree tree =
    tree
        |> Tree.flatten
        |> List.map stringOfBlock
        |> String.join "\n"


{-| Return a string value of an Id
-}
stringFromId : Id -> String
stringFromId id =
    "i" ++ (String.fromInt <| Tuple.first id) ++ "v" ++ (String.fromInt <| Tuple.second id)


stringOfBlock : Block -> String
stringOfBlock (Block id bt lev_ content_) =
    String.repeat (2 * lev_) " "
        ++ BlockType.stringOfBlockType bt
        ++ stringFromId id
        ++ " ("
        ++ String.fromInt lev_
        ++ ") "
        ++ "\n"
        ++ indent lev_ content_


indent : Int -> String -> String
indent k str =
    str
        |> String.split "\n"
        |> List.map (\s -> String.repeat (2 * k) " " ++ s)
        |> String.join "\n"


{-| A string representation of an MDBlockTree. Useful
for verifying the validity of the AST.
-}
stringOfMDBlockTree : Tree MDBlockWithId -> String
stringOfMDBlockTree tree =
    tree
        |> Tree.flatten
        |> List.map stringOfMDBlock
        |> String.join "\n"


stringOfMDBlock : MDBlockWithId -> String
stringOfMDBlock (MDBlockWithId id bt lev_ content_) =
    String.repeat (2 * lev_) " "
        ++ BlockType.stringOfBlockType bt
        ++ stringFromId id
        ++ " ("
        ++ String.fromInt lev_
        ++ ") "
        ++ indent lev_ (stringOfBlockContent content_)


stringOfBlockContent : BlockContent -> String
stringOfBlockContent blockContent =
    case blockContent of
        M mmInline ->
            stringOfMMInline mmInline

        T str ->
            str


stringOfMMInline : MDInline -> String
stringOfMMInline mmInline =
    MDInline.string mmInline



-- AST Tools --


{-| Search the AST for nodes whose label contains the
given string, returning the Id of the first node found,
if any.
-}
searchAST : String -> Tree MDBlockWithId -> Maybe Id
searchAST str ast =
    ast
        |> Tree.flatten
        |> List.filter (\block -> String.contains (Prefix.truncate str |> String.trim) (Prefix.truncate (stringContentFromBlock block)))
        |> List.head
        |> Maybe.map idOfBlock


inspectAST : Tree MDBlockWithId -> List ( Id, String )
inspectAST ast =
    ast
        |> Tree.flatten
        |> List.map (\b -> ( idOfBlock b, stringContentFromBlock b ))


{-| Get lead ing text element from AST
-}
getLeadingTextFromAST : Tree MDBlockWithId -> String
getLeadingTextFromAST ast =
    ast
        |> Tree.flatten
        |> List.map (\b -> stringContentFromBlock b)
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault "_Not found_"


{-| Create a sourceMap from the AST: a dictionary whose keys
are text strings and whose values
ids of the corresponding elements in the DOM
-}
sourceMap : Tree MDBlockWithId -> Dict String String
sourceMap ast =
    let
        list =
            ast
                |> Tree.flatten
                |> List.map (\b -> ( (String.trim << stringContentFromBlock) b, (stringFromId << idOfBlock) b ))
    in
    Dict.fromList list


getId : String -> Dict String String -> ( String, Maybe String )
getId str_ sourceMapDict =
    let
        str =
            toMDBlockTree 0 ExtendedMath str_ |> getLeadingTextFromAST |> String.trim

        id =
            List.filter (\( k, _ ) -> String.contains str k) (Dict.toList sourceMapDict)
                |> List.map (\( _, id_ ) -> id_)
                |> List.head
    in
    ( str, id )


type alias IdRecord =
    { id : Int, version : Int }


idParser : Parser ( Int, Int )
idParser =
    (succeed IdRecord
        |. symbol "i"
        |= int
        |. symbol "v"
        |= int
    )
        |> Parser.map (\r -> ( r.id, r.version ))


idFromString : String -> ( Int, Int )
idFromString str =
    case Parser.run idParser str of
        Ok result ->
            result

        Err _ ->
            ( 0, 0 )


stringFromId : ( Int, Int ) -> String
stringFromId ( id, version ) =
    "i" ++ String.fromInt id ++ "v" ++ String.fromInt version


stringContentFromBlock : MDBlockWithId -> String
stringContentFromBlock (MDBlockWithId _ _ _ c) =
    case c of
        T str ->
            str

        M mdInline ->
            MDInline.string2 mdInline



-- |> Prefix.truncate
