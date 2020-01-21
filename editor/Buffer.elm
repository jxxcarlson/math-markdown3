module Buffer exposing
    ( Buffer(..)
    , Direction(..)
    , at
    , between
    , clampPosition
    , deindentBetween
    , deindentFrom
    , fromString
    , groupEnd
    , groupRange
    , groupStart
    , indentBetween
    , indentFrom
    , indentSize
    , init
    , insert
    , lastPosition
    , lineAt
    , lineEnd
    , lines
    , nearWordChar
    , removeBefore
    , replace
    , toString
    )

{-| Manipulates a string using Positions. The string of characters is stored in
the opaque type `Buffer`. Functions that take a range of positions will
automatically sort the provided positions.
-}

import Array exposing (Array)
import List.Extra
import Maybe.Extra
import Position exposing (Position)
import String.Extra
import Util.Array


type Buffer
    = Buffer String


indentSize : Int
indentSize =
    2


{-| Create a new buffer from a string
-}
init : String -> Buffer
init content =
    Buffer content


{-| Internal function for getting the index of the position in a string
-}
indexFromPosition : String -> Position -> Maybe Int
indexFromPosition buffer position =
    -- Doesn't validate columns, only lines
    if position.line == 0 then
        Just position.column

    else
        String.indexes "\n" buffer
            |> List.Extra.getAt (position.line - 1)
            |> Maybe.map (\line -> line + position.column + 1)


{-| Returns true if the Position is at or after a word character. See isWordChar.
-}
nearWordChar : Position -> Buffer -> Bool
nearWordChar position (Buffer buffer) =
    indexFromPosition buffer position
        |> Maybe.andThen
            (\index ->
                let
                    previousChar =
                        stringCharAt (index - 1) buffer

                    currentChar =
                        stringCharAt index buffer
                in
                Maybe.map isWordChar previousChar
                    |> Maybe.Extra.orElseLazy
                        (\() -> Maybe.map isWordChar currentChar)
            )
        |> Maybe.withDefault False


{-| Insert a string into the buffer.
-}
insert : Position -> String -> Buffer -> Buffer
insert position string (Buffer buffer) =
    indexFromPosition buffer position
        |> Maybe.map (\index -> String.Extra.insertAt string index buffer)
        |> Maybe.withDefault buffer
        |> Buffer


{-| Replace the string between two positions with a different string.
-}
replace : Position -> Position -> String -> Buffer -> Buffer
replace pos1 pos2 string (Buffer buffer) =
    let
        ( start, end ) =
            Position.order pos1 pos2
    in
    Maybe.map2
        (\startIndex endIndex ->
            String.slice 0 startIndex buffer
                ++ string
                ++ String.dropLeft endIndex buffer
        )
        (indexFromPosition buffer start)
        (indexFromPosition buffer end)
        |> Maybe.withDefault buffer
        |> Buffer


{-| Remove the character before the given position. This is useful because
determining the _previous_ valid position is relativly expensive, but it's easy
for the buffer to just use the previous index.
-}
removeBefore : Position -> Buffer -> Buffer
removeBefore position (Buffer buffer) =
    indexFromPosition buffer position
        |> Maybe.map
            (\index ->
                String.slice 0 (max 0 (index - 1)) buffer
                    ++ String.dropLeft index buffer
            )
        |> Maybe.withDefault buffer
        |> Buffer



-- EXTRACTING CONTENT FROM BUFFERS


lines : Buffer -> List String
lines (Buffer content) =
    String.split "\n" content


lineAt : Position -> Buffer -> Maybe String
lineAt position buffer =
    List.Extra.getAt position.line (lines buffer)


fromString : String -> Buffer
fromString str =
    Buffer str


toString : Buffer -> String
toString (Buffer buffer) =
    buffer


{-| Returns a string of characters from between the positions
-}
between : Position -> Position -> Buffer -> String
between pos1 pos2 (Buffer buffer) =
    let
        ( start, end ) =
            Position.order pos1 pos2
    in
    Maybe.map2
        (\startIndex endIndex -> String.slice startIndex endIndex buffer)
        (indexFromPosition buffer start)
        (indexFromPosition buffer end)
        |> Maybe.withDefault ""


{-| Returns the string (a single character) at the given position
-}
at : Position -> Buffer -> String
at pos (Buffer buffer) =
    let
        pos2 =
            { pos | column = pos.column + 1 }
    in
    Maybe.map2
        (\startIndex endIndex -> String.slice startIndex endIndex buffer)
        (indexFromPosition buffer pos)
        (indexFromPosition buffer pos2)
        |> Maybe.withDefault ""



-- INDENTING


{-| Indent the given line from the given column. Returns the modified buffer and
the `column + indentedSize`. It accepts a position rather than a line because the
behavior depends on the column. It moves everything after the column to be
aligned with the indent size, content before the column is not moved.
-}
indentFrom : Position -> Buffer -> ( Buffer, Int )
indentFrom { line, column } (Buffer buffer) =
    indexFromPosition buffer (Position line 0)
        |> Maybe.map
            (\lineStart ->
                let
                    addIndentSize =
                        indentSize
                            - modBy indentSize
                                (String.slice lineStart (lineStart + column) buffer
                                    |> String.length
                                )
                in
                ( Buffer <|
                    String.slice 0 (lineStart + column) buffer
                        ++ String.repeat addIndentSize " "
                        ++ String.dropLeft (lineStart + column) buffer
                , column + addIndentSize
                )
            )
        |> Maybe.withDefault ( Buffer buffer, column )


{-| Indent each line between the given positions (inclusive). All lines will
be indented the full indent size.
-}
indentBetween : Position -> Position -> Buffer -> Buffer
indentBetween pos1 pos2 buffer =
    let
        ( start, end ) =
            Position.order pos1 pos2
    in
    List.range start.line end.line
        |> List.foldl
            (\line ->
                indentFrom { line = line, column = 0 } >> Tuple.first
            )
            buffer


{-| Deindent the given line. Returns the modified buffer and the
`column - deindentedSize`. Unlike `indent`, `deindent` will deindent all the
content in the line, regardless of `position.column`. _Why not just accept a
line then?_, you say. Well, the line might be close to the left, so it won't
deindent the full `indentSize` -- in that case, it's important to know the new
column.
-}
deindentFrom : Position -> Buffer -> ( Buffer, Int )
deindentFrom { line, column } (Buffer buffer) =
    indexFromPosition buffer (Position line 0)
        |> Maybe.map
            (\lineStart ->
                let
                    startChars =
                        String.slice lineStart (lineStart + indentSize) buffer

                    startIndentChars =
                        String.foldl
                            (\char count ->
                                if char == ' ' then
                                    count + 1

                                else
                                    count
                            )
                            0
                            startChars
                in
                ( Buffer <|
                    String.slice 0 lineStart buffer
                        ++ String.dropLeft (lineStart + startIndentChars) buffer
                , column - startIndentChars
                )
            )
        |> Maybe.withDefault ( Buffer buffer, column )


{-| Deindents all the lines between the given positions. Returns the updated
buffer and the new column of each position. The column doesn't always shift
a full indent, if there isn't enough whitespace at the beginning of the line.
-}
deindentBetween : Position -> Position -> Buffer -> ( Buffer, Int, Int )
deindentBetween pos1 pos2 buffer =
    let
        ( pos1Buffer, pos1Column ) =
            deindentFrom pos1 buffer

        ( pos2Buffer, pos2Column ) =
            deindentFrom pos2 pos1Buffer
    in
    if abs (pos1.line - pos2.line) > 1 then
        let
            ( start, end ) =
                Position.order pos1 pos2
        in
        ( List.range (start.line + 1) (end.line - 1)
            |> List.foldl
                (\line ->
                    deindentFrom { line = line, column = 0 }
                        >> Tuple.first
                )
                pos2Buffer
        , pos1Column
        , pos2Column
        )

    else
        ( pos2Buffer, pos1Column, pos2Column )



-- GROUPING


isWhitespace : Char -> Bool
isWhitespace =
    String.fromChar >> String.trim >> (==) ""


isNonWordChar : Char -> Bool
isNonWordChar =
    String.fromChar >> (\a -> String.contains a "/\\()\"':,.;<>~!@#$%^&*|+=[]{}`?-…")


isWordChar : Char -> Bool
isWordChar char =
    not (isNonWordChar char) && not (isWhitespace char)


type Group
    = None
    | Word
    | NonWord


type Direction
    = Forward
    | Backward


{-| Start at the position and move in the direction using the following rules:

  - Skip consecutive whitespace. Skip a single newline if it follows the whitespace,
    then continue skipping whitespace.
  - If the next character is a newline, stop
  - If the next character is a non-word character, skip consecutive non-word
    characters then stop
  - If the next character is a word character, skip consecutive word characters
    then stop

-}
groupHelp : Direction -> Bool -> String -> Position -> Group -> Position
groupHelp direction consumedNewline string position group =
    let
        parts =
            case direction of
                Forward ->
                    String.uncons string

                Backward ->
                    String.uncons (String.reverse string)
                        |> Maybe.map (Tuple.mapSecond String.reverse)
    in
    case parts of
        Just ( char, rest ) ->
            let
                nextPosition changeLine =
                    case ( direction, changeLine ) of
                        ( Forward, True ) ->
                            Position (position.line + 1) 0

                        ( Forward, False ) ->
                            Position.nextColumn position

                        ( Backward, True ) ->
                            if String.contains "\n" rest then
                                Position
                                    (position.line - 1)
                                    (String.Extra.rightOfBack "\n" rest
                                        |> String.length
                                    )

                            else
                                Position
                                    (position.line - 1)
                                    (String.length rest)

                        ( Backward, False ) ->
                            Position.previousColumn position

                next nextConsumedNewline =
                    groupHelp
                        direction
                        nextConsumedNewline
                        rest
                        (nextPosition (consumedNewline /= nextConsumedNewline))
            in
            case group of
                None ->
                    if char == '\n' then
                        if consumedNewline then
                            position

                        else
                            next True None

                    else if isWhitespace char then
                        next consumedNewline None

                    else if isNonWordChar char then
                        next consumedNewline NonWord

                    else
                        next consumedNewline Word

                Word ->
                    if not (isWordChar char) then
                        position

                    else
                        next consumedNewline Word

                NonWord ->
                    if isNonWordChar char then
                        next consumedNewline NonWord

                    else
                        position

        Nothing ->
            position


{-| Start at the position and move right using the following rules:

  - Skip consecutive whitespace. Skip a single newline if it follows the whitespace,
    then continue skipping whitespace.
  - If the next character is a newline, stop
  - If the next character is a non-word character, skip consecutive non-word
    characters then stop
  - If the next character is a word character, skip consecutive word characters
    then stop

-}
groupEnd : Position -> Buffer -> Position
groupEnd position (Buffer buffer) =
    indexFromPosition buffer position
        |> Maybe.map
            (\index -> groupHelp Forward False (String.dropLeft index buffer) position None)
        |> Maybe.withDefault position


{-| Start at the position and move left. Uses the same rules as `groupEnd`.
-}
groupStart : Position -> Buffer -> Position
groupStart position (Buffer buffer) =
    indexFromPosition buffer position
        |> Maybe.map
            (\index ->
                groupHelp
                    Backward
                    False
                    (String.slice 0 index buffer)
                    position
                    None
            )
        |> Maybe.withDefault position


stringCharAt : Int -> String -> Maybe Char
stringCharAt index string =
    String.slice index (index + 1) string
        |> String.uncons
        |> Maybe.map Tuple.first


charsAround : Int -> String -> ( Maybe Char, Maybe Char, Maybe Char )
charsAround index string =
    ( stringCharAt (index - 1) string
    , stringCharAt index string
    , stringCharAt (index + 1) string
    )


tuple3MapAll : (a -> b) -> ( a, a, a ) -> ( b, b, b )
tuple3MapAll fn ( a1, a2, a3 ) =
    ( fn a1, fn a2, fn a3 )


tuple3CharsPred :
    (Char -> Bool)
    -> ( Maybe Char, Maybe Char, Maybe Char )
    -> ( Bool, Bool, Bool )
tuple3CharsPred pred =
    tuple3MapAll (Maybe.map pred >> Maybe.withDefault False)


{-| If the position is in the middle or on the edge of a group, the edges of the
group are returned. Otherwise `Nothing` is returned.
-}
groupRange : Position -> Buffer -> Maybe ( Position, Position )
groupRange position (Buffer buffer) =
    indexFromPosition buffer position
        |> Maybe.andThen
            (\index ->
                let
                    chars =
                        charsAround index buffer

                    range pred =
                        case tuple3CharsPred pred chars of
                            ( True, True, True ) ->
                                Just
                                    ( groupStart position (Buffer buffer)
                                    , groupEnd position (Buffer buffer)
                                    )

                            ( False, True, True ) ->
                                Just
                                    ( position
                                    , groupEnd position (Buffer buffer)
                                    )

                            ( True, True, False ) ->
                                Just
                                    ( groupStart position (Buffer buffer)
                                    , Position.nextColumn position
                                    )

                            ( True, False, _ ) ->
                                Just
                                    ( groupStart position (Buffer buffer)
                                    , position
                                    )

                            ( False, True, False ) ->
                                Just
                                    ( position, Position.nextColumn position )

                            _ ->
                                Nothing
                in
                range isWordChar
                    |> Maybe.Extra.orElseLazy (\() -> range isNonWordChar)
            )


lineEnd : Int -> Buffer -> Maybe Int
lineEnd line =
    lines >> List.Extra.getAt line >> Maybe.map String.length


clampPosition : Direction -> Buffer -> Position -> Position
clampPosition direction buffer position =
    let
        lines_ =
            lines buffer |> Array.fromList
    in
    if position.line < 0 then
        Position 0 0

    else
        case Array.get position.line lines_ of
            Just line ->
                if position.column > String.length line then
                    case direction of
                        Forward ->
                            if position.line < (Array.length lines_ - 1) then
                                Position (position.line + 1) 0

                            else
                                position

                        Backward ->
                            Position position.line (String.length line)

                else if position.column < 0 then
                    Array.get (position.line - 1) lines_
                        |> Maybe.map
                            (String.length >> Position (position.line - 1))
                        |> Maybe.withDefault (Position 0 0)

                else
                    position

            Nothing ->
                case Util.Array.last lines_ of
                    Just ( line, number ) ->
                        Position number (String.length line)

                    Nothing ->
                        Position 0 0


{-| Move the position to the closest valid position in the buffer.

If the cursor is:

    - before the first line, move to the first column of the first line
    - after the last line, move to the last column of the last line
    - past the last column of the line, either go to the last column of the line
      or the first column of the next line, depending on the specified direction
    - before the first column of the line, go to the last column of the previous
      line.

    import Buffer exposing(Buffer(..))

    -- FORWARD

    -- cursor inside line; keep as is
    clampPosition Forward (Buffer "abc\nxyz") {column = 1, line = 0}
    --> {column = 1, line = 0}

    -- cursor at end of first line; keep as is
    clampPosition Forward (Buffer "abc\nxyz") {column = 2, line = 0}
    --> {column = 2, line = 0}

    -- cursor beyond end of first line; move to beginning of next line
    clampPosition Forward (Buffer "abc\nxyz") {column = 3, line = 0}
    --> {column = 0, line = 1}

    -- cursor at end of last line; keep in place (3)
    clampPosition Forward (Buffer "abc\nxyz") {column = 2, line = 1}
    --> {column = 2, line = 1}

    -- cursor beyond end of last line;  move to end of last line
    clampPosition Forward (Buffer "abc\nxyz") {column = 3, line = 1}
    --> {column = 2, line = 1}

    -- line in beyond last line
    clampPosition Forward (Buffer "a") {column = 0, line = 1}
    --> {column = 1, line = 0}

    -- line in beyond last line
    clampPosition Forward (Buffer "a") {column = 3, line = 1}
    --> {column = 1, line = 0}

    BACKWARDS

    -- cursor in line;  move to end of last line
    clampPosition Backward (Buffer "abc\nxyz") {column = 1, line = 0}
    --> {column = 1, line = 0}

    -- cursor at beginning of first line; keep in place
    clampPosition Backward (Buffer "abc\nxyz") {column = 0, line = 0}
    --> {column = 0, line = 0}

    -- cursor at beginning of second line;  keep on place
    clampPosition Backward (Buffer "abc\nxyz") {column = 0, line = 1}
    --> {column = 0, line = 1}

    -- SPECIAL
    clampPosition Forward (Buffer "a") {column = 1, line = 0}
    --> {column = 1, line = 0}

-}
clampPosition2 : Direction -> Buffer -> Position -> Position
clampPosition2 direction buffer position =
    let
        lines_ =
            lines buffer |> Array.fromList
    in
    if position.line < 0 then
        Position 0 0

    else
        case Array.get position.line lines_ of
            Just line ->
                if position.column >= String.length line then
                    -- cursor is beyond end of line
                    case direction of
                        Forward ->
                            if position.line == Array.length lines_ - 1 then
                                -- at last line
                                Position position.line (String.length line - 1)

                            else
                                Position (position.line + 1) 0

                        Backward ->
                            Position position.line (String.length line)

                else if position.column == String.length line - 1 then
                    -- cursor is at end of line
                    position

                else if position.column < 0 then
                    Array.get (position.line - 1) lines_
                        |> Maybe.map
                            (String.length >> Position (position.line - 1))
                        |> Maybe.withDefault (Position 0 0)

                else
                    position

            Nothing ->
                case Util.Array.last lines_ of
                    Just ( line, number ) ->
                        Position number (String.length line)

                    Nothing ->
                        Position 0 0


lastPosition : Buffer -> Position
lastPosition buffer =
    lines buffer
        |> Array.fromList
        |> Util.Array.last
        |> Maybe.map (\( line, index ) -> Position index (String.length line))
        |> Maybe.withDefault (Position 0 0)
