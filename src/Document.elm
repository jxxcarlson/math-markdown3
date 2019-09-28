module Document exposing (Document, setContent, getContent, documentIdentifier
  , create, replaceInList, updateMetaData, footer)

import Time exposing(Posix)
import Utility
import List.Extra
import Parser exposing(Parser, getChompedString, succeed, symbol, chompWhile, (|.))
import Prng.Uuid as Uuid exposing(Uuid)


type alias Document = {
     id : Uuid
   , title : String
   , authorIdentifier: String
   , content : String
   , public: Bool
   , tags : List String
   , slug : String
  }


setContent : String -> Document -> Document
setContent str document =
    { document | content = str }


getContent : Maybe Document -> String
getContent maybeDocument =
    case maybeDocument of
        Just document ->
            document.content
        Nothing -> ""


footer : Document -> String
footer document =
    "\n\n___\n\n````\nAuthor: "
    ++ document.authorIdentifier ++ "\n"
    -- ++ "Document ID: " ++ document.identifier ++ "\n"
    ++ "Document slug: " ++ makeSlug document ++ "\n"
    ++ "Tags: " ++ String.join ", " document.tags ++ "\n"
    ++ "Words: " ++ Utility.wordCount document.content
    ++ "\n" ++ "````" ++ "\n\n"

{-|
    > t = Time.millisToPosix (1568667528 * 1000)
    Posix 1568667528000 : Time.Posix
    > title = "Introduction to Quantum Mechanics"
    > documentId "jxxcarlson" title t
    "jxxcarlson.introduction-to-quantum-mechanics.1568667528"
44
-}
documentIdentifier : String -> String -> Posix -> String
documentIdentifier authorIdentifier title time =
    [authorIdentifier, Utility.normalize title, Utility.stringOfPosix time]
      |> String.join "."

makeSlug : Document -> String
makeSlug document =
    let
      shortHash = Uuid.toString document.id  |> String.right 6
    in
    document.authorIdentifier ++ "." ++ Utility.compress document.title ++ "." ++ shortHash

makeInitiaSlug : String -> String -> Uuid -> String
makeInitiaSlug title authorIdentifier identifier =
    let
      shortHash = Uuid.toString identifier  |> String.right 6
    in
    authorIdentifier ++ "." ++ Utility.compress title ++ "." ++ shortHash

{-|

    > create "jxxcarlson" "Intro to Chromaticity" t "First draft ..."
    --> {   authorIdentifier = "jxxcarlson", children = [], content = "First draft ..."
          , id = "jxxcarlson.intro-to-chromaticity.1568667528"
          , public = False, tags = [], timeCreated = Posix 1568667528000
          , timeUpdated = Posix 1568667528000, title = "Intro to Chromaticity" }
        : Document

-}
create : Uuid -> String -> String -> Posix -> String -> Document
create documentUuid authorIdentifier title time content =
   let
       slug = makeInitiaSlug title authorIdentifier documentUuid
   in
     {  id = documentUuid
      , title = title
      , authorIdentifier = authorIdentifier
      , content = content
      , tags  = [ ]
      , public = False
      , slug = slug


     }

{-|
  Replace by the target document any occurrence of a document in
  the documentList whose id is the same as taht of the target document.
  It is assumed, but not enfornced, that document ids are unique.
-}
replaceInList : Document -> List Document -> List Document
replaceInList targetDocument documentList =
--   let
--       targetDocument = updateMetaData targetDocument_
--    in
    List.Extra.setIf (\doc -> doc.id == targetDocument.id) targetDocument documentList


updateMetaData : Document -> Document
updateMetaData document =
      { document | slug = Debug.log "SLUG" (makeSlug document), title = newTitle document}


newTitle : Document -> String
newTitle document =
    case  getHeading document  of
       Nothing -> document.title
       Just newTitle_ -> newTitle_



getHeading: Document -> Maybe String
getHeading document =
   case  Parser.run parseHeading document.content of
       Ok result -> String.dropLeft 1 result |> String.trim |> Just
       Err _ -> Nothing

parseHeading : Parser String
parseHeading =
  (getChompedString <|
    succeed identity
        |. parseWhile (\c -> c /= '#')
        |. symbol "#"
        |. symbol " "
        |. parseWhile (\c -> c /= '\n'))
    |> Parser.map String.trim

parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    chompWhile accepting |> getChompedString





