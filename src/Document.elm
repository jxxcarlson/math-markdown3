module Document exposing (Document, setContent, getContent, documentIdentifier
  , create, replaceInList, getHeading, updateTitle, footer, slug)

import Time exposing(Posix)
import Utility
import List.Extra
import Parser exposing(Parser, getChompedString, succeed, symbol, chompWhile, (|.))
import Prng.Uuid as Uuid exposing(Uuid)


type alias Document = {
     id : Uuid
   , identifier : String
   , title : String
   , authorIdentifier: String
   , content : String
   , public: Bool
   , tags : List String
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
    ++ "Document slug: " ++ slug document ++ "\n"
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

slug : Document -> String
slug document =
    let
      timeCreated = String.split "." document.identifier
        |> List.reverse
        |> List.head
        |> Maybe.andThen String.toInt
        |> Maybe.withDefault 0

    in
    document.authorIdentifier ++ "." ++ Utility.compress document.title ++ "." ++ Utility.intSlug timeCreated


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
     {  id = documentUuid
      , identifier =  documentIdentifier authorIdentifier title time
      , title = title
      , authorIdentifier = authorIdentifier
      , content = content
      , tags  = [ ]
      , public = False


     }

{-|
  Replace by the target document any occurrence of a document in
  the documentList whose id is the same as taht of the target document.
  It is assumed, but not enfornced, that document ids are unique.
-}
replaceInList : Document -> List Document -> List Document
replaceInList targetDocument_ documentList =
   let
       targetDocument = updateTitle targetDocument_
    in
    List.Extra.setIf (\doc -> doc.identifier == targetDocument.identifier) targetDocument documentList


updateTitle : Document -> Document
updateTitle document =
    case  getHeading document  of
        Nothing -> document
        Just newTitle ->
         if newTitle /= "" then
            { document | title = newTitle }
         else
            document


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





