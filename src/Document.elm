module Document exposing (Document, setContent, getContent, documentIdentifier
  , create, replaceInList, getHeading, footer, slug)

import Time exposing(Posix)
import Utility
import List.Extra
import Parser exposing(Parser, getChompedString, succeed, symbol, chompWhile, (|.))

import Api.Scalar exposing(Id(..))

type alias DocumentID = String

type alias Document = {
     id : Int
   , identifier : String
   , title : String
   , authorIdentifier: String
   , content : String
   , tags : List String
   , timeCreated : Posix
   , timeUpdated: Posix
   , public: Bool

  }


setContent : Posix -> String -> Document -> Document
setContent time str document =
    { document | content = str, timeUpdated = time }


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
    ++ "Created: " ++ Utility.humanDateUTC document.timeCreated ++ " UTC\n"
    ++ "Last modified: " ++ Utility.humanDateUTC document.timeUpdated ++ " UTC\n"
    ++ "Tags: " ++ String.join ", " document.tags ++ "\n"
    ++ "Words: " ++ Utility.wordCount document.content
    ++ "\n" ++ "````" ++ "\n\n"

{-|
    > t = Time.millisToPosix (1568667528 * 1000)
    Posix 1568667528000 : Time.Posix
    > title = "Introduction to Quantum Mechanics"
    > documentId "jxxcarlson" title t
    "jxxcarlson.introduction-to-quantum-mechanics.1568667528"

-}
documentIdentifier : String -> String -> Posix -> String
documentIdentifier authorIdentifier title time =
    [authorIdentifier, Utility.normalize title, Utility.stringOfPosix time]
      |> String.join "."

slug : Document -> String
slug document =
    document.authorIdentifier ++ "." ++ Utility.compress document.title ++ "." ++ Utility.posixSlug document.timeCreated


{-|

    > create "jxxcarlson" "Intro to Chromaticity" t "First draft ..."
    --> {   authorIdentifier = "jxxcarlson", children = [], content = "First draft ..."
          , id = "jxxcarlson.intro-to-chromaticity.1568667528"
          , public = False, tags = [], timeCreated = Posix 1568667528000
          , timeUpdated = Posix 1568667528000, title = "Intro to Chromaticity" }
        : Document

-}
create : String -> String -> Posix -> String -> Document
create authorIdentifier title time content =
     {  id = 0
      , identifier =  documentIdentifier authorIdentifier title time
      , title = title
      , authorIdentifier = authorIdentifier
      , content = content
      , tags  = [ ]
      , timeCreated = time
      , timeUpdated = time
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





