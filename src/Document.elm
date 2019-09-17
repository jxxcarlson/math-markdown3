module Document exposing (Document, setContent, getContent, documentId
  , create, replaceInList)

import Time exposing(Posix)
import Utility
import List.Extra

type alias DocumentID = String

type alias Document = {
     id : DocumentID
   , title : String
   , authorID: String
   , content : String
   , tags : List String
   , timeCreated : Posix
   , timeUpdated: Posix
   , public: Bool
   , children: List DocumentID


  }


setContent : Posix -> String -> Document -> Document
setContent time str document =
    { document | content = str, timeUpdated = time }


getContent : Maybe Document -> String
getContent maybeDocument =
    case maybeDocument of
        Just document ->
           let
              footer = "\n\n___\n\n>> Author: "
                ++ document.authorID ++ "\n"
                ++ "Document ID: " ++ document.id ++ "\n"
                ++ "Created: " ++ Utility.humanDateUTC document.timeCreated ++ " UTC\n"
                ++ "Last modified: " ++ Utility.humanDateUTC document.timeUpdated ++ " UTC\n"
                ++ "Tags: " ++ String.join ", " document.tags

           in
            document.content ++ footer
        Nothing -> ""



{-|
    > t = Time.millisToPosix (1568667528 * 1000)
    Posix 1568667528000 : Time.Posix
    > title = "Introduction to Quantum Mechanics"
    > documentId "jxxcarlson" title t
    "jxxcarlson.introduction-to-quantum-mechanics.1568667528"

-}
documentId : String -> String -> Posix -> String
documentId authorID title time =
    [authorID, Utility.normalize title, Utility.stringOfPosix time]
      |> String.join "."

{-|

    > create "jxxcarlson" "Intro to Chromaticity" t "First draft ..."
    --> {   authorID = "jxxcarlson", children = [], content = "First draft ..."
          , id = "jxxcarlson.intro-to-chromaticity.1568667528"
          , public = False, tags = [], timeCreated = Posix 1568667528000
          , timeUpdated = Posix 1568667528000, title = "Intro to Chromaticity" }
        : Document

-}
create : String -> String -> Posix -> String -> Document
create authorID title time content =
     {  id =  documentId authorID title time
      , title = title
      , authorID = authorID
      , content = content
      , tags  = [ ]
      , timeCreated = time
      , timeUpdated = time
      , public = False
      , children = []


     }

{-|
  Replace by the target document any occurrence of a document in
  the documentList whose id is the same as taht of the target document.
  It is assumed, but not enfornced, that document ids are unique.
-}
replaceInList : Document -> List Document -> List Document
replaceInList targetDocument documentList =
    List.Extra.setIf (\doc -> doc.id == targetDocument.id) targetDocument documentList