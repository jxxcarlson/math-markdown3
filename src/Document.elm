module Document exposing (Document, setContent, getContent)

import Time exposing(Posix)

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


setContent : String -> Document -> Document
setContent str document =
    { document | content = str }

getContent : Maybe Document -> String
getContent maybeDocument =
    case maybeDocument of
        Just document -> document.content
        Nothing -> ""


