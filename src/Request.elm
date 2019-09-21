module Request exposing (RequestMsg(..), createDocument, documentsByAuthor, updateDocument, deleteDocument)

import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Api.Mutation as Mutation
import Api.Query as Query
import RemoteData exposing (RemoteData)
import Document exposing(Document)
import Api.InputObject as InputObject
import Api.Scalar
import Api.Object.Document
import Time
import Api.Scalar exposing(Id(..))


type RequestMsg =
       GotNewDocument (RemoteData (Graphql.Http.Error Document) Document)
     | GotUserDocuments (RemoteData (Graphql.Http.Error (Maybe (List Document))) (Maybe (List Document)))
     | ConfirmUpdatedDocument (RemoteData (Graphql.Http.Error (Maybe Document)) (Maybe Document))
     | ConfirmUDeleteDocument (RemoteData (Graphql.Http.Error (Maybe Document)) (Maybe Document))


endpoint = "https://graphql.fauna.com/graphql"
authorizationToken = "Basic Zm5BRFlteWZIbUFDRW8yNlRVSUI0WXM1cVRqbEpUbVNrci1MQmxIbjo3Y2NmMGU2Ni01MzllLTRjZGQtODBhZS0xOGIyNGFlOWFlMDY6c2VydmVy"


-- DOCUMENT REQUESTS

createDocument : Document -> Cmd RequestMsg
createDocument document =
    Mutation.createDocument (createDocumentRequiredArguments document) documentSelectionSet
         |> Graphql.Http.mutationRequest endpoint
         |> Graphql.Http.withHeader "authorization" authorizationToken         -- |> Graphql.Http.withHeader "Access-Control-Allow-Origin:" "*"
         |> Graphql.Http.send (RemoteData.fromResult >> GotNewDocument)


documentsByAuthor : String -> Cmd RequestMsg
documentsByAuthor authorId =
    Query.documentsByAuthor  { author = authorId } (SelectionSet.list [documentSelectionSet])
      |> Graphql.Http.queryRequest endpoint
      |> Graphql.Http.withHeader "authorization" authorizationToken
      |> Graphql.Http.send (RemoteData.fromResult >> GotUserDocuments)


updateDocument : Document -> Cmd RequestMsg
updateDocument document =
    Mutation.updateDocument (updatedDocumentRequiredArguments document) documentSelectionSet
         |> Graphql.Http.mutationRequest endpoint
         |> Graphql.Http.withHeader "authorization" authorizationToken
         |> Graphql.Http.send (RemoteData.fromResult >> ConfirmUpdatedDocument)


deleteDocument : Document -> Cmd RequestMsg
deleteDocument document =
    Mutation.deleteDocument { id = document.id } documentSelectionSet
         |> Graphql.Http.mutationRequest endpoint
         |> Graphql.Http.withHeader "authorization" authorizationToken
         |> Graphql.Http.send (RemoteData.fromResult >> ConfirmUDeleteDocument)

-- IMPLEMENTATION


updatedDocumentRequiredArguments : Document -> Mutation.UpdateDocumentRequiredArguments
updatedDocumentRequiredArguments document =
    { id = document.id
    , data = documentInput document }

createDocumentRequiredArguments : Document -> Mutation.CreateDocumentRequiredArguments
createDocumentRequiredArguments document =
    { data = documentInput document }

documentInput : Document -> InputObject.DocumentInput
documentInput document =
        {   identifier = document.identifier
          , title = document.title
          , author = document.authorID
          , content = document.content
          , tags = document.tags
          , timeCreated = Time.posixToMillis document.timeCreated // 1000
          , timeUpdated = Time.posixToMillis document.timeUpdated // 1000
          , public = document.public
        }

secondsToPosix : Int -> Time.Posix
secondsToPosix =
    (Time.millisToPosix << ((*) 1000))

documentSelectionSet =
    SelectionSet.succeed Document
        |> with Api.Object.Document.id_
        |> with Api.Object.Document.identifier
        |> with Api.Object.Document.title
        |> with Api.Object.Document.author
        |> with Api.Object.Document.content
        |> with Api.Object.Document.tags
        |> with (SelectionSet.map secondsToPosix Api.Object.Document.timeCreated)
        |> with (SelectionSet.map secondsToPosix Api.Object.Document.timeUpdated)
        |> with Api.Object.Document.public





