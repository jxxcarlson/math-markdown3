module Request exposing (RequestMsg(..), createDocument)

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


endpoint = "https://graphql.fauna.com/graphql"
authorizationToken = "Basic Zm5BRFlteWZIbUFDRW8yNlRVSUI0WXM1cVRqbEpUbVNrci1MQmxIbjo3Y2NmMGU2Ni01MzllLTRjZGQtODBhZS0xOGIyNGFlOWFlMDY6c2VydmVy"

createDocument : Document -> Cmd RequestMsg
createDocument document =
    Mutation.createDocument (createDocumentRequiredArguments document) documentSelectionSet
         |> Graphql.Http.mutationRequest endpoint
         |> Graphql.Http.withHeader "authorization" authorizationToken
         |> Graphql.Http.send (RemoteData.fromResult >> GotNewDocument)



--
--updateDocument : Document -> Cmd RequestMsg
--updateDocument document =
--    Mutation.updateDocument (createDocumentRequiredArguments document) documentSelectionSet
--         |> Graphql.Http.mutationRequest endpoint
--         |> Graphql.Http.withHeader "authorization" "Basic Zm5BRFlmWC1wakFDQVJ2a0RoaFU1UmhDaWc5TVVFQUpBNFBpMTFhSDo3Y2NmMGU2Ni01MzllLTRjZGQtODBhZS0xOGIyNGFlOWFlMDY6c2VydmVy"
--         |> Graphql.Http.send (RemoteData.fromResult >> GotNewDocument)

-- findAllDocumentsByAuthor : String -> Cmd RequestMsg

-- IMPLEMENTATION


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

-- documentSelectionSet : SelectionSet Document b
documentSelectionSet =
    SelectionSet.succeed Document
        |> with Api.Object.Document.identifier
        |> with Api.Object.Document.title
        |> with Api.Object.Document.author
        |> with Api.Object.Document.content
        |> with Api.Object.Document.tags
        |> with (SelectionSet.map secondsToPosix Api.Object.Document.timeCreated)
        |> with (SelectionSet.map secondsToPosix Api.Object.Document.timeUpdated)
        |> with Api.Object.Document.public


stringOfId : Id -> String
stringOfId (Id str) = str



