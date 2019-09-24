module Request exposing (RequestMsg(..), documentsByAuthor)

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
--       GotNewDocument (RemoteData (Graphql.Http.Error Document) Document)
       GotUserDocuments (RemoteData (Graphql.Http.Error (Maybe (List Document))) (Maybe (List Document)))
--     | ConfirmUpdatedDocument (RemoteData (Graphql.Http.Error (Maybe Document)) (Maybe Document))
--     | ConfirmUDeleteDocument (RemoteData (Graphql.Http.Error (Maybe Document)) (Maybe Document))


endpoint = "https://math-markdown-heroku.herokuapp.com/v1/graphql"
authorizationToken =  "x-hasura-admin-secret: abc"

-- DOCUMENT REQUESTS

--createDocument : Document -> Cmd RequestMsg
--createDocument document =
--    Mutation.insert_document identity documentSelectionSet
--         |> Graphql.Http.mutationRequest endpoint
--         |> Graphql.Http.withHeader "authorization" authorizationToken         -- |> Graphql.Http.withHeader "Access-Control-Allow-Origin:" "*"
--         |> Graphql.Http.send (RemoteData.fromResult >> GotNewDocument)


documentsByAuthor : String -> Cmd RequestMsg
documentsByAuthor authorId =
    Query.documentsByAuthor  { author = authorId } (SelectionSet.list [documentSelectionSet])
      |> Graphql.Http.queryRequest endpoint
      |> Graphql.Http.withHeader "authorization" authorizationToken
      |> Graphql.Http.send (RemoteData.fromResult >> GotUserDocuments)

--
--update_document : Document -> Cmd RequestMsg
--update_document document =
--    Mutation.update_document (updatedDocumentRequiredArguments document) documentSelectionSet
--         |> Graphql.Http.mutationRequest endpoint
--         |> Graphql.Http.withHeader "authorization" authorizationToken
--         |> Graphql.Http.send (RemoteData.fromResult >> ConfirmUpdatedDocument)
--
--
--delete_document : Document -> Cmd RequestMsg
--delete_document document =
--    Mutation.delete_document { id = document.id } documentSelectionSet
--         |> Graphql.Http.mutationRequest endpoint
--         |> Graphql.Http.withHeader "authorization" authorizationToken
--         |> Graphql.Http.send (RemoteData.fromResult >> ConfirmUDeleteDocument)

-- IMPLEMENTATION

--
--updatedDocumentRequiredArguments : Document -> Mutation.UpdateDocumentRequiredArguments
--updatedDocumentRequiredArguments document =
--    { id = document.id
--    , data = documentInput document }
--
--createDocumentRequiredArguments : Document -> Mutation.InsertDocumentRequiredArguments
--createDocumentRequiredArguments document =
--    { data = documentInput document }
--
--documentInput : Document -> InputObject.Document_insert_input
--documentInput document =
--        {   id = document.id
--          , identifier = document.identifier
--          , title = document.title
--          , authorIdentifier = document.authorIdentifier
--          , content = document.content
--          , tags = document.tags
--          , timeCreated = Time.posixToMillis document.timeCreated // 1000
--          , timeUpdated = Time.posixToMillis document.timeUpdated // 1000
--          , public = document.public
--        }
--
--secondsToPosix : Int -> Time.Posix
--secondsToPosix =
--    (Time.millisToPosix << ((*) 1000))
--
--documentSelectionSet =
--    SelectionSet.succeed Document
--        |> with Api.Object.Document.id
--        |> with Api.Object.Document.identifier
--        |> with Api.Object.Document.title
--        |> with Api.Object.Document.authorIdentifier
--        |> with Api.Object.Document.content
--        |> with Api.Object.Document.tags
--        |> with (SelectionSet.map secondsToPosix Api.Object.Document.timeCreated)
--        |> with (SelectionSet.map secondsToPosix Api.Object.Document.timeUpdated)
--        |> with Api.Object.Document.public
--
--
