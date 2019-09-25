module Request exposing (RequestMsg(..), fetchUserSummaryDocuments)

import RemoteData exposing(RemoteData)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Document exposing(Document, SummaryDocument)

import Api.Enum.Order_by exposing (Order_by(..))

import Api.InputObject exposing (Boolean_comparison_exp, Document_bool_exp(..), Document_bool_expOptionalFields, String_comparison_exp, buildBoolean_comparison_exp, buildString_comparison_exp, buildDocument_bool_exp)

import Api.Object
import Api.Object.User
import Api.Object.Document exposing (authorIdentifier)
import Api.Query as Query exposing (DocumentOptionalArguments)





type RequestMsg =
--       GotNewDocument (RemoteData (Graphql.Http.Error Document) Document)
       -- GotUserDocuments (RemoteData (Graphql.Http.Error (Maybe (List Document))) (Maybe (List Document)))
        GotUserSummaryDocuments (RemoteData (Graphql.Http.Error (List SummaryDocument)) (List SummaryDocument))
--     | ConfirmUpdatedDocument (RemoteData (Graphql.Http.Error (Maybe Document)) (Maybe Document))
--     | ConfirmUDeleteDocument (RemoteData (Graphql.Http.Error (Maybe Document)) (Maybe Document))


-- GENERAL --

graphql_url : String
graphql_url =
    "https://math-markdown-heroku.herokuapp.com/v1/graphql"

getAuthHeader : String -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
getAuthHeader token =

    Graphql.Http.withHeader "Authorization" ("Bearer " ++ token)


makeGraphQLQuery : String -> SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGraphQLQuery authToken query decodesTo =
    query
        |> Graphql.Http.queryRequest graphql_url
        {-
           queryRequest signature is of the form
               String -> SelectionSet decodesTo RootQuery -> Request decodesTo
               url    -> SelectionSet TasksWUser RootQuery -> Request TasksWUser
        -}
        |> getAuthHeader authToken
        |> Graphql.Http.send decodesTo

-- Cmd RequestMsg --

fetchUserSummaryDocuments: String -> String -> Cmd RequestMsg
fetchUserSummaryDocuments authToken authorIdentifier =
    makeGraphQLQuery authToken
        (fetchUserSummaryDocumentsQuery authorIdentifier)
        (RemoteData.fromResult >> GotUserSummaryDocuments)


-- Summary Document Helpers --


fetchUserSummaryDocumentsQuery : String -> SelectionSet (List SummaryDocument) RootQuery
fetchUserSummaryDocumentsQuery author =
    Query.document (documentListOptionalArgument author) documentListSelection


documentListOptionalArgument : String -> DocumentOptionalArguments -> DocumentOptionalArguments
documentListOptionalArgument author optionalArgs =
    { optionalArgs | where_ = hasAuthor author }


hasAuthor : String -> OptionalArgument Document_bool_exp
hasAuthor author =
    Present <| buildDocument_bool_exp (\args -> { args | authorIdentifier = equalToString author })

equalToString : String -> OptionalArgument String_comparison_exp
equalToString  str =
    Present <| buildString_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present str})

-- XXXX --

whereIsPublic : Bool -> OptionalArgument Document_bool_exp
whereIsPublic isPublic =
    Present <| buildDocument_bool_exp (\args -> { args | public = equalToBoolean isPublic })


equalToBoolean : Bool -> OptionalArgument Boolean_comparison_exp
equalToBoolean isPublic =
    Present <| buildBoolean_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present isPublic })


-- C --

documentListSelection : SelectionSet SummaryDocument Api.Object.Document
documentListSelection =
    SelectionSet.map4 SummaryDocument
        Api.Object.Document.id
        Api.Object.Document.identifier
        Api.Object.Document.title
        Api.Object.Document.authorIdentifier


-- C --

endpoint = "https://math-markdown-heroku.herokuapp.com/v1/graphql"
authorizationToken =  "x-hasura-admin-secret: abc"

-- DOCUMENT REQUESTS

--createDocument : Document -> Cmd RequestMsg
--createDocument document =
--    Mutation.insert_document identity documentSelectionSet
--         |> Graphql.Http.mutationRequest endpoint
--         |> Graphql.Http.withHeader "authorization" authorizationToken         -- |> Graphql.Http.withHeader "Access-Control-Allow-Origin:" "*"
--         |> Graphql.Http.send (RemoteData.fromResult >> GotNewDocument)


--documentsByAuthor : String -> Cmd RequestMsg
--documentsByAuthor author =
--    Query.document  { author = author } (SelectionSet.list [documentSelectionSet])
--      |> Graphql.Http.queryRequest endpoint
--      |> Graphql.Http.withHeader "authorization" authorizationToken
--      |> Graphql.Http.send (RemoteData.fromResult >> GotUserDocuments)
--

--equalToString : String -> OptionalArgument Boolean_comparison_exp
--equalToString str =
--    Present <| buildBoolean_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present str })



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
