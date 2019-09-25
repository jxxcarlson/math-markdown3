module Request exposing (RequestMsg(..), documentsByAuthor)

import RemoteData exposing(RemoteData)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Document exposing(Document, Document)
import CustomScalarCodecs exposing(Jsonb(..))

import Api.Enum.Order_by exposing (Order_by(..))

import Api.InputObject exposing (
   Boolean_comparison_exp
 , Document_bool_exp(..)
 , Document_bool_expOptionalFields
 , String_comparison_exp
 , buildBoolean_comparison_exp
 , buildString_comparison_exp
 , buildDocument_bool_exp
 , Document_insert_input
 , buildDocument_insert_input )

import Api.Object
import Api.Object.User
import Api.Object.Document exposing (authorIdentifier)
import Api.Query as Query exposing (DocumentOptionalArguments)
import Api.Mutation as Mutation
import Api.Object.Document_mutation_response as DocumentMutation

import CustomScalarCodecs





type RequestMsg =
        GotNewDocument (RemoteData (Graphql.Http.Error Document) Document)
       | GotUserDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
--     | ConfirmUpdatedDocument (RemoteData (Graphql.Http.Error (Maybe Document)) (Maybe Document))
--     | ConfirmUDeleteDocument (RemoteData (Graphql.Http.Error (Maybe Document)) (Maybe Document))


-- GENERAL --

endpoint = "https://math-markdown-heroku.herokuapp.com/v1/graphql"

graphql_url : String
graphql_url =
    "https://math-markdown-heroku.herokuapp.com/v1/graphql"

getAuthHeader : String -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
getAuthHeader token =
    Graphql.Http.withHeader "x-hasura-admin-secret" token


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


makeGraphQLMutation : String -> SelectionSet decodesTo RootMutation -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGraphQLMutation authToken query decodesTo =
    query
        |> Graphql.Http.mutationRequest graphql_url
        {-
           mutationRequest signature is of the form
               String -> SelectionSet decodesTo RootMutation -> Request decodesTo
               url    -> SelectionSet TasksWUser RootMutation -> Request TasksWUser
        -}
        |> getAuthHeader authToken
        |> Graphql.Http.send decodesTo

-- Cmd RequestMsg --

documentsByAuthor: String -> String -> Cmd RequestMsg
documentsByAuthor authToken authorIdentifier =
    makeGraphQLQuery authToken
        (fetchUserSummaryDocumentsQuery authorIdentifier)
        (RemoteData.fromResult >> GotUserDocuments)


-- Summary Document Helpers --


fetchUserSummaryDocumentsQuery : String -> SelectionSet (List Document) RootQuery
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

documentListSelection: SelectionSet Document Api.Object.Document
documentListSelection =
    SelectionSet.map7  Document
        Api.Object.Document.id
        Api.Object.Document.identifier
        Api.Object.Document.title
        Api.Object.Document.authorIdentifier
        Api.Object.Document.content
        Api.Object.Document.public
        (Api.Object.Document.tags identity |> SelectionSet.map (\(Jsonb x) -> x))


---- D --
--
--insertDocument : Document -> Document_insert_input
--insertDocument newDocument =
--    buildDocument_insert_input
--        (\args ->
--            { args
--                | id = Present newDocument.id
--                , identifier = Present newDocument.identifier
--                , title = Present newDocument.title
--                , authorIdentifier = Present newDocument.authorIdentifier
--                , content = Present newDocument.content
--                , public = Present newDocument.public
--                , tags = Present newDocument.tags
--            }
--        )
--
--insertArgs : String -> Bool -> InsertTodosRequiredArguments
--insertArgs newTodo isPublic =
--
--    InsertTodosRequiredArguments [ insertTodoObjects newTodo isPublic ]
--
--getTodoListInsertObject : String -> Bool -> SelectionSet (Maybe MutationResponse) RootMutation
--
--getTodoListInsertObject newTodo isPublic =
--
--    insert_todos identity (insertArgs newTodo isPublic) mutationResponseSelection
--
--mutationResponseSelection : SelectionSet MutationResponse Hasura.Object.Todos_mutation_response
--
--mutationResponseSelection =
--
--    SelectionSet.map MutationResponse
--
--        TodosMutation.affected_rows
--
--makeMutation : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd Msg
--
--makeMutation mutation authToken =
--
--    makeGraphQLMutation authToken mutation (RemoteData.fromResult >> GraphQLResponse >> InsertPrivateTodoResponse)
----