module Request exposing (RequestMsg(..), GraphQLResponse(..), documentsByAuthor, insertDocument, updateDocument, deleteDocument)

import RemoteData exposing(RemoteData)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Document exposing(Document, Document)
import CustomScalarCodecs exposing(Jsonb(..))

import Api.Enum.Order_by exposing (Order_by(..))

import Prng.Uuid exposing(Uuid(..))


import Api.InputObject exposing (
   Boolean_comparison_exp
 , Document_bool_exp(..)
 , Document_bool_expOptionalFields
 , String_comparison_exp
 , buildBoolean_comparison_exp
 , buildString_comparison_exp
 , buildDocument_bool_exp
 , Document_insert_input
 , buildDocument_insert_input
 , Uuid_comparison_exp
 , buildUuid_comparison_exp
 , Document_set_input
 , buildDocument_set_input)

import Api.Object
import Api.Object.User
import Api.Object.Document exposing (authorIdentifier)
import Api.Query as Query exposing (DocumentOptionalArguments)
import Api.Mutation as Mutation exposing(
   InsertDocumentRequiredArguments
 , insert_document
 , update_document
 , delete_document
 , UpdateDocumentOptionalArguments
 , UpdateDocumentRequiredArguments
 , DeleteDocumentRequiredArguments
  )

import Api.Object.Document_mutation_response as DocumentMutation

import CustomScalarCodecs
import Json.Encode as Encode


-- MSG --


type RequestMsg =
         GotUserDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
       | InsertDocumentResponse (GraphQLResponse (Maybe MutationResponse))
       | UpdateDocumentResponse (GraphQLResponse (Maybe MutationResponse))
       | DeleteDocumentResponse (GraphQLResponse (Maybe MutationResponse))


-- Cmd RequestMsg --

documentsByAuthor: String -> String -> Cmd RequestMsg
documentsByAuthor authToken authorIdentifier =
    makeGraphQLQuery authToken
        (fetchUserDocumentsQuery authorIdentifier)
        (RemoteData.fromResult >> GotUserDocuments)

insertDocument : String -> Document -> Cmd RequestMsg
insertDocument authToken newDocument =
    makeMutation (getDocumentInsertObject newDocument) authToken


updateDocument : String -> Document -> Cmd RequestMsg
updateDocument authToken document  =
    makeUpdateDocumentMutation (getDocumentUpdateObject document) authToken


makeUpdateDocumentMutation : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd RequestMsg
makeUpdateDocumentMutation mutation authToken =
    makeGraphQLMutation authToken mutation (RemoteData.fromResult >> GraphQLResponse >> UpdateDocumentResponse)


deleteDocument : String -> Document -> Cmd RequestMsg
deleteDocument authToken document =
    makeDeleteDocumentMutation (getDocumentDeleteObject document) authToken

makeDeleteDocumentMutation : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd RequestMsg
makeDeleteDocumentMutation mutation authToken =
    makeGraphQLMutation authToken mutation (RemoteData.fromResult >> GraphQLResponse >> DeleteDocumentResponse)



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

-- Summary Document Helpers --


fetchUserDocumentsQuery : String -> SelectionSet (List Document) RootQuery
fetchUserDocumentsQuery author =
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
        Api.Object.Document.title
        Api.Object.Document.authorIdentifier
        Api.Object.Document.content
        Api.Object.Document.public
        (Api.Object.Document.tags identity |> SelectionSet.map (\(Jsonb x) -> x))
        Api.Object.Document.slug


-- INSERT DOCUMENT


insertDocumentObjects : Document -> Document_insert_input
insertDocumentObjects newDocument =
    buildDocument_insert_input
        (\args ->
            { args
                | id = Present newDocument.id
                , title = Present newDocument.title
                , authorIdentifier = Present newDocument.authorIdentifier
                , content = Present newDocument.content
                , public = Present newDocument.public
               -- , tags = Present (newDocument.tags |> String.join ", " |> Encode.string)
              --  , tags = Present newDocument.tags
            }
        )


insertArgs : Document -> InsertDocumentRequiredArguments
insertArgs newDocument =
    InsertDocumentRequiredArguments [ insertDocumentObjects newDocument ]

getDocumentInsertObject : Document -> SelectionSet (Maybe MutationResponse) RootMutation
getDocumentInsertObject newDocument =
    insert_document identity (insertArgs newDocument ) mutationResponseSelection

-- UP --

getDocumentUpdateObject : Document -> SelectionSet (Maybe MutationResponse) RootMutation
getDocumentUpdateObject document =
    update_document
       (setDocumentUpdateOptionalArgs document)
       (setDocumentUpdateWhere document.id)
       mutationResponseSelection

getDocumentDeleteObject : Document -> SelectionSet (Maybe MutationResponse) RootMutation
getDocumentDeleteObject document =
    delete_document
       (setDocumentDeleteWhere document.id)
       mutationResponseSelection


{-
delete_document :
     DeleteDocumentRequiredArguments
  -> SelectionSet decodesTo Api.Object.Document_mutation_response
  -> SelectionSet (Maybe decodesTo) RootMutation
delete_document requiredArgs object_ =
-}
mutationResponseSelection : SelectionSet MutationResponse Api.Object.Document_mutation_response
mutationResponseSelection =
    SelectionSet.map MutationResponse
        DocumentMutation.affected_rows


makeMutation : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd RequestMsg
makeMutation mutation authToken =
    makeGraphQLMutation authToken mutation (RemoteData.fromResult >> GraphQLResponse >> InsertDocumentResponse)


type alias MutationResponse =
    { affected_rows : Int

    }

type alias MaybeMutationResponse =
    Maybe MutationResponse

type GraphQLResponse decodesTo
    = GraphQLResponse (RemoteData (Graphql.Http.Error decodesTo) decodesTo)

-- UPDATE DOCUMENT

type alias DocumentoData =
    RemoteData (Graphql.Http.Error Document) Document

type alias UpdateDocumentResponse =
    RemoteData (Graphql.Http.Error (Maybe MutationResponse)) (Maybe MutationResponse)

--updateDocumentContent : Uuid -> String -> SelectionSet (Maybe MutationResponse) RootMutation
--updateDocumentContent documentId content =
--    Mutation.update_document (setDocumentUpdateArgs content) (setDocumentUpdateWhere documentId) mutationResponseSelection

setDocumentSetArg : Document -> Document_set_input
setDocumentSetArg document =
    buildDocument_set_input
        (\args ->
            { args
                |   content = OptionalArgument.Present document.content
                  , title = OptionalArgument.Present document.title
            }
        )

setDocumentUpdateOptionalArgs : Document ->  UpdateDocumentOptionalArguments -> UpdateDocumentOptionalArguments
setDocumentUpdateOptionalArgs document optionalArgs =
    { optionalArgs
        | set_ =  Present (setDocumentSetArg document)
    }

setDocumentValueForId : Uuid -> Uuid_comparison_exp
setDocumentValueForId uuid =
    buildUuid_comparison_exp
        (\args ->
            { args
                | eq_ = Present uuid
            }
        )

setDocumentUpdateWhere : Uuid -> UpdateDocumentRequiredArguments
setDocumentUpdateWhere uuid =
    UpdateDocumentRequiredArguments
        (buildDocument_bool_exp
            (\args ->
                { args
                    | id = Present (setDocumentValueForId uuid)
                }
            )
        )

setDocumentDeleteWhere : Uuid -> DeleteDocumentRequiredArguments
setDocumentDeleteWhere uuid =
    DeleteDocumentRequiredArguments
        (buildDocument_bool_exp
            (\args ->
                { args
                    | id = Present (setDocumentValueForId uuid)
                }
            )
        )
