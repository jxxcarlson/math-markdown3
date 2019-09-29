module Request exposing (GraphQLResponse(..), RequestMsg(..), deleteDocument, documentsByAuthor, insertDocument, publicDocuments, updateDocument)

import Api.Enum.Order_by exposing (Order_by(..))
import Api.InputObject
    exposing
        ( Boolean_comparison_exp
        , Document_bool_exp(..)
        , Document_bool_expOptionalFields
        , Document_insert_input
        , Document_set_input
        , String_comparison_exp
        , Uuid_comparison_exp
        , buildBoolean_comparison_exp
        , buildDocument_bool_exp
        , buildDocument_insert_input
        , buildDocument_set_input
        , buildString_comparison_exp
        , buildUuid_comparison_exp
        )
import Api.Mutation as Mutation
    exposing
        ( DeleteDocumentRequiredArguments
        , InsertDocumentRequiredArguments
        , UpdateDocumentOptionalArguments
        , UpdateDocumentRequiredArguments
        , delete_document
        , insert_document
        , update_document
        )
import Api.Object
import Api.Object.Document exposing (authorIdentifier)
import Api.Object.Document_mutation_response as DocumentMutation
import Api.Object.User
import Api.Query as Query exposing (DocumentOptionalArguments)
import CustomScalarCodecs exposing (Jsonb(..))
import Document exposing (Document)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Encode as Encode
import Prng.Uuid exposing (Uuid(..))
import RemoteData exposing (RemoteData)



-- MSG --


type RequestMsg
    = GotUserDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | InsertDocumentResponse (GraphQLResponse (Maybe MutationResponse))
    | UpdateDocumentResponse (GraphQLResponse (Maybe MutationResponse))
    | DeleteDocumentResponse (GraphQLResponse (Maybe MutationResponse))



-- Cmd RequestMsg --


documentsByAuthor : String -> String -> Cmd RequestMsg
documentsByAuthor authToken authorIdentifier =
    makeGraphQLQuery authToken
        (fetchUserDocumentsQuery authorIdentifier)
        (RemoteData.fromResult >> GotUserDocuments)


publicDocuments : String -> Cmd RequestMsg
publicDocuments authToken =
    makeGraphQLQuery authToken
        fetchPublicDocumentsQuery
        (RemoteData.fromResult >> GotUserDocuments)


insertDocument : String -> Document -> Cmd RequestMsg
insertDocument authToken newDocument =
    makeMutation (getDocumentInsertObject newDocument) authToken


updateDocument : String -> Document -> Cmd RequestMsg
updateDocument authToken document =
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


endpoint =
    "https://math-markdown-heroku.herokuapp.com/v1/graphql"


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


fetchPublicDocumentsQuery : SelectionSet (List Document) RootQuery
fetchPublicDocumentsQuery =
    Query.document publicDocumentListOptionalArgument documentListSelection


documentListOptionalArgument : String -> DocumentOptionalArguments -> DocumentOptionalArguments
documentListOptionalArgument author optionalArgs =
    { optionalArgs | where_ = hasAuthor author }


publicDocumentListOptionalArgument : DocumentOptionalArguments -> DocumentOptionalArguments
publicDocumentListOptionalArgument optionalArgs =
    { optionalArgs | where_ = isPublic }


isPublic : OptionalArgument Document_bool_exp
isPublic =
    Present <| buildDocument_bool_exp (\args -> { args | public = equalToBoolean True })


hasAuthor : String -> OptionalArgument Document_bool_exp
hasAuthor author =
    Present <| buildDocument_bool_exp (\args -> { args | authorIdentifier = equalToString author })



-- XXXX --


equalToString : String -> OptionalArgument String_comparison_exp
equalToString str =
    Present <| buildString_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present str })



-- XXX --


whereIsPublic : Bool -> OptionalArgument Document_bool_exp
whereIsPublic isPublic_ =
    Present <| buildDocument_bool_exp (\args -> { args | public = equalToBoolean isPublic_ })


equalToBoolean : Bool -> OptionalArgument Boolean_comparison_exp
equalToBoolean isPublic_ =
    Present <| buildBoolean_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present isPublic_ })


documentListSelection : SelectionSet Document Api.Object.Document
documentListSelection =
    SelectionSet.map7 Document
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

                -- , tags = Present newDocument.tags
                -- , tags = Present (newDocument.tags |> String.join ", " |> Encode.string)
            }
        )


insertArgs : Document -> InsertDocumentRequiredArguments
insertArgs newDocument =
    InsertDocumentRequiredArguments [ insertDocumentObjects newDocument ]


getDocumentInsertObject : Document -> SelectionSet (Maybe MutationResponse) RootMutation
getDocumentInsertObject newDocument =
    insert_document identity (insertArgs newDocument) mutationResponseSelection



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


setDocumentSetArg : Document -> Document_set_input
setDocumentSetArg document =
    buildDocument_set_input
        (\args ->
            { args
                | content = OptionalArgument.Present document.content
                , title = OptionalArgument.Present document.title
                , slug = OptionalArgument.Present document.slug
                , public = OptionalArgument.Present document.public

                -- , tags = OptionalArgument.Present document.tags
            }
        )


setDocumentUpdateOptionalArgs : Document -> UpdateDocumentOptionalArguments -> UpdateDocumentOptionalArguments
setDocumentUpdateOptionalArgs document optionalArgs =
    { optionalArgs
        | set_ = Present (setDocumentSetArg document)
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
