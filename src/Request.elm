module Request exposing
    ( GraphQLResponse(..)
    , RequestMsg(..)
    , deleteDocument
    , documentsByAuthor
    , documentsByAuthorAndTitle
    , insertDocument
    , publicDocuments
    , publicDocumentsByTitle
    , updateDocument
    )

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
import Api.Mutation
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
import Api.Query as Query exposing (DocumentOptionalArguments)
import CustomScalarCodecs exposing (Jsonb(..))
import Document exposing (Document)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Prng.Uuid exposing (Uuid(..))
import RemoteData exposing (RemoteData)



-- MSG --


type RequestMsg
    = GotUserDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotPublicDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | InsertDocumentResponse (GraphQLResponse (Maybe MutationResponse))
    | UpdateDocumentResponse (GraphQLResponse (Maybe MutationResponse))
    | DeleteDocumentResponse (GraphQLResponse (Maybe MutationResponse))



-- PARAMETERS --


endpoint =
    "https://math-markdown-heroku.herokuapp.com/v1/graphql"



-- CMD: Top level, exported --


documentsByAuthor : String -> String -> Cmd RequestMsg
documentsByAuthor authToken authorIdentifier =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (hasAuthor authorIdentifier))
        (RemoteData.fromResult >> GotUserDocuments)



--documentsByAuthorAndTitle : String -> String -> Cmd RequestMsg
--documentsByAuthorAndTitle authToken key =
--    makeGraphQLQuery authToken
--        (fetchDocumentsQuery (hasTitle ("%" ++ key ++ "%")))
--        (RemoteData.fromResult >> GotUserDocuments)


documentsByAuthorAndTitle : String -> String -> String -> Cmd RequestMsg
documentsByAuthorAndTitle authToken authorIdentifier titleKey =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (hasAuthorAndTitle authorIdentifier ("%" ++ titleKey ++ "%")))
        (RemoteData.fromResult >> GotUserDocuments)


publicDocuments : String -> Cmd RequestMsg
publicDocuments authToken =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery isPublic)
        (RemoteData.fromResult >> GotPublicDocuments)


publicDocumentsByTitle : String -> String -> Cmd RequestMsg
publicDocumentsByTitle authToken titleKey =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (isPublicAndTitle ("%" ++ titleKey ++ "%")))
        (RemoteData.fromResult >> GotPublicDocuments)


insertDocument : String -> Document -> Cmd RequestMsg
insertDocument authToken newDocument =
    makeMutation (getDocumentInsertObject newDocument) authToken


updateDocument : String -> Document -> Cmd RequestMsg
updateDocument authToken document =
    makeUpdateDocumentMutation (getDocumentUpdateObject document) authToken


deleteDocument : String -> Document -> Cmd RequestMsg
deleteDocument authToken document =
    makeDeleteDocumentMutation (getDocumentDeleteObject document) authToken



-- HELPERS --


makeUpdateDocumentMutation : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd RequestMsg
makeUpdateDocumentMutation mutation authToken =
    makeGraphQLMutation authToken mutation (RemoteData.fromResult >> GraphQLResponse >> UpdateDocumentResponse)


makeDeleteDocumentMutation : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd RequestMsg
makeDeleteDocumentMutation mutation authToken =
    makeGraphQLMutation authToken mutation (RemoteData.fromResult >> GraphQLResponse >> DeleteDocumentResponse)


getAuthHeader : String -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
getAuthHeader token =
    Graphql.Http.withHeader "x-hasura-admin-secret" token


makeGraphQLQuery : String -> SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGraphQLQuery authToken query decodesTo =
    query
        |> Graphql.Http.queryRequest endpoint
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
        |> Graphql.Http.mutationRequest endpoint
        {-
           mutationRequest signature is of the form
               String -> SelectionSet decodesTo RootMutation -> Request decodesTo
               url    -> SelectionSet TasksWUser RootMutation -> Request TasksWUser
        -}
        |> getAuthHeader authToken
        |> Graphql.Http.send decodesTo


fetchDocumentsQuery : OptionalArgument Document_bool_exp -> SelectionSet (List Document) RootQuery
fetchDocumentsQuery doc_bool_exp =
    Query.document (documentListOptionalArgument doc_bool_exp) documentListSelection


documentListOptionalArgument : OptionalArgument Document_bool_exp -> DocumentOptionalArguments -> DocumentOptionalArguments
documentListOptionalArgument doc_bool_exp optionalArgs =
    { optionalArgs | where_ = doc_bool_exp }



-- DOCUMENT BOOLEAN EXPRESSIONS --


isPublic : OptionalArgument Document_bool_exp
isPublic =
    Present <| isPublic_


isPublic_ : Document_bool_exp
isPublic_ =
    buildDocument_bool_exp (\args -> { args | public = equalToBoolean True })


hasAuthor : String -> OptionalArgument Document_bool_exp
hasAuthor author =
    Present <| hasAuthor_ author


hasAuthor_ : String -> Document_bool_exp
hasAuthor_ author =
    buildDocument_bool_exp (\args -> { args | authorIdentifier = equalToString author })


hasTitle : String -> OptionalArgument Document_bool_exp
hasTitle key =
    Present <| hasTitle_ key


hasTitle_ : String -> Document_bool_exp
hasTitle_ key =
    buildDocument_bool_exp (\args -> { args | title = likeString key })


hasAuthorAndTitle : String -> String -> OptionalArgument Document_bool_exp
hasAuthorAndTitle author titleKey =
    Present <| buildDocument_bool_exp (\args -> { args | and_ = Present <| [ Just <| hasAuthor_ author, Just <| hasTitle_ titleKey ] })


isPublicAndTitle : String -> OptionalArgument Document_bool_exp
isPublicAndTitle titleKey =
    Present <| buildDocument_bool_exp (\args -> { args | and_ = Present <| [ Just <| isPublic_, Just <| hasTitle_ titleKey ] })



--hasTag : String -> OptionalArgument Document_bool_exp
--hasTag key =
--    Present <| buildDocument_bool_exp (\args -> { args | tags = likeString key })


equalToString : String -> OptionalArgument String_comparison_exp
equalToString str =
    Present <| buildString_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present str })



--inStringList : String -> OptionalArgument String_comparison_exp
--inStringList str =
--    Present <| buildString_comparison_exp (\args -> { args | in_ = OptionalArgument.Present str })


likeString : String -> OptionalArgument String_comparison_exp
likeString str =
    Present <| buildString_comparison_exp (\args -> { args | ilike_ = OptionalArgument.Present str })


equalToBoolean : Bool -> OptionalArgument Boolean_comparison_exp
equalToBoolean bit =
    Present <| equalToBoolean_ bit


equalToBoolean_ : Bool -> Boolean_comparison_exp
equalToBoolean_ bit =
    buildBoolean_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present bit })



-- DOCUMENT --


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



-- DOCUMENT OBJECTS --


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
                , tags = Present (newDocument.tags |> (\list -> Jsonb list)) -- Present (newDocument.tags |> String.join ", ")
            }
        )


insertArgs : Document -> InsertDocumentRequiredArguments
insertArgs newDocument =
    InsertDocumentRequiredArguments [ insertDocumentObjects newDocument ]


getDocumentInsertObject : Document -> SelectionSet (Maybe MutationResponse) RootMutation
getDocumentInsertObject newDocument =
    insert_document identity (insertArgs newDocument) mutationResponseSelection


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


type alias DocumentToData =
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
                , tags = Present (document.tags |> (\list -> Jsonb list))
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
