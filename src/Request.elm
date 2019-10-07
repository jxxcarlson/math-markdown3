module Request exposing
    ( GraphQLResponse(..)
    , RequestHandler
    , RequestMsg(..)
    , deleteDocument
    , documentsInIdList
    , documentsWithAuthor
    , documentsWithAuthorAndTag
    , documentsWithAuthorAndTitle
    , insertDocument
    , publicDocuments
    , publicDocumentsWithTag
    , publicDocumentsWithTitle
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
        , Uuid_comparison_expOptionalFields
        , buildBoolean_comparison_exp
        , buildDocument_bool_exp
        , buildDocument_insert_input
        , buildDocument_set_input
        , buildJsonb_comparison_exp
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
import Document exposing (DocType(..), Document, MarkdownFlavor(..))
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Maybe.Extra
import Prng.Uuid as Uuid exposing (Uuid(..))
import RemoteData exposing (RemoteData)
import Utility



-- MSG --


type RequestMsg
    = GotUserDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotChildDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotCandidateChildDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotPublicDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | InsertDocumentResponse (GraphQLResponse (Maybe MutationResponse))
    | UpdateDocumentResponse (GraphQLResponse (Maybe MutationResponse))
    | DeleteDocumentResponse (GraphQLResponse (Maybe MutationResponse))


type alias RequestHandler =
    RemoteData (Graphql.Http.Error (List Document)) (List Document) -> RequestMsg



-- PARAMETERS --


endpoint =
    "https://math-markdown-heroku.herokuapp.com/v1/graphql"


documentsWithAuthorAndTag : String -> String -> String -> RequestHandler -> Cmd RequestMsg
documentsWithAuthorAndTag authToken author tag requestHandler =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (Present <| hasAuthorAndTag author tag))
        (RemoteData.fromResult >> requestHandler)



-- CMD: Top level, exported --


documentsInIdList : String -> List Uuid -> Cmd RequestMsg
documentsInIdList authToken uuiIdList =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (Present <| inUuidList uuiIdList))
        (RemoteData.fromResult >> GotChildDocuments)


documentsWithAuthorAndTag1 : String -> String -> String -> Cmd RequestMsg
documentsWithAuthorAndTag1 authToken author tag =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (Present <| hasAuthorAndTag author tag))
        (RemoteData.fromResult >> GotUserDocuments)


publicDocumentsWithTag : String -> String -> Cmd RequestMsg
publicDocumentsWithTag authToken tag =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (Present <| hasTagAndIsPublic tag))
        (RemoteData.fromResult >> GotUserDocuments)


documentsWithAuthor : String -> String -> Cmd RequestMsg
documentsWithAuthor authToken authorIdentifier =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (Present <| hasAuthor_ authorIdentifier))
        (RemoteData.fromResult >> GotUserDocuments)


documentsWithAuthorAndTitle : String -> String -> String -> RequestHandler -> Cmd RequestMsg
documentsWithAuthorAndTitle authToken authorIdentifier titleKey requestHandler =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (Present <| hasAuthorAndTitle authorIdentifier ("%" ++ titleKey ++ "%")))
        (RemoteData.fromResult >> requestHandler)


publicDocuments : String -> Cmd RequestMsg
publicDocuments authToken =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery <| Present <| isPublic_)
        (RemoteData.fromResult >> GotPublicDocuments)


publicDocumentsWithTitle : String -> String -> Cmd RequestMsg
publicDocumentsWithTitle authToken titleKey =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (Present <| isPublicAndTitle ("%" ++ titleKey ++ "%")))
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



-- BOOLEAN EXPRESSIONS --


isPublic_ : Document_bool_exp
isPublic_ =
    buildDocument_bool_exp (\args -> { args | public = Present <| equalToBoolean_ True })


hasAuthor_ : String -> Document_bool_exp
hasAuthor_ author =
    buildDocument_bool_exp (\args -> { args | authorIdentifier = Present <| equalToString author })


hasTitle_ : String -> Document_bool_exp
hasTitle_ key =
    buildDocument_bool_exp (\args -> { args | title = Present <| likeString key })


hasAuthorAndTitle : String -> String -> Document_bool_exp
hasAuthorAndTitle author titleKey =
    buildDocument_bool_exp (\args -> { args | and_ = Present <| [ Just <| hasAuthor_ author, Just <| hasTitle_ titleKey ] })


hasTagAndIsPublic : String -> Document_bool_exp
hasTagAndIsPublic tag =
    buildDocument_bool_exp (\args -> { args | and_ = Present <| [ Just <| isPublic_, Just <| hasTag tag ] })


hasAuthorAndTag : String -> String -> Document_bool_exp
hasAuthorAndTag author tag =
    buildDocument_bool_exp (\args -> { args | and_ = Present <| [ Just <| hasAuthor_ author, Just <| hasTag tag ] })


isPublicAndTitle : String -> Document_bool_exp
isPublicAndTitle titleKey =
    buildDocument_bool_exp (\args -> { args | and_ = Present <| [ Just <| isPublic_, Just <| hasTitle_ titleKey ] })


equalToString : String -> String_comparison_exp
equalToString str =
    buildString_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present str })


inUuidList : List Uuid -> Document_bool_exp
inUuidList uuiIdList =
    buildDocument_bool_exp (\args -> { args | id = Present <| inUuidList_ uuiIdList })


inUuidList_ : List Uuid -> Uuid_comparison_exp
inUuidList_ uuiIdList =
    buildUuid_comparison_exp (\args -> { args | in_ = OptionalArgument.Present uuiIdList })



-- XXXXX --


hasTag : String -> Document_bool_exp
hasTag key =
    buildDocument_bool_exp (\args -> { args | tags = Present <| hasTag_ key })


hasTag_ : String -> Api.InputObject.Jsonb_comparison_exp
hasTag_ x =
    buildJsonb_comparison_exp (\args -> { args | has_key_ = OptionalArgument.Present x })



-- XXXXX --


likeString : String -> String_comparison_exp
likeString str =
    buildString_comparison_exp (\args -> { args | ilike_ = OptionalArgument.Present str })


equalToBoolean_ : Bool -> Boolean_comparison_exp
equalToBoolean_ bit =
    buildBoolean_comparison_exp (\args -> { args | eq_ = OptionalArgument.Present bit })



-- DOCUMENT --


{-| Change this after running 'sh api.sh'
-}
documentListSelection : SelectionSet Document Api.Object.Document
documentListSelection =
    SelectionSet.succeed Document
        |> with Api.Object.Document.id
        |> with Api.Object.Document.title
        |> with Api.Object.Document.authorIdentifier
        |> with Api.Object.Document.content
        |> with Api.Object.Document.public
        |> with (Api.Object.Document.tags identity |> SelectionSet.map (\(Jsonb x) -> x))
        |> with Api.Object.Document.slug
        |> with (Api.Object.Document.docType |> SelectionSet.map Document.docTypeFromString)
        |> with
            (Api.Object.Document.children identity
                |> SelectionSet.map
                    (\(Jsonb x) -> List.map (Uuid.fromString >> Maybe.withDefault Utility.id0) x)
            )
        |> with
            (Api.Object.Document.childLevels identity
                |> SelectionSet.map
                    (\(Jsonb x) -> List.map (String.toInt >> Maybe.withDefault 0) x)
            )



-- DOCUMENT OBJECTS --


{-| Change this after running 'sh api.sh'
-}
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
                , docType = Present (newDocument.docType |> Document.stringFromDocType)
                , children = Present (newDocument.children |> List.map Uuid.toString |> (\list -> Jsonb list))
                , childLevels = Present (newDocument.childLevels |> List.map String.fromInt |> (\list -> Jsonb list))
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


{-| Change this after running 'sh api.sh'
-}
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
                , docType = Present (document.docType |> Document.stringFromDocType)
                , children = Present (document.children |> List.map Uuid.toString |> (\list -> Jsonb list))
                , childLevels = Present (document.childLevels |> List.map String.fromInt |> (\list -> Jsonb list))
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
