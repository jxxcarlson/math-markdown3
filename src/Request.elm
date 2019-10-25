module Request exposing
    ( AuthReply(..)
    , GraphQLResponse(..)
    , RequestHandler
    , RequestMsg(..)
    , deleteDocument
    , documentsInIdList
    , documentsWithAuthorAndTagSorted
    , documentsWithAuthorAndTitleSorted
    , documentsWithAuthorSorted
    , getUserByUsername
    , insertDocument
    , insertUser
    , orderByMostRecentFirst
    , orderByTitleAsc
    , publicDocuments
    , publicDocumentsWithTag
    , publicDocumentsWithTitle
    , signInUser
    , signUpUser
    , stringFromHttpError
    , updateDocument
    , updateUser
    )

import Api.Enum.Order_by exposing (Order_by(..))
import Api.InputObject
    exposing
        ( Boolean_comparison_exp
        , Document_bool_exp(..)
        , Document_bool_expOptionalFields
        , Document_insert_input
        , Document_order_by(..)
        , Document_set_input
        , String_comparison_exp
        , User_bool_exp(..)
        , User_insert_input
        , User_set_input
        , Uuid_comparison_exp
        , Uuid_comparison_expOptionalFields
        , buildBoolean_comparison_exp
        , buildDocument_bool_exp
        , buildDocument_insert_input
        , buildDocument_order_by
        , buildDocument_set_input
        , buildJsonb_comparison_exp
        , buildString_comparison_exp
        , buildUser_bool_exp
        , buildUser_insert_input
        , buildUser_set_input
        , buildUuid_comparison_exp
        )
import Api.Mutation
    exposing
        ( DeleteDocumentRequiredArguments
        , InsertDocumentRequiredArguments
        , InsertUserRequiredArguments
        , UpdateDocumentOptionalArguments
        , UpdateDocumentRequiredArguments
        , UpdateUserOptionalArguments
        , UpdateUserRequiredArguments
        , delete_document
        , insert_document
        , insert_user
        , update_document
        , update_user
        )
import Api.Object
import Api.Object.Document exposing (authorIdentifier)
import Api.Object.Document_mutation_response as DocumentMutation
import Api.Object.User exposing (id)
import Api.Object.User_mutation_response as UserMutation
import Api.Query as Query exposing (DocumentOptionalArguments, UserOptionalArguments)
import Codec
import CustomScalarCodecs exposing (Jsonb(..))
import Document exposing (DocType(..), Document, MarkdownFlavor(..))
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe.Extra
import Prng.Uuid as Uuid exposing (Uuid(..))
import RemoteData exposing (RemoteData)
import User exposing (AuthorizedUser, User)



-- MSG --


type RequestMsg
    = GotUserDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotDocumentsForDeque (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotChildDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotDequeDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotCandidateChildDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotPublicDocuments (RemoteData (Graphql.Http.Error (List Document)) (List Document))
    | GotUserAtSignin (RemoteData (Graphql.Http.Error (List User)) (List User))
    | InsertDocumentResponse (GraphQLResponse (Maybe MutationResponse))
    | InsertUserResponse (GraphQLResponse (Maybe MutationResponse))
    | UpdateDocumentResponse (GraphQLResponse (Maybe MutationResponse))
    | DeleteDocumentResponse (GraphQLResponse (Maybe MutationResponse))
    | GotUserSignUp (Result Http.Error AuthReply)
    | GotUserSignIn (Result Http.Error AuthReply)


type alias RequestHandler =
    RemoteData (Graphql.Http.Error (List Document)) (List Document) -> RequestMsg


type alias MutationHandler =
    GraphQLResponse (Maybe MutationResponse) -> RequestMsg



-- PARAMETERS --


endpoint =
    "https://math-markdown-heroku.herokuapp.com/v1/graphql"


authorizationEndpoint =
    "https://tlogic-auth.herokuapp.com"


documentsWithAuthorAndTagSorted : String -> String -> String -> OptionalArgument (List Document_order_by) -> RequestHandler -> Cmd RequestMsg
documentsWithAuthorAndTagSorted authToken author tag sortData requestHandler =
    makeGraphQLQuery authToken
        (fetchSortedDocumentsQuery (Present <| hasAuthorAndTag author tag) sortData)
        (RemoteData.fromResult >> requestHandler)


documentsWithAuthorAndTitleSorted : String -> String -> String -> OptionalArgument (List Document_order_by) -> RequestHandler -> Cmd RequestMsg
documentsWithAuthorAndTitleSorted authToken authorIdentifier titleKey sortData requestHandler =
    makeGraphQLQuery authToken
        (fetchSortedDocumentsQuery (Present <| hasAuthorAndTitle authorIdentifier ("%" ++ titleKey ++ "%")) sortData)
        (RemoteData.fromResult >> requestHandler)



-- CMD: Top level, exported --


documentsInIdList : String -> List Uuid -> RequestHandler -> Cmd RequestMsg
documentsInIdList authToken uuiIdList requestHandler =
    makeGraphQLQuery authToken
        (fetchDocumentsQuery (Present <| inUuidList uuiIdList))
        (RemoteData.fromResult >> requestHandler)


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



-- XXX:HERE:


documentsWithAuthorSorted : String -> String -> OptionalArgument (List Document_order_by) -> Cmd RequestMsg
documentsWithAuthorSorted authToken authorIdentifier sortData =
    makeGraphQLQuery authToken
        (fetchSortedDocumentsQuery (Present <| hasAuthor_ authorIdentifier) sortData)
        (RemoteData.fromResult >> GotUserDocuments)


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
    makeMutation (getDocumentInsertObject newDocument) authToken InsertDocumentResponse


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



-- order_by : OptionalArgument (List Api.InputObject.Document_order_by)


fetchSortedDocumentsQuery :
    OptionalArgument Document_bool_exp
    -> OptionalArgument (List Document_order_by)
    -> SelectionSet (List Document) RootQuery
fetchSortedDocumentsQuery doc_bool_exp sortData =
    Query.document (documentListOptionalArgument2 doc_bool_exp sortData) documentListSelection


documentListOptionalArgument : OptionalArgument Document_bool_exp -> DocumentOptionalArguments -> DocumentOptionalArguments
documentListOptionalArgument doc_bool_exp optionalArgs =
    { optionalArgs | where_ = doc_bool_exp, order_by = orderByMostRecent Desc }


orderByMostRecentFirst : OptionalArgument (List Document_order_by)
orderByMostRecentFirst =
    orderByMostRecent Desc


orderByTitleAsc : OptionalArgument (List Document_order_by)
orderByTitleAsc =
    orderByTitle Asc


documentListOptionalArgument2 : OptionalArgument Document_bool_exp -> OptionalArgument (List Document_order_by) -> DocumentOptionalArguments -> DocumentOptionalArguments
documentListOptionalArgument2 doc_bool_exp sortData optionalArgs =
    { optionalArgs | where_ = doc_bool_exp, order_by = sortData }


orderByTitle : Order_by -> OptionalArgument (List Document_order_by)
orderByTitle order =
    Present <| [ buildDocument_order_by (\args -> { args | title = OptionalArgument.Present order }) ]


orderByMostRecent : Order_by -> OptionalArgument (List Document_order_by)
orderByMostRecent order =
    Present <| [ buildDocument_order_by (\args -> { args | timeStamp = OptionalArgument.Present order }) ]



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


hasTag : String -> Document_bool_exp
hasTag key =
    buildDocument_bool_exp (\args -> { args | tags = Present <| hasTag_ key })


hasTag_ : String -> Api.InputObject.Jsonb_comparison_exp
hasTag_ x =
    buildJsonb_comparison_exp (\args -> { args | has_key_ = OptionalArgument.Present x })


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
            (Api.Object.Document.childInfo identity
                |> SelectionSet.map
                    (\(Jsonb x) -> List.map Codec.getPair x |> Maybe.Extra.values)
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
                , childInfo = Present (newDocument.childInfo |> List.map uuidIntPairToString |> (\list -> Jsonb list))
            }
        )


insertDocumentArgs : Document -> InsertDocumentRequiredArguments
insertDocumentArgs newDocument =
    InsertDocumentRequiredArguments [ insertDocumentObjects newDocument ]


getDocumentInsertObject : Document -> SelectionSet (Maybe MutationResponse) RootMutation
getDocumentInsertObject newDocument =
    insert_document identity (insertDocumentArgs newDocument) mutationResponseDocumentSelection


getDocumentUpdateObject : Document -> SelectionSet (Maybe MutationResponse) RootMutation
getDocumentUpdateObject document =
    update_document
        (setDocumentUpdateOptionalArgs document)
        (setDocumentUpdateWhere document.id)
        mutationResponseDocumentSelection


getDocumentDeleteObject : Document -> SelectionSet (Maybe MutationResponse) RootMutation
getDocumentDeleteObject document =
    delete_document
        (setDocumentDeleteWhere document.id)
        mutationResponseDocumentSelection



{-
   delete_document :
        DeleteDocumentRequiredArguments
     -> SelectionSet decodesTo Api.Object.Document_mutation_response
     -> SelectionSet (Maybe decodesTo) RootMutation
   delete_document requiredArgs object_ =
-}


mutationResponseDocumentSelection : SelectionSet MutationResponse Api.Object.Document_mutation_response
mutationResponseDocumentSelection =
    SelectionSet.map MutationResponse
        DocumentMutation.affected_rows


makeMutation1 : SelectionSet (Maybe MutationResponse) RootMutation -> String -> Cmd RequestMsg
makeMutation1 mutation authToken =
    makeGraphQLMutation authToken mutation (RemoteData.fromResult >> GraphQLResponse >> InsertDocumentResponse)


makeMutation : SelectionSet (Maybe MutationResponse) RootMutation -> String -> MutationHandler -> Cmd RequestMsg
makeMutation mutation authToken mutationHahdler =
    makeGraphQLMutation authToken mutation (RemoteData.fromResult >> GraphQLResponse >> mutationHahdler)


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
                , childInfo = Present (document.childInfo |> List.map uuidIntPairToString |> (\list -> Jsonb list))
            }
        )


uuidIntPairToString : ( Uuid, Int ) -> String
uuidIntPairToString ( uuid, k ) =
    "(" ++ Uuid.toString uuid ++ "," ++ String.fromInt k ++ ")"


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



-- USER AUTHENTICATION --


signUpUser : String -> String -> String -> String -> Cmd RequestMsg
signUpUser username email password confirmPassword =
    Http.request
        { method = "POST"
        , headers = []
        , url = authorizationEndpoint ++ "/signup"
        , body = Http.jsonBody (encodeAuthorizedUserForSignUp username email password confirmPassword)
        , expect = expectJsonFromTLogicAuth GotUserSignUp decodeToken
        , timeout = Nothing
        , tracker = Nothing
        }


signInUser : String -> String -> Cmd RequestMsg
signInUser username password =
    Http.request
        { method = "POST"
        , headers = []
        , url = authorizationEndpoint ++ "/login"
        , body = Http.jsonBody (encodeAuthorizedUserForSignIn username password)
        , expect = expectJsonFromTLogicAuth GotUserSignIn decodeToken
        , timeout = Nothing
        , tracker = Nothing
        }


expectJsonFromTLogicAuth : (Result Http.Error AuthReply -> msg) -> Decoder AuthReply -> Http.Expect msg
expectJsonFromTLogicAuth toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    if String.contains "duplicate key" body then
                        Ok (AuthError "Username already exists")

                    else if String.contains "Passwords do not match" body then
                        Ok (AuthError "Passwords do not match")

                    else if String.contains "Invalid password" body then
                        Ok (AuthError "Invalid password or username")

                    else if String.contains "Unknown user" body then
                        Ok (AuthError "Invalid password or username")

                    else if String.contains "Password must be at least" body then
                        Ok (AuthError "Password must be at least 8 characters long")

                    else if String.contains "email" body && String.contains "shorter" body then
                        Ok (AuthError "Email should not be shorter than 7 characters")

                    else
                        Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString decodeToken body of
                        Ok (AuthToken value) ->
                            Ok (AuthToken value)

                        Ok (AuthError message) ->
                            Err (Http.BadBody message)

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))


encodeAuthorizedUserForSignUp : String -> String -> String -> String -> Encode.Value
encodeAuthorizedUserForSignUp username email password confirmPassword =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "email", Encode.string email )
        , ( "password", Encode.string password )
        , ( "confirmPassword", Encode.string confirmPassword )
        ]


encodeAuthorizedUserForSignIn : String -> String -> Encode.Value
encodeAuthorizedUserForSignIn username password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]


decodeAuthorizedUser : Decoder AuthorizedUser
decodeAuthorizedUser =
    Decode.map4 AuthorizedUser
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "token" Decode.string)


type AuthReply
    = AuthToken String
    | AuthError String


decodeToken : Decoder AuthReply
decodeToken =
    Decode.field "token" Decode.string |> Decode.map AuthToken



-- USER --


insertUser : String -> User -> Cmd RequestMsg
insertUser authToken newUser =
    makeMutation (getUserInsertObject newUser) authToken InsertUserResponse


updateUser : String -> User -> Cmd RequestMsg
updateUser authToken user =
    makeUpdateDocumentMutation (getUserUpdateObject user) authToken


getUserUpdateObject : User -> SelectionSet (Maybe MutationResponse) RootMutation
getUserUpdateObject user =
    update_user
        (setUserUpdateOptionalArgs user)
        (setUserUpdateWhere user.id)
        mutationResponseUserSelection


setUserUpdateOptionalArgs : User -> UpdateUserOptionalArguments -> UpdateUserOptionalArguments
setUserUpdateOptionalArgs user optionalArgs =
    { optionalArgs
        | set_ = Present (setUserSetArg user)
    }


{-| Change this after running 'sh api.sh'
-}
setUserSetArg : User -> User_set_input
setUserSetArg user =
    buildUser_set_input
        (\args ->
            { args
                | id = Present user.id
                , username = Present user.username
                , email = Present user.email
                , firstName = Present user.firstName
                , lastName = Present user.lastName
                , admin = Present user.admin
                , recentDocs = Present (user.recentDocs |> List.map Uuid.toString |> (\list -> Jsonb list))
            }
        )


setUserUpdateWhere : Uuid -> UpdateUserRequiredArguments
setUserUpdateWhere uuid =
    UpdateUserRequiredArguments
        (buildUser_bool_exp
            (\args ->
                { args
                    | id = Present (setUserValueForId uuid)
                }
            )
        )


setUserValueForId : Uuid -> Uuid_comparison_exp
setUserValueForId uuid =
    buildUuid_comparison_exp
        (\args ->
            { args
                | eq_ = Present uuid
            }
        )



-- XXX


getUserByUsername : String -> String -> Cmd RequestMsg
getUserByUsername authToken userName =
    makeGraphQLQuery authToken
        (fetchUsersQuery (Present <| hasUsername_ userName))
        (RemoteData.fromResult >> GotUserAtSignin)


hasUsername_ : String -> User_bool_exp
hasUsername_ username =
    buildUser_bool_exp (\args -> { args | username = Present <| equalToString username })


fetchUsersQuery : OptionalArgument User_bool_exp -> SelectionSet (List User) RootQuery
fetchUsersQuery user_bool_exp =
    Query.user (userListOptionalArgument user_bool_exp) userListSelection


{-| Change this after running 'sh api.sh'
-}
userListSelection : SelectionSet User Api.Object.User
userListSelection =
    SelectionSet.succeed User
        |> with Api.Object.User.id
        |> with Api.Object.User.username
        |> with Api.Object.User.email
        |> with Api.Object.User.firstName
        |> with Api.Object.User.lastName
        |> with Api.Object.User.admin
        |> with
            (Api.Object.User.recentDocs identity
                |> SelectionSet.map (\(Jsonb x) -> List.map Uuid.fromString x |> Maybe.Extra.values)
            )


userListOptionalArgument : OptionalArgument User_bool_exp -> UserOptionalArguments -> UserOptionalArguments
userListOptionalArgument user_bool_exp optionalArgs =
    { optionalArgs | where_ = user_bool_exp }


getUserInsertObject : User -> SelectionSet (Maybe MutationResponse) RootMutation
getUserInsertObject newUser =
    insert_user identity (insertUserArgs newUser) mutationResponseUserSelection


insertUserArgs : User -> InsertUserRequiredArguments
insertUserArgs newUser =
    InsertUserRequiredArguments [ insertUserObject newUser ]


mutationResponseUserSelection : SelectionSet MutationResponse Api.Object.User_mutation_response
mutationResponseUserSelection =
    SelectionSet.map MutationResponse
        UserMutation.affected_rows


{-| Change this after running 'sh api.sh'
-}
userSelection : SelectionSet User Api.Object.User
userSelection =
    -- XXX
    SelectionSet.succeed User
        |> with Api.Object.User.id
        |> with Api.Object.User.username
        |> with Api.Object.User.email
        |> with Api.Object.User.firstName
        |> with Api.Object.User.lastName
        |> with Api.Object.User.admin
        |> with
            (Api.Object.User.recentDocs identity
                |> SelectionSet.map (\(Jsonb x) -> List.map Uuid.fromString x |> Maybe.Extra.values)
            )


{-| Change this after running 'sh api.sh'
-}
insertUserObject : User -> User_insert_input
insertUserObject newUser =
    --- XXX
    buildUser_insert_input
        (\args ->
            { args
                | id = Present newUser.id
                , username = Present newUser.username
                , email = Present newUser.email
                , firstName = Present newUser.firstName
                , lastName = Present newUser.lastName
                , admin = Present newUser.admin
                , recentDocs = Present (newUser.recentDocs |> List.map Uuid.toString |> (\list -> Jsonb list))
            }
        )


{-| Change this after running 'sh api.sh'
-}
setUsertSetArg : User -> User_set_input
setUsertSetArg user =
    buildUser_set_input
        (\args ->
            { args
                | id = Present user.id
                , username = Present user.username
                , email = Present user.email
                , firstName = Present user.firstName
                , lastName = Present user.lastName
                , admin = Present user.admin
                , recentDocs = Present (user.recentDocs |> List.map Uuid.toString |> (\list -> Jsonb list))
            }
        )



-- HELPERS ---


stringFromHttpError : Error -> String
stringFromHttpError error =
    case error of
        BadUrl str ->
            "Bad url: " ++ str

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network error"

        BadStatus k ->
            "Bad status: " ++ String.fromInt k

        BadBody body ->
            "Bad body: " ++ body



-- Ellie for troubleshooting requests: https://ellie-app.com/6VWhRnBgXsga1
{- SIGNUP ERRORS:

      {
       "errors": [
         {
           "location": "params",
           "param": "confirmPassword",
           "msg": "Passwords do not match"
         }
       ]
      }

     {
       "message": "insert into \"users\" (\"email\", \"password\", \"token\", \"username\") values ($1, $2, $3, $4) returning \"id\" - duplicate key value violates unique constraint \"users_username_unique\"",
       "type": "UnknownError",
       "data": {}
     }

    SIGNIN ERRORS

      {
        "error": "Invalid password"
      }

   {
     "error": "Unknown user"
   }

-}
