-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.InputObject exposing (DocumentInput, DocumentInputRequiredFields, UserInput, UserInputRequiredFields, buildDocumentInput, buildUserInput, encodeDocumentInput, encodeUserInput)

import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


buildDocumentInput : DocumentInputRequiredFields -> DocumentInput
buildDocumentInput required =
    { identifier = required.identifier, title = required.title, content = required.content, author = required.author, tags = required.tags, timeCreated = required.timeCreated, timeUpdated = required.timeUpdated, public = required.public }


type alias DocumentInputRequiredFields =
    { identifier : String
    , title : String
    , content : String
    , author : String
    , tags : List String
    , timeCreated : Int
    , timeUpdated : Int
    , public : Bool
    }


{-| Type for the DocumentInput input object.
-}
type alias DocumentInput =
    { identifier : String
    , title : String
    , content : String
    , author : String
    , tags : List String
    , timeCreated : Int
    , timeUpdated : Int
    , public : Bool
    }


{-| Encode a DocumentInput into a value that can be used as an argument.
-}
encodeDocumentInput : DocumentInput -> Value
encodeDocumentInput input =
    Encode.maybeObject
        [ ( "identifier", Encode.string input.identifier |> Just ), ( "title", Encode.string input.title |> Just ), ( "content", Encode.string input.content |> Just ), ( "author", Encode.string input.author |> Just ), ( "tags", (Encode.string |> Encode.list) input.tags |> Just ), ( "timeCreated", Encode.int input.timeCreated |> Just ), ( "timeUpdated", Encode.int input.timeUpdated |> Just ), ( "public", Encode.bool input.public |> Just ) ]


buildUserInput : UserInputRequiredFields -> UserInput
buildUserInput required =
    { email = required.email, public = required.public, firstName = required.firstName, lastName = required.lastName, timeEnrolled = required.timeEnrolled, timeUpdated = required.timeUpdated, admin = required.admin }


type alias UserInputRequiredFields =
    { email : String
    , public : Bool
    , firstName : String
    , lastName : String
    , timeEnrolled : Int
    , timeUpdated : Int
    , admin : Bool
    }


{-| Type for the UserInput input object.
-}
type alias UserInput =
    { email : String
    , public : Bool
    , firstName : String
    , lastName : String
    , timeEnrolled : Int
    , timeUpdated : Int
    , admin : Bool
    }


{-| Encode a UserInput into a value that can be used as an argument.
-}
encodeUserInput : UserInput -> Value
encodeUserInput input =
    Encode.maybeObject
        [ ( "email", Encode.string input.email |> Just ), ( "public", Encode.bool input.public |> Just ), ( "firstName", Encode.string input.firstName |> Just ), ( "lastName", Encode.string input.lastName |> Just ), ( "timeEnrolled", Encode.int input.timeEnrolled |> Just ), ( "timeUpdated", Encode.int input.timeUpdated |> Just ), ( "admin", Encode.bool input.admin |> Just ) ]
