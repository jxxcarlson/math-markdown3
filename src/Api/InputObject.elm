-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.InputObject exposing (DocumentInput, DocumentInputRequiredFields, buildDocumentInput, encodeDocumentInput)

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
    { identifier = required.identifier, title = required.title, content = required.content, author = required.author, tags = required.tags, timeCreated = required.timeCreated, timeUpdated = required.timeUpdated, public = required.public, children = required.children }


type alias DocumentInputRequiredFields =
    { identifier : String
    , title : String
    , content : String
    , author : Api.ScalarCodecs.Id
    , tags : List String
    , timeCreated : Int
    , timeUpdated : Int
    , public : Bool
    , children : List String
    }


{-| Type for the DocumentInput input object.
-}
type alias DocumentInput =
    { identifier : String
    , title : String
    , content : String
    , author : Api.ScalarCodecs.Id
    , tags : List String
    , timeCreated : Int
    , timeUpdated : Int
    , public : Bool
    , children : List String
    }


{-| Encode a DocumentInput into a value that can be used as an argument.
-}
encodeDocumentInput : DocumentInput -> Value
encodeDocumentInput input =
    Encode.maybeObject
        [ ( "identifier", Encode.string input.identifier |> Just ), ( "title", Encode.string input.title |> Just ), ( "content", Encode.string input.content |> Just ), ( "author", (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecId) input.author |> Just ), ( "tags", (Encode.string |> Encode.list) input.tags |> Just ), ( "timeCreated", Encode.int input.timeCreated |> Just ), ( "timeUpdated", Encode.int input.timeUpdated |> Just ), ( "public", Encode.bool input.public |> Just ), ( "children", (Encode.string |> Encode.list) input.children |> Just ) ]