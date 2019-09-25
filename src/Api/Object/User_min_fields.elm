-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.User_min_fields exposing (email, firstName, id, lastName, timeStamp, username)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.Union
import CustomScalarCodecs
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


email : SelectionSet (Maybe String) Api.Object.User_min_fields
email =
    Object.selectionForField "(Maybe String)" "email" [] (Decode.string |> Decode.nullable)


firstName : SelectionSet (Maybe String) Api.Object.User_min_fields
firstName =
    Object.selectionForField "(Maybe String)" "firstName" [] (Decode.string |> Decode.nullable)


id : SelectionSet (Maybe Int) Api.Object.User_min_fields
id =
    Object.selectionForField "(Maybe Int)" "id" [] (Decode.int |> Decode.nullable)


lastName : SelectionSet (Maybe String) Api.Object.User_min_fields
lastName =
    Object.selectionForField "(Maybe String)" "lastName" [] (Decode.string |> Decode.nullable)


timeStamp : SelectionSet (Maybe CustomScalarCodecs.Timestamptz) Api.Object.User_min_fields
timeStamp =
    Object.selectionForField "(Maybe CustomScalarCodecs.Timestamptz)" "timeStamp" [] (CustomScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder |> Decode.nullable)


username : SelectionSet (Maybe String) Api.Object.User_min_fields
username =
    Object.selectionForField "(Maybe String)" "username" [] (Decode.string |> Decode.nullable)
