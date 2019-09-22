-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.User_var_pop_fields exposing (id, timeEnrolled, timeUpdated)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


id : SelectionSet (Maybe Float) Api.Object.User_var_pop_fields
id =
    Object.selectionForField "(Maybe Float)" "id" [] (Decode.float |> Decode.nullable)


timeEnrolled : SelectionSet (Maybe Float) Api.Object.User_var_pop_fields
timeEnrolled =
    Object.selectionForField "(Maybe Float)" "timeEnrolled" [] (Decode.float |> Decode.nullable)


timeUpdated : SelectionSet (Maybe Float) Api.Object.User_var_pop_fields
timeUpdated =
    Object.selectionForField "(Maybe Float)" "timeUpdated" [] (Decode.float |> Decode.nullable)
