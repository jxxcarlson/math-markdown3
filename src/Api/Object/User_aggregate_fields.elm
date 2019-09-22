-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.User_aggregate_fields exposing (CountOptionalArguments, avg, count, max, min, stddev, stddev_pop, stddev_samp, sum, var_pop, var_samp, variance)

import Api.Enum.User_select_column
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


avg : SelectionSet decodesTo Api.Object.User_avg_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
avg object_ =
    Object.selectionForCompositeField "avg" [] object_ (identity >> Decode.nullable)


type alias CountOptionalArguments =
    { columns : OptionalArgument (List Api.Enum.User_select_column.User_select_column)
    , distinct : OptionalArgument Bool
    }


count : (CountOptionalArguments -> CountOptionalArguments) -> SelectionSet (Maybe Int) Api.Object.User_aggregate_fields
count fillInOptionals =
    let
        filledInOptionals =
            fillInOptionals { columns = Absent, distinct = Absent }

        optionalArgs =
            [ Argument.optional "columns" filledInOptionals.columns (Encode.enum Api.Enum.User_select_column.toString |> Encode.list), Argument.optional "distinct" filledInOptionals.distinct Encode.bool ]
                |> List.filterMap identity
    in
    Object.selectionForField "(Maybe Int)" "count" optionalArgs (Decode.int |> Decode.nullable)


max : SelectionSet decodesTo Api.Object.User_max_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
max object_ =
    Object.selectionForCompositeField "max" [] object_ (identity >> Decode.nullable)


min : SelectionSet decodesTo Api.Object.User_min_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
min object_ =
    Object.selectionForCompositeField "min" [] object_ (identity >> Decode.nullable)


stddev : SelectionSet decodesTo Api.Object.User_stddev_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
stddev object_ =
    Object.selectionForCompositeField "stddev" [] object_ (identity >> Decode.nullable)


stddev_pop : SelectionSet decodesTo Api.Object.User_stddev_pop_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
stddev_pop object_ =
    Object.selectionForCompositeField "stddev_pop" [] object_ (identity >> Decode.nullable)


stddev_samp : SelectionSet decodesTo Api.Object.User_stddev_samp_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
stddev_samp object_ =
    Object.selectionForCompositeField "stddev_samp" [] object_ (identity >> Decode.nullable)


sum : SelectionSet decodesTo Api.Object.User_sum_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
sum object_ =
    Object.selectionForCompositeField "sum" [] object_ (identity >> Decode.nullable)


var_pop : SelectionSet decodesTo Api.Object.User_var_pop_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
var_pop object_ =
    Object.selectionForCompositeField "var_pop" [] object_ (identity >> Decode.nullable)


var_samp : SelectionSet decodesTo Api.Object.User_var_samp_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
var_samp object_ =
    Object.selectionForCompositeField "var_samp" [] object_ (identity >> Decode.nullable)


variance : SelectionSet decodesTo Api.Object.User_variance_fields -> SelectionSet (Maybe decodesTo) Api.Object.User_aggregate_fields
variance object_ =
    Object.selectionForCompositeField "variance" [] object_ (identity >> Decode.nullable)
