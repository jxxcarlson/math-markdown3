-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.User exposing (DocumentsAggregateOptionalArguments, DocumentsOptionalArguments, admin, documents, documents_aggregate, email, firstName, id, lastName, timeStamp, username)

import Api.Enum.Document_select_column
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


admin : SelectionSet Bool Api.Object.User
admin =
    Object.selectionForField "Bool" "admin" [] Decode.bool


type alias DocumentsOptionalArguments =
    { distinct_on : OptionalArgument (List Api.Enum.Document_select_column.Document_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Api.InputObject.Document_order_by)
    , where_ : OptionalArgument Api.InputObject.Document_bool_exp
    }


{-| An array relationship

  - distinct\_on - distinct select on columns
  - limit - limit the nuber of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
documents : (DocumentsOptionalArguments -> DocumentsOptionalArguments) -> SelectionSet decodesTo Api.Object.Document -> SelectionSet (List decodesTo) Api.Object.User
documents fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum Api.Enum.Document_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (Api.InputObject.encodeDocument_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ Api.InputObject.encodeDocument_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "documents" optionalArgs object_ (identity >> Decode.list)


type alias DocumentsAggregateOptionalArguments =
    { distinct_on : OptionalArgument (List Api.Enum.Document_select_column.Document_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Api.InputObject.Document_order_by)
    , where_ : OptionalArgument Api.InputObject.Document_bool_exp
    }


{-| An aggregated array relationship

  - distinct\_on - distinct select on columns
  - limit - limit the nuber of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
documents_aggregate : (DocumentsAggregateOptionalArguments -> DocumentsAggregateOptionalArguments) -> SelectionSet decodesTo Api.Object.Document_aggregate -> SelectionSet decodesTo Api.Object.User
documents_aggregate fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum Api.Enum.Document_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (Api.InputObject.encodeDocument_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ Api.InputObject.encodeDocument_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "documents_aggregate" optionalArgs object_ identity


email : SelectionSet String Api.Object.User
email =
    Object.selectionForField "String" "email" [] Decode.string


firstName : SelectionSet String Api.Object.User
firstName =
    Object.selectionForField "String" "firstName" [] Decode.string


id : SelectionSet Int Api.Object.User
id =
    Object.selectionForField "Int" "id" [] Decode.int


lastName : SelectionSet String Api.Object.User
lastName =
    Object.selectionForField "String" "lastName" [] Decode.string


timeStamp : SelectionSet Api.ScalarCodecs.Timestamptz Api.Object.User
timeStamp =
    Object.selectionForField "ScalarCodecs.Timestamptz" "timeStamp" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)


username : SelectionSet String Api.Object.User
username =
    Object.selectionForField "String" "username" [] Decode.string
