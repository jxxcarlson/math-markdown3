-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Scalar exposing (Codecs, Id(..), Jsonb(..), Timestamptz(..), Uuid(..), defaultCodecs, defineCodecs, unwrapCodecs, unwrapEncoder)

import Graphql.Codec exposing (Codec)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Id
    = Id String


type Jsonb
    = Jsonb String


type Timestamptz
    = Timestamptz String


type Uuid
    = Uuid String


defineCodecs :
    { codecId : Codec valueId
    , codecJsonb : Codec valueJsonb
    , codecTimestamptz : Codec valueTimestamptz
    , codecUuid : Codec valueUuid
    }
    -> Codecs valueId valueJsonb valueTimestamptz valueUuid
defineCodecs definitions =
    Codecs definitions


unwrapCodecs :
    Codecs valueId valueJsonb valueTimestamptz valueUuid
    ->
        { codecId : Codec valueId
        , codecJsonb : Codec valueJsonb
        , codecTimestamptz : Codec valueTimestamptz
        , codecUuid : Codec valueUuid
        }
unwrapCodecs (Codecs unwrappedCodecs) =
    unwrappedCodecs


unwrapEncoder getter (Codecs unwrappedCodecs) =
    (unwrappedCodecs |> getter |> .encoder) >> Graphql.Internal.Encode.fromJson


type Codecs valueId valueJsonb valueTimestamptz valueUuid
    = Codecs (RawCodecs valueId valueJsonb valueTimestamptz valueUuid)


type alias RawCodecs valueId valueJsonb valueTimestamptz valueUuid =
    { codecId : Codec valueId
    , codecJsonb : Codec valueJsonb
    , codecTimestamptz : Codec valueTimestamptz
    , codecUuid : Codec valueUuid
    }


defaultCodecs : RawCodecs Id Jsonb Timestamptz Uuid
defaultCodecs =
    { codecId =
        { encoder = \(Id raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Id
        }
    , codecJsonb =
        { encoder = \(Jsonb raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Jsonb
        }
    , codecTimestamptz =
        { encoder = \(Timestamptz raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Timestamptz
        }
    , codecUuid =
        { encoder = \(Uuid raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Uuid
        }
    }
