
module CustomScalarCodecs exposing (Id, Jsonb(..), Timestamptz(..), Uuid, codecs)

import Json.Decode as Decode
import Json.Encode as Encode
import Api.Scalar exposing(defaultCodecs)
import Time exposing(Posix(..))
import Graphql.Internal.Builder.Object as Object
import Prng.Uuid exposing(Uuid(..))
import Utility

type Id =
    Id String

type Jsonb = Jsonb (List String)

type Timestamptz = Timestamptz String

type alias PosixTime =
    Time.Posix

type alias Uuid = Prng.Uuid.Uuid




codecs : Api.Scalar.Codecs Id Jsonb Timestamptz Uuid
codecs =
    Api.Scalar.defineCodecs
        { codecId =
             { encoder = \(Id raw) -> Encode.string raw
               , decoder = Object.scalarDecoder |> Decode.map Id
             }
        , codecJsonb =
             {   encoder = \(Jsonb raw) -> raw |> String.join "," |> Encode.string
               , decoder = Decode.list Decode.string |> Decode.map Jsonb
             }
        , codecTimestamptz =
             { encoder = \(Timestamptz raw) -> Encode.string raw
             , decoder = Object.scalarDecoder |> Decode.map Timestamptz
             }
         , codecUuid =
             { encoder = \uuid -> Encode.string (Prng.Uuid.toString uuid)
             , decoder = Object.scalarDecoder |> Decode.map (Prng.Uuid.fromString >> Maybe.withDefault Utility.id0)
             }
        }



