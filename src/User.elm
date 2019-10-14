module User exposing (AuthorizedUser, User, dummy)

import Prng.Uuid exposing (Uuid(..))
import Utility exposing (getId)


type alias User =
    { id : Uuid
    , username : String
    , email : String
    , public : Bool
    , firstName : String
    , lastName : String
    , admin : Bool
    }


type alias AuthorizedUser =
    { id : Int
    , username : String
    , token : String
    }


dummy username =
    { id = getId 6
    , username = username
    , email = username ++ "@gmail.com"
    , public = True
    , firstName = "James"
    , lastName = "Carlson"
    , admin = False
    }
