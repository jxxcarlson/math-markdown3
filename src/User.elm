module User exposing (AuthorizedUser, User, barebonesUser, dummy)

import Prng.Uuid exposing (Uuid(..))
import Utility exposing (getId)


type alias User =
    { id : Uuid
    , username : String
    , email : String
    , firstName : String
    , lastName : String
    , admin : Bool
    }


type alias AuthorizedUser =
    { id : Int
    , username : String
    , email : String
    , token : String
    }


barebonesUser : Uuid -> String -> String -> User
barebonesUser uuid username email =
    { id = uuid
    , username = username
    , email = email
    , firstName = "not_yet"
    , lastName = "not_yet"
    , admin = False
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
