module User exposing (User, dummy)

import Time exposing(Posix)
import Prng.Uuid as Uuid exposing(Uuid)


type alias User = {
      id : Uuid
    , username : String
    , email : String
    , public : Bool
    , firstName : String
    , lastName : String
    , admin : Bool
  }


dummy = {
    id = 0
   , username = "jxxcarlson"
   , email = "jxxcarlson@gmail.com"
   , public = True
   , firstName = "James"
   , lastName = "Carlson"
   , admin = False
  }