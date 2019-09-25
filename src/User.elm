module User exposing (User, dummy)

import Time exposing(Posix)


type alias User = {
      id : Int
    , username : String
    , email : String
    , public : Bool
    , firstName : String
    , lastName : String
    , admin : Bool
  }


dummy = {
    id = 0
   , username = "demo"
   , email = "demo@gmail.com"
   , public = True
   , firstName = "James"
   , lastName = "Carlson"
   , admin = False
  }