module User exposing (User, dummy)

import Time exposing(Posix)


type alias User = {
      id : Int
    ,  username : String
    , email : String
    , public : Bool
    , firstName : String
    , lastName : String
    , timeEnrolled : Posix
    , timeUpdated : Posix
    , admin : Bool
  }


dummy = {
     username = "demo"
   , email = "demo@gmail.com"
   , public = True
   , firstName = "James"
   , lastName = "Carlson"
   , timeEnrolled = Time.millisToPosix (1568667528 * 1000)
   , timeUpdated = Time.millisToPosix (1568667528 * 1000)
   , admin = False
  }