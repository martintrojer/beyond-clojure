module Players.Models (..) where


type alias PlayerId =
  Int


type alias Player =
  { id : PlayerId
  , name : String
  , level : Int
  }


new : Player
new =
  { name = "change me"
  , level = 1
  , id = 1
  }
