module Vec2 (Vec2(..), add) where


data Vec2 = Vec2
  { x :: Int
  , y :: Int
  } deriving (Show, Eq)


add :: Vec2 -> Vec2 -> Vec2
add (Vec2 x1 y1) (Vec2 x2 y2) =
  Vec2 (x1 + x2) (y1 + y2)
