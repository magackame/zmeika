module Direction (Direction(..), opposite, fromKey) where


data Direction
  = U
  | D
  | L
  | R
  deriving (Show, Eq)


opposite :: Direction -> Direction
opposite direction =
  case direction of
    U ->
      D

    D ->
      U

    L ->
      R

    R ->
      L


fromKey :: String -> Maybe Direction
fromKey key =
  case key of
    "\ESC[A" ->
      Just U

    "\ESC[B" ->
      Just D

    "\ESC[D" ->
      Just L

    "\ESC[C" ->
      Just R

    _ ->
      Nothing
