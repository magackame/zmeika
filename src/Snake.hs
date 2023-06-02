module Snake (Snake, move, elongate) where


import Vec2 (Vec2(..))
import qualified Vec2
import Direction (Direction(..))
import qualified Direction


type Snake = [Vec2]

move :: Direction -> Snake -> Snake
move direction snake =
  movedHead : init snake
  where
    movedHead = moveHead (head snake) direction


elongate :: Snake -> Snake
elongate snake = snake ++ [last snake `Vec2.add` Vec2 (-1) (-1)]


moveHead :: Vec2 -> Direction -> Vec2
moveHead head direction =
  case direction of
    U ->
      head `Vec2.add` Vec2 0 (-1)

    D ->
      head `Vec2.add` Vec2 0 1

    L ->
      head `Vec2.add` Vec2 (-1) 0

    R ->
      head `Vec2.add` Vec2 1 0
