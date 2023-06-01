module World (World, emptyWorld, drawSnake, drawFood) where


import Snake
import Vec2 (Vec2(..))


type World = [[Char]]


drawSnake :: Snake -> World -> World
drawSnake [] world = world
drawSnake (head:tail) world = drawSnake tail $ replace head snakeChar world


drawFood :: Vec2 -> World -> World
drawFood foodPosition = replace foodPosition foodChar


replace :: Vec2 -> Char -> World -> World
replace (Vec2 x y) c world = worldPrefix ++ [prefix ++ [c] ++ tail suffix] ++ tail worldSuffix
  where
    row = world !! y
    (worldPrefix, worldSuffix) = splitAt y world
    (prefix, suffix) = splitAt x row


topBar :: Int -> String
topBar columnCount = '/' : replicate (columnCount - 1) '-' ++ "\\\n"


bottomBar :: Int -> String
bottomBar columnCount = '\\' : replicate (columnCount - 1) '-' ++ "/\n"


emptyRow :: Int -> String
emptyRow columnCount = '|' : replicate (columnCount - 1) ' ' ++ "|\n"


emptyWorld :: Int -> Int -> World
emptyWorld rowCount columnCount = top : replicate (rowCount - 2) middle ++ bottom
  where
    top = topBar columnCount
    middle = emptyRow columnCount
    bottom = [bottomBar columnCount]


snakeChar :: Char
snakeChar = '#'


foodChar :: Char
foodChar = 'x'
