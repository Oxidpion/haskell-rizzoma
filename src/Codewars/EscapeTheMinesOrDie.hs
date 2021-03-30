module Codewars.EscapeTheMinesOrDie where

import Control.Monad (guard)
import Control.Monad.State.Lazy
  ( MonadState (get),
    State,
    evalState,
    runState,
  )
import Data.Maybe (catMaybes, listToMaybe)

type XY = (Int, Int)

data Move = U | D | R | L deriving (Eq, Show)

solve :: [[Bool]] -> XY -> XY -> Maybe [Move]
solve mp boy door = listToMaybe $ catMaybes $ evalState (helper mp boy door) []

helper :: [[Bool]] -> XY -> XY -> State [XY] [Maybe [Move]]
helper mp boy door
  | not $ selectField mp boy = return [Nothing]
  | boy == door = return [Just []]
  | otherwise = do
    currentHistoryMove <- get
    return $ do
      move <- possibleMove mp boy
      let nextBoy = step boy move

      let (results, historyMove) = runState (helper mp nextBoy door) (boy : currentHistoryMove)
      guard $ notElem nextBoy historyMove

      result <- results
      return $ fmap (move :) result

selectField :: [[a]] -> (Int, Int) -> a
selectField mp (x, y) = mp !! x !! y

step :: (Num a1, Num a2) => (a1, a2) -> Move -> (a1, a2)
step (x, y) L = (x - 1, y)
step (x, y) U = (x, y - 1)
step (x, y) R = (x + 1, y)
step (x, y) D = (x, y + 1)

possibleMove :: [[Bool]] -> (Int, Int) -> [Move]
possibleMove mp boy@(x, y) = do
  move <- [U, D, R, L]
  guard $ case move of
    L -> x /= 0
    U -> y /= 0
    R -> length mp > x + 1
    D -> length (mp !! x) > y + 1
  guard $ selectField mp $ step boy move
  return move
