module Main where

import Control.Monad.Bayes.Class (
  MonadSample,
  categorical,
 )
import Control.Monad.Bayes.Enumerator (enumerate)
import Data.Vector as V (fromList)
import Main.Utf8 qualified as Utf8
import Prelude hiding (Down, Left, Right, state)

data Direction = Left | Right | Up | Down

data TileState = Empty | Goal | Obstacle | Fail deriving stock (Show, Eq, Ord)

board :: [[TileState]]
board =
  [ [Empty, Empty, Empty, Goal]
  , [Empty, Obstacle, Empty, Fail]
  , [Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty]
  ]

deterministic :: forall {a}. Num a => Direction -> [a]
deterministic x =
  case x of
    Left -> [1, 0, 0, 0]
    Right -> [0, 1, 0, 0]
    Up -> [0, 0, 1, 0]
    Down -> [0, 0, 0, 1]

stochastic :: forall {a}. Fractional a => Direction -> [a]
stochastic x = case x of
  Left -> [0.7, 0, 0.15, 0.15]
  Right -> [0, 0.7, 0.15, 0.15]
  Up -> [0.15, 0.15, 0.7, 0]
  Down -> [0.15, 0.15, 0, 0.7]

stochasticAction :: forall {m :: Type -> Type}. MonadSample m => Direction -> m Direction
stochasticAction x =
  do
    real <- categorical (V.fromList (stochastic x))
    return
      ( case real of
          0 -> Left
          1 -> Right
          2 -> Up
          3 -> Down
          _ -> error "Wrong categorical result"
      )

fromPosition :: (Int, Int) -> Maybe TileState
fromPosition (x, y) = board !!? x >>= (!!? y)

data ActionResult
  = Continue (Int, Int)
  | StopGoal
  | StopFail

futureState :: (Int, Int) -> Maybe Direction -> (TileState, (Int, Int))
futureState (x, y) act =
  let prospectivePosition = case act of
        Just Left -> (x, y - 1)
        Just Right -> (x, y + 1)
        Just Up -> (x - 1, y)
        Just Down -> (x + 1, y)
        Nothing -> (x, y)
   in case (fromPosition prospectivePosition, fromPosition (x, y)) of
        (Just a, _) -> (a, prospectivePosition)
        (Nothing, Just currState) -> (currState, (x, y))
        _ -> error "impossible scenario"

applyAction :: (Int, Int) -> Maybe Direction -> ActionResult
applyAction pos act =
  let (state, newPos) = futureState pos act
   in case state of
        Empty -> Continue newPos
        Goal -> StopGoal
        Obstacle -> Continue pos
        Fail -> StopFail

model1 :: forall {m :: Type -> Type}. MonadSample m => (Int, Int) -> [Direction] -> m TileState
model1 position [] = do
  let (newState, _) = futureState position Nothing
  return newState
model1 position (action : remainingActions) = do
  realAction <- stochasticAction action
  let newState = applyAction position (Just realAction)
  do
    case newState of
      Continue pos -> model1 pos remainingActions
      StopGoal -> return Goal
      StopFail -> return Fail

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    let x = enumerate (model1 (3, 1) [Up, Right, Up, Up, Right])
    do print x
