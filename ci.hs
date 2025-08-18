{-# LANGUAGE Arrows #-}

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random

data WorkflowContext = WorkflowContext
  { env :: Map String String,
    cwd :: String
  }

data WorkflowResult t
  = Success t
  | Failure String

type Stage = StateT WorkflowContext IO

type Workflow t = Kleisli Stage (MVar t) (MVar t)

stage :: IO t -> Workflow t
stage task = proc input -> do
  output <- Kleisli (const $ liftIO newEmptyMVar) -< ()
  _thread <- Kleisli (liftIO . forkIO . inner) -< (input, output)
  returnA -< output
  where
    inner (input, output) = do
      readMVar input
      result <- task
      putMVar output result

checkout :: Workflow (WorkflowResult ())
checkout = stage $ do
  putStrLn "git checkout"
  delay <- randomRIO (1000000, 2000000)
  threadDelay delay
  return $ Success ()

node :: String -> Workflow (WorkflowResult ())
node cmd = stage $ do
  putStrLn cmd
  delay <- randomRIO (1000000, 2000000)
  threadDelay delay
  return $ Success ()

main :: IO ()
main = do
  start <- newMVar (Success ())
  end <- evalStateT (runKleisli workflow start) (WorkflowContext {env = Map.empty, cwd = "/workspace"})
  result <- takeMVar end
  case result of
    Success _ -> return ()
    Failure msg -> error msg

merge :: Kleisli Stage [MVar (WorkflowResult a)] (MVar (WorkflowResult ()))
merge =
  Kleisli
    ( \mvs -> liftIO $ do
        statuses <- mapM readMVar mvs
        newMVar $ all_ statuses
    )
  where
    all_ [] = Success ()
    all_ ((Success _) : ss) = all_ ss
    all_ ((Failure msg) : _) = Failure msg

-- Workflow File:

workflow :: Workflow (WorkflowResult ())
workflow = proc start -> do
  a <- checkout -< start
  b <- node "npm ci" -< a
  c <- node "npm run build" -< b
  d <- node "npm run lint" -< b
  e <- node "npm run fmt" -< b
  f <- node "npm run migrate" -< c
  g <- node "npm test" -< f
  merge -< [d, e, g]
