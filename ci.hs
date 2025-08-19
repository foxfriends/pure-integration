{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random

data Env = Env
  { env :: Map String String,
    cwd :: String
  }

type WorkflowResult = Either String

type Context = StateT Env IO

type Pipeline f t = Kleisli Context f t

type Stage f t = Pipeline (MVar f) (MVar t)

type Workflow = Stage (WorkflowResult ()) (WorkflowResult ())

stage :: (f -> IO t) -> Stage f t
stage task = proc input -> do
  output <- Kleisli (const $ liftIO newEmptyMVar) -< ()
  _thread <- Kleisli (liftIO . forkIO . inner) -< (input, output)
  returnA -< output
  where
    inner (input, output) = do
      status <- readMVar input
      result <- task status
      putMVar output result

checkout :: Stage (WorkflowResult ()) (WorkflowResult ())
checkout = stage $ \_ -> do
  putStrLn "git checkout"
  delay <- randomRIO (1000000, 2000000)
  threadDelay delay
  return $ Right ()

node :: String -> Stage (WorkflowResult ()) (WorkflowResult ())
node cmd = stage $ \_ -> do
  putStrLn cmd
  delay <- randomRIO (1000000, 2000000)
  threadDelay delay
  return $ Right ()

main :: IO ()
main = do
  start <- newMVar (Right ())
  end <- evalStateT (runKleisli workflow start) (Env {env = Map.empty, cwd = "/workspace"})
  result <- takeMVar end
  case result of
    Right _ -> return ()
    Left msg -> error msg

merge :: Pipeline [MVar (WorkflowResult a)] (MVar (WorkflowResult [a]))
merge =
  Kleisli
    ( \mvs -> liftIO $ do
        statuses <- mapM readMVar mvs
        newMVar $ all statuses
    )
  where
    all [] = Right []
    all ((Right s) : ss) = (s :) <$> all ss
    all ((Left msg) : _) = Left msg

end :: Stage (WorkflowResult a) (WorkflowResult ())
end = stage $ \case
  Right _ -> return $ Right ()
  Left msg -> return $ Left msg

-- Workflow File:

workflow :: Workflow
workflow = proc start -> do
  a <- checkout -< start
  b <- node "npm ci" -< a
  c <- node "npm run build" -< b
  d <- node "npm run lint" -< b
  e <- node "npm run fmt" -< b
  f <- node "npm run migrate" -< c
  g <- node "npm test" -< f
  deg <- merge -< [d, e, g]
  end -< deg
