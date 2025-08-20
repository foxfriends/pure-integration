{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module PureIntegration (RadicleEvent) where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import System.Command
import System.Random

type NodeId = String

type RepoId = String

type PatchId = String

type BranchName = String

type Oid = String

type RefString = String

data RadicleEvent
  = BranchCreated {from_node :: NodeId, repo :: RepoId, branch :: BranchName, tip :: Oid}
  | BranchUpdated {from_node :: NodeId, repo :: RepoId, branch :: BranchName, tip :: Oid, old_tip :: Oid}
  | BranchDeleted {from_node :: NodeId, repo :: RepoId, branch :: BranchName, tip :: Oid}
  | TagCreated {from_node :: NodeId, repo :: RepoId, tag_name :: RefString, tip :: Oid}
  | TagUpdated {from_node :: NodeId, repo :: RepoId, tag_name :: RefString, tip :: Oid, old_type :: Oid}
  | TagDeleted {from_node :: NodeId, repo :: RepoId, tag_name :: RefString, tip :: Oid}
  | PatchCreated {from_node :: NodeId, repo :: RepoId, patch :: PatchId, new_tip :: Oid}
  | PatchUpdated {from_node :: NodeId, repo :: RepoId, patch :: PatchId, new_tip :: Oid}
  deriving (Generic, Show)

instance ToJSON RadicleEvent where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RadicleEvent

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

runWorkflow :: Workflow -> IO (WorkflowResult ())
runWorkflow workflow = do
  start <- newMVar (Right ())
  end <- evalStateT (runKleisli workflow start) (Env {env = Map.empty, cwd = "/workspace"})
  takeMVar end

dockerLogin :: String -> String -> String -> IO ()
dockerLogin server username password =
  cmd ["docker", "login", "--username", username, "--password", password, server]

type ActionResult = ()

data DockerArgs = ActionArgs {env :: Maybe [(String, String)], dir :: Maybe String, entrypoint :: Maybe [String], args :: Maybe [String]}

type DockerAction = DockerArgs -> Stage () ()

dockerAction :: String -> DockerAction
dockerAction image (ActionArgs {env, dir, entrypoint, args}) = do
  cmd ["docker", "run", "-v"] [image] args

dockerPull :: String -> DockerAction
dockerPull imageName = do
  (exit :: Exit, stdout :: Stdout, stderr :: Stderr) <- cmd ["docker", "pull"] imageName
  return $ dockerAction imageName
