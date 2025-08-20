module Main (main) where

import Data.Maybe
import Language.Haskell.Interpreter
import System.Directory
import System.Environment

runtime :: String -> InterpreterT IO String
runtime src = do
  workflowsDir <- liftIO $ (fromMaybe "./workflows" <$> lookupEnv "PURE_WORKFLOWS") >>= makeAbsolute
  runtimeDir <- liftIO $ (fromMaybe "./src" <$> lookupEnv "PURE_RUNTIME") >>= makeAbsolute
  set
    [ searchPath := [workflowsDir, runtimeDir]
    ]
  loadModules ["workflows/Workflow.hs"]
  setImports ["Workflow"]
  eval src

main :: IO ()
main = do
  result <- runInterpreter (runtime "workflow")
  print result
