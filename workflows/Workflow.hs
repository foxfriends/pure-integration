module Workflow where

import PureIntegration

node = dockerPull "node:latest"

helloWorldWorkflow = "Hello World"

workflow :: Workflow
workflow = proc start -> do
  a <- checkout -< start
  b <- dockerRun node "npm ci" -< a
  c <- dockerRun node "npm run build" -< b
  d <- dockerRun node "npm run lint" -< b
  e <- dockerRun node "npm run fmt" -< b
  f <- dockerRun node "npm run migrate" -< c
  g <- dockerRun node "npm test" -< f
  deg <- merge -< [d, e, g]
  end -< deg
