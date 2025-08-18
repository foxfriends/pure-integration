# Pure Integration

Half baked concept of using Haskell as a workflow definition language for CI pipelines.

The use of arrows (`Control.Arrow`) enables declaratively describing the stages of the
pipeline and their dependencies in a way that automatically enables both reuse of the
results of previous steps as well as automatically parallelizing portions of the
workflow which do not depend on each other.

Meanwhile, the use of a full programming language (as opposed to YAML or other such
configuration files) means that workflows can be much more expressive and flexible.
Additionally, since Haskell is statically typed, this helps to eliminate an entire
class of errors that we tend to have in CI workflow files (typos).

The tradeoff is of course, the potential for there to be *too much* flexibility, as
now there are ways to write "workflows" that aren't workflows, or which arbitrarily
spawn untracked background tasks, meaning debugging a workflow is potentially much
harder, as we've added multiple new sources of error (concurrency, control flow...).

The next steps would be to have such a workflow orchestrate varying steps that are
run in Docker containers over a shared volume containing the repository state.
