-- | Runs the linter over this package.
-- |
-- | No rules yet - the wiring is here so adding one is a single line.
module Test.Lint (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Process (exit')
import Lint (runLinter)
import Lint.RuleSet (Rule)

rules :: Array Rule
rules = []

main :: Effect Unit
main = launchAff_ do
  clean <- runLinter rules
  liftEffect (exit' (if clean then 0 else 1))
