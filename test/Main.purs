module Test.Main where

import Base

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = log "Hello from Base!"
