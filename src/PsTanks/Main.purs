module PsTanks.Main where

import PsGame (Game, start)

import PsTanks.Event (update)
import PsTanks.Inputs (deriveEvents, controls)
import PsTanks.State (initialState)
import PsTanks.View (view)

main ∷ Game ()
main =
  start
    initialState
    controls
    deriveEvents
    update
    view
