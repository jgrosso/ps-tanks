module PsTanks.Main where

import PsGame (Game, start)

import PsTanks.Controls (controls)
import PsTanks.Event (update)
import PsTanks.State (initialState)
import PsTanks.View (view)

main ∷ Game ()
main =
  start
    initialState
    controls
    update
    view
