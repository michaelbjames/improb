{-# LANGUAGE QuasiQuotes #-}
module Examples.Branching where

import Improb.Quote

{-
This generates a short MIDI file, with a piano (at 120 bpm). The piano will
start with a C, in the 4th octave, playing for a quarter note. From there, the
compiler will randomly choose to play either an F or a G. This random walk
continues until it reaches an endpoint, so the A in this case.
-}

[improb|
tempo: 120

:piano:
=> (C4,4)
(C4,4) => (F4,4)
(C4,4) => (G4,4)
(G4,4) => (C4,4)
(F4,4) => (G4,4)
(G4,4) => (A4,4)
|]
