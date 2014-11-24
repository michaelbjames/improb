{-# LANGUAGE QuasiQuotes #-}
module Examples.Branching where

import Improb.Quote

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
