{-# LANGUAGE QuasiQuotes #-}
module Examples.Motif where

import Improb.Quote

{-
This will randomly generate a MIDI file the begins with the motif as defined.
It will then transition randomly between the two variations becfore eventaully
reaching the end node and the short piece will end.
-}


[improb|
tempo: 100

motif := (C4,4) -> (F4,4) -> (G4,4) -> (R,4)
variation1 := (C4,4) -> (A4,4) -> (E4,4) -> (R,4)
variation2 := (C4,4) -> (F4,4) -> (G4,4) -> (R,4)
end := (C4,4) -> (F4,4) -> (C4,4)

:piano:
=> motif
motif => variation1
motif => variation2
variation1 => motif
variation2 => motif
variation2 => end
|]
