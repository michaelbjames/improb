{-# LANGUAGE QuasiQuotes #-}

import Improb.Quote

[improb|
tempo: 60

motif := (C4,1) -> (F4,1) -> (G4,1) -> (R,1)
variation1 := (C4,1) -> (Ab4,1) -> (E4,1) -> (R,1)
variation2 := (C4,1) -> (F4,1) -> (E4,1) -> (R,1)
end := (C4,1) -> (F4,1) -> (C4,1)

:organ:
=> motif
motif => variation1
motif => variation2
variation1 => motif
variation2 => motif
variation2 => end
|]

