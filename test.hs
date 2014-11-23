{-# LANGUAGE QuasiQuotes #-}

import Improb.Quote

[improb|
tempo: 240

motif := [(C4,4),(G4,4)] -> (F4,4) -> (G4,4) -> (R,4)
v1 := (C4,4) -> (Ab4,4) -> (E4,4) -> (R,4)
v2 := (C4,4) -> (F4,4) -> (E4,4) -> (R,4)
end := (C4,4) -> (F4,4) -> (C4,4)

:violin:
=> motif
motif => v1 -> v2 -> v2
motif => v1 -> v2 -> v1
motif => v2 -> v1
motif => v1 -> v1
v1 -> v2 => motif
v1 => v2
v1 => motif
v2 => motif
motif => end

:cello:
=> motif
motif => v1 -> (R,4)
motif => v2
motif => v2 -> (C4,4)
(R,1) -> (R,4) => motif
v2 -> (C4,4) => motif
v2 => end

|]

