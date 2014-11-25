module Examples.Tests.MotifEuterpea where

import Euterpea

-- Test case generated with seed 0
motifEuterpea = Modify (Tempo (1 / 2)) (Modify (Instrument (Custom "organ")) (Prim (Note (1 / 1) (C,4)) :+: (Prim (Note (1 / 1) (F,4)) :+: (Prim (Note (1 / 1) (G,4)) :+: (Prim (Rest (1 / 1)) :+: (Prim (Note (1 / 1) (C,4)) :+: (Prim (Note (1 / 1) (F,4)) :+: (Prim (Note (1 / 1) (E,4)) :+: (Prim (Rest (1 / 1)) :+: (Prim (Note (1 / 1) (C,4)) :+: (Prim (Note (1 / 1) (F,4)) :+: Prim (Note (1 / 1) (C,4))))))))))))) :: Music Pitch

