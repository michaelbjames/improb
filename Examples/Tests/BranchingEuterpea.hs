module Examples.Tests.BranchingEuterpea where

import Euterpea

-- Test case generated with seed 0
branchingEuterpea = Modify (Tempo (1 / 1)) (Modify (Instrument AcousticGrandPiano) (Prim (Note (1 / 4) (C,4)) :+: Prim (Note (1 / 2) (Ef,4)))) :: Music Pitch
