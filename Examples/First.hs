module Examples.First where

import Test.HUnit hiding (test)
import Improb.Parser
import Improb.CodeGen

import Examples.Tests.Branching
import Examples.Tests.BranchingEuterpea
import Examples.Tests.Motif
import Examples.Tests.MotifEuterpea
import Examples.Tests.Duet
import Examples.Tests.DuetEuterpea


import Text.Parsec.Error
import System.Random

test = runTestTT testList

testList = TestList
    [ TestLabel "branching" branchingTest
    , TestLabel "branching Euterpea representation" branchEuterpeaTest
    , TestLabel "motif" motifTest
    , TestLabel "motif Euterpea representation" motifEuterpeaTest
    , TestLabel "duet" duetTest
    , TestLabel "duet Euterpea representation" duetEuterpeaTest
    ]

-- Uh, this feels distinctly bad.
-- I've never used HUnit before, hopefully better practices will come
-- later in the course.
instance Eq ParseError where
    e1 == e2 = (show e1) == (show e2)


branchingObserved = parseProgram branchingString
branchingTest = TestCase (branchExpects @=? branchingObserved)

branchEuterpeaObserved =
    case parseProgram branchingString  of
        Right prg -> genEuterpeaMusic (mkStdGen 0) prg
        Left _ -> error "branch did not parse"
branchEuterpeaTest = TestCase (branchingEuterpea @=? branchEuterpeaObserved)



motifObserved = parseProgram motifString
motifTest = TestCase (motifExpects @=? motifObserved)

motifEuterpeaObserved =
    case parseProgram motifString  of
        Right prg -> genEuterpeaMusic (mkStdGen 0) prg
        Left _ -> error "branch did not parse"
motifEuterpeaTest = TestCase (motifEuterpea @=? motifEuterpeaObserved)



duetObserved = parseProgram duetString
duetTest = TestCase (duetExpects @=? duetObserved)

duetEuterpeaObserved =
    case parseProgram duetString  of
        Right prg -> genEuterpeaMusic (mkStdGen 0) prg
        Left _ -> error "branch did not parse"
duetEuterpeaTest = TestCase (duetEuterpea @=? duetEuterpeaObserved)

