module Examples.First where

import Test.HUnit hiding (test)
import Parser
import Examples.Branching
import Examples.Motif
import Examples.Duet
import Text.Parsec.Error

test = runTestTT testList

testList = TestList
    [ TestLabel "branching" branchingTest
    , TestLabel "motif" motifTest
    , TestLabel "duet" duetTest
    ]

-- Uh, this feels distinctly bad.
-- I've never used HUnit before, hopefully better practices will come
-- later in the course.
instance Eq ParseError where
    e1 == e2 = (show e1) == (show e2)

branchingObserved = parseProgram branchingString
branchingTest = TestCase (branchExpects @=? branchingObserved)

motifObserved = parseProgram motifString
motifTest = TestCase (motifExpects @=? motifObserved)

duetObserved = parseProgram duetString
duetTest = TestCase (duetExpects @=? duetObserved)
