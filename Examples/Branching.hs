module Examples.Branching where

import Improb.AST

-- TODO: Read in from the actual file and roll in the IO monad
branchingString = unlines
    [ "tempo: 120"
    , ":piano:"
    , "=> (C4,4)"
    , "(C4,4) => (F4,2)"
    , "(C4,4) => (Eb4,2)"
    , "(F4,2) => (G4,4)"
    ]

c44 = (Single (NoteLiteral (Note (Tone {octave = 4, key = C}) 4)))
f42 = (Single (NoteLiteral (Note (Tone {octave = 4, key = F}) 2)))
eb42 = (Single (NoteLiteral (Note (Tone {octave = 4, key = Compound E Flat}) 2)))
g44 = (Single (NoteLiteral (Note (Tone {octave = 4, key = G}) 4)))

branchExpects = Right (
    Program 120 [] [
        Voice "piano" [
              Intro c44
            , Transition c44 f42
            , Transition c44 eb42
            , Transition f42 g44
            ]
        ]
    )
