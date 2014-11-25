module Examples.Tests.Duet where

import Improb.AST

-- TODO: Read in from the actual file and roll in the IO monad
duetString = unlines
    [ "tempo: 100"
    , "motif := [(C4,1),(G4,2)] -> (F4,1) -> (G4,1) -> (R,1)"
    , "v1 := (C4,1) -> (Ab4,1) -> (E4,1) -> (R,1)"
    , "v2 := (C4,1) -> (F4,1) -> (E4,1) -> (R,1)"
    , "end := (C4,1) -> (F4,1) -> (C4,1)"
    , ":violin:"
    , "=> motif"
    , "motif => v1 -> v2 -> v2"
    , "motif => v1 -> v2 -> v1"
    , "motif => v2 -> v1"
    , "motif => v1 -> v1"
    , "v1 -> v2 => motif"
    , "v1 => v2"
    , "v1 => motif"
    , "v2 => motif"
    , "v2 => end"
    , ":cello:"
    , "=> motif"
    , "motif => v1 -> (R,4)"
    , "motif => v2"
    , "motif => v2 -> (C4,4)"
    , "(R,1) -> (R,4) => motif"
    , "v2 -> (C4,4) => motif"
    , "v2 => end"
    ]

duetExpects = Right (
    Program 100
        [ Alias
            { identifier = "motif"
            , pattern =
                Continuation (Single (Chord [Note (Tone {octave = 4, key = C}) 1,Note (Tone {octave = 4, key = G}) 2])) 
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = F}) 1))) 
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = G}) 1)))
                (Single (Rest 1))))
            }
        , Alias
            { identifier = "v1"
            , pattern =
                Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = C}) 1)))
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = Compound A Flat}) 1)))
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = E}) 1)))
                (Single (Rest 1))))
            }
        , Alias
            { identifier = "v2"
            , pattern =
                Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = C}) 1)))
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = F}) 1)))
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = E}) 1)))
                (Single (Rest 1))))
            }
        , Alias
            { identifier = "end"
            , pattern =
                Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = C}) 1)))
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = F}) 1)))
                (Single (NoteLiteral (Note (Tone {octave = 4, key = C}) 1))))
            }
        ]
        [ Voice "violin"
            [ Intro (Lookup "motif")
            , Transition
                (Lookup "motif")
                (Continuation (Lookup "v1")
                    (Continuation (Lookup "v2") (Lookup "v2")))
            , Transition
                (Lookup "motif")
                (Continuation (Lookup "v1")
                    (Continuation (Lookup "v2") (Lookup "v1")))
            , Transition
                (Lookup "motif")
                (Continuation (Lookup "v2") (Lookup "v1"))
            , Transition
                (Lookup "motif")
                (Continuation (Lookup "v1") (Lookup "v1"))
            , Transition 
                (Continuation (Lookup "v1") (Lookup "v2"))
                (Lookup "motif")
            , Transition (Lookup "v1") (Lookup "v2")
            , Transition (Lookup "v1") (Lookup "motif")
            , Transition (Lookup "v2") (Lookup "motif")
            , Transition (Lookup "v2") (Lookup "end")
            ]
        , Voice "cello"
            [ Intro (Lookup "motif")
            , Transition
                (Lookup "motif")
                (Continuation (Lookup "v1") (Single (Rest 4)))
            , Transition (Lookup "motif") (Lookup "v2")
            , Transition
                (Lookup "motif")
                (Continuation
                    (Lookup "v2")
                    (Single (NoteLiteral (Note (Tone {octave = 4, key = C}) 4))))
            , Transition 
                (Continuation (Single (Rest 1)) (Single (Rest 4)))
                (Lookup "motif")
            , Transition
                (Continuation (Lookup "v2")
                    (Single (NoteLiteral (Note (Tone {octave = 4, key = C}) 4))))
                (Lookup "motif")
            , Transition (Lookup "v2") (Lookup "end")
            ]
        ]
    )
