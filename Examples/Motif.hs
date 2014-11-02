module Examples.Motif where

import AST

motifString = unlines [
      "tempo: 60"
    , "motif := (C4,1) -> (F4,1) -> (G4,1) -> (R,1)"
    , "variation1 := (C4,1) -> (Ab4,1) -> (E4,1) -> (R,1)"
    , "variation2 := (C4,1) -> (F4,1) -> (E4,1) -> (R,1)"
    , "end := (C4,1) -> (F4,1) -> (C4,1)"
    , ":organ:"
    , "=> motif"
    , "motif => variation1"
    , "motif => variation2"
    , "variation1 => motif"
    , "variation2 => motif"
    , "variation2 => end"
    ]

motifExpects = Right (
    Program 60
        [ Alias
            { identifier = "motif"
            , pattern =
                Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = C}) 1)))
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = F}) 1)))
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = G}) 1)))
                (Single (Rest 1))))
            }
        , Alias
            { identifier = "variation1"
            , pattern =
                Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = C}) 1)))
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = Compound A [Flat]}) 1)))
                (Continuation (Single (NoteLiteral (Note (Tone {octave = 4, key = E}) 1)))
                (Single (Rest 1))))
            }
        , Alias
            { identifier = "variation2"
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
        [ Voice "organ"
            [ Intro (Lookup "motif")
            , Transition (Lookup "motif") (Lookup "variation1")
            , Transition (Lookup "motif") (Lookup "variation2")
            , Transition (Lookup "variation1") (Lookup "motif")
            , Transition (Lookup "variation2") (Lookup "motif")
            , Transition (Lookup "variation2") (Lookup "end")
            ]
        ]
    )
