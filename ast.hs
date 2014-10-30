module AST where

data Program = Program [Alias] [Voice]

data Alias = Alias
    { identifier :: String
    , pattern :: MusicPattern
    }

data Voice = Voice Instrument [Transition]

data Instrument = Instrument String

data Transition
    = Intro MusicPattern
    | Transition MusicPattern MusicPattern

data MusicPattern
    = Single MusicLiteral
    | Continuation MusicPattern MusicPattern

data MusicLiteral = Chord [MusicLiteral] | Note Tone Integer

data Tone = Tone {octave :: Integer, key :: Key}

data Key = A | B | C | D | E | F | G | Compound Key Modifier

data Modifier = Sharp | Flat | DoubleMod Modifier


