module AST where

data Program = Program [Alias] Tempo [Voice]

data Alias = Alias
    { identifier :: String
    , pattern :: MusicPattern
    }

type Tempo = Integer

data Voice = Voice Instrument [Transition]

type Instrument = String

data Transition
    = Intro MusicPattern
    | Transition MusicPattern MusicPattern deriving (Show)

data MusicPattern
    = Single MusicLiteral
    | Continuation MusicPattern MusicPattern deriving (Show)

data MusicLiteral = Chord [Note] | NoteLiteral Note | Rest Duration deriving (Show)

data Note = Note Tone Duration deriving (Show)

type Duration = Integer

data Tone = Tone {octave :: Integer, key :: Key} deriving (Show)

data Key = A | B | C | D | E | F | G | Compound Key [Modifier] deriving (Show)

data Modifier = Sharp | Flat deriving (Show)


