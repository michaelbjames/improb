module Improb.AST where

data Program = Program Tempo [Alias] [Voice] deriving (Eq, Show)

data Alias = Alias
    { identifier :: String
    , pattern :: MusicPattern
    } deriving (Eq, Show)

type Tempo = Integer

data Voice = Voice Instrument [Transition] deriving (Eq, Show)

type Instrument = String

data Transition
    = Intro MusicPattern
    | Transition MusicPattern MusicPattern deriving (Eq, Show)

data MusicPattern
    = Single MusicLiteral
    | Lookup String
    | Continuation MusicPattern MusicPattern deriving (Eq, Show)

data MusicLiteral = Chord [Note] | NoteLiteral Note | Rest Duration deriving (Eq, Show)

data Note = Note Tone Duration deriving (Eq, Show)

type Duration = Integer

data Tone = Tone {octave :: Integer, key :: Key} deriving (Eq, Show)

data Key = A | B | C | D | E | F | G | Compound Key [Modifier] deriving (Eq, Show)

data Modifier = Sharp | Flat deriving (Eq, Show)


