{-# LANGUAGE TemplateHaskell  #-}

module Improb.CodeGen where

import Improb.AST as IS
import Improb.Parser

import Prelude hiding (lookup)

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax(Name(..), NameFlavour(..), showName)
import Data.HashMap.Strict (HashMap, insert, empty, lookup)
import qualified Data.Maybe as Maybe

import qualified Euterpea as EU
import Codec.Midi (Midi)

import System.IO.Unsafe (unsafePerformIO)
import Control.Exception.Base
import Debug.Trace

make_improb_declarations :: Program -> Q [Dec]
make_improb_declarations = genImprobDecl

genImprobDecl :: Program -> Q [Dec]
genImprobDecl (Program t aliases voices) = do
    let aliasStore = mkAliases aliases
    let unAliased = expandTransitions aliasStore voices
    let debug = show unAliased
    [d| a = $([|debug|] ) |]

mkAliases :: [Alias] -> HashMap String MusicPattern
mkAliases = foldr (\a db -> insert (identifier a) (pattern a) db) empty

expandTransitions :: HashMap String MusicPattern -> [Voice] -> [Voice]
expandTransitions store voices = map unAliasVoice voices
    where
        unAliasVoice :: Voice -> Voice
        unAliasVoice (Voice instrument transitions) =
            Voice instrument (map unAliasTransition transitions) 

        unAliasTransition :: Transition -> Transition
        unAliasTransition (Intro mp) =
            (Intro (unAlias mp))
        unAliasTransition (Transition mpL mpR) =
            (Transition (unAlias mpL) (unAlias mpR))

        --BUG : Infinite loop possible with mutually recursive aliases
        unAlias :: MusicPattern -> MusicPattern
        unAlias (Single ml) = Single ml
        unAlias (Continuation head rest) = Continuation (unAlias head) (unAlias rest)
        unAlias (Lookup str) =
            case lookup str store of
                Just mp -> unAlias mp
                Nothing -> error $ "Alias (" ++ str ++ "), not found"

-- The voice is guaranteed to be without aliases
genMap :: Voice -> HashMap MusicPattern MusicPattern
genMap = undefined

walkTransition :: HashMap MusicPattern MusicPattern -> [MusicLiteral]
walkTransition = undefined

toEuterpea :: [MusicLiteral] -> EU.Performance
toEuterpea = undefined

litToEuterpea :: MusicLiteral -> EU.Music EU.Pitch
litToEuterpea (Rest dur) = EU.Prim (EU.Rest (1/ (toRational dur)))
litToEuterpea (Chord ns) = foldr1 ((EU.:+:)) (map (EU.Prim . noteToEuterpea) ns)
litToEuterpea (NoteLiteral n) = EU.Prim (noteToEuterpea n)

noteToEuterpea :: Note -> EU.Primitive EU.Pitch
noteToEuterpea (Note t dur) =
    EU.Note (1/ (toRational dur)) (toPitchClass $ key t, fromInteger $ octave t)

toPitchClass :: Key -> EU.PitchClass
toPitchClass A = EU.A
toPitchClass B = EU.B
toPitchClass C = EU.C
toPitchClass D = EU.D
toPitchClass E = EU.E
toPitchClass F = EU.F
toPitchClass G = EU.G
toPitchClass (Compound k ms) = error "unimplemented"

pickInstrument :: Instrument -> EU.UserPatchMap
pickInstrument = undefined

makethemidi :: EU.Performance -> EU.UserPatchMap -> Midi
makethemidi = undefined

writeMidi :: EU.Performable a => FilePath -> EU.Music a -> IO ()
writeMidi = undefined


