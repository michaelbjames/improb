{-# LANGUAGE TemplateHaskell  #-}

module Improb.CodeGen where

import Improb.AST
import Improb.Parser
import Improb.Transition as IT

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
        unAliased = expandTransitions aliasStore voices
        voiceMapping :: [(Voice, IT.MarkovMap)]
        voiceMapping = map (\x -> (x, genMap x)) unAliased
        finalTransitions :: [IO [MusicLiteral]]
        finalTransitions = (map walkTransition voiceMapping)
    d <- runIO . sequence $ finalTransitions
    let debug = show $ head d
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
-- One MP may map to several options
-- This is a slow opertion because there is no ordering or hashing on MusicPatterns
genMap :: Voice -> IT.MarkovMap
genMap (Voice instrument transitions) =
    let addToStore (Intro mp) store = store
        addToStore (Transition mpl mpr) [] = [(mpl, [mpr])]
        addToStore (Transition mpl mpr) ((left, rights):store) =
            if (mpl == left)
                then (left, mpr:rights):store
                else (left,rights):(addToStore (Transition mpl mpr) store)
    in
        foldr addToStore [] transitions

walkTransition :: (Voice, IT.MarkovMap) -> IO [MusicLiteral]
walkTransition ((Voice instrument transitions), store) = do
    let intros = foldr getIntro [] transitions
    patternChain <- IT.walkTilDone store intros
    let literals = flattenPattern patternChain
    return literals

    where
        getIntro (Intro mp) intros = mp : intros
        getIntro _ intros = intros

        flattenPattern :: MusicPattern -> [MusicLiteral]
        flattenPattern (Single ml) = [ml]
        flattenPattern (Continuation mpl mpr) = flattenPattern mpl ++ flattenPattern mpr
        flattenPattern (Lookup _ ) = error "No lookup should be here"



-- how do i handle multiple voices?
toEuterpea :: [MusicLiteral] -> EU.Performance
toEuterpea mls =
    let music = foldr1 (EU.:+:) (map litToEuterpea mls)
    in  EU.defToPerf music

litToEuterpea :: MusicLiteral -> EU.Music EU.Pitch
litToEuterpea (Rest dur) = EU.Prim (EU.Rest (1/ (toRational dur)))
litToEuterpea (Chord ns) = foldr1 (EU.:=:) (map (EU.Prim . noteToEuterpea) ns)
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
toPitchClass (Compound k m) = case (k,m) of
    (A, Sharp) -> EU.As
    (A, Flat)  -> EU.Af
    (B, Sharp) -> EU.Bs
    (B, Flat)  -> EU.Bf
    (C, Sharp) -> EU.Cs
    (C, Flat)  -> EU.Cf
    (D, Sharp) -> EU.Ds
    (D, Flat)  -> EU.Df
    (E, Sharp) -> EU.Es
    (E, Flat)  -> EU.Ef
    (F, Sharp) -> EU.Fs
    (F, Flat)  -> EU.Ff
    (G, Sharp) -> EU.Gs
    (G, Flat)  -> EU.Gf

pickInstrument :: Instrument -> EU.UserPatchMap
pickInstrument = undefined

makethemidi :: EU.Performance -> EU.UserPatchMap -> Midi
makethemidi = undefined

writeMidi :: EU.Performable a => FilePath -> EU.Music a -> IO ()
writeMidi = undefined


