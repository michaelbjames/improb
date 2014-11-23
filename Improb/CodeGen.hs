{-# LANGUAGE TemplateHaskell  #-}

module Improb.CodeGen where

import Improb.AST
import Improb.Parser
import Improb.Transition as IT
import Improb.Translation

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
        finalTransitions :: [IO (Instrument, [MusicLiteral])]
        finalTransitions = (map walkTransition voiceMapping)
    transitions <- runIO . sequence $ finalTransitions
    let euterpeaPerformance = translateToEuterpea transitions
        finalMidi = makethemidi euterpeaPerformance
    runIO (EU.exportMidiFile "improb.mid" finalMidi)
    let debug = show $ transitions
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

walkTransition :: (Voice, IT.MarkovMap) -> IO (Instrument, [MusicLiteral])
walkTransition ((Voice instrument transitions), store) = do
    let intros = foldr getIntro [] transitions
    patternChain <- IT.walkTilDone store intros
    let literals = flattenPattern patternChain
    return (instrument, literals)

    where
        getIntro (Intro mp) intros = mp : intros
        getIntro _ intros = intros

        flattenPattern :: MusicPattern -> [MusicLiteral]
        flattenPattern (Single ml) = [ml]
        flattenPattern (Continuation mpl mpr) = flattenPattern mpl ++ flattenPattern mpr
        flattenPattern (Lookup _ ) = error "No lookup should be here"



translateToEuterpea :: [(Instrument, [MusicLiteral])] -> EU.Performance
translateToEuterpea = toEuterpea

makethemidi :: EU.Performance -> Midi
makethemidi perf = EU.toMidi perf EU.defUpm


