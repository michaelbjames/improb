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
import System.FilePath
import qualified Data.Maybe as Maybe

import qualified Euterpea as EU
import Codec.Midi (Midi)

make_improb_declarations :: Program -> Q [Dec]
make_improb_declarations = genImprobDecl

genImprobDecl :: Program -> Q [Dec]
genImprobDecl (Program tempo aliases voices) = do
    let aliasStore = mkAliases aliases
        unAliased = expandTransitions aliasStore voices
        voiceMapping :: [(Voice, IT.MarkovMap)]
        voiceMapping = map (\x -> (x, genMap x)) unAliased
        finalTransitions :: [IO (Instrument, [MusicLiteral])]
        finalTransitions = (map walkTransition voiceMapping)
    loc <- location
    transitions <- runIO . sequence $ finalTransitions
    let euterpeaMusic = translateToEuterpea tempo transitions
        filename = loc_filename loc
        newFilename = (dropExtension filename) <.> "mid"
    runIO (EU.writeMidi newFilename euterpeaMusic)
    let debug = show $ euterpeaMusic
    [d| _ = $([|debug|])|]

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
-- This is a slow opertion because there is no ordering or hashing on MusicPatterns
-- So we must use a data structure with only Eq and not Ord or Hashable
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



translateToEuterpea :: Tempo -> [(Instrument, [MusicLiteral])] -> EU.Music EU.Pitch
translateToEuterpea = toEuterpea



