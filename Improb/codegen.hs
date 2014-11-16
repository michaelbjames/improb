{-# LANGUAGE TemplateHaskell  #-}

module Improb.CodeGen where

import Improb.AST as IS
import Improb.Parser

import Prelude hiding (lookup)

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax(Name(..), NameFlavour(..), showName)
import Data.HashMap.Strict (HashMap, insert, empty, lookup)

import qualified Data.Maybe as Maybe

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

        unAlias :: MusicPattern -> MusicPattern
        unAlias (Single ml) = Single ml
        unAlias (Continuation head rest) = Continuation (unAlias head) (unAlias rest)
        unAlias (Lookup str) =
            case lookup str store of
                Just mp -> mp
                Nothing -> error $ "Alias (" ++ str ++ "), not found"
