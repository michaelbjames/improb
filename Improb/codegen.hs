{-# LANGUAGE TemplateHaskell  #-}

module Improb.CodeGen where

import Improb.AST as IS
import Improb.Parser

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax(Name(..), NameFlavour(..), showName)

import qualified Data.Maybe as Maybe

make_improb_declarations :: Program -> Q [Dec]
make_improb_declarations = genImprobDecl

genImprobDecl :: Program -> Q [Dec]
genImprobDecl (Program t aliases voices) =
    return [ValD (LitP (CharL 'a')) (NormalB (LitE (CharL 'e'))) []]
