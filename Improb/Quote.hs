{-# LANGUAGE TemplateHaskell  #-}

module Improb.Quote where

import System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import qualified Improb.Parser as P
import Improb.CodeGen


{-
  The PADS quasi-quoter only has declaration forms, so the forms
  for expressions, patterns, and types should not arise.
-}
improb :: QuasiQuoter
improb  = QuasiQuoter (error "parse expression")
                    (error "parse pattern")
                    (error "parse type")
                    imparse

--imparse :: String -> TH.Q [TH.Dec]
--imparse input = do
--    loc <- TH.location
--    let fileName = TH.loc_filename loc
--    let (line,column) = TH.loc_start loc
--    case P.parsePadsDecls fileName line column input of
--      Left err -> unsafePerformIO $ fail $ show err
--      Right x  -> make_improb_declarations x

imparse :: String -> TH.Q [TH.Dec]
imparse input = do
    case P.parseProgram input of
        Left err -> unsafePerformIO $ fail $ show err
        Right x -> make_improb_declarations x
        --Right x -> error "unimplemented"

