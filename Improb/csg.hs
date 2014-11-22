module Improb.CFG where

import Data.List
import System.Environment
import qualified Data.Map as Map
import Control.Monad.State
import System.Random

--train :: trans -> [trans] -> State trans -> State trans

--transition :: State trans -> trans -> g -> trans

-- thanks http://codereview.stackexchange.com/questions/24791/haskell-markov-text-generator

type MarkovMap = Map.Map String String
type MarkovState = (StdGen, String)

transition :: MarkovMap -> State MarkovState Char
transition m = do 
    (gen, sofar) <- get
    let options = m Map.! sofar
        (index, newGen) = randomR (0, length options - 1) gen
        next = options !! index
    put (newGen, tail sofar ++ [next])
    return next

getWords :: MarkovMap -> Int -> [String]
getWords m n =
    let keys        = filter ((==) ' ' . last) $ Map.keys m 
        (r, gen)    = randomR (0, length keys - 1) $ mkStdGen 137
        startState  = keys !! r 
        markovChars = evalState (sequence . repeat $ transition m) (gen, startState)
    in  take n . words $ markovChars
