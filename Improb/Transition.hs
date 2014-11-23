module Improb.Transition where

import Improb.AST

import System.Random

type MarkovMap = [(MusicPattern, [MusicPattern])]
type MarkovState = (MusicPattern, StdGen)

begin :: [MusicPattern] -> IO MarkovState
begin beginStates = do
    myGen <- newStdGen
    let (index, newGen) = randomR (0, length beginStates - 1) myGen
    let startPattern = beginStates !! index
    return (startPattern, newGen)

transition :: MarkovMap -> MarkovState -> Either MarkovState MusicPattern
transition store (state, gen) =
    let options = possibleTransitions store state
    in  case options of
            [] -> Right state
            _  ->
                let (index, newGen) = randomR (0, length options - 1) gen
                    nextSegment = options !! index
                in  Left (rightAlign (Continuation state nextSegment), newGen)

rightAlign :: MusicPattern -> MusicPattern
rightAlign (Continuation (Continuation mpll mplr) mpr) =
    (Continuation (rightAlign mpll) (Continuation (rightAlign mplr) mpr))
rightAlign (Lookup str) = lookupError 
rightAlign mp = mp

-- [(MP,MP)] = [(start.)]
possibleTransitions :: MarkovMap -> MusicPattern -> [MusicPattern]
possibleTransitions store (Continuation mpl mpr) =
    (case lookup (Continuation mpl mpr) store of
        Just c -> c
        Nothing -> []) ++ (possibleTransitions store mpr)
possibleTransitions store (Single ml) =
    maybe [] id $ lookup (Single ml) store
possibleTransitions store (Lookup str) = lookupError


lookupError = error "There should be no lookups at this point"


walkTilDone :: MarkovMap -> [MusicPattern] -> MusicPattern
walkTilDone store beginStates = undefined
