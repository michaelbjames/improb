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
    where
        rightAlign :: MusicPattern -> MusicPattern
        rightAlign (Continuation (Continuation mpll mplr) mpr) =
            (Continuation (rightAlign mpll) (Continuation (rightAlign mplr) mpr))
        rightAlign (Lookup str) = lookupError 
        rightAlign mp = mp


possibleTransitions :: MarkovMap -> MusicPattern -> [MusicPattern]
possibleTransitions store (Continuation mpl mpr) =
    (case lookup (Continuation mpl mpr) store of
        Just c -> c
        Nothing -> []) ++ (possibleTransitions store mpr)
possibleTransitions store (Single ml) =
    maybe [] id $ lookup (Single ml) store
possibleTransitions store (Lookup str) = lookupError

willTerminate :: MarkovMap -> Bool
willTerminate store =
    let ends :: [(MusicPattern, MusicPattern)] -- [(pattern match, transition)]
        ends = foldr (++) [] $ map (\(x,ys) -> map ((,) x ) ys) store
        startHasEnd (start, transition) = (start, possibleTransitions store transition)
        endPoints = filter (\(st, trans) -> (length trans) == 0 ) $ map startHasEnd ends
    in
        length endPoints > 0

lookupError = error "There should be no lookups at this point"


walkTilDone :: MarkovMap -> [MusicPattern] -> IO MusicPattern
walkTilDone store beginStates =
    if (willTerminate store)
    then do
        (state, gen) <- begin beginStates
        keepWalking (state, gen)
    else error "Music cannot loop forever! Add an end node!"
    where
        keepWalking :: MarkovState -> IO MusicPattern
        keepWalking markovState =
            case transition store markovState of
                Right finalState -> return finalState
                Left (nextState, nextGen) -> keepWalking (nextState, nextGen)
