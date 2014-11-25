module Improb.Transition where

import Improb.AST

import System.Random

type MarkovMap = [(MusicPattern, [MusicPattern])]
type MarkovState = (MusicPattern, StdGen)

begin :: StdGen -> [MusicPattern] -> MarkovState
begin g beginStates =
    let (index, newGen) = randomR (0, length beginStates - 1) g
        startPattern = beginStates !! index
    in
        (startPattern, newGen)

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


walkTilDone :: StdGen -> MarkovMap -> [MusicPattern] -> MusicPattern
walkTilDone g store beginStates =
    if (willTerminate store)
    then
        let (state, gen) = begin g beginStates
        in
            keepWalking (state, gen)
    else error "Music cannot loop forever! Add an end node!"
    where
        keepWalking :: MarkovState -> MusicPattern
        keepWalking markovState =
            case transition store markovState of
                Right finalState -> finalState
                Left markovState -> keepWalking markovState
