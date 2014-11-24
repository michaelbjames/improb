module Improb.Translation where

import Improb.AST

import qualified Euterpea as EU
import Euterpea.IO.MIDI.ToMidi
import Codec.Midi (Midi)


toEuterpea :: Tempo -> [(Instrument, [MusicLiteral])] -> EU.Music EU.Pitch
toEuterpea tempo voices =
    let euterpeaVoices :: [(EU.InstrumentName, EU.Music EU.Pitch)]
        euterpeaVoices = map genEuterpeaMusic voices
        litsToMusic :: [MusicLiteral] -> EU.Music EU.Pitch
        litsToMusic mls = foldr1 (EU.:+:) (map litToEuterpea mls)
        genEuterpeaMusic :: (Instrument, [MusicLiteral]) -> (EU.InstrumentName, EU.Music EU.Pitch)
        genEuterpeaMusic (instr, mls) = (translateInstrumentName instr, litsToMusic mls)
        addInstrument :: (EU.InstrumentName, EU.Music EU.Pitch) -> EU.Music EU.Pitch
        addInstrument (name, m) = EU.Modify (EU.Instrument name) m
        addTempo :: EU.Music a -> EU.Music a
        addTempo = EU.Modify (EU.Tempo ((toRational tempo) / 120))
        voiceMusic :: [EU.Music EU.Pitch]
        voiceMusic = map addInstrument euterpeaVoices
        grandMusic :: EU.Music EU.Pitch
        grandMusic = foldr1 (EU.:=:) voiceMusic
    in
        addTempo grandMusic

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

translateInstrumentName :: String -> EU.InstrumentName
translateInstrumentName "piano" = EU.AcousticGrandPiano
translateInstrumentName "violin" = EU.Violin
translateInstrumentName "cello" = EU.Cello
translateInstrumentName str = EU.Custom str

