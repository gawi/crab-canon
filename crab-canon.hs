module CrabCanon where

import Haskore.Music
import Haskore.Music.Standard
import Haskore.Melody.Standard as Melody

import qualified Haskore.Interface.MIDI.Render as MidiRender
import Haskore.Music.GeneralMIDI as MidiMusic

import qualified Data.List as List
import System.Exit

fd :: t -> (t -> NoteAttributes -> m) -> m 
fd duration n = n duration Melody.na

melody :: [Melody.T]
melody =    (map (fd hn) [c  4, ef 4, g  4, af 4, b  3])
         ++ [(loudness1 0) $ (b 3 qn) Melody.na]
         ++ (map (fd hn) [g  4, fs 4, f  4, e  4, ef 4])
         ++ (map (fd qn) [d  4, df 4, c  4, b  3, g  3, c  4, f 4])
         ++ (map (fd hn) [ef 4, d  4, c  4, ef 4])
         ++ (map (fd en) [g  4, f  4, g  4, c  5, g  4, ef 4, d  4, ef 4,
                          f  4, g  4, a  4, b  4, c  5, ef 4, f  4, g  4,
                          af 4, d  4, ef 4, f  4, g  4, f  4, ef 4, d  4,
                          ef 4, f  4, g  4, af 4, bf 4, af 4, g  4, f  4,
                          g  4, af 4, bf 4, c  5, df 5, bf 4, af 4, g  4,
                          a  4, b  4, c  5, d  5, ef 5, c  5, b  4, a  4,
                          b  4, c  5, d  5, ef 5, f  5, d  5, g  4, d  5,
                          c  5, d  5, ef 5, f  5, ef 5, d  5, c  5, b  4])
         ++ (map (fd qn) [c  5, g  4, ef 4, c 4])

crabCanon :: Melody.T
crabCanon = (line melody)
             =:= 
            (line (List.reverse melody))

play :: IO ExitCode
play = MidiRender.playTimidity $ (MidiMusic.fromStdMelody MidiMusic.Harpsichord $ transpose (-36) $ changeTempo 2 $ (crabCanon))