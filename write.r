library(tidyverse)
library(tuneR)

source("lilyinput.r")

midiNotes <- getMidiNotes(readMidi("FF1airsh.mid"))

channel_0 <- midiNotes %>% filter(channel == 0) %>% prepare_input

lilyinput2(channel_0,
          file = "Rsong.ly",
          Major = TRUE,
          key = "c",
          clef = "treble",
          time = "4/4",
          endbar = FALSE,
          midi = TRUE,
          tempo = "4 = 150")

# a lilypond file named "Rsong.ly" will be written to the current directory.
# if you have lilypond installed, you can drag and drop this file onto the
# lilypond desktop shortcut. Then you will have a score and midi file generated
# from your lilypond file. Both should correspond to the 'channel_0' data frame. 