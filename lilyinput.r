library(tidyverse)
library(tuneR)

#### ticks2rhythm ####

# ticks2rhythm takes arguments tick_value and tpqn.
# tick_value is defined by a note length or inter onset interval
# tpqn is a constant that defines the ticks per quarter note (defaults to 480)

# ticks2rhythm returns a string that is a precursor to lilypond notation. 

# ticks2rhythm checks whether tick_value can be converted to a lilypond rhythm.
# If tick_value matches up with a rhythm, the function returns that rhythm.
# If tick_value exceeds the max allowable rhythm (1 whole note),
# ticks2rhythm returns a series of tied notes equivalent to the long tick value.
# If tick_value can't be converted, ticks2rhythm defaults to a quarter note.

ticks2rhythm <- function(tick_value, tpqn = 480){
        
        #set of allowable note symbol durations
        rhythms <- list("1", "2.", "2", "4.", "4", "8.", 
                        "8","16.","16","32", "64")
        
        #ticks corresponding to undotted 1/64 -> 1
        undotted <- 2^(-4:2)*tpqn
        undotted <- sort(undotted, decreasing =TRUE)
        
        #ticks corresponding to dotted 1/16 -> 1
        dotted <- 2^(-3:0)*tpqn + 2^(-2:1)*tpqn
        dotted <- sort(dotted, decreasing =TRUE)
        
        #collect allowable tick durations
        ticks <- sort(c(undotted,dotted), decreasing = TRUE)
        
        lookup_rhythm <- function(tick_value){
                #returns the rhythm that corresponds with tick_value        
                do.call(switch, c(tick_value, setNames(rhythms, ticks)))
        }
        
        
        if(any(tick_value %in% ticks)){
                
                #simple case of looking up the rhythm for a matching tick_value
                
                ticks <- as.character(ticks)
                tick_value <- as.character(tick_value)
                ret <-  lookup_rhythm(tick_value)
                
        }else if(tick_value > max(ticks)){
                
                difference <- tick_value
                tied_notes <- integer()
                
                # looping method for when tick_value > whole note:
                # take the biggest possible undotted tick value, the subtrahend.
                # combine it with all previous subtrahends in tied_notes.
                # subtract subtrahend from difference.
                # loop until difference can coerce to an undotted tick value.
                while(!difference %in% undotted){
                        subtrahend <- head(undotted[difference>undotted],1)
                        tied_notes <- c(tied_notes, subtrahend)
                        difference <- difference - subtrahend
                }
                
                # combine difference with subtrahends in tied_notes
                tied_notes <- c(tied_notes, difference)
                tied_notes <- as.character(tied_notes)
                
                # convert tied_notes to lilypond rhythms
                tied_notes <- lapply(tied_notes, lookup_rhythm)
                tied_notes <- unlist(tied_notes)
                
                # this string is a precursor to lilypond input
                # tie together notes with ~
                # "z" is a placeholder for future pitch value
                ret <- paste0("z", 
                              tied_notes,
                              c(rep("~ ", length(tied_notes)-1),""),
                              collapse = " ")
                
        }else{ret = "4"} #TODO handle more cases; e.g. miditick is prime
        
        ret
}

#### prepare_input ####

# prepare_input adds new variables to a data frame 
# so that lilyinput2 can use the data

# prepare_input takes as input the output of tuneR::getMidiNotes
# it adjusts the values in the notename column to conform with lilypond style.
# it also adds columns that lilyinput2 needs for writing notes, including:
# * a column for interonset interval (IOI),
# * a column for duration. has strings almost written in lilypond notation,
# * a column indicating whether a given duration ties notes together.

prepare_input <- function(midiNotes){
        mutate( .data    = midiNotes,
                notename = str_replace(notename, "#", "is"),
                ioi      = c(diff(time), last(length)),
                duration = sapply(ioi, ticks2rhythm),
                is_tied  = str_detect(duration, "~"))
}

#### lilyinput 2 ####

# lilyinput2 reuses a lot of code from tuneR::lilyinput.

# lilyinput2 takes a data frame as input and has many arguments for options

# the data frame needs columns named pitch, duration, and is_tied
# and will use those columns to write the notes in the lilypond file.
# you can generate these columns using the prepare_input function with
# the output of tuneR::getMidiNotes as the input. 

lilyinput2 <- function(X, file = "Rsong.ly", 
                      Major = TRUE, key = "c", 
                      clef = c("treble", "bass", "alto", "tenor"), 
                      time = "4/4", endbar = TRUE, midi = TRUE, 
                      tempo = "2 = 60", 
                      textheight = 220, linewidth = 150, indent = 0, 
                      fontsize = 14){
        
        clef <- match.arg(clef)
        
        # Initializing
        notes <- character(length(X$notename)) 
        
        # note components: pitch? duration? is it tied?
        pitch <- ifelse(is.na(X$notename), "r", X$notename)
        duration <- ifelse(is.na(X$duration), NA, X$duration)
        is_tied <- X$is_tied
     
        # join notes:
        notes <- ifelse(is_tied, 
                        str_replace_all(duration, "z", pitch),
                        paste(pitch, duration, sep = ""))
        
        # mode
        mode <- if(Major) "\\major" else "\\minor"        
        
        # key
        if(Major){
                pot <- c("fis" , "h" , "e" , "a" , "d" , "g" , "c" , "f" , 
                         "b" , "es" , "as" , "des" , "ges") 
                if(!(key %in% pot))
                        stop("Wrong key, possible major keys are:\n", 
                             paste(pot, collapse = " "), "\n")
        }else{
                pot <- c("cis" , "gis" , "dis" , "fis" , "h" , "e" , "a" , 
                         "d" , "g" , "c" , "f" , "b" , "es")
                if(!(key %in% pot))
                        stop("Wrong key, possible minor keys are:\n", 
                             paste(pot, collapse = " "), "\n")
        }
        if(key == "b"){key <- "bes"
        }else {if(key == "h") key <- "b"}
        
        # generate LilyPond file:
        
        write(file = file,
              c("\\version \"2.18.2\"",
                paste("#(set-global-staff-size", fontsize, ")"),
                "\\header{tagline = \"\"}",
                "\\melody = {",
                paste("    \\time", time),
                paste("    \\key", key, mode),
                paste("    \\clef", clef),
                paste("   ", notes),
                if(endbar) 
                        "   \\bar \"|.\"",
                "  }",
                "  \\paper{",
                paste("    textheight = ", textheight, ".\\mm", sep = ""),
                paste("    linewidth = ", linewidth, ".\\mm", sep = ""),
                paste("    indent = ", indent, ".\\mm", sep = ""),
                "  }",  
                "   \\score{",
                "   \\melody",
                "   \\layout{ }",
                if(midi){ c("    \\midi{",  paste("      \\tempo", tempo), "  }")},
                "   }"))
}

