library(tidyverse)
library(tuneR)

source("load.R")

symbolize_dur <- function(x, tpqn = 480){
        
        durs <- list("1", "2.", "2", "4.", "4", "8.", "8","16.","16","32", "64")
        
        #undotted 1/64 -> 1
        undot <- 2^(-4:2)*tpqn
        undot <- sort(undot, decreasing =TRUE)
        
        #dotted 1/16 -> 1
        dot <- 2^(-3:0)*tpqn + 2^(-2:1)*tpqn
        dot <- sort(dot, decreasing =TRUE)
        
        ticks <- sort(c(undot,dot), decreasing = TRUE)
        
        if(any(x %in% ticks)){
                ticks <- as.character(ticks)
                ret <-  do.call(switch, c(as.character(x), setNames(durs, ticks)))
        }else if(x > max(ticks)){
                temp1 <- x
                temp2 <- integer()
                while(!temp1 %in% undot){
                        temp2 <- c(temp2, head(undot[temp1>undot],1))
                        temp1 <- temp1 - head(undot[temp1>undot],1)
                }
                temp2 <- c(temp2, temp1)
                temp2 <- as.character(temp2)
                temp2 <- lapply(temp2, function(x) do.call(switch, 
                                                  c(x, setNames(durs, ticks))))
                temp2 <- unlist(temp2)
                
                ret <- paste0("z", 
                       temp2, 
                       c(rep("~ ", length(temp2)-1),""), 
                       collapse = " ")
        }
        ret
}

lilyinput <- function(X, file = "Rsong.ly", 
                      Major = TRUE, key = "c", 
                      clef = c("treble", "bass", "alto", "tenor"), 
                      time = "4/4", endbar = TRUE, midi = TRUE, 
                      tempo = "2 = 60", 
                      textheight = 220, linewidth = 150, indent = 0, 
                      fontsize = 14)
{
        clef <- match.arg(clef)
        
        # notes, 97 entries in the pot (a,,, - a'''''):
        if(Major){
                pot <- 
                        switch(key,
                               d = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
                               e = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "ais", "b"),
                               f = c("c", "cis", "d", "es", "e", "f", "fis", "g", "as", "a", "bes", "b"),
                               g = c("c", "cis", "d", "es", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
                               a = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
                               b = c("c", "des", "d", "es", "e", "f", "fis", "g", "as", "a", "bes", "b"),
                               es = c("c", "des", "d", "es", "e", "f", "ges", "g", "as", "a", "bes", "b"),
                               c("c", "cis", "d", "es", "e", "f", "fis", "g", "gis", "a", "bes", "b")
                        )
        }else{
                pot <- 
                        switch(key,
                               h = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
                               cis = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "ais", "b"),
                               d = c("c", "cis", "d", "es", "e", "f", "fis", "g", "as", "a", "bes", "b"),
                               e = c("c", "cis", "d", "es", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
                               fis = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
                               g = c("c", "des", "d", "es", "e", "f", "fis", "g", "as", "a", "bes", "b"),
                               c = c("c", "des", "d", "es", "e", "f", "ges", "g", "as", "a", "bes", "b"),
                               c("c", "cis", "d", "es", "e", "f", "fis", "g", "gis", "a", "bes", "b")
                        )
        }  
        
        pot <- unlist(lapply(
                c(",,,", ",,", ",", "", "'", "''", "'''", "''''", "'''''"), 
                function(x) paste(pot, x, sep="")))[-c(1:9, 107:108)]
        # Initializing
        tie <- toene <- character(length(X$note)) 
        
        # note: pitch, length, punctuation
        note <- ifelse(is.na(X$note), "r", X$note)
        duration <- ifelse(is.na(X$duration), NA, X$duration)
        tie <- X$tie
        #punctuation <- ifelse(X$punctuation, ".", "")
        
        # # start/end of slurs:
        # if(sum(X$slur) %% 2) 
        #         stop("More starting than ending slurs")
        # slur[which(X$slur)] <- 
        #         rep(c("(", ")"), sum(X$slur) %/% 2)
        # join notes:
        toene <- ifelse(tie, 
                        str_replace_all(duration, "z", note),
                        paste(note, duration, sep = ""))
        
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
        write(file = "test.txt",
              c("\\version \"2.18.2\"",
                paste("#(set-global-staff-size", fontsize, ")"),
                "\\header{tagline = \"\"}",
                "\\Melody = {",
                paste("    \\time", time),
                paste("    \\key", key, mode),
                paste("    \\clef", clef),
                paste("   ", toene),
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

FF1airsh <- filter(midi_db, title == "FF1airsh.mid", channel == 0)
FF1airsh <- FF1airsh %>%
            mutate(ioi      = c(diff(time, time, lag = 1, 
                                     differences = 1), last(length)),
                   dur      = sapply(ioi, symbolize_dur))

X <- with(FF1airsh, data.frame(  note = notename %>% sub(pattern = "#", 
                                                         replacement = "is",
                                                         x = .), 
                                 duration = dur,
                                 tie = dur %>%
                                         as.character %>%
                                         str_detect("~")))

X <- X %>% mutate_if(is.factor, as.character)

lilyinput(X,
          file = "Rsong.ly",
          Major = TRUE,
          key = "c",
          clef = "treble",
          time = "4/4",
          endbar = FALSE,
          midi = TRUE,
          tempo = "4 = 150")



## test:
#  X <- data.frame(note = c(3, 0, 1, 3, -4, -2, 0, 1, 3, 1, 0, -2, NA),  
#    duration = c(2, 4, 8, 2, 2, 8, 8, 8, 8, 4, 4, 1, 1), 
#    punctuation = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
#    slur = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
#
#  lilyinput(X, file = "c:/test2.ly")
#  
# Y <- data.frame(note = c(3, 5, 7, 8, 10, 10, 12, 12, 12, 12, 10, 12, 12, 12, 12, 10, 8, 8, 8, 8, 7, 7, 5, 5, 5, 5, 3, NA),  
#    duration = c(4, 4, 4, 4, 2, 2, 4, 4, 4, 4, 2, 4, 4, 4, 4, 2, 4, 4, 4, 4, 2, 2, 4, ,4, 4, 4, 2, 1), 
#    punctuation = FALSE, 
#    slur = FALSE)
#    
#lilyinput(Y, file="c:/Bsp.ly")

