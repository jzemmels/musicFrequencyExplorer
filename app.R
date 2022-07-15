#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tuneR)
library(tidyverse)


load("data/noteFreqTable.RData")
source("code/helperFunctions.R")

noteFreqTable <- noteFreqTable %>%
  mutate(Note = str_extract(Note,"[A-Z][#]?[0-9]")) %>%
  mutate(noteLabel = Note)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type='text/css', ".irs-grid-text { font-size: 7pt; }"),
  shinyjs::useShinyjs(),
  column(width = 6,
         selectInput(inputId = "waveType",label = "Wave Type",choices = c("sine","pulse")),#,"square","sawtooth")),
         actionButton(inputId = "nextChord",label = "Next Chord"),
         uiOutput(outputId = "noteSelection"),
         actionButton(inputId = "addNote",label = "Add a note")
  ),
  column(width = 6,
         uiOutput(outputId = "chordPlay"),
         plotOutput(outputId = "chordPlot"),
         plotOutput(outputId = "chordSpect")),
  fluidRow(
    div(id = "songUI",
        uiOutput(outputId = "songPlay"),
        downloadButton(outputId = "songExport",label = "Export Song"),
        plotOutput(outputId = "songWaveform"),
        plotOutput(outputId = "songSpect"))
    )

)

# Define server logic required to draw a histogram
server <- function(session,input, output){

  # this will be reactive so that the dependent UI will update when it changes
  noteCount <- reactiveVal(value = 0)
  # we also keep a static version of the count so that we can access it
  # outside of a reactive environment (see the for loop below)

  # the user will add one or more notes to play simultaneously
  observeEvent(input$addNote,{

    noteCount(noteCount() + 1)

    # for each note, we render a slider input to select a specific note on the
    # western chromatic scale.
    output$noteSelection <- renderUI({

      noteSelectionList <-
        map(#1:
          noteCount(),
          function(ind){

            return(tags$div(
              br(),
              id = paste0("note",ind,"ui"),
              sliderTextInput(inputId = paste0("note",ind),
                              label = "Choose a note",
                              choices = noteFreqTable$Note,
                              selected = "C4",
                              grid = TRUE,
                              force_edges = TRUE,
                              width = "100%"),
              # plotOutput(paste0("note",ind,"WavePlot")),
              uiOutput(outputId = paste0("note",ind,"Player"))
            ))

            # uiOutput(outputId = uiInd)

          })

      # we keep track of the list of note selections in
      # the global environment
      if(exists("noteSelections")){

        noteSelections <<- c(noteSelections,noteSelectionList)

      }
      else{

        noteSelections <<- noteSelectionList

      }

      return(tagList(noteSelections))

    })

  })

  #
  numChords <- 1

  # we limit the total number of possible notes per chord to 5. the code below
  # creates a
  observeEvent(list(input$note1,input$note2,input$note3,input$note4,input$note5,input$waveType),{

    req(noteCount() > 0)

    map(1:noteCount(),
        function(ind){

          # pull the frequency information from the selected note
          selectedNoteFreq <- noteFreqTable %>%
            filter(noteLabel == input[[paste0("note",ind)]]) %>%
            pull(freq_Hz)

          # create a signal of a specific wave type
          noteWave <- periodicSignal(waveType = input$waveType,
                                     frequency = selectedNoteFreq)

          filename <- paste0("www/note",ind,".mp3")

          # save it to the www file
          tuneR::writeWave(noteWave,filename = filename)

          # render an audio player below the slider input
          output[[paste0("note",ind,"Player")]] <- renderUI({

            return(tags$audio(controls = TRUE,
                              tags$source(src = paste0("note",ind,".mp3")),
                              title = paste0("Note ",ind)))

          })

        })

    audioFiles <- list.files("www/",pattern = "note[0-9].mp3",full.names = TRUE)

    req(length(audioFiles) > 0)

    audioList <- map(audioFiles,
                     function(audioFile){

                       wave <- tuneR::readWave(audioFile)

                     })

    combinedAudio <- Reduce(`+`,audioList)

    output$chordPlay <- renderUI({

      tuneR::writeWave(normalize(combinedAudio),filename = paste0("www/chord",numChords,".mp3"))

      return(tags$audio(controls = TRUE,
                        tags$source(src = paste0("chord",numChords,".mp3")),
                        title = "Listen to chord"))

    })

    output$chordPlot <- renderPlot({

      individualWaves <- audioList %>%
        map_dfr(.id = "note",
                ~ {

                  data.frame(val = .@left,
                             sec = seq(0,1,length.out = combinedAudio@samp.rate))

                })

      data.frame(val = combinedAudio@left,
                 sec = seq(0,1,length.out = combinedAudio@samp.rate)) %>%
        ggplot(aes(x = sec,y = val)) +
        geom_line(size = 1.5) +
        geom_line(data = individualWaves,
                  aes(colour = note),alpha = .7) +
        xlim(c(NA,.05)) +
        labs(x = "Wavelength (sec)",
             y = "Amplitude",
             title = "Chord Waveform (50 ms)") +
        theme_minimal()

    })

    output$chordSpect <- renderPlot({

      spec <- signal::specgram(combinedAudio@left)

      expand_grid(tim = spec$t,f = spec$f) %>%
        mutate(S = abs(c(spec$S))) %>%
        mutate(S = 10*log10(S/max(S)),
               tim = 2*tim/combinedAudio@samp.rate) %>%
        ggplot(aes(x=tim,y=f,fill = S)) +
        geom_raster() +
        scale_fill_gradient(low = "purple",high = "orange") +
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        labs(x = "Time (sec)",
             y = "Frequency",
             title = "Chord Spectrogram") +
        coord_cartesian(expand = FALSE)

    })
  })

  # we limit the nubmer of notes per chord to 5
  observe({

    if(noteCount() == 5){

      shinyjs::hideElement("addNote")

    }
    else{

      shinyjs::showElement("addNote")

    }

  })

  # the user can only move on to the next chord if they have at least one note
  observe({

    if(noteCount() == 0){

      shinyjs::hideElement(id = "nextChord")

    }
    else{

      shinyjs::showElement(id = "nextChord")

    }

  })

  # we won't show the song UI until the user starts a second chord
  observe({

    if(numChords < 2){

      shinyjs::hideElement(id = "songUI")

    }

  })

  observeEvent(input$nextChord,{

    # concatenate the current chord to the song file in the data folder
    chord <- tuneR::readWave(paste0("www/chord",numChords,".mp3"))

    if(file.exists("www/song.mp3")){

      song <- tuneR::readWave(paste0("www/song.mp3"))

      tuneR::writeWave(object = tuneR::bind(song,chord),filename = "www/song.mp3")

    }
    else{

      tuneR::writeWave(object = chord,filename = "www/song.mp3")

      song <- chord

    }

    # browser()

    numChords <<- numChords + 1

    shinyjs::showElement("songUI")

    # visualize the song UI at the bottom of the app

    output$songPlay <- renderUI({

      tags$audio(controls = TRUE,
                 tags$source(src = paste0("song.mp3")),
                 title = "Listen to song")

    })

    output$songExport <- downloadHandler(
      filename = function(){

        paste0("song",Sys.Date(),".mp3",sep = "")

      },
      content = function(file){

        ret <- tuneR::readWave("www/song.mp3")

        tuneR::writeWave(ret,file)

      }
    )

    browser()

    output$songPlot <- renderPlot({

      ret <- data.frame(val = song@left,
                 sec = seq(0,1,length.out = song@samp.rate)) %>%
        ggplot(aes(x = sec,y = val)) +
        geom_line(size = 1.5) +
        labs(x = "Wavelength (sec)",
             y = "Amplitude",
             title = "Song Waveform") +
        theme_minimal()

      return(ret)

    })

    output$songSpect <- renderPlot({

      spec <- signal::specgram(song@left)

      expand_grid(tim = spec$t,f = spec$f) %>%
        mutate(S = abs(c(spec$S))) %>%
        mutate(S = 10*log10(S/max(S)),
               tim = 2*tim/song@samp.rate) %>%
        ggplot(aes(x=tim,y=f,fill = S)) +
        geom_raster() +
        scale_fill_gradient(low = "purple",high = "orange") +
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        labs(x = "Time (sec)",
             y = "Frequency",
             title = "Song Spectrogram") +
        coord_cartesian(expand = FALSE)

    })

    # reset the note and chord UI to start a new chord
    rm(noteSelections,envir = .GlobalEnv)
    output$noteSelection <- NULL
    output$chordPlay <- NULL
    output$chordPlot <- NULL
    output$chordSpect <- NULL
    noteCount(0)

  })

  # delete the temporary mp3 files and list of selected notes created while
  # running the app
  session$onSessionEnded(function() {

    if(exists("noteSelections")){

      rm(noteSelections,envir = .GlobalEnv)

    }

    file.remove(list.files(path = "www",pattern = "note[0-9].mp3",full.names = TRUE))
    file.remove(list.files(path = "www",pattern = "chord[0-9].mp3",full.names = TRUE))
    file.remove(list.files(path = "www",pattern = "song.mp3",full.names = TRUE))
  })

}

# Run the application
shinyApp(ui = ui, server = server)

