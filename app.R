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
library(ggiraph)

# clear out www folder in case it still has mp3s in it
if(!dir.exists("www")){
  dir.create("www")
}
file.remove(list.files(path = "www",pattern = "note[0-9].mp3",full.names = TRUE))
file.remove(list.files(path = "www",pattern = "chord[0-9].mp3",full.names = TRUE))
file.remove(list.files(path = "www",pattern = "song.mp3",full.names = TRUE))

load("data/noteFreqTable.RData")
load("data/pianoKeyPlotData.RData")
load("data/pianoKeyPolygonData.RData")
source("code/helperFunctions.R")

noteFreqTable <- noteFreqTable %>%
  # mutate(Note = str_extract(Note,"[A-Z][#]?[0-9]")) %>%
  mutate(noteLabel = ifelse(str_detect(Note,"/"),
                            Note %>%
                              str_split("/") %>%
                              map_chr(~ paste0(.[2],"/",.[1])) %>%
                              str_remove("[0-9]"),
                            Note))

# save this to global environment so that it can be reset after finishing a chord
pianoKeyPlot <-
  pianoKeyPolygonData %>%
  ggplot() +
  geom_polygon_interactive(aes(x=x,y=y,
                               group = noteOctave,data_id = noteOctave,fill = key),
                           colour = "black") +
  geom_text(data = pianoKeyPlotData %>%
              group_by(noteOctave,key) %>%
              summarise(x = mean(c(xmin,xmax)),
                        y = ifelse(str_detect(noteOctave,"b"),.35,.05)),
            aes(x = x,y = y,label = noteOctave,colour = key),
            size = 2) +
  scale_fill_manual(values = c("black","white")) +
  scale_colour_manual(values = c("white","black")) +
  # coord_fixed(ratio = 50,expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type='text/css', ".irs-grid-text { font-size: 7pt; }"),
  shinyjs::useShinyjs(),
  fluidRow(column(width = 12,ggiraph::ggiraphOutput(outputId = "pianoKeys",width = "100%",height = "500px"))),
  splitLayout(selectInput(inputId = "waveType",label = "Wave Type",choices = c("sine","pulse")),
              numericInput(inputId = "chordDuration",label = "Duration (sec)",value = 1,min = 0),
              actionButton(inputId = "nextChord",label = "Next Chord"),
              cellWidths = c("10%","10%","10%")),
  br(),
  fluidRow(column(width = 4,
                  uiOutput(outputId = "chordPlay"),
         uiOutput(outputId = "noteSelection")),
  column(width = 8,
         plotOutput(outputId = "chordPlot")
         # ,plotOutput(outputId = "chordSpect")
         )),
  br(),
  fluidRow(
    div(id = "songUI",
        uiOutput(outputId = "songPlay"),
        downloadButton(outputId = "songExport",label = "Export Song"),
        # plotOutput(outputId = "songWaveform")
        plotOutput(outputId = "songSpect")
        )
    )

)

# Define server logic required to draw a histogram
server <- function(session,input, output){

  output$pianoKeys <- renderGirafe({

    return(ggiraph::girafe(ggobj = pianoKeyPlot,
                    options = list(opts_selection(type = "multiple",
                                                  css = "fill:red;stroke:black")),
                    width_svg = 10,
                    height_svg = 2))

  })

  output$noteSelection <- renderUI({

    req(length(input$pianoKeys_selected) > 0)

    selectedNotes <- input$pianoKeys_selected

    ret <- map2(selectedNotes,
        1:length(selectedNotes),
        function(note,ind){

          return(tags$div(
            br(),
            id = paste0("note",ind,"ui"),
            h5(paste0("Listen to ",note,":")),
            uiOutput(outputId = paste0("note",ind,"Player"))
          ))

        })

    return(tagList(ret))


  })

  numChords <- 1

  observeEvent(list(input$pianoKeys_selected,input$waveType,input$chordDuration),{

    req(length(input$pianoKeys_selected) > 0)

    #clear out the www folder each time we add a new note -- this ensures that
    #the user can remove a note from a chord and it will be removed from the UI
    file.remove(list.files(path = "www",pattern = "note[0-9].mp3",full.names = TRUE))
    file.remove(list.files(path = "www",pattern = paste0("chord[0-9].mp3"),full.names = TRUE))

    # save individual note mp3s
    map2(input$pianoKeys_selected,
        1:length(input$pianoKeys_selected),
        function(note,ind){

          # pull the frequency information from the selected note
          selectedNoteFreq <- noteFreqTable %>%
            filter(noteLabel == note) %>%
            pull(freq_Hz)

          # create a signal of a specific wave type
          noteWave <- periodicSignal(waveType = input$waveType,
                                     frequency = selectedNoteFreq,
                                     duration = round(input$chordDuration*44100))

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

      return(tags$div(
        tags$h5("Listen to full chord:"),
        tags$audio(controls = TRUE,
                        tags$source(src = paste0("chord",numChords,".mp3")),
                        title = "Listen to chord")))

    })

    output$chordPlot <- renderPlot({

      individualWaves <- audioList %>%
        map_dfr(.id = "note",
                ~ {

                  data.frame(val = .@left,
                             sec = seq(1/combinedAudio@samp.rate,input$chordDuration,by = 1/combinedAudio@samp.rate))

                })

      data.frame(val = combinedAudio@left,
                 sec = seq(1/combinedAudio@samp.rate,input$chordDuration,by = 1/combinedAudio@samp.rate)) %>%
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

    # output$chordSpect <- renderPlot({
    #
    #   spec <- signal::specgram(combinedAudio@left)
    #
    #   expand_grid(tim = spec$t,f = spec$f) %>%
    #     mutate(S = abs(c(spec$S))) %>%
    #     mutate(S = 10*log10(S/max(S)),
    #            tim = 2*tim/combinedAudio@samp.rate) %>%
    #     ggplot(aes(x=tim,y=f,fill = S)) +
    #     geom_raster() +
    #     scale_fill_gradient(low = "purple",high = "orange") +
    #     theme_minimal() +
    #     theme(panel.grid = element_blank()) +
    #     labs(x = "Time (sec)",
    #          y = "Frequency",
    #          title = "Chord Spectrogram") +
    #     coord_cartesian(expand = FALSE)
    #
    # })
  })

  # the user can only move on to the next chord if they have at least one note
  observe({

    if(length(input$pianoKeys_selected) == 0){

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
      # also save the song to the data folder since there's a lot of exhanging of files going on in www
      tuneR::writeWave(object = tuneR::bind(song,chord),filename = "data/song.mp3")

    }
    else{

      tuneR::writeWave(object = chord,filename = "www/song.mp3")
      tuneR::writeWave(object = chord,filename = "data/song.mp3")

      song <- chord

    }

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

        ret <- tuneR::readWave("data/song.mp3")

        tuneR::writeWave(ret,file)

      }
    )

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
    # output$noteSelection <- NULL
    shinyjs::reset(id = "noteSelection")
    output$chordPlay <- NULL
    output$chordPlot <- NULL
    output$chordSpect <- NULL
    output$pianoKeys <- NULL
    output$pianoKeys <- renderGirafe({

      return(ggiraph::girafe(ggobj = pianoKeyPlot,
                             options = list(opts_selection(type = "multiple",
                                                           css = "fill:red;stroke:black")),
                             width_svg = 10,
                             height_svg = 2))

    })

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
    file.remove(list.files(path = "data",pattern = "song.mp3",full.names = TRUE))
  })

}

# Run the application
shinyApp(ui = ui, server = server)

