##### LIBRARIES #####
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyFiles)
library(Rvision)


##### GLOBAL #####
if (Sys.info()["sysname"] == "Linux") {
    cam_ids <- system("v4l2-ctl --list-devices", intern = TRUE)
    cam_ids <- grep("brio", cams, ignore.case = TRUE) + 1
    cam_ids <- as.numeric(gsub("\t/dev/video", "", cams[idx]))
} else {
    # stop("Unsupported platform.")
    cam_ids <- 0
}

cams <- lapply(cam_ids, function(x) {
    st <- stream(x)
    setProp(st, "FOURCC", fourcc("MJPG"))
    setProp(st, "FRAME_WIDTH", 4096)
    setProp(st, "FRAME_HEIGHT", 2160)
    st
})


##### UI #####
ui <- fluidPage(
    useShinyjs(),

    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    fluidRow(
        column(
            12,

            tags$hr(),

            panel(
                heading = tags$table(
                    width = "100%",
                    tags$tr(
                        tags$td(width = "100%", h4("Select a camera")),
                        tags$td(width = "0%",
                                pickerInput("camera", NULL, width = "100%",
                                            choices = NULL))
                    )
                ),

                sliderInput("displaySize", "Display size", width = "100%",
                            value = 1, min = 0.1, max = 1, step = 0.1)
            ),

            panel(heading = tags$table(
                width = "100%",
                tags$tr(
                    tags$td(width = "100%", h4("Autofocus")),
                    tags$td(width = "0%",
                            switchInput("autofocus", NULL, TRUE, inline = TRUE,
                                        onStatus = "success", offStatus = "danger", size = "small"))
                )
            ),
            sliderInput("focus", NULL, width = "100%",
                        value = 0, min = 0, max = 255, step = 1)
            ),

            panel(heading = tags$table(
                width = "100%",
                tags$tr(
                    tags$td(width = "100%", h4("Auto-exposure")),
                    tags$td(width = "0%",
                            switchInput("autoexposure", NULL, TRUE, inline = TRUE,
                                        onStatus = "success", offStatus = "danger", size = "small"))
                )
            ),
            sliderInput("exposure", NULL, width = "100%",
                        value = 250, min = 3, max = 2047, step = 1)
            ),

            panel(heading = h4("Timelapse"),
                  tags$table(
                      width = "100%",
                      style = "margin-bottom: 10px;",
                      tags$tr(
                          tags$td(width = "49%",
                                  numericInput("interval", "Interval (sec)",
                                               1, step = 1, width = "100%")
                          ),
                          tags$td(width = "2%"),
                          tags$td(width = "49%",
                                  numericInput("duration", "Duration (sec)",
                                               86400, step = 1, width = "100%")
                          )
                      ),
                      tags$tr(height = "10px"),
                      tags$tr(
                          tags$td(width = "49%",
                                  shinyDirButton("savedir", "Save to...", "Select a folder",
                                                 style = "width: 100%;")
                          ),
                          tags$td(width = "2%"),
                          tags$td(width = "49%",
                                  actionButton("startstop", "Start", width = "100%")
                          )
                      )
                  )
            ),

            tags$hr()
        )

    )
)


##### SERVER #####
server <- function(input, output, session) {

    observe({
        updatePickerInput(session, "camera",
                          choices = paste0("Camera ", 1:length(cams)))
    })

    observe({
        invalidateLater(1000 / 30, session)

        if (!is.null(input$camera)) {
            ix <- as.numeric(gsub("Camera ", "", input$camera))
            # display(readNext(cams[[ix]]), delay = 1,
            #         height = nrow(cams[[ix]]) * input$displaySize,
            #         width = ncol(cams[[ix]]) * input$displaySize)
        }
    })

    observe({
        if (!is.null(input$camera)) {
            if (input$autofocus == TRUE) {
                ix <- as.numeric(gsub("Camera ", "", input$camera))
                setProp(cams[[ix]], "AUTOFOCUS", 1)
                disable("focus")
            } else {
                ix <- as.numeric(gsub("Camera ", "", input$camera))
                setProp(cams[[ix]], "AUTOFOCUS", 0)
                enable("focus")
            }
        }
    })

    observe({
        if (!is.null(input$camera)) {
            if (input$autoexposure == TRUE) {
                ix <- as.numeric(gsub("Camera ", "", input$camera))
                setProp(cams[[ix]], "AUTO_EXPOSURE", 3)
                disable("exposure")
            } else {
                ix <- as.numeric(gsub("Camera ", "", input$camera))
                setProp(cams[[ix]], "AUTO_EXPOSURE", 1)
                enable("exposure")
            }
        }
    })

    observe({
        if (!is.null(input$camera)) {
            if (input$autofocus == FALSE) {
                ix <- as.numeric(gsub("Camera ", "", input$camera))
                setProp(cams[[ix]], "FOCUS", input$focus)
            }
        }
    })

    observe({
        if (!is.null(input$camera)) {
            if (input$autoexposure == FALSE) {
                ix <- as.numeric(gsub("Camera ", "", input$camera))
                setProp(cams[[ix]], "EXPOSURE", input$exposure)
            }
        }
    })

    session$onSessionEnded(function() {
        destroyAllDisplays()
        lapply(cams, function(x) release(x))
    })
}


##### RUN #####
shinyApp(ui = ui, server = server)
