##### LIBRARIES #####
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyFiles)
library(Rvision)


##### GLOBAL #####
if (Sys.info()["sysname"] == "Linux") {
    cam_ids <- system("v4l2-ctl --list-devices", intern = TRUE)
    ix <- grep("brio", cam_ids, ignore.case = TRUE) + 1
    cam_ids <- as.numeric(gsub("\t/dev/video", "", cam_ids[ix]))
} else {
    stop("Unsupported platform.")
    # cam_ids <- 0
}

cams <- lapply(cam_ids, function(x) {
    st <- stream(x, api = "V4L2")
    # st <- stream(x)
    setProp(st, "FOURCC", fourcc("MJPG"))
    setProp(st, "FRAME_WIDTH", 4096)
    setProp(st, "FRAME_HEIGHT", 2160)
    st
})

zoom <- function(img, f) {
    if (f < 1)
        stop("Incompatible zoom factor")

    w <- ncol(img) / f
    h <- nrow(img) / f
    x <- 1 + (ncol(img) - w) / 2
    y <- 1 + (nrow(img) - h) / 2

    resize(subImage(img, x, y, w, h), nrow(img), ncol(img), interpolation = "cubic")
}

volumes <- getVolumes()


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
                        tags$td(width = "95%", h4("Select a camera")),
                        tags$td(width = "0%",
                                pickerInput("camera", NULL, width = "100%",
                                            choices = NULL)),
                        tags$td(width = "5%"),
                        tags$td(width = "0%",
                                switchInput("display", NULL, TRUE, inline = TRUE,
                                            onStatus = "success", offStatus = "danger", size = "small")
                        )
                    )
                ),

                sliderInput("displaySize", "Display size", width = "100%",
                            value = 0.2, min = 0.1, max = 1, step = 0.1),
                sliderInput("zoom", "Zoom", width = "100%",
                            value = 1, min = 1, max = 5, step = 0.1)
            ),

            panel(heading = tags$table(
                width = "100%",
                tags$tr(
                    tags$td(width = "100%", h4("Autofocus")),
                    tags$td(width = "0%",
                            switchInput("autofocus", NULL, TRUE, inline = TRUE,
                                        onStatus = "success", offStatus = "danger", size = "small")
                    )
                )
            ),
            sliderInput("focus", NULL, width = "100%",
                        value = 0, min = 0, max = 255, step = 5)
            ),

            panel(heading = tags$table(
                width = "100%",
                tags$tr(
                    tags$td(width = "100%", h4("Auto-exposure")),
                    tags$td(width = "0%",
                            switchInput("autoexposure", NULL, TRUE, inline = TRUE,
                                        onStatus = "success", offStatus = "danger", size = "small")
                    )
                )
            ),
            sliderInput("exposure", NULL, width = "100%",
                        value = 3, min = 3, max = 2047, step = 1)
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
                                  disabled(actionButton("start", "Start", width = "100%"))
                          )
                      )
                  ),
                  progressBar("pb", value = 0, display_pct = TRUE)
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
        if (!is.null(input$camera)) {
            ix <- as.numeric(gsub("Camera ", "", input$camera))
            updateSwitchInput(session, "autoexposure",
                              value = getProp(cams[[ix]], "AUTO_EXPOSURE") == 3)
            updateSliderInput(session, "exposure",
                              value = getProp(cams[[ix]], "EXPOSURE"))
            updateSwitchInput(session, "autofocus",
                              value = getProp(cams[[ix]], "AUTOFOCUS") == 1)
            updateSliderInput(session, "focus",
                              value = getProp(cams[[ix]], "FOCUS"))
        }
    })

    frames <- reactiveVal()
    observe({
        invalidateLater(1000 / 30, session)

        if (!is.null(input$camera)) {
            isolate({
                frames(lapply(cams, readNext))
            })
        }
    })

    observe({
        if (!is.null(frames())) {
            if (input$display == TRUE) {
                ix <- as.numeric(gsub("Camera ", "", input$camera))
                display(zoom(frames()[[ix]], input$zoom), delay = 1,
                        height = nrow(frames()[[ix]]) * input$displaySize,
                        width = ncol(frames()[[ix]]) * input$displaySize)
            } else {
                destroyAllDisplays()
            }
        }
    })

    observe({
        if (!is.null(input$camera) & is.null(end())) {
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
        if (!is.null(input$camera) & is.null(end())) {
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

    shinyDirChoose(
        input,
        "savedir",
        roots = volumes(),
        session = session
    )

    observe({
        path <- parseDirPath(volumes(), input$savedir)

        if (length(path) > 0) {
            enable("start")
        }
    })

    start <- reactiveVal()
    end <- reactiveVal()
    counter <- reactiveVal()
    observe({
        if (input$start > 0) {
            isolate({ path <- parseDirPath(volumes(), input$savedir) })
            for (i in 1:length(cams)) {
                dir.create(paste0(path, "/Camera ", i))
            }

            updateSwitchInput(session, "display", value = FALSE)

            isolate({
                st <- Sys.time()
                start(st)
                end(st + input$duration)
                counter(0)
            })

        }
    })

    observe({
        if (!is.null(end()) & !is.null(frames())) {
            if (Sys.time() <= end()) {
                disable("focus")
                disable("exposure")
                updateSwitchInput(session, "autofocus", disabled = TRUE)
                updateSwitchInput(session, "autoexposure", disabled = TRUE)
                disable("start")
                disable("savedir")
                disable("duration")
                disable("interval")

                elapsed <- as.numeric(Sys.time() - (start() + counter() * input$interval), units = "secs")

                if (elapsed > input$interval) {
                    isolate({ path <- parseDirPath(volumes(), input$savedir) })

                    for (i in 1:length(cams)) {
                        write.Image(frames()[[i]],
                                    paste0(path, "/Camera ", i, "/",
                                           format(Sys.time(), "%m-%d-%Y_%H-%M-%S.png")))
                    }

                    isolate({
                        pc <- 100 * as.numeric(Sys.time() - start(), units = "secs") /
                            as.numeric(end() - start(), units = "secs")
                        updateProgressBar(session, "pb", value = pc)
                    })

                    counter(counter() + 1)
                }
            } else {
                updateProgressBar(session, "pb", value = 100)
                start(NULL)
                end(NULL)
                counter(NULL)
                enable("focus")
                enable("exposure")
                updateSwitchInput(session, "autofocus", disabled = FALSE)
                updateSwitchInput(session, "autoexposure", disabled = FALSE)
                enable("savedir")
                enable("duration")
                enable("interval")
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
