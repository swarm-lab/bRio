#### UI ####
fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  useShinyjs(),

  div(
    style = "width: calc(100% - 430px); position: absolute; left: 10px; top: 10px; height: auto;",

    panel(
      imageOutput("displayImg", height = "auto")
    )
  ),

  div(
    style = "width: 400px; position: absolute; left: calc(100% - 410px); top: 10px; height: auto;",

    panel(
      panel(
        heading = tags$table(
          width = "100%",
          tags$tr(
            tags$td(width = "95%", h4("Select a camera")),
            tags$td(width = "0%",
                    pickerInput("camera", NULL, width = "100%",
                                choices = NULL))
          )
        ),
        sliderInput("zoom", "Zoom", width = "100%",
                    value = 1, min = 1, max = 5, step = 0.1),
        sliderInput("focus", "Focus", width = "100%",
                    value = 0, min = 0, max = 255, step = 5),
        sliderInput("exposure", "Exposure", width = "100%",
                    value = 1024, min = 3, max = 2047, step = 1),
        sliderInput("gain", "Gain", NULL, width = "100%",
                    value = 0, min = 0, max = 255, step = 1),
        sliderInput("brightness", "Brightness", NULL, width = "100%",
                    value = 128, min = 0, max = 255, step = 1)
      ),


      panel(heading = h4("Recording"),
            tags$table(
              width = "100%",
              style = "margin-bottom: 10px;",
              tags$tr(
                tags$td(width = "49%",
                        numericInput("interval", "Interval (sec)",
                                     1, step = 0.02, min = 0, width = "100%")
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
      )
    )
  )
)
