library(shiny)
# library(shinyjs)
library(shinyFiles)
# library(shinyWidgets)

ui <- fluidPage(
    # useShinyjs(),

    fluidRow(
        shinyDirButton("savedir", "Save to...", "Select a folder",
                       style = "width: 100%;")
    )
)

server <- function(input, output, session) {

    volumes <- getVolumes()

    shinyDirChoose(
        input,
        "savedir",
        roots = volumes(),
        session = session
    )
}

shinyApp(ui = ui, server = server)
