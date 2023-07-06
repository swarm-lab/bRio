#### Server ####
function(input, output, session) {
  grab <- reactiveVal()
  grabD <- debounce(grab, 20)
  refreshDisplay <- reactiveVal(0)
  printDisplay <- reactiveVal()
  tmpDir <- tempdir()
  frames <- list()
  timeStamps <- list()

  observe({
    if (length(cams) > 0) {
      updatePickerInput(session, "camera",
                        choices = paste0("Camera ", 1:length(cams)))
    }
  })

  observeEvent(input$camera, {
    ix <- as.numeric(gsub("Camera ", "", input$camera))
    updateSwitchInput(session, "autoexposure",
                      value = getProp(cams[[ix]], "AUTO_EXPOSURE") == 3)
    updateSliderInput(session, "exposure",
                      value = getProp(cams[[ix]], "EXPOSURE"))
    updateSwitchInput(session, "autofocus",
                      value = getProp(cams[[ix]], "AUTOFOCUS") == 1)
    updateSliderInput(session, "focus",
                      value = getProp(cams[[ix]], "FOCUS"))
    frames <<- lapply(cams, readNext)
    timeStamps <<- lapply(cams, getProp, property = "POS_MSEC")
    grab(1)
  }, ignoreNULL = TRUE)

  observeEvent(grabD(), {
    lapply(1:length(cams), function(i) readNext(cams[[i]], frames[[i]]))
    timeStamps <<- lapply(cams, getProp, property = "POS_MSEC")
    refreshDisplay(refreshDisplay() + 1)
    grab(grab() + 1)
  }, ignoreNULL = TRUE)

  observeEvent(refreshDisplay(), {
    if (refreshDisplay() > 0) {
      ix <- as.numeric(gsub("Camera ", "", input$camera))

      if (isImage(frames[[ix]])) {
        suppressMessages(write.Image(frames[[ix]], paste0(tmpDir, "/display.jpg"), TRUE))
      } else {
        suppressMessages(write.Image(zeros(1080, 1920, 3), paste0(tmpDir, "/display.jpg"), TRUE))
      }

      if (is.null(printDisplay())) {
        printDisplay(1)
      } else {
        printDisplay(printDisplay() + 1)
      }
    }
  }, ignoreNULL = TRUE)

  output$displayImg <- renderImage({
    ix <- as.numeric(gsub("Camera ", "", input$camera))
    iw <- 1920
    ih <- 1080

    if (length(ix) > 0) {
      if (isImage(frames[[ix]])) {
        iw <- ncol(frames[[ix]])
        ih <- nrow(frames[[ix]])
      }
    }

    ww <- session$clientData[["output_displayImg_width"]] - 15
    wh <- (session$clientData[["output_displayImg_width"]] - 15) * ih / iw

    list(src = paste0(tmpDir, "/display.jpg"),
         width = ww,
         height = wh)
  }, deleteFile = FALSE)

  session$onSessionEnded(function() {
    destroyAllDisplays()
    lapply(cams, function(x) release(x))
  })
}
