#### Server ####
function(input, output, session) {
  grab <- reactiveVal()
  grabD <- debounce(grab, 20)
  refreshDisplay <- reactiveVal(0)
  printDisplay <- reactiveVal()
  tmpDir <- tempdir()
  frames <- list()
  timeStamps <- list()
  toDisplay <- zeros(2160 / 3.2, 4096 / 3.2)

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
        if (input$zoom > 1) {
          w <- 4096 / input$zoom
          h <- 2160 / input$zoom
          x <- 1 + (4096 - w) / 2
          y <- 1 + (2160 - h) / 2
          resize(subImage(frames[[ix]], x, y, w, h), 2160 / 3.2, 4096 / 3.2, interpolation = "cubic", target = toDisplay)
        } else {
          resize(frames[[ix]], 2160 / 3.2, 4096 / 3.2, interpolation = "cubic", target = toDisplay)
        }

        clock <- as.character(Sys.time(), 2L)
        drawRectangle(toDisplay, 1, 1, 442, 42, color = "white", thickness = -1)
        drawText(toDisplay, clock, 10, 10, color = "black", thickness = 2)
        suppressMessages(write.Image(toDisplay, paste0(tmpDir, "/display.jpg"), TRUE))
      } else {
        suppressMessages(write.Image(zeros(2160 / 3.2, 4096 / 3.2, 3), paste0(tmpDir, "/display.jpg"), TRUE))
      }

      if (is.null(printDisplay())) {
        printDisplay(1)
      } else {
        printDisplay(printDisplay() + 1)
      }
    }
  }, ignoreNULL = TRUE)

  output$displayImg <- renderImage({
    printDisplay()
    ix <- as.numeric(gsub("Camera ", "", input$camera))
    iw <- 4096 / 3.2
    ih <- 2160 / 3.2
    ww <- session$clientData[["output_displayImg_width"]]
    wh <- session$clientData[["output_displayImg_width"]] * ih / iw

    list(src = paste0(tmpDir, "/display.jpg"),
         width = ww,
         height = wh)
  }, deleteFile = FALSE)



  observeEvent(input$autofocus, {
    if (!is.null(input$camera)) {
      if (input$autofocus == TRUE) {
        ix <- as.numeric(gsub("Camera ", "", input$camera))
        setProp(cams[[ix]], "AUTOFOCUS", 1)
        disable("focus")
      } else {
        ix <- as.numeric(gsub("Camera ", "", input$camera))
        setProp(cams[[ix]], "AUTOFOCUS", 0)
        updateSliderInput(session, "focus", value = getProp(cams[[ix]], "FOCUS"))
        enable("focus")
      }
    }
  })

  observeEvent(input$focus, {
    if (!is.null(input$camera)) {
      if (input$autofocus == FALSE) {
        ix <- as.numeric(gsub("Camera ", "", input$camera))
        setProp(cams[[ix]], "FOCUS", input$focus)
      }
    }
  })

  observeEvent(input$autoexposure, {
    if (!is.null(input$camera)) {
      if (input$autoexposure == TRUE) {
        ix <- as.numeric(gsub("Camera ", "", input$camera))
        setProp(cams[[ix]], "AUTO_EXPOSURE", 3)
        disable("exposure")
      } else {
        ix <- as.numeric(gsub("Camera ", "", input$camera))
        setProp(cams[[ix]], "AUTO_EXPOSURE", 1)
        updateSliderInput(session, "exposure", value = getProp(cams[[ix]], "EXPOSURE"))
        enable("exposure")
      }
    }
  })

  observeEvent(input$exposure, {
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
