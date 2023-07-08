#### Server ####
function(input, output, session) {
  display <- FALSE
  displayTimer <- reactiveTimer(40, session)

  grabDisplay <- reactiveVal()
  grabDisplayD <- debounce(grabDisplay, 20)
  refreshDisplay <- reactiveVal(0)
  printDisplay <- reactiveVal()
  tmpDir <- tempdir()
  frame <- zeros(2160, 4096)
  toDisplay <- zeros(2160 / 3.2, 4096 / 3.2)
  frames <- list()
  vws <- list()
  record <- reactiveVal()
  grabRecord <- reactiveVal()
  grabRecordD <- debounce(grabRecord, 1)
  counter <- 0
  steps <- 0
  end <- 0


  #### Init ####
  observe({
    if (length(cams) > 0) {
      updatePickerInput(session, "camera",
                        choices = paste0("Camera ", 1:length(cams)))
    }
  })

  observeEvent(input$camera, {
    ix <- as.numeric(gsub("Camera ", "", input$camera))
    setProp(cams[[ix]], "AUTOFOCUS", 0)
    updateSliderInput(session, "focus",
                      value = getProp(cams[[ix]], "FOCUS"))
    setProp(cams[[ix]], "AUTO_EXPOSURE", 1)
    updateSliderInput(session, "exposure",
                      value = getProp(cams[[ix]], "EXPOSURE"))
    updateSliderInput(session, "gain",
                      value = getProp(cams[[ix]], "GAIN"))
    updateSliderInput(session, "brightness",
                      value = getProp(cams[[ix]], "BRIGHTNESS"))
    display <<- TRUE
  }, ignoreNULL = TRUE)


  #### Display ####
  observeEvent(displayTimer(), {
    if (display == TRUE) {
      ix <- as.numeric(gsub("Camera ", "", input$camera))
      readNext(cams[[ix]]$queue, frame)

      if (input$zoom > 1) {
        w <- 4096 / input$zoom
        h <- 2160 / input$zoom
        x <- 1 + (4096 - w) / 2
        y <- 1 + (2160 - h) / 2
        resize(subImage(frame, x, y, w, h), 2160 / 3.2, 4096 / 3.2, interpolation = "cubic", target = toDisplay)
      } else {
        resize(frame, 2160 / 3.2, 4096 / 3.2, interpolation = "cubic", target = toDisplay)
      }

      clock <- as.character(Sys.time(), 2L)
      drawRectangle(toDisplay, 1, 1, 442, 42, color = "white", thickness = -1)
      drawText(toDisplay, clock, 10, 10, color = "black", thickness = 2)
      suppressMessages(write.Image(toDisplay, paste0(tmpDir, "/display.jpg"), TRUE))
      refreshDisplay(refreshDisplay() + 1)
    }
  })

  # observeEvent(refreshDisplay(), {
  #   if (refreshDisplay() > 0) {
  #     ix <- as.numeric(gsub("Camera ", "", input$camera))
  #
  #     if (isImage(frames[[ix]])) {
  #       if (input$zoom > 1) {
  #         w <- 4096 / input$zoom
  #         h <- 2160 / input$zoom
  #         x <- 1 + (4096 - w) / 2
  #         y <- 1 + (2160 - h) / 2
  #         resize(subImage(frames[[ix]], x, y, w, h), 2160 / 3.2, 4096 / 3.2, interpolation = "cubic", target = toDisplay)
  #       } else {
  #         resize(frames[[ix]], 2160 / 3.2, 4096 / 3.2, interpolation = "cubic", target = toDisplay)
  #       }
  #
  #       clock <- as.character(Sys.time(), 2L)
  #       drawRectangle(toDisplay, 1, 1, 442, 42, color = "white", thickness = -1)
  #       drawText(toDisplay, clock, 10, 10, color = "black", thickness = 2)
  #       suppressMessages(write.Image(toDisplay, paste0(tmpDir, "/display.jpg"), TRUE))
  #     } else {
  #       suppressMessages(write.Image(zeros(2160 / 3.2, 4096 / 3.2, 3), paste0(tmpDir, "/display.jpg"), TRUE))
  #     }
  #
  #     if (is.null(printDisplay())) {
  #       printDisplay(1)
  #     } else {
  #       printDisplay(printDisplay() + 1)
  #     }
  #   }
  # }, ignoreNULL = TRUE)

  output$displayImg <- renderImage({
    refreshDisplay()
    ix <- as.numeric(gsub("Camera ", "", input$camera))
    iw <- 4096 / 3.2
    ih <- 2160 / 3.2
    ww <- session$clientData[["output_displayImg_width"]]
    wh <- session$clientData[["output_displayImg_width"]] * ih / iw

    list(src = paste0(tmpDir, "/display.jpg"),
         width = ww,
         height = wh)
  }, deleteFile = FALSE)


  #### Controls ####
  observeEvent(input$focus, {
    if (!is.null(input$camera)) {
      ix <- as.numeric(gsub("Camera ", "", input$camera))
      setProp(cams[[ix]], "FOCUS", input$focus)
    }
  })

  observeEvent(input$exposure, {
    if (!is.null(input$camera)) {
      ix <- as.numeric(gsub("Camera ", "", input$camera))
      setProp(cams[[ix]], "EXPOSURE", input$exposure)
    }
  })

  observeEvent(input$gain, {
    if (!is.null(input$camera)) {
      ix <- as.numeric(gsub("Camera ", "", input$camera))
      setProp(cams[[ix]], "GAIN", input$gain)
    }
  })

  observeEvent(input$brightness, {
    if (!is.null(input$camera)) {
      ix <- as.numeric(gsub("Camera ", "", input$camera))
      setProp(cams[[ix]], "BRIGHTNESS", input$brightness)
    }
  })


  #### Recording ####
  shinyDirChoose(
    input,
    "savedir",
    roots = volumes,
    session = session
  )

  observeEvent(input$savedir, {
    path <- parseDirPath(volumes, input$savedir)

    if (length(path) > 0) {
      enable("start")
    }
  })

  observeEvent(input$start, {
    disable("displayImg")
    disable("camera")
    disable("zoom")
    disable("focus")
    disable("exposure")
    disable("gain")
    disable("brightness")
    disable("interval")
    disable("duration")
    disable("savedir")
    disable("start")

    path <- parseDirPath(volumes, input$savedir)
    vws <<- lapply(1:length(cams), function(i) {
      videoWriter(paste0(path, "/Camera_", i, ".mp4"), "avc1", 30, 2160, 4096)
    })

    counter <<- 1
    updateProgressBar(session, "pb", value = 0)
    lapply(1:length(cams), function(i) readNext(cams[[i]], frames[[i]]))
    steps <<- (as.numeric(Sys.time()) * 1000) +
      ((1:round(input$duration / input$interval)) * (1000 *  input$interval))
    end <<- length(steps)
    grabDisplay(NULL)
    grabRecord(1)
  }, ignoreInit = TRUE)

  observeEvent(grabRecordD(), {
    lapply(1:length(cams), function(i) readNext(cams[[i]], frames[[i]]))
    now <- as.numeric(Sys.time()) * 1000

    if (now >= steps[counter]) {
      ts <- getTextSize(as.character(now), thickness = 2)
      lapply(1:length(cams), function(i) {
        drawRectangle(frames[[i]], 1, 1, ts[2] + 20, ts[1] + 20, color = "white", thickness = -1)
        drawText(frames[[i]], as.character(now), 10, 10, color = "black", thickness = 2)
        writeFrame(vws[[i]], frames[[i]])
      })

      counter <<- counter + 1
      updateProgressBar(session, "pb", value = 100 * (counter / end))
    }

    if (counter > end) {
      updateProgressBar(session, "pb", value = 100)
      grabDisplay(1)
      grabRecord(NULL)
      enable("displayImg")
      enable("camera")
      enable("zoom")
      enable("focus")
      enable("exposure")
      enable("gain")
      enable("brightness")
      enable("interval")
      enable("duration")
      enable("savedir")
      suppressWarnings(lapply(vws, release))
    } else {
      grabRecord(grabRecord() + 1)
    }
  }, ignoreNULL = TRUE)


  #### Cleanup ####
  session$onSessionEnded(function() {
    destroyAllDisplays()
    if (length(vws) > 1)
      lapply(vws, release)
    lapply(cams, release)
  })
}
