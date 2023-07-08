#### Server ####
function(input, output, session) {
  display <- FALSE
  displayTimer <- reactiveTimer(40, session)
  refreshDisplay <- reactiveVal(0)
  tmpDir <- tempdir()
  frameSize <- c(2160, 4096) # c(1080, 1920)
  frame <- zeros(frameSize[1], frameSize[2])
  toDisplay <- zeros(frameSize[1] / 3.2, frameSize[2] / 3.2)
  frames <- list()
  vws <- list()
  record <- FALSE
  recordTimer <- reactiveTimer(1, session)
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
      readNext(cams[[ix]], frame)

      if (input$zoom > 1) {
        w <- frameSize[2] / input$zoom
        h <- frameSize[1] / input$zoom
        x <- 1 + (frameSize[2] - w) / 2
        y <- 1 + (frameSize[1] - h) / 2
        resize(subImage(frame, x, y, w, h), frameSize[1] / 3.2, frameSize[2] / 3.2,
               interpolation = "cubic", target = toDisplay)
      } else {
        resize(frame, frameSize[1] / 3.2, frameSize[2] / 3.2,
               interpolation = "cubic", target = toDisplay)
      }

      clock <- as.character(Sys.time(), 2L)
      drawRectangle(toDisplay, 1, 1, 442, 42, color = "white", thickness = -1)
      drawText(toDisplay, clock, 10, 10, color = "black", thickness = 2)
      suppressMessages(write.Image(toDisplay, paste0(tmpDir, "/display.jpg"), TRUE))
      refreshDisplay(refreshDisplay() + 1)
    }
  })

  output$displayImg <- renderImage({
    refreshDisplay()
    ix <- as.numeric(gsub("Camera ", "", input$camera))
    iw <- frameSize[2] / 3.2
    ih <- frameSize[1] / 3.2
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
      videoWriter(paste0(path, "/Camera_", i, ".mp4"), "avc1", 30, frameSize[1], frameSize[2])
    })

    counter <<- 1
    updateProgressBar(session, "pb", value = 0)
    frames <<- lapply(1:length(cams), function(i) readNext(cams[[i]]))
    steps <<- (as.numeric(Sys.time()) * 1000) +
      ((1:round(input$duration / input$interval)) * (1000 *  input$interval))
    end <<- length(steps)
    display <<- FALSE
    record <<- TRUE
  }, ignoreInit = TRUE)

  observeEvent(recordTimer(), {
    if (record == TRUE) {
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
        display <<- TRUE
        record <<- FALSE
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
      }
    }
  }, ignoreNULL = TRUE)


  #### Cleanup ####
  session$onSessionEnded(function() {
    destroyAllDisplays()
    if (length(vws) > 1)
      suppressWarnings(lapply(vws, release))
    suppressWarnings(lapply(cams, release))
  })
}
