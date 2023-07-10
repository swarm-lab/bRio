#### Server ####
function(input, output, session) {
  origTime <- as.numeric(Sys.time()) * 1000
  display <- FALSE
  displayInterval <- 40
  refreshDisplay <- reactiveVal(0)
  tmpDir <- tempdir()
  frame <- zeros(frameSize[2], frameSize[1])
  toDisplay <- zeros(frameSize[2], frameSize[1])
  frames <- list()
  vws <- list()
  record <- FALSE
  recordInterval <- displayInterval
  endRecordTime <- origTime
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
  observe({
    if (display == TRUE) {
      ix <- as.numeric(gsub("Camera ", "", isolate(input$camera)))
      readNext(cams[[ix]], frame)

      if (input$zoom > 1) {
        w <- frameSize[1] / isolate(input$zoom)
        h <- frameSize[2] / isolate(input$zoom)
        x <- 1 + (frameSize[1] - w) / 2
        y <- 1 + (frameSize[2] - h) / 2
        resize(subImage(frame, x, y, w, h), frameSize[2], frameSize[1],
               interpolation = "cubic", target = toDisplay)
      } else {
        cloneImage(frame, toDisplay)
      }

      clock <- as.character(Sys.time(), 2L)
      drawRectangle(toDisplay, 1, 1, 442, 42, color = "white", thickness = -1)
      drawText(toDisplay, clock, 10, 10, color = "black", thickness = 2)
      suppressMessages(write.Image(toDisplay, paste0(tmpDir, "/display.jpg"), TRUE))
      isolate(refreshDisplay(refreshDisplay() + 1))
    }

    invalidateLater(displayInterval - ((as.numeric(Sys.time()) * 1000 - origTime) %% displayInterval))
  })

  output$displayImg <- renderImage({
    refreshDisplay()
    ix <- as.numeric(gsub("Camera ", "", input$camera))
    iw <- frameSize[1]
    ih <- frameSize[2]
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
    display <<- FALSE
    record <<- TRUE
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
      videoWriter(paste0(path, "/Camera_", i, ".mp4"), "avc1", 25, frameSize[2], frameSize[1])
    })

    updateProgressBar(session, "pb", value = 0)
    frames <<- lapply(1:length(cams), function(i) readNext(cams[[i]]))
    origTime <<- as.numeric(Sys.time()) * 1000
    endRecordTime <<- origTime + input$duration * 1000
    recordInterval <<- input$interval * 1000
  }, ignoreInit = TRUE)

  # observeEvent(recordTimer(), {
  #   if (record == TRUE) {
  #     lapply(1:length(cams), function(i) readNext(cams[[i]], frames[[i]]))
  #     now <- as.numeric(Sys.time()) * 1000
  #
  #     if (now >= steps[counter]) {
  #       ts <- getTextSize(as.character(now), thickness = 2)
  #       lapply(1:length(cams), function(i) {
  #         drawRectangle(frames[[i]], 1, 1, ts[2] + 20, ts[1] + 20, color = "white", thickness = -1)
  #         drawText(frames[[i]], as.character(now), 10, 10, color = "black", thickness = 2)
  #         writeFrame(vws[[i]], frames[[i]])
  #       })
  #
  #       counter <<- counter + 1
  #       updateProgressBar(session, "pb", value = 100 * (counter / end))
  #     }
  #
  #     if (counter > end) {
  #       updateProgressBar(session, "pb", value = 100)
  #       display <<- TRUE
  #       record <<- FALSE
  #       enable("displayImg")
  #       enable("camera")
  #       enable("zoom")
  #       enable("focus")
  #       enable("exposure")
  #       enable("gain")
  #       enable("brightness")
  #       enable("interval")
  #       enable("duration")
  #       enable("savedir")
  #       suppressWarnings(lapply(vws, release))
  #     }
  #   }
  # }, ignoreNULL = TRUE)

  observe({
    if (record == TRUE) {
      lapply(1:length(cams), function(i) readNext(cams[[i]], frames[[i]]))
      now <- as.numeric(Sys.time()) * 1000

      ts <- getTextSize(as.character(now), thickness = 2)
      lapply(1:length(cams), function(i) {
        drawRectangle(frames[[i]], 1, 1, ts[2] + 20, ts[1] + 20, color = "white", thickness = -1)
        drawText(frames[[i]], as.character(now), 10, 10, color = "black", thickness = 2)
        writeFrame(vws[[i]], frames[[i]])
      })

      updateProgressBar(session, "pb", value = 100 * (now - origTime) / (endRecordTime - origTime))

      if (now > endRecordTime) {
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
        recordInterval <<- displayInterval
        suppressWarnings(lapply(vws, release))
      }
    }

    invalidateLater(recordInterval - ((as.numeric(Sys.time()) * 1000 - origTime) %% recordInterval))
  })


  #### Cleanup ####
  session$onSessionEnded(function() {
    if (length(vws) > 1) {
      suppressWarnings(lapply(vws, function(vw) {
        if (isVideoWriter(vw)) {
          release(vw)
        }
      }))
    }
    suppressWarnings(lapply(cams, release))
  })
}
