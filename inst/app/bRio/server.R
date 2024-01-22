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
  ts <- matrix()
  pos <- 0
  recordInterval <- displayInterval
  endRecordTime <- origTime
  block <- 0
  blocks <- NULL


  #### Init ####
  observe({
    if (length(cams) > 0) {
      l <- seq_along(cams)
      names(l) <- paste0("Camera ", l)
      updatePickerInput(session, "camera", choices = l)
      updatePickerInput(session, "cameraRecord", choices = l)
    }
  })

  observeEvent(input$camera, {
    ix <- as.numeric(input$camera)
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
    updateSliderInput(session, "temperature",
                      value = getProp(cams[[ix]], "TEMPERATURE"))
    display <<- TRUE
  }, ignoreNULL = TRUE)


  #### Display ####
  observe({
    if (display == TRUE) {
      ix <- isolate(as.numeric(input$camera))
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
      suppressMessages(
        write.Image(toDisplay, paste0(tmpDir, "/display.jpg"), TRUE)
      )
      isolate(refreshDisplay(refreshDisplay() + 1))
    }

    invalidateLater(
      displayInterval - ((as.numeric(Sys.time()) * 1000 - origTime) %% displayInterval)
    )
  })

  output$displayImg <- renderImage({
    refreshDisplay()
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
      ix <- as.numeric(input$camera)
      setProp(cams[[ix]], "FOCUS", input$focus)
    }
  })

  observeEvent(input$exposure, {
    if (!is.null(input$camera)) {
      ix <- as.numeric(input$camera)
      setProp(cams[[ix]], "EXPOSURE", input$exposure)
    }
  })

  observeEvent(input$gain, {
    if (!is.null(input$camera)) {
      ix <- as.numeric(input$camera)
      setProp(cams[[ix]], "GAIN", input$gain)
    }
  })

  observeEvent(input$brightness, {
    if (!is.null(input$camera)) {
      ix <- as.numeric(input$camera)
      setProp(cams[[ix]], "BRIGHTNESS", input$brightness)
    }
  })

  observeEvent(input$temperature, {
    if (!is.null(input$camera)) {
      ix <- as.numeric(input$camera)
      setProp(cams[[ix]], "TEMPERATURE", input$temperature)
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
    block <<- 1
    display <<- FALSE
    disable("displayImg")
    disable("camera")
    disable("zoom")
    disable("focus")
    disable("exposure")
    disable("gain")
    disable("brightness")
    disable("blockN")
    disable("blockInt")
    disable("blockLength")
    disable("frameInt")
    disable("savedir")
    disable("start")

    ix <- as.numeric(input$cameraRecord)
    path <- parseDirPath(volumes, input$savedir)
    vws <<- lapply(seq_along(ix), function(i) {
      videoWriter(paste0(path, "/Camera_", ix[i], ".mp4"), "avc1", 25,
                  frameSize[2], frameSize[1], TRUE, "FFMPEG")
    })

    ts <<- matrix(NA, nrow = (25 + input$blockLength / input$frameInt) * input$blockN, ncol = 1)
    pos <<- 26

    today <- format(Sys.time(), "%F")
    lapply(seq_along(ix), function(i) {
      frame %i*% 0
      txt1 <- paste0("Camera ", ix[i], ", Block 1")
      ts <- rbind(getTextSize(txt1, font_scale = 2, thickness = 3),
                  getTextSize(today, font_scale = 1.5, thickness = 2))
      drawText(frame, txt1, (frameSize[1] / 2) - (ts[1, 2] / 2),
               (frameSize[2] / 2) + (ts[1, 1] / 2), color = "white",
               font_scale = 2, thickness = 3)
      drawText(frame, today, (frameSize[1] / 2) - (ts[2, 2] / 2),
               (frameSize[2] / 2) - 2 * ts[2, 1], color = "white",
               font_scale = 1.5, thickness = 2)
      lapply(1:25, function(j) writeFrame(vws[[i]], frame))
    })

    updateProgressBar(session, "pb", value = 0)
    frames <<- lapply(seq_along(ix), function(i) readNext(cams[[ix[i]]]))
    recordInterval <<- input$frameInt * 1000
    origTime <<- as.numeric(Sys.time()) * 1000
    blocks <<- data.frame(
      start = origTime + (0:(input$blockN - 1)) * input$blockInt * 1000,
      end = origTime + (0:(input$blockN - 1)) * input$blockInt * 1000 +
        input$blockLength * 1000)
  }, ignoreInit = TRUE)

  observe({
    now <- as.numeric(Sys.time()) * 1000

    if (block > 0) {
      if (block > nrow(blocks)) {
        block <<- 0
        display <<- TRUE
        enable("displayImg")
        enable("camera")
        enable("zoom")
        enable("focus")
        enable("exposure")
        enable("gain")
        enable("brightness")
        enable("blockN")
        enable("blockInt")
        enable("blockLength")
        enable("frameInt")
        enable("savedir")
        recordInterval <<- displayInterval
        suppressWarnings(
          lapply(vws, function(vw) {
            write.csv(ts, paste0(vw$output(), ".csv"))
            release(vw)
          })
        )
      } else {
        if (now > blocks$end[block]) {
          block <<- block + 1

          if (block <= nrow(blocks)) {
            pos <<- pos + 25
            today <- format(Sys.time(), "%F")
            ix <- as.numeric(input$cameraRecord)
            lapply(seq_along(ix), function(i) {
              frame %i*% 0
              txt1 <- paste0("Camera ", ix[i], ", Block ", block)
              ts <- rbind(getTextSize(txt1, font_scale = 2, thickness = 3),
                          getTextSize(today, font_scale = 1.5, thickness = 2))
              drawText(frame, txt1, (frameSize[1] / 2) - (ts[1, 2] / 2),
                       (frameSize[2] / 2) + (ts[1, 1] / 2), color = "white",
                       font_scale = 2, thickness = 3)
              drawText(frame, today, (frameSize[1] / 2) - (ts[2, 2] / 2),
                       (frameSize[2] / 2) - 2 * ts[2, 1], color = "white",
                       font_scale = 1.5, thickness = 2)
              lapply(1:25, function(j) writeFrame(vws[[i]], frame))
            })
          }
        } else {
          if (now >= blocks$start[block]) {
            ts[pos] <<- as.character(now)
            pos <<- pos + 1
            ix <- as.numeric(input$cameraRecord)
            lapply(seq_along(ix), function(i) {
              readNext(cams[[i]], frames[[i]])
              writeFrame(vws[[i]], frames[[i]])
            })
          }
        }
      }

      updateProgressBar(session, "pb", value = 100 * (now - blocks$start[1]) /
                          (tail(blocks$end, n = 1) - blocks$start[1]))
    }

    invalidateLater(
      max(recordInterval - ((as.numeric(Sys.time()) * 1000 - origTime) %% recordInterval), 1)
    )
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
