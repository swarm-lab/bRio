#### Libraries ####
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyFiles)
library(Rvision)


#### Globals #####
if (Sys.info()["sysname"] == "Linux") {
  dev <- suppressWarnings(system("ls /dev/video*", intern = TRUE,
                ignore.stderr = TRUE))

  if (length(dev > 0)) {
    dev_id <- as.numeric(gsub("/dev/video", "", dev))
    cams <- list()

    cams <- lapply(dev_id, function(i) {
      st <- tryCatch(stream(i, api = "V4L2"), error = function(e) FALSE)

      if (isStream(st)) {
        setProp(st, "FOURCC", fourcc("MJPG"))
        setProp(st, "FRAME_WIDTH", 4096)
        setProp(st, "FRAME_HEIGHT", 2160)

        if (getProp(st, "FRAME_WIDTH") == 4096 & getProp(st, "FRAME_HEIGHT") == 2160) {
          st
        } else {
          release(st)
          NULL
        }
      } else {
        NULL
      }
    })

    cams <- cams[lengths(cams) != 0]
  } else {
    cams <- list()
  }
} else {
  stop("Unsupported platform.")
}

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
