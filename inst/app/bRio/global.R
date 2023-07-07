#### Libraries ####
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyFiles)
library(Rvision)


#### Custom functions ####
listCams <- function() {
  if (Sys.info()["sysname"] == "Linux") {
    dev <- system2("ls", "/dev/video*", TRUE)
    dev_id <- as.numeric(gsub("/dev/video", "", dev))
  } else if (Sys.info()["sysname"] == "Darwin") {
    dev <- system2("system_profiler", "SPCameraDataType", TRUE)
    dev_id <- 0:(sum(grepl("Unique ID", dev)) - 1)
  } else {
    list()
  }

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
  cams
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


#### Globals #####
cams <- listCams()
volumes <- getVolumes()
