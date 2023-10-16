#### Libraries ####
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyFiles)
library(Rvision)


#### Custom functions ####
listCams <- function(width = 1920, height = 1080, fps = 30) {
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
    if (Sys.info()["sysname"] == "Linux") {
      st <- tryCatch(stream(i, api = "V4L2"), error = function(e) FALSE)
    } else if (Sys.info()["sysname"] == "Darwin") {
      st <- tryCatch(stream(i, api = "AVFOUNDATION"), error = function(e) FALSE)
    }

    if (isStream(st)) {
      setProp(st, "FOURCC", fourcc("MJPG"))
      setProp(st, "AUTO_EXPOSURE", 1)
      setProp(st, "AUTOFOCUS", 0)
      setProp(st, "FRAME_WIDTH", width)
      setProp(st, "FRAME_HEIGHT", height)
      setProp(st, "FPS", fps)
      setProp(st, "AUTO_WB", 0)

      if (getProp(st, "FRAME_WIDTH") == width & getProp(st, "FRAME_HEIGHT") == height) {
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


#### Globals #####
frameSize <- c(1920, 1080) # c(4096, 2160)
fps <- 60
cams <- listCams(frameSize[1], frameSize[2], fps)
volumes <- c(Home = fs::path_home(), getVolumes()())
