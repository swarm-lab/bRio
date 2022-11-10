library(Rvision)

if (Sys.info()["sysname"] == "Linux") {
  cams <- system("v4l2-ctl --list-devices", intern = TRUE)
  cams <- grep("brio", cams, ignore.case = TRUE) + 1
  cams <- as.numeric(gsub("\t/dev/video", "", cams[idx]))
} else {
  stop("Unsupported platform.")
}


st <- stream(cams[1])

setProp(st, "FOURCC", fourcc("MJPG"))
setProp(st, "FRAME_WIDTH", 4096)
setProp(st, "FRAME_HEIGHT", 2160)
setProp(st, "AUTOFOCUS", 0)
setProp(st, "AUTO_EXPOSURE", 3)
setProp(st, "EXPOSURE", 1000)
