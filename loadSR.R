# Modified water::loadImageSR() with correct filename pattern
loadSR = function (path = getwd(), aoi) 
{
  files <- list.files(path = path, pattern = "_SR_B+[1-7].TIF$", 
                      full.names = T)
  stack1 <- list()
  for (i in 1:7) {
    stack1[i] <- raster(files[i])
  }
  image_SR <- do.call(stack, stack1)
  image_SR <- water:::aoiCrop(image_SR, aoi)
  # image_SR <- image_SR/10000
  bandnames <- c("SB", "B", "G", "R", "NIR", 
                 "SWIR1", "SWIR2")
  image_SR <- water:::saveLoadClean(imagestack = image_SR, stack.names = bandnames, 
                                    file = "image_SR", overwrite = TRUE)
  return(image_SR)
}