#' Convert cimg object to grayscale via XYZ
#'
#' Improves on the XYZ method of \code{\link[imager]{grayscale}}
#' by handling alpha channel and retaining relative luminance of
#' the image
#'
#' @inheritParams ort
#' @export
ensure_grayscale = function(img, bg=1) {
  if (spectrum(img) == 1) {
    return(img)
  } else if (spectrum(img) == 3) {
    # imager::grayscale XYZ method with no XYZtoRGB
    # should be idempotent
    Y = img %>% .sRGBtoRGB %>% RGBtoXYZ %>% G
    out = Y %>% add.colour %>% .RGBtosRGB %>% R
    return(out)
  } else if (spectrum(img) == 4) {
    Y = img[,,,1:3,drop=FALSE] %>% .sRGBtoRGB %>% RGBtoXYZ %>% G
    out = lerp(bg, Y, img[,,,4,drop=FALSE]) %>% add.colour %>% .RGBtosRGB %>% R
    return(out)
  } else {
    # TODO: complain
  }
}
