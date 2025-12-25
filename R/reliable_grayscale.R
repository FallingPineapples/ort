#' Convert cimg object to grayscale via XYZ
#'
#' Improves on the XYZ method of \code{\link[imager]{grayscale}}
#' by handling alpha channel and retaining relative luminance of
#' the image
#'
#' @inheritParams ort
#' @examples
#' \dontrun{
#' # Load image using imager
#' pineapple_img = load.image(system.file("FallingPineapple_750x750.png", package = "ort"))
#'
#' # Create grayscale version
#' pineapple_gray = ensure_grayscale(pineapple_img)
#' plot(pineapple_gray)
#'
#' # Example with alpha background
#' alph_img = load.image(system.file("alpha_gradient_test.png", package = "ort"))
#'
#' # default assumes bg = 1 (white background)
#' alph_gray = ensure_grayscale(alph_img)
#' plot(alph_gray)
#'
#' alph_gray_darker = ensure_grayscale(alph_img, bg = 0.75)
#' plot(alph_gray_darker)
#' }
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
