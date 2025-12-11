#' Create a data frame representaiton of an image
#'
#' Takes a \code{\link[imager]{cimg}} object and
#' attempts to create a data frame that, when plotted using the default
#' \code{\link[graphics]{plot}} in base R, looks like the original image.
#'
#' @param img An \code{\link[imager]{cimg}} object containing an image (from imager)
#' @param bg Background color (default is white)
#' @param grid_size Size of grid used for conversion
#' @param grid_offset Offset of grid used for conversion
#'
#' @return A data frame with two columns, `x` and `y` that
#' hopefully resembles the image when plotted
#' @export
ort = function(img, bg=1,
  grid_size = function(x) {1/(250*(1.0001-x))},
  grid_offset = function(x) {sin(x*100)*10}
) {
  value_pineapple_img = ensure_grayscale(img, bg)
  building_pineapple_data = matrix(nrow=0, ncol=2,
                                   dimnames=list(c(), c('x', 'y')))

  for (x in 1:dim(value_pineapple_img)[1]) {
    for (y in 1:dim(value_pineapple_img)[2]) {
      s = grid_size(value_pineapple_img[x, y, 1, 1]) *
        max(dim(value_pineapple_img))
      o = grid_offset(value_pineapple_img[x, y, 1, 1]) *
        max(dim(value_pineapple_img))
      xmin = ceiling((x - o) / s)
      xmax = floor((x - o + 1) / s)
      ymin = ceiling((y - o) / s)
      ymax = floor((y - o + 1) / s)
      if (xmin > xmax | ymin > ymax) {
        next
      }
      for (xs in xmin:xmax) {
        for (ys in ymin:ymax) {
          pointx = xs * s + o
          pointy = ys * s + o
          building_pineapple_data %<>% rbind(c(pointx, -pointy))
        }
      }
    }
  }

  building_pineapple_data %>% as.data.frame()
}
