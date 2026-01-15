lerp = function(a, b, t) {
  a + (b - a) * t
}

.pkg_env <- new.env(parent = emptyenv())
.onLoad = function(libname, pkgname) {
  im <- structure(c(1, 1, 1), dim = c(1L, 1L, 1L, 3L), class = c("cimg", "imager_array", "numeric"))
  .pkg_env$is_fixed = isTRUE(all.equal(imager::RGBtosRGB(im), im, tolerance=1e-6))
}

.RGBtosRGB = function (im) {
  if (.pkg_env$is_fixed) {
    imager::RGBtosRGB(im)
  } else {
    imager::RGBtosRGB(im*255)/255
  }
}

.sRGBtoRGB = function (im) {
  if (.pkg_env$is_fixed) {
    imager::sRGBtoRGB(im)
  } else {
    imager::sRGBtoRGB(im*255)/255
  }
}
