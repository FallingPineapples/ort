# imager nonsense ------------

# plot(ort(load.image(system.file("FallingPineapple_750x750.png", package = "ort"))))
# plot(ort(load.image(system.file("big_gradient.png", package = "ort"))))
# load.image(system.file("alpha_gradient_test.png", package = "ort"))

test_grayscale_idempotence = function() {
  library(animation)
  ani.options(interval = 0.2, nmax = 100)
  im = load.image(system.file("FallingPineapple_750x750.png", package = "ort"))
  ani.record(reset = TRUE)
  for (i in 1:ani.options('nmax')) {
    # im = add.color(grayscale(im, method='XYZ'))
    im = add.color(ensure_grayscale(im))
    plot(im, main=i)
    ani.record()
  }
  # saveGIF(ani.replay(), 'grayscale_xyz_being_not_idempotent.gif')
}
# test_grayscale_idempotence()

gen_gradient_test_image = function() {
  # R = G = B = x
  # A = y
  im = array(data=0,dim=c(256,256,1,4)) %>% as.cimg
  im[,,1,1:3] = 0:255 %o% rep(1, 256)
  im[,,1,4] = rep(1, 256) %o% 0:255
  save.image(im,'inst/alpha_gradient_test.png')
}
#gen_gradient_test_image()

# str(as.cimg(c(1,1,1)) %>% sRGBtoRGB)
# str(as.cimg(c(1,1,1)) %>% sRGBtoLab %>% LabtoRGB)
# str(as.cimg(c(1,1,1)) %>% sRGBtoLab %>% LabtoRGB %>% RGBtosRGB)
# ??????
# bug report time!!1

patch_srgb_conversions = function() {
  assign('sRGBtoRGB', function (im) {
    imager::sRGBtoRGB(im*255)/255
  }, globalenv())
  assign('RGBtosRGB', function (im) {
    imager::RGBtosRGB(im*255)/255
  }, globalenv())
}



# examples ----------------------------------------------------------------
# Load image using imager
pineapple_img = load.image(system.file("FallingPineapple_750x750.png", package = "ort"))

# Convert to data frame plot
pineapple_ort = ort(pineapple_img)
plot(pineapple_ort)

# Example with alpha background
alph_img = load.image(system.file("alpha_gradient_test.png", package = "ort"))

# default assumes bg = 1 (white background)
alph_ort = ort(alph_img)
plot(alph_ort)

alph_ort_grey = ort(alph_img, bg = 0.5)
plot(alph_ort_grey)

# ensure_grayscale
# Load image using imager
pineapple_img = load.image(system.file("FallingPineapple_750x750.png", package = "ort"))

# Create greyscale version
pineapple_gray = ensure_grayscale(pineapple_img)
plot(pineapple_gray)

# Example with alpha background
alph_img = load.image(system.file("alpha_gradient_test.png", package = "ort"))

# default assumes bg = 1 (white background)
alph_gray = ensure_grayscale(alph_img)
plot(alph_gray)

alph_gray_darker = ensure_grayscale(alph_img, bg = 0.75)
plot(alph_gray_darker)
