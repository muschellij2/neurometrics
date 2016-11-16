
#' @title Get image values
#' @description Simple wrapper for robust extraction of
#' values from images
#'
#' @param img1 Object of class \code{nifti}, array, or filename of image
#' @param img2 Object of class \code{nifti}, array, or filename of image
#' @param mask Binary bject of class \code{nifti}, array, or filename of image.
#' If \code{NULL}
#'
#' @return List of values from the images
#' @importFrom neurobase check_nifti same_dims mask_vals
#' @importFrom methods as
get_value_data = function(
  img1,
  img2,
  mask = NULL) {
  L = list(img1 = img1, img2 = img2)

  L = lapply(L, function(img) {
    if (is.character(img)) {
      img = check_nifti(img)
    }
    img = as.array(img)
    img = as(img, "array")
    return(img)
  })

  if (!same_dims(L)) {
    stop("Dimensions of images are not the same!")
  }

  if (!is.null(mask)) {
    if (!same_dims(L, mask)) {
      stop("Dimensions of images and the mask are not the same!")
    }

    L = lapply(L, mask_vals, mask = mask)
  } else {
    L = lapply(L, c)
  }
  return(L)
}