
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

  same_dims(L)

  if (!is.null(mask)) {
    same_dims(L, mask)
    L = lapply(L, mask_vals, mask = mask)
  } else {
    L = lapply(L, c)
  }
  return(L)
}