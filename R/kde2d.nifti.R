#' @title Two-Dimensional Kernel Density Estimation of Images
#' @description Two-dimensional kernel density estimation of
#' 2 nifti images
#'
#' @param img1 Object of class \code{nifti}, array, or filename of image
#' @param img2 Object of class \code{nifti}, array, or filename of image
#' @param mask Binary bject of class \code{nifti}, array, or filename of image.
#' If \code{NULL}
#' @param ... Additional arguments to pass to \code{\link{kde2d}}
#'
#' @return Matrix of the joint PDF
#' @export
#' @importFrom MASS kde2d
kde2d.nifti = function(
  img1,
  img2,
  mask = NULL,
  ...) {


  L = get_value_data(
    img1 = img1,
    img2 = img2,
    mask = mask)

  res = MASS::kde2d(L$img1, L$img2, ...)

  nbins = length(res$y)
  stopifnot( nbins ==  length(res$x))

  N = length(L$img1)

  est_bias = nbins / (2 * log(2) * N)

  joint = res$z
  joint = joint / sum(joint)

  return(joint)

}


