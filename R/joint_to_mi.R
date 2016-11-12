#' @title Joint PDF to mutual information
#' @description Calculates mutual information from a joint density.
#' @param joint Matrix of joint densities.  Does not need to be square.
#'
#' @return Scalar value of mutual information
#' @export
joint_to_mi = function(joint) {

  joint = joint / sum(joint)
  p_x = rowSums(joint)
  p_y = colSums(joint)

  marg_prod = outer(p_x, p_y, FUN = "*")
  keep = joint > 0
  mut = sum(joint[keep] * log(joint[keep] / marg_prod[keep]  ))
  return(mut)
}