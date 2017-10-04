#' @title Joint Entropy
#' @description Calculates Shannon entropy from a joint density.
#' @param joint Matrix of joint densities.
#'
#' @return Scalar value of entropy
#' @export
joint_entropy = function(joint) {
  joint = joint / sum(joint)

  keep = joint > 0
  ent = -sum(joint[keep] * log(joint[keep]))
  return(ent)
}