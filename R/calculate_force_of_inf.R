#' Calculate the force of infection
#'
#' @param suff_contact_matrix this is the contact matrix multiplied
#' by the probability of transmission
#'
#' @param prev the prevalence vector
#'
#' @export
calculate_force_of_inf <- function(suff_contact_matrix, prev) {
  foi <- (suff_contact_matrix %*% prev)

  # convert 1-column matrix to vector and return
  foi[, 1]
}
