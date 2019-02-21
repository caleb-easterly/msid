#' define transmission probabilities between sexes
#'
#' transmission probabilities are by partnership
#'
#' @param betaMM prob. between men
#' @param betaMW prob. between men and women (assumed to be the same in both directions)
#' @param betaWW prob. between women
#' @param demo_indices data frame mapping demographic groups to unique identifiers
#'
#' @export
define_transmission_prob_matrix <- function(betaMM, betaMW, betaWW, demo_indices){
    n_grps <- nrow(demo_indices)
    tpm <- matrix(0, n_grps, n_grps)
    sex <- demo_indices$sex
    for (i in 1:n_grps){
        for (j in 1:n_grps){
             if (sex[i] == "m" & sex[j] == "m"){
                 tpm[i, j] <- betaMM
             }
             if ((sex[i] == "m" & sex[j] == "w")|
                 (sex[i] == "w" & sex[j] == "m")){
                 tpm[i, j] <- betaMW
             }
             if (sex[i] == "w" & sex[j] == "w"){
                tpm[i, j] <- betaWW
             }
        }
    }
    return(tpm)
}

