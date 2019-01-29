#' @export
define_transmission_prob_matrix <- function(betaMM, betaMW, betaWW, parm_indices){
    n_grps <- nrow(parm_indices)
    tpm <- matrix(0, n_grps, n_grps)
    sex <- parm_indices$sex
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

