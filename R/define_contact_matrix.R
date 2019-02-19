#' define partnering rates between demographic groups
#'
#' @param contact_df data.frame with partnering rates in the \code{corrected_r} column
#' @param parm_indices data.frame with the mapping of demographic groups to unique identifiers
#' @importFrom reshape2 dcast
#' @export
define_contact_matrix <- function(contact_df, parm_indices){
    n_demos <- nrow(contact_df)
    # join with contact df
    contact_df$r_uid <- sapply(1:n_demos, function(i) get_parm_index(contact_df$r_sex[i],
                                                                  contact_df$r_sexid[i],
                                                                  contact_df$r_sexact[i],
                                                                  parm_indices))
    contact_df$rp_uid <- sapply(1:n_demos, function(i) get_parm_index(contact_df$rp_sex[i],
                                                                               contact_df$rp_sexid[i],
                                                                               contact_df$rp_sexact[i],
                                                                               parm_indices))
    # cast so that the respondents are in the rows and the
    # partners are in the columns
    mat <- dcast(contact_df, r_uid ~ rp_uid, value.var = "corrected_r")

    # first col isn't important, because it just has the r_uid in it
    mat_clean <- data.matrix(mat[, -1])

    # replace na with 0
    mat_clean[is.na(mat_clean)] <- 0

    # return
    return(mat_clean)
}
