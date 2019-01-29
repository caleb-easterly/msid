#' todo: write test
#' @importFrom reshape2 dcast
#' @export
define_contact_matrix <- function(contact_df, parm_indices){
    # join with contact df
    contact_df$r_uid <- sapply(1:nrow(contact_df), function(i) get_parm_index(contact_df$r_sex[i],
                                                                  contact_df$r_sexid[i],
                                                                  contact_df$r_sexact[i],
                                                                  parm_indices))
    contact_df$rp_uid <- sapply(1:nrow(contact_df), function(i) get_parm_index(contact_df$rp_sex[i],
                                                                               contact_df$rp_sexid[i],
                                                                               contact_df$rp_sexact[i],
                                                                               parm_indices))

    # cast, parm_uid ~ parm_uid?
    mat <- dcast(contact_df, r_uid ~ rp_uid, value.var = "corrected_r")

    # first col isn't important
    mat_clean <- data.matrix(mat[, -1])

    # replace na with 0
    mat_clean[is.na(mat_clean)] <- 0

    # return
    return(mat_clean)
}
