
#' @export
make_parm_gender_vecs <- function(mval, wval, parm_indices){
    parm_df <- data.frame(sex = c('m', 'w'),
                          parm = c(mval, wval),
                          stringsAsFactors = FALSE)
    joined <- left_join(parm_indices, parm_df, by = "sex") %>%
        arrange(parm_uid)
    return(joined$parm)
}
