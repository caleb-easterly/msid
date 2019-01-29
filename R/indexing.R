#' define index df
#'
#' @importFrom dplyr left_join
#' @export
define_index_df <- function(constants) {
    # indices for full model
    model_indices <- expand.grid(constants, stringsAsFactors = FALSE)
    model_indices$model_uid <- as.numeric(row.names(model_indices))

    # indices for mixing matrix, etc
    parm_indices <- expand.grid(constants[-1], stringsAsFactors = FALSE)
    parm_indices$parm_uid <- as.numeric(row.names(parm_indices))

    # join parm_indices to model_indices
    model_indices <- left_join(model_indices, parm_indices, by = c("sex", "sexact", "sexid"))
    return(list(model_indices = model_indices,
                parm_indices = parm_indices))
}

#' todo: write test
#' @export
get_parm_index <- function(qsex, qsexid, qsexact, parm_indices){
    with(parm_indices, parm_uid[sexact == qsexact & sexid == qsexid & sex == qsex])
}
