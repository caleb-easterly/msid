#' define index df
#'
#' @importFrom dplyr left_join
#' @export
define_index_df <- function(constants) {
    # indices for full model
    model_indices <- expand.grid(constants, stringsAsFactors = FALSE)
    model_indices$model_uid <- as.numeric(row.names(model_indices))

    # indices for mixing matrix, etc
    demo_indices <- expand.grid(constants[-1], stringsAsFactors = FALSE)
    demo_indices$demo_uid <- as.numeric(row.names(demo_indices))

    # join demo_indices to model_indices
    model_indices <- left_join(model_indices, demo_indices, by = c("sex", "sexact", "sexid"))
    return(list(model_indices = model_indices,
                demo_indices = demo_indices))
}

#' todo: write test
#' @export
get_demo_index <- function(qsex, qsexid, qsexact, demo_indices){
    with(demo_indices, demo_uid[sexact == qsexact & sexid == qsexid & sex == qsex])
}
