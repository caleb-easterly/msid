#' define index df
#'
#' @param constants output from \code{\link{load_constants}},
#' specific to the sexid structure specified to that function
#'
#' @return A list of "model indices" and "demo indices".
#' The model indices specify the indices of demographic and epidemiological groups,
#' while the demo indices only specify the demographic indices (not including epi groups)
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

#' Load constants
#'
#' @description These are used throughout the code to
#' keep track of compartments
#'
#' @param ids either msid, het, or all
#'
#' @export
load_constants <- function(ids){
    sexact <- c('high', 'low')
    sex <- c('m', 'w')
    if (ids == 'msid'){
        sexid <- c('het', 'gay', 'bi')
    } else if (ids == 'het') {
        sexid <- 'het'
    } else if (ids == 'msid_avg'){
        sexid <- 'all'
    }
    epi <- c('X', 'Y', 'Z', 'V', 'W', 'I')
    return(list(epi=epi, sexact=sexact, sexid=sexid, sex=sex))
}

#' Get the index for a demographic group
#'
#' @description Get the index for a given demographic group.
#' This is the demo index (see \code{\link{define_index_df}}),
#' not the model index.
#'
#' @export
get_demo_index <- function(qsex, qsexid, qsexact, demo_indices){
    with(demo_indices, demo_uid[sexact == qsexact & sexid == qsexid & sex == qsex])
}
