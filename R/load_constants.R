#' These are used throughout the code to
#' keep track of compartments and such
#'
#' @export
load_constants <- function(ids){
    sexact <- c('high', 'low')
    sex <- c('m', 'w')
    if (ids == 'msid'){
        sexid <- c('het', 'gay', 'bi')
    } else {
        sexid <- c('het')
    }
    epi <- c('X', 'Y', 'Z', 'V', 'W', 'I')
    return(list(epi=epi, sexact=sexact, sexid=sexid, sex=sex))
}

