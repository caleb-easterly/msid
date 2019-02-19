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

from_std_unif <- function(x, min, max){
    min +  x * (max - min)
}

logit <- function(x) {
    log( x / ( 1 - x))
}

ilogit <- function(x) {
    exp(x)/(1 + exp(x))
}

#' @export
gamma_params_mom <- function(mean, sigma, scale = F){
    if (scale){
        alpha <- (mean^2)/(sigma^2)
        beta <- (sigma^2)/mean
    } else {
        alpha <- (mean^2)/(sigma^2)
        beta  <- mean/(sigma^2)
    }
    params <- list(alpha = alpha, beta = beta)
    return(params)
}