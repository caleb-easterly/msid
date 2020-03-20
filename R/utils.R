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
    if (sigma == 0 | mean == 0) {
        alpha <- NA
        beta <- NA
    }
    else if (scale){
        alpha <- (mean^2)/(sigma^2)
        beta <- (sigma^2)/mean
    } else {
        alpha <- (mean^2)/(sigma^2)
        beta  <- mean/(sigma^2)
    }
    params <- list(alpha = alpha, beta = beta)
    return(params)
}
