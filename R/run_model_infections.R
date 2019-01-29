#' Calculate infections
#'
#' @importFrom deSolve lsoda
#' @export
run_model_infections <- function(pars, vacc, endtime){
    steady_start_modres <- run_to_steady(parms = pars, vaccination = vacc)
    # take last row (steady state) and remove first (time) column
    steady_start_vec <- steady_start_modres[2, -1]

    # set inc compartments to 0
    inc <- pars$structural$inc_compartments
    steady_start_vec[inc$model_uid] <- 0

    accrue_infections <- lsoda(y = steady_start_vec,
                               times = c(0, endtime),
                               func = model_function,
                               parms = pars,
                               vaccination = vacc)[, -1] # remove time column
    inc_return <- data.frame(inc, 'inf' = accrue_infections[2, inc$model_uid],
                             stringsAsFactors = FALSE)
    return(inc_return)
}
