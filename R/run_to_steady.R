#' Run the model to steady state, based on parameters and vaccination
#' @importFrom deSolve lsodar
#' @export
run_to_steady <- function(parms, vaccination){
    mod <- lsodar(y = parms$structural$init_cond,
                  times = c(0, 1000), # stop it at 1000 years if steady state is not reached
                  func = model_function,
                  parms = parms,
                  vaccination = vaccination,
                  rootfunc = root_fun)
    return(mod)
}

root_fun <- function(t, x, parms, vaccination){
    dstate <- model_function(t, x, parms, vaccination)
    inc_compartments <- parms$structural$inc_compartments$model_uid
    # average values of derivs in abs compartments
    mean(abs(dstate[[1]][-inc_compartments])) - 1e-8
}

