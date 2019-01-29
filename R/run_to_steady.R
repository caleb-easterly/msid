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
