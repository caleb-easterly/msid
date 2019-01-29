root_fun <- function(t, x, parms, vaccination){
    dstate <- model_function(t, x, parms, vaccination)
    inc_compartments <- parms$structural$inc_compartments$model_uid
    # average values of derivs in abs compartments
    mean(abs(dstate[[1]][-inc_compartments])) - 1e-8
}
