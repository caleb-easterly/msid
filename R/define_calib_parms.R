#' take back-transformed calib results and pass to define_parms
#' @export
define_calib_parms <- function(i, btparms, so, pop_dist, contact){
    parms <- define_parameters(betaMM = btparms[i, 1],
                               betaMW = btparms[i, 2],
                               betaWW = btparms[i, 3],
                               inf_clear_rate_M = btparms[i, 4],
                               inf_clear_rate_W = btparms[i, 5],
                               nat_imm_wane_rate_M = btparms[i, 6],
                               nat_imm_wane_rate_W = btparms[i, 7],
                               sexids = so,
                               population_dist = pop_dist,
                               contact_df = contact)
    return(parms)
}
