#' Sample from the prior distributions.
#'
#' Called from IMIS.
#'
#' In this function, the prior samples are transformed to be unbounded, which
#' makes a better search space for IMIS. Then, the parameters are "back-transformed"
#' before being sent to the model function.
#'
#' @export
sample.prior <- function(nsamps){
    # beta
    logit_betaMM <- logit(rbeta(nsamps, 3, 1))
    logit_betaMF <- logit(rbeta(nsamps, 3, 1))
    logit_betaFF <- logit(rbeta(nsamps, 3, 1))

    # InfClearRate
    estInfDur <- estimate_inf_duration()
    log_inf_dur_M <- log(rgamma(nsamps,
                                shape = estInfDur$gammaM$alpha,
                                rate = estInfDur$gammaM$beta))
    log_inf_dur_F <- log(rgamma(nsamps,
                                shape = estInfDur$gammaF$alpha,
                                rate = estInfDur$gammaF$beta))

    # NatImmWane.
    nat_imm_dur_params <- gamma_params_mom(20, 15, scale = FALSE)
    log_nat_imm_dur_M <- log(rgamma(nsamps, shape = nat_imm_dur_params$alpha, rate = nat_imm_dur_params$beta))
    log_nat_imm_dur_F <- log(rgamma(nsamps, shape = nat_imm_dur_params$alpha, rate = nat_imm_dur_params$beta))

    cbind(logit_betaMM,
          logit_betaMF,
          logit_betaFF,
          log_inf_dur_M,
          log_inf_dur_F,
          log_nat_imm_dur_M,
          log_nat_imm_dur_F)
}


#' prior density of parms.
#' takes parameters directly from sample.prior()
#'
#' @export
prior <- function(parms){
    bt_parms <- backtransform_parms(parms)

    # beta
    betaMM_like <- dbeta(bt_parms[, 1], 3, 1)
    betaMF_like <- dbeta(bt_parms[, 2], 3, 1)
    betaFF_like <- dbeta(bt_parms[, 3], 3, 1)

    # InfClearRate
    estInfDur <- estimate_inf_duration()

    # take reciprocal of inf rate to get inf dur
    inf_dur_M <- 1/bt_parms[, "inf_clear_rate_M"]
    inf_dur_M_like <- dgamma(inf_dur_M,
                                    shape = estInfDur$gammaM$alpha,
                                    rate = estInfDur$gammaM$beta)
    inf_dur_F <- 1/bt_parms[, "inf_clear_rate_F"]
    inf_dur_F_like <- dgamma(inf_dur_F,
                                    shape = estInfDur$gammaF$alpha,
                                    rate = estInfDur$gammaF$beta)

    # NatImmWane
    estNatImmDur <- gamma_params_mom(20, 15, scale = FALSE)
    # bt_parms returns rate, but we want duration - inverse
    nat_imm_dur_M <- 1/bt_parms[, "nat_imm_wane_rate_M"]
    nat_imm_dur_M_like <- dgamma(nat_imm_dur_M,
                                 shape = estNatImmDur$alpha,
                                 rate = estNatImmDur$beta)
    nat_imm_dur_F <- 1/bt_parms[, "nat_imm_wane_rate_F"]
    nat_imm_dur_F_like <- dgamma(nat_imm_dur_F,
                                 shape = estNatImmDur$alpha,
                                 rate = estNatImmDur$beta)

    dens_mat <- cbind(betaMM_like,
                      betaMF_like,
                      betaFF_like,
                      inf_dur_M_like,
                      inf_dur_F_like,
                      nat_imm_dur_M_like,
                      nat_imm_dur_F_like)
    apply(dens_mat, 1, prod)
}

#' Used to define a likelihood function either for het or so mixing structures
#'
#' @param so sex orientation: either "het" or "msid"
#' @param pop_dist path to the pop dist table
#' @param contact path to the contact table
#' @import parallel
#' @export
likelihood_generator <- function(so, pop_dist, contact){
    likelihood <- function(parms) {
        btparms <- backtransform_parms(parms)
        fTrue <- female_prevalence_target()
        mTrue <- male_prevalence_target()
        cl <- makeForkCluster(detectCores())
        like <- parSapply(cl, 1:nrow(btparms), function(i) {
            # run model
            f_mod_prev <- run_model_with_btparms(i, btparms, so, pop_dist, contact)

            # group prevalence to gender
            prev_grpd <- group_prevalence_to_gender(f_mod_prev)
            m_mod_prev <- with(prev_grpd, mean_prev[sex == 'm'])
            w_mod_prev <- with(prev_grpd, mean_prev[sex == 'w'])
            dnorm(x = mTrue[1], mean = m_mod_prev, sd = mTrue[2]) *
                dnorm( x = fTrue[1], mean = w_mod_prev, sd = fTrue[2])
        })
        stopCluster(cl)
        return(like)
    }
    return(likelihood)
}

#' Run the model with the ith set of bback-transformed parameters
#'
#' Called from the likelihood function.
#'
#' @export
run_model_with_btparms <- function(i, btparms, so, pop_dist, contact){
    # define one set of parameters
    parms <- define_calib_parms(i, btparms, so, pop_dist, contact)
    # estimate prevalence under no-vaccine conditions
    mod_prev <- run_to_steady(parms, vaccination = parms$structural$vacc0)
    # calculate prevalence, given
    calcd_prev <- calc_prevalence(mod_prev[nrow(mod_prev), -1], parms)
    # puts the model results in a data frame
    f_mod_prev <- format_prevalence(calcd_prev, parms)
    return(f_mod_prev)
}

#' take back-transformed calib results and pass to define_parms
#' @export
define_calib_parms <- function(i, btparms, so, pop_dist, contact){
    ith_set <- btparms[i, ]
    parms <- define_parameters(betaMM = ith_set["betaMM"],
                               betaMW = ith_set["betaMF"],
                               betaWW = ith_set["betaFF"],
                               inf_clear_rate_M = ith_set["inf_clear_rate_M"],
                               inf_clear_rate_W = ith_set["inf_clear_rate_F"],
                               nat_imm_wane_rate_M = ith_set["nat_imm_wane_rate_M"],
                               nat_imm_wane_rate_W = ith_set["nat_imm_wane_rate_F"],
                               sexids = so,
                               population_dist = pop_dist,
                               contact_df = contact)
    return(parms)
}



#' Back-transform parameters from the
#' sampled form to the model and/or prior form
#'
#' @export
backtransform_parms <- function(parms){
    betaMM <- ilogit(parms[, "logit_betaMM"])
    betaMF <- ilogit(parms[, "logit_betaMF"])
    betaFF <- ilogit(parms[, "logit_betaFF"])
    # transform the durations to rates by
    # exponentiating and taking the inverse
    inf_clear_rate_M <- 1/exp(parms[, "log_inf_dur_M"])
    inf_clear_rate_F <- 1/exp(parms[, "log_inf_dur_F"])
    nat_imm_wane_rate_M <- 1/exp(parms[, "log_nat_imm_dur_M"])
    nat_imm_wane_rate_F <- 1/exp(parms[, "log_nat_imm_dur_F"])
    cbind(betaMM,
          betaMF,
          betaFF,
          inf_clear_rate_M,
          inf_clear_rate_F,
          nat_imm_wane_rate_M,
          nat_imm_wane_rate_F)
}


#' Use data to estimate gamma distributions for the infection duration
#' Returns parameters for gamma distributions, male and female respectively
#'
#' @export
estimate_inf_duration <- function(){
    # "gamma" is the old name I was using for inf_clear_rate

    # male data from Giuliano, 2011
    mu_gammaM <- 12.2/12
    ub_gammaM <- 20.2/12
    lb_gammaM <- 7.4/12
    se_gammaM <- (ub_gammaM - lb_gammaM)/(1.96*2) * 2
    params_gammaM <- gamma_params_mom(mean = mu_gammaM, sigma = se_gammaM, scale = F)

    # female data from Jaisamrarn, et al., 2013
    mu_gammaF <- 17.11/12
    ub_gammaF <- 30.26/12
    lb_gammaF <- 7.80/12
    se_gammaF <- (ub_gammaF - lb_gammaF)/(1.96*2) * 2
    params_gammaF <- gamma_params_mom(mean = mu_gammaF, sigma = se_gammaF, scale = F)

    list("gammaM" = params_gammaM, "gammaF" = params_gammaF)
}




