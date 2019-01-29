#' @export
sample.prior <- function(nsamps){
    # beta
    betaMM <- logit(rbeta(nsamps, 3, 1))
    betaMF <- logit(rbeta(nsamps, 3, 1))
    betaFF <- logit(rbeta(nsamps, 3, 1))

    # InfClearRate
    estInfDur <- estimate_inf_duration()
    inf_dur_M <- log(rgamma(nsamps, shape = estInfDur$gammaM$alpha,
                         rate = estInfDur$gammaM$beta))
    inf_dur_F <- log(rgamma(nsamps, shape = estInfDur$gammaF$alpha,
                         rate = estInfDur$gammaF$beta))

    # NatImmWane.
    nat_imm_dur_params <- gamma_params_mom(20, 15, scale = FALSE)
    nat_imm_dur_M <- log(rgamma(nsamps, shape = nat_imm_dur_params$alpha, rate = nat_imm_dur_params$beta))
    nat_imm_dur_F <- log(rgamma(nsamps, shape = nat_imm_dur_params$alpha, rate = nat_imm_dur_params$beta))

    cbind(betaMM,
          betaMF,
          betaFF,
          inf_dur_M,
          inf_dur_F,
          nat_imm_dur_M,
          nat_imm_dur_F)
}


#' prior density of parms
#'
#' @export
#'
prior <- function(parms){
    bt_parms <- backtransform_parms(parms)

    # beta
    betaMM_like <- dbeta(bt_parms[, 1], 3, 1)
    betaMF_like <- dbeta(bt_parms[, 2], 3, 1)
    betaFF_like <- dbeta(bt_parms[, 3], 3, 1)

    # InfClearRate
    estInfDur <- estimate_inf_duration()

    # take reciprocal of inf duration to get inf clearance rate
    inf_clear_rate_M <- 1/bt_parms[, 4]
    inf_clear_rate_M_like <- dgamma(inf_clear_rate_M,
                                    shape = estInfDur$gammaM$alpha,
                                    rate = estInfDur$gammaM$beta)
    inf_clear_rate_F <- 1/bt_parms[, 5]
    inf_clear_rate_F_like <- dgamma(inf_clear_rate_F,
                                    shape = estInfDur$gammaF$alpha,
                                    rate = estInfDur$gammaF$beta)

    # NatImmWane
    estNatImmDur <- gamma_params_mom(20, 15, scale = FALSE)
    # bt_parms returns rate, but we want duration - inverse
    nat_imm_dur_M <- 1/bt_parms[, 6]
    nat_imm_dur_M_like <- dgamma(nat_imm_dur_M,
                                 shape = estNatImmDur$alpha,
                                 rate = estNatImmDur$beta)
    nat_imm_dur_F <- 1/bt_parms[, 7]
    nat_imm_dur_F_like <- dgamma(nat_imm_dur_F,
                                 shape = estNatImmDur$alpha,
                                 rate = estNatImmDur$beta)

    dens_mat <- cbind(betaMM_like,
                      betaMF_like,
                      betaFF_like,
                      inf_clear_rate_M_like,
                      inf_clear_rate_F_like,
                      nat_imm_dur_M_like,
                      nat_imm_dur_F_like)
    apply(dens_mat, 1, prod)
}

#' @export
run_model_with_btparms <- function(i, btparms, so, pop_dist, contact){
    parms <- define_calib_parms(i, btparms, so, pop_dist, contact)
    # estimate prevalence
    mod_prev <- run_to_steady(parms, vaccination = parms$structural$vacc0)
    calcd_prev <- calc_prevalence(mod_prev[nrow(mod_prev), -1], parms)
    f_mod_prev <- format_prevalence(calcd_prev, parms)
    return(f_mod_prev)
}

#' Takes formatted prev to gender
#' @export
#'
group_prevalence_to_gender <- function(fprev){
    fprev %>%
        group_by(sex) %>%
        summarise(mean_prev = sum(prev * prop) / sum(prop))
}


#' Used to define a likelihood function either for het or so mixing structures
#'
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

#' Back-transform parameters from the
#' sampled form to the model and/or prior form
#'
#' @export
backtransform_parms <- function(parms){
    betaMM <- ilogit(parms[, 1])
    betaMF <- ilogit(parms[, 2])
    betaFF <- ilogit(parms[, 3])
    inf_clear_rate_M <- 1/exp(parms[, 4])
    inf_clear_rate_W <- 1/exp(parms[, 5])
    nat_imm_wane_rate_M <- 1/exp(parms[, 6])
    nat_imm_wane_rate_W <- 1/exp(parms[, 7])
    cbind(betaMM,
          betaMF,
          betaFF,
          inf_clear_rate_M,
          inf_clear_rate_W,
          nat_imm_wane_rate_M,
          nat_imm_wane_rate_W)
}

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




