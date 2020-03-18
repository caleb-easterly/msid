#' Sample from the prior distributions.
#'
#' Called from IMIS.
#'
#' In this function, the prior samples are transformed to be unbounded, which
#' makes a better search space for IMIS. Then, the parameters are "back-transformed"
#' before being sent to the model function.
#'
#' @export
sample_prior_generator <- function(rep_df, so){
    sample.prior <- function(nsamps) {
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

        # contact
        contact_gams <- define_npr_gammas(rep_df)
        nprs <- data.frame(t(replicate(nsamps,
                                       log(samp_npr_gammas(contact_gams)),
                                       simplify = TRUE)))
        # change names to
        names(nprs) <- paste(rep_df$r_demo, rep_df$rp_sex, sep = "_")

        # select nprs to calibrate
        ids <- names(nprs)
        if (so == "msid") {
            ids_not_2calib <- c("m_gay_low_w", "w_gay_low_m")
            ids2calib <- ids[!(ids %in% ids_not_2calib)]
        }
        if (so == "het") {
            ids_not_2calib <- c("m_het_high_m",
                                "m_het_low_m",
                                "w_het_high_w",
                                "w_het_low_w")
            ids2calib <- ids[!(ids %in% ids_not_2calib)]
        }

        nprs_clean <- nprs[, ids2calib]

        # needs to be a matrix for IMIS
        df <- data.frame("logit_betaMM" = logit_betaMM,
                         "logit_betaMF" = logit_betaMF,
                         "logit_betaFF" = logit_betaFF,
                         "log_inf_dur_M" = log_inf_dur_M,
                         "log_inf_dur_F" = log_inf_dur_F,
                         "log_nat_imm_dur_M" = log_nat_imm_dur_M,
                         "log_nat_imm_dur_F" = log_nat_imm_dur_F,
                         nprs_clean)

        data.matrix(df)
    }
    return(sample.prior)
}

select_nprs_to_calibrate <- function(so) {
    if (so == "msid") {
        # everything but "m_het_low_m" and "w_het_low_w"
        ids <- c("m_bi_high_m",  "m_bi_high_w",
                 "m_bi_low_m",   "m_bi_low_w",
                 "m_gay_high_m", "m_gay_high_w",
                 "m_gay_low_m",  # removed m_gay_low_w
                 "m_het_high_m", "m_het_high_w",
                 "m_het_low_m",  "m_het_low_w",
                 "w_bi_high_m",  "w_bi_high_w",
                 "w_bi_low_m",   "w_bi_low_w",
                 "w_gay_high_m", "w_gay_high_w",
                                 "w_gay_low_w", # removed w_gay_low_m
                 "w_het_high_m", "w_het_high_w",
                 "w_het_low_m",  "w_het_low_w")
        ids
    } else {
        ids <- c("m_het_high_w",
                 "w_het_high_m",
                 "m_het_low_w",
                 "w_het_low_m")
    }
}

#' take parameters (sampled from prior) and
#' replace the 0-nprs that were removed earlier
#'
#' @export
replace_filtered_nprs <- function(filt_nprs, so) {
    if (so == "msid") {
        filt_nprs <- data.frame(filt_nprs)
        filt_nprs[, "m_gay_low_w"] <-
            filt_nprs[, "w_gay_low_m"] <- 0

        # make sure they are in the right order
        ids <- c("m_bi_high_m",  "m_bi_high_w",
                 "m_bi_low_m",   "m_bi_low_w",
                 "m_gay_high_m", "m_gay_high_w",
                 "m_gay_low_m",  "m_gay_low_w",
                 "m_het_high_m", "m_het_high_w",
                 "m_het_low_m",  "m_het_low_w",
                 "w_bi_high_m",  "w_bi_high_w",
                 "w_bi_low_m",   "w_bi_low_w",
                 "w_gay_high_m", "w_gay_high_w",
                 "w_gay_low_m",  "w_gay_low_w",
                 "w_het_high_m", "w_het_high_w",
                 "w_het_low_m",  "w_het_low_w")
        full_nprs <- filt_nprs %>%
            select(ids)
        return(full_nprs)
    }
    if (so == "het") {
        filt_nprs <- data.frame(filt_nprs)
        filt_nprs[, "m_het_high_m"] <-
            filt_nprs[, "m_het_low_m"] <-
            filt_nprs[, "w_het_high_w"] <-
            filt_nprs[, "w_het_low_w"] <- 0

        # make sure they are in the right order
        ids <- c("m_het_high_m", "m_het_high_w",
                 "m_het_low_m",  "m_het_low_w",
                 "w_het_high_m", "w_het_high_w",
                 "w_het_low_m",  "w_het_low_w")
        full_nprs <- filt_nprs %>%
            select(ids)
        return(full_nprs)
    }
}


#' prior density of parms.
#' takes parameters directly from sample.prior()
#'
#' @export
prior_generator <- function(rep_df, so){
    prior <- function(parms) {
        bt_parms <- backtransform_parms(parms, rep_df, so)

        # beta
        betaMM_like <- dbeta(bt_parms[, "betaMM"], 3, 1)
        betaMF_like <- dbeta(bt_parms[, "betaMF"], 3, 1)
        betaFF_like <- dbeta(bt_parms[, "betaFF"], 3, 1)

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

        # contact
        contact_gams <- define_npr_gammas(rep_df)
        npr_names <- paste(rep_df$r_demo, rep_df$rp_sex, sep = "_")
        nprs <- bt_parms[, npr_names]
        npr_like <- d_npr_gammas(nprs, contact_gams)

        dens_mat <- cbind(betaMM_like,
                          betaMF_like,
                          betaFF_like,
                          inf_dur_M_like,
                          inf_dur_F_like,
                          nat_imm_dur_M_like,
                          nat_imm_dur_F_like,
                          npr_like)
        # remove NAs because some of the NPRs may have NA likelihood
        apply(dens_mat, 1, FUN = function(x) prod(x, na.rm = TRUE))
    }
    return(prior)
}



#' Used to define a likelihood function either for het or so mixing structures
#'
#' @param so sex orientation: either "het" or "msid"
#' @param rep_df reported partnerships dataframe
#' @import parallel
#' @export
likelihood_generator <- function(so, rep_df, prop_df){
    likelihood <- function(parms) {
        btparms <- backtransform_parms(parms, rep_df, so)
        nsets <- nrow(btparms)
        fTrue <- female_prevalence_target()
        mTrue <- male_prevalence_target()
        cl <- makeCluster(detectCores())
        clusterExport(cl, c("btparms", "so", "rep_df", "prop_df"), envir = environment())
        like <- parSapply(cl, 1:nsets, function(i) {
        # for (i in 1:nsets) {
            # run model
            f_mod_prev <- run_model_with_btparms(i, btparms, so, rep_df, prop_df)

            # group prevalence to gender
            prev_grpd <- group_prevalence_to_gender(f_mod_prev)
            m_mod_prev <- with(prev_grpd, mean_prev[sex == 'm'])
            w_mod_prev <- with(prev_grpd, mean_prev[sex == 'w'])
            dnorm(x = mTrue[1], mean = m_mod_prev, sd = mTrue[2]) *
                dnorm( x = fTrue[1], mean = w_mod_prev, sd = fTrue[2])
        # }
        })
        stopCluster(cl)
        return(like)
    }
    return(likelihood)
}

#' @export
run_calib_results <- function(btparms, so, rep_df, prop_df){
    nsets <- nrow(btparms)
    cl <- makeCluster(detectCores())
    clusterExport(cl, c("btparms", "so", "rep_df", "prop_df"), envir = environment())
    prev <- parLapply(cl, 1:nsets, function(i) {
        # for (i in 1:nsets) {
        # run model
        f_mod_prev <- run_model_with_btparms(i, btparms, so, rep_df, prop_df)

        # group prevalence to gender
        prev_grpd <- group_prevalence_to_gender(f_mod_prev)
        prev_grpd$run <- i
        prev_grpd
    })
    stopCluster(cl)
    prev
}

#' Run the model with the ith set of bback-transformed parameters
#'
#' Called from the likelihood function.
#'
#' @export
run_model_with_btparms <- function(i, btparms, so, rep_df, prop_df){
    # define one set of parameters
    parms <- define_calib_parms(i, btparms, so, rep_df, prop_df)
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
define_calib_parms <- function(i, btparms, so, rep_df, prop_df){
    # access npr columns and ith row
    npr_names <- paste(rep_df$r_demo, rep_df$rp_sex, sep = "_")
    nprs <- btparms[i, npr_names]
    rep_df$pt_p <- nprs
    contact <- het_rep_to_bal(rep_df, prop_df)
    parms <- define_parameters(betaMM = btparms[i, "betaMM"],
                               betaMW = btparms[i, "betaMF"],
                               betaWW = btparms[i, "betaFF"],
                               inf_clear_rate_M = btparms[i, "inf_clear_rate_M"],
                               inf_clear_rate_W = btparms[i, "inf_clear_rate_F"],
                               nat_imm_wane_rate_M = btparms[i, "nat_imm_wane_rate_M"],
                               nat_imm_wane_rate_W = btparms[i, "nat_imm_wane_rate_F"],
                               sexids = so,
                               contact_df = rep_df,
                               prop_df = prop_df)
    return(parms)
}


#' Back-transform parameters from the
#' sampled form to the model and/or prior form
#'
#' @export
backtransform_parms <- function(parms, rep_df, so){
    betaMM <- ilogit(parms[, "logit_betaMM"])
    betaMF <- ilogit(parms[, "logit_betaMF"])
    betaFF <- ilogit(parms[, "logit_betaFF"])
    # transform the durations to rates by
    # exponentiating and taking the inverse
    inf_clear_rate_M <- 1/exp(parms[, "log_inf_dur_M"])
    inf_clear_rate_F <- 1/exp(parms[, "log_inf_dur_F"])
    nat_imm_wane_rate_M <- 1/exp(parms[, "log_nat_imm_dur_M"])
    nat_imm_wane_rate_F <- 1/exp(parms[, "log_nat_imm_dur_F"])

    # nprs
    npr_names <- select_nprs_to_calibrate(so)
    nprs <- exp(parms[, npr_names])
    full_nprs <- replace_filtered_nprs(nprs, so)

    data.matrix(
        data.frame(
            "betaMM" = betaMM,
            "betaMF" = betaMF,
            "betaFF" = betaFF,
            "inf_clear_rate_M" = inf_clear_rate_M,
            "inf_clear_rate_F" = inf_clear_rate_F,
            "nat_imm_wane_rate_M" = nat_imm_wane_rate_M,
            "nat_imm_wane_rate_F" =nat_imm_wane_rate_F,
            full_nprs
        )
    )
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




