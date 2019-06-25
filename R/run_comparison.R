#' @export
define_vaccination <- function(parms, m_vacc, w_vacc){
    make_parm_gender_vecs(mval = m_vacc,
                          wval = w_vacc,
                          parms$structural$demo_indices)
}

#' function to calibrate
#'
#' @export
calibrate <- function(so, rep_df, n_samp, n_resamp) {
    ### calibrate
    likelihood <- likelihood_generator(so = so, rep_df)
    prior <- prior_generator(rep_df)
    sample.prior <- sample_prior_generator(rep_df)

    environment(IMIS) <- environment() # this helps IMIS() run...for some reason
    calib <- IMIS(n_samp, n_resamp)

    calib_parms <- backtransform_parms(calib$resample, rep_df)
    return(list("parms" = calib_parms, "imis" = calib))
}


#' Run the comparison for SMDM 2018
#' 50 female vaccination versus 50 male and female vaccination
#'
#' @param so either 'het' or 'msid'
#' @param contacts path to the contact df
#' @param props path to the proportion df
#' @param n_samp number of samples at each step in IMIS (\code{B} in \code{IMIS})
#' @param n_resamp desired number of posterior samples (\code{B_k} in \code{IMIS()})
#' @param n_run_samp samples from \code{<imis_results>$resample}
#'
#' @importFrom IMIS IMIS
#' @importFrom mvtnorm rmvnorm dmvnorm
#' @export
run_comparison <- function(so, contacts, n_samp, n_resamp, n_run_samp){

    reds <- prevs <- vector(mode = "list", length = n_resamp)

    # todo: parallelize this part
    comps <- vector(mode = "list", length = n_run_samp)

    for (i in 1:n_run_samp){
        j <- sample(1:n_resamp, 1) # choose a random parameter set

        ## calculate prevalence (to compare to targets)
        # j selects the set of parameters
        prev <- run_model_with_btparms(i = j, btparms = calib_parms, so = so,
                                       contact = contacts)

        # run is used in group_by() to get variation
        # used i because it's guaranteed to be unique
        prev$run <- i

        ## number of infections ##
        # use j to define the parms
        p <- define_calib_parms(i = j, btparms = calib_parms,
                                so = so, contact = contacts )

        vacc_level <- 0.5
        comp_endtime <- 20

        # 50% female vaccination
        f50 <- define_vaccination(p, m_vacc = 0, w_vacc = vacc_level)
        f50_inf <- run_model_infections(p, f50, endtime = comp_endtime)

        # cases with 50% M&F vaccination
        mf50 <- define_vaccination(p, m_vacc = vacc_level, w_vacc = vacc_level)
        mf50_inf <- run_model_infections(p, mf50, endtime = comp_endtime)

        # summarise cases by sex
        # multiply by 1000 so we get cases per 1000 people
        denom <- 1000
        f50_inf_sex <- f50_inf %>%
            group_by(sex) %>%
            summarise(numc = sum(inf) * denom)

        mf50_inf_sex <- mf50_inf %>%
            group_by(sex) %>%
            summarise(numc = sum(inf) * denom)

        # calculate reduction
        red <- left_join(f50_inf_sex, mf50_inf_sex, by = "sex",
                         suffix = c(".f", ".mf")) %>%
            mutate(abs_red = numc.f - numc.mf,
                   rel_red = (numc.f - numc.mf) / numc.f * 100)
        red$run <- i
        comps[[i]] <- list(prev, red)
    }

    prevs <- lapply(1:n_run_samp, function(i) comps[[i]][[1]])
    all_prevs <- prevs %>%
        bind_rows()

    reds <- lapply(1:n_run_samp, function(i) comps[[i]][[2]])
    all_reds <- reds %>%
        bind_rows()

    return(list('reds' = all_reds, 'prevs' = all_prevs, 'imis' = calib, 'parms' = calib_parms))
}


