# todo: write test
#' @export
calc_prevalence <- function(x, parms){
    # only need structural parameters
    st <- parms$structural
    population_dist <- st$population_dist
    n_demo_grps <- st$n_demo_grps

    # indices
    model_indices <- st$model_indices
    parm_indices <- st$parm_indices

    # make prevalence vector
    prev <- rep(0, n_demo_grps)
    for (i in 1:n_demo_grps){
        # get the indices for the infected people within each group
        model_ind <- with(model_indices,
                          model_uid[parm_uid == i & (epi == 'Y' | epi == 'W')])
        prev[i] <- sum(x[model_ind], na.rm = TRUE)/population_dist[i]
    }

    # replace NAs with zeros so they don't propagate
    prev[is.na(prev)] <- 0

    return(prev)
}

#' @export
format_prevalence <- function(prev, parms){
    # make prevalence df with parm_uids
    prev_df <- data.frame(parm_uid = 1:parms$structural$n_demo_grps,
                          prev)

    # join with parms
    prev_pretty <- left_join(prev_df, parms$structural$parm_indices, by = "parm_uid")

    # join with dist
    prev_pretty_d <- left_join(prev_pretty, parms$structural$pdist_df, by = c("sexact", "sexid", "sex")) %>%
        select(sex, sexid, sexact, prev, prop, parm_uid)
    return(prev_pretty_d)
}

#' Takes formatted prevalence to gender-specific prevalence
#'
#' @export
group_prevalence_to_gender <- function(fprev){
    fprev %>%
        group_by(sex) %>%
        summarise(mean_prev = sum(prev * prop) / sum(prop))
}

#### calibration targets ####

#' @export
female_prevalence_target <- function() {
    # from ARTISTIC trial
    # doi:10.1038/sj.bjc.6603210
    # target age groups are 20-24, 25-34
    # model assumes 20 - 34
    age <- c(20, 25)
    ninf <- sum(c(315, 320))
    ntot <- sum(c(2575, 6271))
    prev <- ninf/ntot
    stderr <- sqrt((prev * (1 - prev))/ntot)
    c(prev, stderr)
}

#' @export
male_prevalence_target <- function(){
    # doi:  10.1158/1055-9965.EPI-08-0151
    # age groups that we use here are 20 - 24, 25 - 29, 30 - 34
    age <- c(20, 25, 30)
    n_in_age_group <- c(155, 43, 30)
    ntot <- sum(n_in_age_group)
    n_any <- c(95, 30, 22)
    ratio_16_to_any <- 6.7 / 61.3
    prevAny <- sum(n_any) / sum(n_in_age_group)
    prev16 <- prevAny * ratio_16_to_any
    stderr <- sqrt((prev16 * (1 - prev16))/ntot)
    c(prev16, stderr)
}

#' @export
calc_prev_by_sex <- function(all_prevs) {
    # calculate the prevalence for each sex
    # need to weight by proportion in each group
    cprev_bsex <- all_prevs %>%
        bind_rows() %>%
        group_by(run, sex) %>%
        summarise(prev = sum(prev * prop)/sum(prop),
                  sdprev = sd((prev * prop)/sum(prop)))

    ftar <- female_prevalence_target()
    mtar <- male_prevalence_target()
    tars <- data.frame('sex' = c('m', 'w'),
                       'tar_mean' = c(mtar[1], ftar[1]),
                       'tar_sd' = c(mtar[2], ftar[2]),
                       stringsAsFactors = FALSE)

    cprev_wtar <- inner_join(cprev_bsex, tars, by = "sex")
    return(cprev_wtar)
}
