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
