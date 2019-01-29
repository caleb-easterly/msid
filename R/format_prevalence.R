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
