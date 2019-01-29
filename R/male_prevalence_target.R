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
