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
