
# the q stands for "query"
# denom stands for denominator
get_proportion <- function(natsal,
                           q_sex = c("m", "w"),
                           q_sexid = c("het", "gay", "bi"),
                           q_sexact = c("high", "low"),
                           denom_sex = c("m", "w"),
                           denom_sexid = c("het", "gay", "bi"),
                           denom_sexact = c("high", "low")){
    denom <- sum(natsal$total_wt[natsal$r_sex %in% denom_sex &
                                     natsal$r_sexid %in% denom_sexid &
                                     natsal$r_sexact %in% denom_sexact])
    qpop <- sum(natsal[natsal$r_sex %in% q_sex &
                           natsal$r_sexid %in% q_sexid &
                           natsal$r_sexact %in% q_sexact, "total_wt"])
    qpop / denom
}

weighted.sd <- function(x, w){
    weighted_mean <- weighted.mean(x, w)
    sum_wts <- sum(w)
    wt_var <- t(w) %*% (x - weighted_mean)^2 / (sum_wts - 1)
    return(sqrt(wt_var))
}

#' @import dplyr
#' @export
calc_reported_het <- function(df) {
    df %>%
        filter(r_sexid == "het") %>%
        group_by(r_sex, r_sexact, r_sexid) %>%
        summarise(part_p = weighted.mean(hetnonew, w = total_wt),
                  sdpart = weighted.sd(hetnonew, w = total_wt)) %>%
        mutate(rp_sex = ifelse(r_sex == "m", 'w', 'm')) %>%
        rowwise() %>%
        mutate(prop = get_proportion(all_sex,
                              q_sex = r_sex,
                              q_sexact = r_sexact,
                              q_sexid = r_sexid,
                              denom_sexid = "het"),
               r_demo = paste(r_sex, r_sexid, r_sexact, sep="_")) %>%
        ungroup()
}

#' @export
calc_part_g <- function(df) {
    df %>%
        mutate(part_g = part_p * prop)
}

#' @export
make_offered_df <- function(df, sid = "het") {
    df %>%
        group_by(r_sex) %>%
        mutate(prop_of_avail = part_g / sum(part_g)) %>%
        select(sex1 = r_sex,
               sexact1 = r_sexact,
               sexid1 = r_sexid,
               sex2 = rp_sex,
               prop_of_avail) %>%
        ungroup()
}

#' @export
distribute_partnerships <- function(report, dist) {
    left_join(report, dist,
              by = c("rp_sex" = "sex1", "r_sex" = "sex2")) %>%
        rename(rp_sexact = sexact1,
               rp_sexid = sexid1) %>%
        mutate(d_part_p = part_p * prop_of_avail,
               r_demo = paste(r_sex, r_sexid, r_sexact, sep="_"),
               rp_demo = paste(rp_sex, rp_sexid, rp_sexact, sep="_")) %>%
        select(r_demo, r_sex, r_sexact, r_sexid,
               rp_demo, rp_sex, rp_sexid, rp_sexact, d_part_p, prop)
}

#' @export
make_bidirectional <- function(df) {
    df %>%
        # do a self-join to calculate partners from perspective of rp
        inner_join(df,
                   by = c("r_demo" = "rp_demo",
                          "rp_demo" = "r_demo"),
                   suffix = c('.r', '.rp'))
}

#' @export
balance_het <- function(df, theta = 0.5) {
    df %>%
        mutate(np_r = d_part_p.r * prop.r,
               np_rp = d_part_p.rp * prop.rp) %>%
        mutate(imbalance = np_r / np_rp,
               corrected_r = d_part_p.r / imbalance^(1 - theta),
               corrected_rp = d_part_p.rp * imbalance^theta,
               cnr = corrected_r * prop.r,
               cnrp = corrected_rp * prop.rp) %>%
        select(r_demo,
               r_sex = r_sex.r, r_sexid = r_sexid.r, r_sexact = r_sexact.r,
               rp_sex = rp_sex.r, rp_sexid = rp_sexid.r, rp_sexact = rp_sexact.r,
               rp_demo, prop.r,
               d_part_p.r, corrected_r,
               cnr, cnrp)
}

#' @export
clean_bal_het_df <- function(df) {
    df <- df %>%
        select(r_sex, r_sexid, r_sexact, r_demo,
               rp_sex, rp_sexid, rp_sexact, rp_demo,
               prop = prop.r, corrected_r)
    na_demos <- mutate(df,
                       r_demo = r_demo,
                       rp_demo = rev(rp_demo),
                       rp_sex = rev(rp_sex),
                       rp_sexid = rev(rp_sexid),
                       rp_sexact = rev(rp_sexact),
                       corrected_r = 0)
    df <- rbind(df, na_demos)
    return(df)
}

#' @export
define_npr_gammas <- function(rep_df) {
    ngrp <- nrow(rep_df)
    gams <- lapply(1:ngrp, function(i) gamma_params_mom(rep_df$part_p[i], rep_df$sdpart[i]))
    gams
}

#' @export
samp_npr_gammas <- function(gams, rep_df) {
    ngrp <- length(gams)
    part_p <- sapply(gams, function(x) rgamma(1, shape = x$alpha, rate = x$beta))
    return(part_p)
}

#' @export
d_npr_gammas <- function(samp, npr_gammas) {
    nsamps <- nrow(samp)
    ngrps <- length(npr_gammas)
    like <- matrix(0, nrow = nsamps, ncol = ngrps)
    for (i in 1:nsamps) {
        ith_samp <- samp[i, ]
        for (j in 1:ngrps){
            like[i, j] <- dgamma(ith_samp[j],
                                 shape = npr_gammas[[j]]$alpha,
                                 rate = npr_gammas[[j]]$beta)
        }
    }
    return(like)
}

#' @export
het_rep_to_bal <- function(het_rep) {
    w_part_g <- calc_part_g(het_rep)
    offered <- make_offered_df(w_part_g)
    dist <- distribute_partnerships(w_part_g, offered)
    bidi <- make_bidirectional(dist)
    balanced <- balance_het(bidi)
    clean <- clean_bal_het_df(balanced)
    return(clean)
}
