#' @export
calc_part_g <- function(df) {
    df %>%
        mutate(part_g = pt_p * prop)
}

#' This calculates the proportion of partnerships
#' "offered to" sex A that come from people with
#' sex r_sex, sex activity r_sexact, and sexid r_sexid
#' @export
make_offered_df <- function(df) {
    df %>%
        group_by(r_sex, rp_sex) %>%
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
        mutate(d_pt_p = pt_p * prop_of_avail,
               r_demo = paste(r_sex, r_sexid, r_sexact, sep="_"),
               rp_demo = paste(rp_sex, rp_sexid, rp_sexact, sep="_")) %>%
        select(r_demo, r_sex, r_sexact, r_sexid,
               rp_demo, rp_sex, rp_sexid, rp_sexact, d_pt_p, prop)
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

#' Balance the number of partnerships between groups,
#' The balanced partnership rate is called "corrected_r"
#' @export
balance_het <- function(df, theta = 0.5) {
    df %>%
        mutate(np_r = d_pt_p.r * prop.r,
               np_rp = d_pt_p.rp * prop.rp) %>%
        mutate(imbalance = np_r / np_rp,
               corrected_r = d_pt_p.r / imbalance^(1 - theta),
               corrected_rp = d_pt_p.rp * imbalance^theta,
               cnr = corrected_r * prop.r,
               cnrp = corrected_rp * prop.rp) %>%
        select(r_demo,
               r_sex = r_sex.r, r_sexid = r_sexid.r, r_sexact = r_sexact.r,
               rp_sex = rp_sex.r, rp_sexid = rp_sexid.r, rp_sexact = rp_sexact.r,
               rp_demo, prop.r,
               d_pt_p.r, corrected_r,
               cnr, cnrp)
}

#' @export
clean_bal_het_df <- function(df) {
    df <- df %>%
        select(r_sex, r_sexid, r_sexact, r_demo,
               rp_sex, rp_sexid, rp_sexact, rp_demo,
               prop = prop.r, corrected_r)
    df$corrected_r[is.nan(df$corrected_r)] <- 0
    return(df)
}

#' @export
define_npr_gammas <- function(rep_df) {
    ngrp <- nrow(rep_df)
    gams <- lapply(1:ngrp, function(i) gamma_params_mom(rep_df$pt_p[i], rep_df$sdpart[i]))
    gams
}

#' @export
samp_npr_gammas <- function(gams, rep_df) {
    ngrp <- length(gams)
    pt_p <- sapply(gams, function(x) rgamma(1, shape = x$alpha, rate = x$beta))
    return(pt_p)
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
het_rep_to_bal <- function(het_rep, het_props) {
    joined <- left_join(het_rep, het_props,
                        by = c("r_sex", "r_sexid", "r_sexact"))
    w_part_g <- calc_part_g(joined)
    offered <- make_offered_df(w_part_g)
    dist <- distribute_partnerships(w_part_g, offered)
    bidi <- make_bidirectional(dist)
    balanced <- balance_het(bidi)
    clean <- clean_bal_het_df(balanced)
    return(clean)
}
