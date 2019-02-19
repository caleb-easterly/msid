#'
#' todo: test
#' @import dplyr
#' @export
define_parameters <- function(inf_clear_rate_M = 1/2,
                              inf_clear_rate_W = 1/2,
                              nat_imm_wane_rate_M = 1/20,
                              nat_imm_wane_rate_W= 1/20,
                              efficacy = 0.9,
                              entry_exit_rate = 1/14,
                              sexids = c('het', 'msid'),
                              contact_df = NULL,
                              betaMM = 0.8,
                              betaMW = 0.8,
                              betaWW = 0.2){

    # vaccine parameters
    vacc <- list(efficacy = efficacy)

    # structural parameters
    id <- match.arg(sexids)
    constants <- load_constants(id)
    index_dfs <- define_index_df(constants)
    model_indices <- index_dfs$model_indices
    parm_indices <- index_dfs$parm_indices
    n_equations <- nrow(model_indices)
    n_demo_grps <- nrow(parm_indices)
    vacc0 <- rep(0, n_demo_grps)

    # incidence compartments
    # removed in root fun to calculate steady state.
    inc_compartments <- model_indices[model_indices$epi == "I", ]

    # define population dist
    # join with parm_indices
    pdist_df <- left_join(parm_indices, contact_df,
                           by = c("sex" = "r_sex",
                                "sexid" = "r_sexid",
                                "sexact" = "r_sexact")) %>%
        arrange(parm_uid) %>%
        select(sex, sexid, sexact, prop) %>%
        unique()
    pdist_vector <- pdist_df$prop

    # define (arbitrary) initial condition
    # see make_init_condition for more details
    init_cond <- make_init_condition(model_indices, pdist_df)

    structural <- list(entry_exit_rate = entry_exit_rate,
                       population_dist = pdist_vector,
                       pdist_df = pdist_df,
                       constants = constants,
                       model_indices = model_indices,
                       parm_indices = parm_indices,
                       n_demo_grps = n_demo_grps,
                       init_cond = init_cond,
                       n_equations = n_equations,
                       vacc0 = vacc0,
                       inc_compartments = inc_compartments)

    # epi parameters
    # define inf_clear_rate vector from male and female values
    inf_clear_rate_vec <- make_parm_gender_vecs(inf_clear_rate_M, inf_clear_rate_W, parm_indices)

    # same for nat_imm_wane_rate
    nat_imm_wane_rate_vec <- make_parm_gender_vecs(nat_imm_wane_rate_M, nat_imm_wane_rate_W, parm_indices)
    epi <- list(inf_clear_rate = inf_clear_rate_vec,
                nat_imm_wane_rate = nat_imm_wane_rate_vec)

    # behavioral parameters
    # calculate sufficient contacts

    # contact matrix
    contact_matrix <- define_contact_matrix(contact_df, parm_indices)

    # transmission probability matrix
    trans_prob_matrix <- define_transmission_prob_matrix(betaMM, betaMW, betaWW, parm_indices)

    # the sufficient contact matrix is the element-wise product of these two matrices
    suff_contact_matrix <- contact_matrix * trans_prob_matrix

    behav <- list(contact_matrix = contact_matrix,
                  trans_prob_matrix = trans_prob_matrix,
                  suff_contact_matrix = suff_contact_matrix)

    ### combine all lists
    parms <- list(epi = epi,
                  vacc = vacc,
                  structural = structural,
                  behav = behav)

    return(parms)
}


#' for parameters with a male value and a female value
#' takes the value and turns into a vector of male and female values
#'
#' @export
make_parm_gender_vecs <- function(mval, wval, parm_indices){
    parm_df <- data.frame(sex = c('m', 'w'),
                          parm = c(mval, wval),
                          stringsAsFactors = FALSE)
    joined <- left_join(parm_indices, parm_df, by = "sex") %>%
        arrange(parm_uid)
    return(joined$parm)
}
