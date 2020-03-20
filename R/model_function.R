#' Contains the differential equations that describe the
#' HPV transmission model
#'
#' @param y the current estimate of the variables in the ODE system
#' @param t the current time point in the integration
#' @param parms a list of parameters from \code{define_parameters()}
#' @param vaccination the vaccination strategy, given as a vector of coverage
#' values for each population group
#'
#' @export
model_function <- function(t, y, parms, vaccination){
    # epi parameters
    epi <- parms$epi
    # these are both vectors, with length equal to n_demo_grps
    inf_clear_rate <- epi$inf_clear_rate
    nat_imm_wane_rate <- epi$nat_imm_wane_rate

    # vaccine parameters
    efficacy <- parms$vacc$efficacy

    # behavioral parameters
    suff_contact_matrix <- parms$behav$suff_contact_matrix

    # structural parameters
    st <- parms$structural
    entry_exit_rate <- st$entry_exit_rate
    population_dist <- st$population_dist
    model_indices <- st$model_indices
    demo_indices <- st$demo_indices
    n_demo_grps <- st$n_demo_grps
    n_equations <- st$n_equations

    # calculate prevalence
    prev <- calc_prevalence(y, parms)

    # calculate force_of_inf - product of suff_contact_matrix and prev
    force_of_inf <- calculate_force_of_inf(suff_contact_matrix, prev)

    #### calculate derivatives ####
    # make derivative vector
    derivs <- rep(0, n_equations)

    # rename x for clarity
    popvec <- y

    # loop through equations
    for (i in 1:n_demo_grps){
        # define model vector indices for this pass through loop
        this_group_indices <- with(model_indices, model_uid[demo_uid == i])
        this_x <- this_group_indices[1]
        this_y <- this_group_indices[2]
        this_z <- this_group_indices[3]
        this_v <- this_group_indices[4]
        this_w <- this_group_indices[5]
        this_i <- this_group_indices[6]

        # reduced force of infection for those vaccinated
        foi_with_vacc <- force_of_inf[i]*(1 - efficacy)

        # calculate derivatives
        derivs[this_x] <- entry_exit_rate*population_dist[i]*(1 - vaccination[i]) +
            nat_imm_wane_rate[i]*popvec[this_z] -
            (force_of_inf[i] + entry_exit_rate)*popvec[this_x]

        derivs[this_y] <- force_of_inf[i]*popvec[this_x] -
            (inf_clear_rate[i] + entry_exit_rate)*popvec[this_y]

        derivs[this_z] <- inf_clear_rate[i]*popvec[this_y] -
            (nat_imm_wane_rate[i] + entry_exit_rate)*popvec[this_z]

        derivs[this_v] <- entry_exit_rate*population_dist[i]*vaccination[i] +
            inf_clear_rate[i]*popvec[this_w] -
            (foi_with_vacc + entry_exit_rate)*popvec[this_v]

        derivs[this_w] <- foi_with_vacc*popvec[this_v] -
            (inf_clear_rate[i] + entry_exit_rate)*popvec[this_w]

        derivs[this_i] <- force_of_inf[i]*popvec[this_x] +
            foi_with_vacc*popvec[this_v]
    }
    return(list(derivs))
}

