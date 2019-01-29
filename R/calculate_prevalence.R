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
