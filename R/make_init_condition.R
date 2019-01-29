make_init_condition <- function(model_indices, pdist_df){
    # assume initial prevalence of 7% (also is the target)
    prev <- data.frame(epi = c("X", "Y", "Z", "V", "W", "I"),
                       perc = c(0.6, 0.07, 0.33, 0, 0, 0),
                       stringsAsFactors = FALSE)
    # now, calculate group-specific prevalence by
    # multiplying overall prevalence by proportion in that demo group
    init_df <- prev %>%
        left_join(
            left_join(model_indices, pdist_df, by = c("sexid", "sexact", "sex")),
            by = "epi"
        ) %>%
        # make sure vector is in the right order
        arrange(model_uid) %>%
        mutate(init = perc * prop)
    # select only the init column, return as a vector
    return(init_df$init)
}
