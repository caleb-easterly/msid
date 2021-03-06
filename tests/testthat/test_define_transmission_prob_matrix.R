context("transmission probability matrix")
library(msid)

data("all_sexid_rep")
data("all_sexid_props")
msid_parms <- define_parameters(contact_df = all_sexid_rep,
                                prop_df = all_sexid_props,
                                sexids = 'msid')
demo_indices <- msid_parms$structural$demo_indices

test_that("transmission prob. mat", {
    mm <- 0.5
    mw <- 0.7
    ww <- 0.9
    mat <- define_transmission_prob_matrix(mm, mw, ww, demo_indices)
    mind <- 1:6
    wind <- 7:12
    # men with men
    expect_true(all(mat[mind, mind] == mm))

    # women with women
    expect_true(all(mat[wind, wind] == ww))

    # men with women, women with men
    expect_true(all(mat[mind, wind] == mw))
    expect_true(all(mat[wind, mind] == mw))
})
