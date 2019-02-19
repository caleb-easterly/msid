context("model")

test_that("model runs, results > 0", {
    data("contact_msid_base")
    msid_parms <- define_parameters(contact_df = contact_msid_base, sexids = 'msid')
    sted <- run_to_steady(msid_parms, rep(0, 12))
    expect_true(all(sted > -.Machine$double.eps))
})
