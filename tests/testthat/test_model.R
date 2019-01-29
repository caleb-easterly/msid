context("model")

test_that("model runs, results > 0", {
    msid_contacts <- read_contact_matrix('../../../fullSO_SMDM/data/natsal_msid.tab')
    msid_props <- read_population_dist('../../../fullSO_SMDM/data/natsal_msid_props.tab')
    msid_parms <- define_parameters(contact_df = msid_contacts, sexids = 'msid',
                                    population_dist = msid_props)
    sted <- run_to_steady(msid_parms, rep(0, 12))
    expect_true(all(sted > -.Machine$double.eps))
})
