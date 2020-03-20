context("define parameters")
library(msid)

data("het_rep")
data("het_props")
data("all_sexid_rep")
data("all_sexid_props")

parameters <- define_parameters(sexids = "het",
                                contact_df = het_rep,
                                prop_df = het_props)


test_that("parameters are organized into categories", {
    p_cats <- names(parameters)
    expect_setequal(p_cats, c("epi", "vacc", "structural", "behav"))
})

test_that("msid parameters", {
    parameters <- define_parameters(sexids = "msid",
                                    contact_df = all_sexid_rep,
                                    prop_df = all_sexid_props)
    p_cats <- names(parameters)
    expect_setequal(p_cats, c("epi", "vacc", "structural", "behav"))
})
