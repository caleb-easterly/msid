context("define parameters")
library(msid)

data(contact_het_base)

parameters <- define_parameters(sexids = "het",
                                contact_df = contact_het_base)


test_that("parameters are organized into categories", {
    p_cats <- names(parameters)
    expect_setequal(p_cats, c("epi", "vacc", "structural", "behav"))
})

test_that("msid parameters", {
    data(contact_msid_base)
    parameters <- define_parameters(sexids = "msid",
                                    contact_df = contact_msid_base)
    p_cats <- names(parameters)
    expect_setequal(p_cats, c("epi", "vacc", "structural", "behav"))
})
