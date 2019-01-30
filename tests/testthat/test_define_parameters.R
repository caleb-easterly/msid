context("define parameters")
library(msid)

data("het_contact")
data("het_props")

parameters <- define_parameters(sexids = "het",
                                population_dist = het_props, contact_df = het_contact)


test_that("parameters are organized into categories", {
    p_cats <- names(parameters)
    expect_setequal(p_cats, c("epi", "vacc", "structural", "behav"))
})
