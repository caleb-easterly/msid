context('run comparison')
library(msid)

data("contact_msid_base")

test_that('run comparison runs', {
    expect_output(run_comparison(so = 'msid',
                                contacts = contact_msid_base,
                                n_samp = 5,
                                n_resamp = 2,
                                n_run_samp = 2),
                  regexp = "50 likelihoods are evaluated")
    }
)
