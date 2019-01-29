context('run comparison')
library(msid)

test_that('run comparison runs', {
    expect_output(run_comparison(so = 'msid',
                                contacts = '../../../fullSO_SMDM/data/natsal_msid.tab',
                                props = '../../../fullSO_SMDM/data/natsal_msid_props.tab',
                                n_samp = 5,
                                n_resamp = 2,
                                n_run_samp = 2),
                  regexp = "50 likelihoods are evaluated")
    }
)
