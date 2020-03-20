context("imis functions")
library(msid)

data("all_sexid_rep")
data("all_sexid_props")
sample.prior <- sample_prior_generator(all_sexid_rep, "msid")
prior <- prior_generator(all_sexid_rep, "msid")
likelihood <- likelihood_generator('msid', all_sexid_rep, all_sexid_props)

test_that("estimate_inf_duration goes to mean duration", {
    gparms <- estimate_inf_duration()
    mean_dur_M <- 12.2/12
    set.seed(101)
    samp_dur_M <- rgamma(1e4, gparms$gammaM$alpha, rate = gparms$gammaM$beta)
    expect_equal(mean_dur_M, mean(samp_dur_M), tolerance = 1e-3)
})

test_that("sample.prior() runs", {
    expect_silent(sample.prior(1000))
    }
)

test_that("the averages from sample.prior() are correct", {
    set.seed(1010)
    raw_samps <- sample.prior(1e4)
    bt_samps <- backtransform_parms(raw_samps, all_sexid_rep, "msid")
    # beta distributions
    # the parameters are alpha = 3, beta = 1, so the mean should be
    # 3 / (3 + 1) = 4
    expect_equal(colMeans(bt_samps[, c("betaMM", "betaMF", "betaFF")]), rep(3/4, 3),
                 tolerance = 1e-1, check.attributes = FALSE)

    # infection duration
    ## men
    expect_equal(mean(1/bt_samps[, "inf_clear_rate_M"]), 12.2/12,
                 check.attributes = FALSE,
                 tolerance = 1e-1)

})

test_that("prior density runs", {
    nsamp <- 100
    expect_equal(length(prior(sample.prior(nsamp))), nsamp)
})

test_that("likelihood generator works", {
    # is function
    expect_is(likelihood, "function")

    # calculates likelihood of parms
    nsamps <- 2
    likes <- likelihood(sample.prior(nsamps))
    expect_equal(length(likes), nsamps)
})
