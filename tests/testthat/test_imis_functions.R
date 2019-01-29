context("imis functions")
library(msid)

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
    raw_samps <- sample.prior(1e6)
    bt_samps <- backtransform_parms(raw_samps)
    # beta distributions
    # the parameters are alpha = 3, beta = 1, so the mean should be
    # 3 / (3 + 1) = 4
    expect_equal(colMeans(bt_samps[, c("betaMM", "betaMF", "betaFF")]), rep(3/4, 3),
                 tolerance = 1e-2, check.attributes = FALSE)

    # infection duration
    ## men
    expect_equal(mean(1/bt_samps[, "inf_clear_rate_M"]), 12.2/12,
                 check.attributes = FALSE,
                 tolerance = 1e-2)

})

test_that("prior density runs", {
    nsamp <- 100
    expect_equal(length(prior(sample.prior(nsamp))), nsamp)
})
