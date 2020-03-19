context("foi")

test_that("force of infection is calculated properly", {
    suff_contact <- matrix(c(5, 1, 4, 2), nrow = 2, byrow = TRUE)
    prev <- c(1, 0.5)
    exp_foi <- c(5.5, 5)
    calc_foi <- calculate_force_of_inf(suff_contact, prev)
    expect_equal(exp_foi, calc_foi)

    suff_contact2 <- matrix(1:9, nrow = 3, byrow = TRUE)
    prev2 <- c(0.1, 0.5, 1)
    exp_foi2 <- c(0.1+1+3, 0.4+2.5+6, 0.7+4+9)
    calc_foi2 <- calculate_force_of_inf(suff_contact2, prev2)
    expect_equal(exp_foi2, calc_foi2)

    # check that this automatically throws an error
    # if ncol(suff_contact) != length(prev)
    expect_error(calculate_force_of_inf(suff_contact, prev2))
})
