context('contact matrix')
library(msid)

data("contact_het_base")

het_demo_ind <- define_index_df(load_constants("het"))$demo_indices

test_that("contact matrix has choosers on rows", {
    contact <- define_contact_matrix(contact_het_base, het_demo_ind)
    m_high_rep_with_w_high <- contact_het_base[contact_het_base$r_demo == "m_het_high" &
                                                   contact_het_base$rp_demo == "w_het_high", ]$corrected_r
    expect_equal(contact[1, 3], m_high_rep_with_w_high,
                 check.attributes = FALSE)
})

test_that("het contact matrix has 0s within sexes", {
    contact <- define_contact_matrix(contact_het_base, het_demo_ind)
    expect_true(all(contact[1:2, 1:2] == 0))
    expect_true(all(contact[3:4, 3:4] == 0))
})

data("contact_avg_base")
test_that("avg matrix has no zeroes", {
    avg_demo_ind <- define_index_df(load_constants("msid_avg"))$demo_indices
    contact <- define_contact_matrix(contact_avg_base, avg_demo_ind)
    expect_true(all(contact != 0))
    expect_equal(dim(contact), c(4, 4))
})
