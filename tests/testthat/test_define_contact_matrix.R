context('contact matrix')
library(msid)

het_demo_ind <- define_index_df(load_constants("het"))$demo_indices
data("het_rep")
data("het_props")

bal <- het_rep_to_bal(het_rep, het_props)
test_that("contact matrix has choosers on rows", {
    contact <- define_contact_matrix(bal, het_demo_ind)
    m_high_rep_with_w_high <- bal[bal$r_demo == "m_het_high" &
                                                   bal$rp_demo == "w_het_high", ]$corrected_r
    expect_equal(contact[1, 3], m_high_rep_with_w_high,
                 check.attributes = FALSE)
})

test_that("het contact matrix has 0s within sexes", {
    contact <- define_contact_matrix(bal, het_demo_ind)
    expect_true(all(contact[1:2, 1:2] == 0))
    expect_true(all(contact[3:4, 3:4] == 0))
})

### all_sexid
data("all_sexid_props")
data("all_sexid_rep")
all_sexid_bal <- het_rep_to_bal(all_sexid_rep, all_sexid_props)
all_sexid_demo_ind <- define_index_df(load_constants("msid"))$demo_indices
test_that("all_sexid contact matrix has the correct dimensions", {
    contact <- define_contact_matrix(all_sexid_bal, all_sexid_demo_ind)
    expect_equal(dim(contact), c(12, 12))
})

data("sexid_tog_props")
data("sexid_tog_rep")
all_bal <- het_rep_to_bal(sexid_tog_rep, sexid_tog_props)
test_that("avg matrix has no zeroes", {
    avg_demo_ind <- define_index_df(load_constants("msid_avg"))$demo_indices
    contact <- define_contact_matrix(all_bal, avg_demo_ind)
    expect_true(all(contact != 0))
    expect_equal(dim(contact), c(4, 4))
})

