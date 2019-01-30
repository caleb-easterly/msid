context('contact matrix')
library(msid)

data("het_contact")
data("het_props")

parm_indices <- define_index_df(load_constants("het"))$parm_indices

test_that("contact matrix has choosers on rows", {
    contact <- define_contact_matrix(het_contact, parm_indices)
    m_high_rep_with_w_high <- het_contact[het_contact$r_demo == "m_high" &
                                              het_contact$rp_demo == "w_high", "corrected_r"]
    expect_equal(contact[1, 3], m_high_rep_with_w_high,
                 check.attributes = FALSE)
})
