context("indexing")
library(msid)

# constants
test_that("constants", {
    het_const <- load_constants("het")
    expect_equal(het_const$sexid, "het")

    msid_const <- load_constants("msid")
    expect_setequal(msid_const$sexid, c("het", "bi", "gay"))

    avg_const <- load_constants("msid_avg")
    expect_equal(avg_const$sexid, "all")
})

# index dataframes
test_that("define index df", {
    # het
    het_indices <- define_index_df(load_constants("het"))
    expect_equal(names(het_indices), c("model_indices", "demo_indices"))
    expect_equal(het_indices$demo_indices$demo_uid, 1:4)

    # msid
    msid_indices <- define_index_df(load_constants("msid"))
    expect_equal(names(msid_indices), c("model_indices", "demo_indices"))
    expect_equal(msid_indices$demo_indices$demo_uid, 1:12)

    # msid_avg
    avg_indices <- define_index_df(load_constants("msid_avg"))
    expect_equal(names(avg_indices), c("model_indices", "demo_indices"))
    expect_equal(avg_indices$demo_indices$demo_uid, 1:4)
})
