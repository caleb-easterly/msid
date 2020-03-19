context("model")

model_test_fun1 <- function(rep, props, sexids) {
    parms <- define_parameters(contact_df = rep,
                               prop_df = props, sexids = sexids)
    test_vacc <- define_vaccination(parms, 0, 0)
    init_cond <- parms$structural$init_cond
    output <- model_function(0, init_cond, parms, test_vacc)[[1]]

    #### test 1: input and output vectors should be the same length
    expect_equal(length(init_cond), length(output))

    #### test 2: without vaccination, vacc compartments should be 0
    v_ids <- parms$structural$model_indices %>% filter(epi == "V") %>%
        select(model_uid) %>% unlist()

    expect_equal(sum(output[v_ids]), 0)

    #### test 3: when solved, no compartment should be negative
    sted <- run_to_steady(parms, test_vacc)[2, -1]
    expect_true(all(sted > -.Machine$double.eps))

    #### test 4: the sum should also be equal to 1, for all non-incidence compartments
    non_inc <- parms$structural$model_indices %>% filter(epi != "I") %>%
        select(model_uid) %>% unlist()
    expect_equal(sum(sted[non_inc]), 1)
}

model_test_fun2 <- function(rep, props, sexids) {
    parms <- define_parameters(contact_df = rep,
                               prop_df = props, sexids = sexids)
    test_vacc <- define_vaccination(parms, 0.5, 0.5)
    sted <- run_to_steady(parms, test_vacc)[2, -1]

    #### test 1: with vaccination, vacc compartments should be > 0
    v_ids <- parms$structural$model_indices %>% dplyr::filter(epi == "V") %>%
        select(model_uid) %>% unlist()
    expect_true(all(sted[v_ids] > 0))
}

test_that("model runs, results appropriate", {
    data("all_sexid_rep")
    data("all_sexid_props")
    data("het_rep")
    data("het_props")
    model_test_fun1(all_sexid_rep, all_sexid_props, 'msid')
    model_test_fun1(het_rep, het_props, "het")
    # model_test_fun1(sexid_tog_rep, sexid_tog_props, "msid_avg")
})

test_that("positive vaccination compartments w/ vacc", {
    data("all_sexid_rep")
    data("all_sexid_props")
    model_test_fun2(all_sexid_rep, all_sexid_props, "msid")

    data("het_rep")
    data("het_props")
    model_test_fun2(het_rep, het_props, "het")
})
