library(msid)
imis_n_samp <- 100
imis_n_resamp <- 300

# reproducible results
set.seed(101)

### MSID ###
### calibrate
data("all_sexid_rep")
data("all_sexid_props")

## calibration
all_sexid_calib_results <- calibrate_model(so = 'msid', rep_df = all_sexid_rep,
                                           prop_df = all_sexid_props,
                                           n_samp = imis_n_samp, n_resamp = imis_n_resamp)
# save results
save(all_sexid_calib_results, file="results/all_sexid_calib_results_7_6_21.rda")

### AVG ###
### calibrate
set.seed(1010)
data('sexid_tog_rep')
data('sexid_tog_props')
msid_avg_calib_results <- calibrate_model(so = "msid_avg",
                                     rep_df = sexid_tog_rep,
                                     prop_df = sexid_tog_props,
                                     n_samp = imis_n_samp, n_resamp = imis_n_resamp)
save(msid_avg_calib_results, file="results/msid_avg_calib_results_7_21.rda")

## calibrate het
set.seed(10101)
data("het_rep")
data("het_props")

## calibration
het_only_calib_results <- calibrate_model(so = 'het', rep_df = het_rep,
                                          prop_df = het_props,
                                          n_samp = imis_n_samp, n_resamp = imis_n_resamp)

# save results
save(het_only_calib_results, file="results/het_calib_results_7_21.rda")
