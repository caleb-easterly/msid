library(msid)
imis_n_samp <- 1000
imis_n_resamp <- 3000
post_samp <- 1000

# reproducible results
set.seed(101)

### MSID ###
### calibrate
data("contact_msid_base")
msid_reds <- run_comparison(so = 'msid',
                            contacts = contact_msid_base,
                            n_samp = imis_n_samp,
                            n_resamp = imis_n_resamp,
                            n_run_samp = post_samp)


### HET ###
### calibrate

het_comps <- run_comparison(so = 'het',
                            contacts = '../fullSO_SMDM/data/natsal_het_balanced.tab',
                            props = '../fullSO_SMDM/data/natsal_het_props.tab',
                            n_samp = imis_n_samp,
                            n_resamp = imis_n_resamp,
                            n_run_samp = post_samp)

# compare prevalence to targets
het_cprev_wtar <- calc_prev_by_sex(het_comps$prevs)

### AVG ###
data("contact_avg_base")
avg_comps <- run_comparison(so = 'msid_avg',
                            contacts = contact_avg_base,
                            n_samp = imis_n_samp,
                            n_resamp = imis_n_resamp,
                            n_run_samp = post_samp)

# compare prevalence to targets
avg_cprev_wtar <- calc_prev_by_sex(avg_comps$prevs)

# save.image(file = 'calib_results_1_29_19.rda')

