library(msid)
imis_n_samp <- 50
imis_n_resamp <- 100
post_samp <- 50

# reproducible results
set.seed(101)

### MSID ###
### calibrate
msid_reds <- run_comparison(so = 'msid',
                            contacts = '../fullSO_SMDM/data/natsal_msid.tab',
                            props = '../fullSO_SMDM/data/natsal_msid_props.tab',
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

save.image(file = 'calib_results_1_29_19.rda')

