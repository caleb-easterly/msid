library(msid)
imis_n_samp <- 1000
imis_n_resamp <- 3000

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
save(all_sexid_calib_results, file="results/all_sexid_calib_results_1_24_2020.rda")

# load("results/all_sexid_calib_results_1_24_2020.rda")
# run model with samples and no vaccination
m_tar <- male_prevalence_target()
f_tar <- female_prevalence_target()
all_sexid_prevs <- run_calib_results(all_sexid_calib_results$parms, so = 'msid',
                                     rep_df = all_sexid_rep, prop_df = all_sexid_props)
library(dplyr)
all_sexid_prevs_comb <- bind_rows(all_sexid_prevs)
# plot results against targets

par(mfrow = c(2, 1))
plot(density(filter(all_sexid_prevs_comb, sex == "m")$mean_prev),
     main = "All sexid M")
abline(v = m_tar[1])
abline(v = m_tar[1] + m_tar[2], lty = 3)
abline(v = m_tar[1] - m_tar[2], lty = 3)

plot(density(filter(all_sexid_prevs_comb, sex == "w")$mean_prev),
     main = "All sexid F")
abline(v = f_tar[1])
abline(v = f_tar[1] + f_tar[2], lty = 3)
abline(v = f_tar[1] - f_tar[2], lty = 3)

## calibrate het
### MSID ###
### calibrate
data("het_rep")
data("het_props")

set.seed(101)

## calibration
het_only_calib_results <- calibrate_model(so = 'het', rep_df = het_rep,
                                           prop_df = het_props,
                                           n_samp = imis_n_samp, n_resamp = imis_n_resamp)

# save results
save(het_only_calib_results, file="results/het_calib_results_1_24_2020.rda")

# run model with samples and no vaccination
m_tar <- male_prevalence_target()
f_tar <- female_prevalence_target()
het_prevs <- run_calib_results(het_only_calib_results$parms, so = 'het',
                                     rep_df = het_rep, prop_df = het_props)
library(dplyr)
het_prevs_comb <- bind_rows(het_prevs)
# plot results against targets

par(mfrow = c(2, 1))
plot(density(filter(het_prevs_comb, sex == "m")$mean_prev),
     main = "Het M")
abline(v = m_tar[1])
abline(v = m_tar[1] + m_tar[2], lty = 3)
abline(v = m_tar[1] - m_tar[2], lty = 3)

plot(density(filter(het_prevs_comb, sex == "w")$mean_prev),
     main = "Het F")
abline(v = f_tar[1])
abline(v = f_tar[1] + f_tar[2], lty = 3)
abline(v = f_tar[1] - f_tar[2], lty = 3)

## plot calibration results
het_prevs_comb$mod <- "hetOnly"
all_sexid_prevs_comb$mod <- "multiSexID"
tog_prevs <- rbind(het_prevs_comb, all_sexid_prevs_comb)
tog_prevs$sex[tog_prevs$sex == "m"] <- "Men"
tog_prevs$sex[tog_prevs$sex == "w"] <- "Women"

# targets

target_df <- data.frame("mean" = c(m_tar[1], f_tar[1])*100,
                        "sex" = c("Men", "Women"))

ggplot(tog_prevs) +
  geom_density(aes(x = mean_prev * 100, fill = mod), alpha = 0.5) +
  geom_vline(data = target_df, aes(xintercept = mean), linetype = 2) +
  facet_grid(.~sex) +
  theme_bw() +
  labs(x = "HPV16 Prevalence (%)", y = "Density") +
  scale_fill_discrete(name = "Model")

ggsave("plots/calib.png", width = 6, height = 4, units = "in")

# msid_reds <- run_comparison(so = 'msid',
#                             contacts = contact_msid_base,
#                             n_samp = imis_n_samp,
#                             n_resamp = imis_n_resamp,
#                             n_run_samp = post_samp)
#
#
# ### HET ###
# ### calibrate
#
# het_comps <- run_comparison(so = 'het',
#                             contacts = '../fullSO_SMDM/data/natsal_het_balanced.tab',
#                             props = '../fullSO_SMDM/data/natsal_het_props.tab',
#                             n_samp = imis_n_samp,
#                             n_resamp = imis_n_resamp,
#                             n_run_samp = post_samp)
#
# # compare prevalence to targets
# het_cprev_wtar <- calc_prev_by_sex(het_comps$prevs)
#
# ### AVG ###
# data("contact_avg_base")
# avg_comps <- run_comparison(so = 'msid_avg',
#                             contacts = contact_avg_base,
#                             n_samp = imis_n_samp,
#                             n_resamp = imis_n_resamp,
#                             n_run_samp = post_samp)
#
# # compare prevalence to targets
# avg_cprev_wtar <- calc_prev_by_sex(avg_comps$prevs)
#
# # save.image(file = 'calib_results_1_29_19.rda')
#
