set.seed(101)

library(msid)

## vaccine coverage
vacc <- 0.7

## multisexID ##
data("all_sexid_rep")
data("all_sexid_props")
load("results/all_sexid_calib_results_3_23_2020_v2.rda")
n_samps <- 1000
multisexid_reds <- run_comparison("msid", all_sexid_rep, all_sexid_props,
                                  all_sexid_calib_results$parms, n_samps, vacc)

## averaged ##
data("sexid_tog_rep")
data("sexid_tog_props")
load("results/msid_avg_calib_results_3_23_2020_v2.rda")
msid_avg_reds <- run_comparison("msid_avg", sexid_tog_rep, sexid_tog_props,
                                  msid_avg_calib_results$parms, n_samps, vacc)


## het ##
data("het_rep")
data("het_props")
load("results/het_calib_results_3_23_2020_v2.rda")
het_reds <- run_comparison("het", het_rep, het_props,
                           het_only_calib_results$parms, n_samps, vacc)

## plot results
all_reds <- rbind(
  cbind(het_reds$reds, mod = "hetOnly"),
  cbind(msid_avg_reds$reds, mod = "mergedSexID"),
  cbind(multisexid_reds$reds, mod = "multiSexID")
)

both_reds$sex[both_reds$sex == "m"] <- "Men"
both_reds$sex[both_reds$sex == "w"] <- "Women"

ggplot(all_reds) +
  geom_density(aes(x = abs_red_mf_v_f, fill = mod), alpha = 0.5) +
  facet_grid(.~sex) +
  labs(x = "Absolute reduction in HPV Infections (per 1000)", y = "Density") +
  scale_fill_discrete(name = "Model") +
  theme_bw()
ggsave("plots/absolute_50.png", width = 6, height = 4, units = "in")

# save.image("results/analyis.rda")
