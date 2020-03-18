## multisexid ##
### MSID ###
### calibrate
data("all_sexid_rep")
data("all_sexid_props")
load("results/all_sexid_calib_results_1_24_2020.rda")

n_samps <- 1000

## reductions
multisexid_reds <- run_comparison("msid", all_sexid_rep, all_sexid_props,
                                  all_sexid_calib_results$parms, n_samps)

# library(ggplot2)
# ggplot(multisexid_reds$reds) +
#   geom_boxplot(aes(y = abs_red, x = sex)) +
#   theme_bw()


## het ##
data("het_rep")
data("het_props")
load("results/het_calib_results_1_24_2020.rda")

het_reds <- run_comparison("het", het_rep, het_props,
                           het_only_calib_results$parms, n_samps)

## plot results
both_reds <- rbind(
  cbind(het_reds$reds, mod = "hetOnly"),
  cbind(multisexid_reds$reds, mod = "multiSexID")
)

both_reds$sex[both_reds$sex == "m"] <- "Men"
both_reds$sex[both_reds$sex == "w"] <- "Women"

ggplot(both_reds) +
  geom_density(aes(x = abs_red, fill = mod), alpha = 0.5) +
  facet_grid(.~sex) +
  labs(x = "Absolute reduction in HPV Infections (per 1000)", y = "Density") +
  scale_fill_discrete(name = "Model") +
  theme_bw()
ggsave("plots/absolute_50.png", width = 6, height = 4, units = "in")

save.image("results/analyis.rda")
