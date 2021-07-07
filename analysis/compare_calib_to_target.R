library(ggplot2)
library(dplyr)


data("all_sexid_rep")
data("all_sexid_props")
data("sexid_tog_rep")
data("sexid_tog_props")
data("het_rep")
data("het_props")

load("results/all_sexid_calib_results_3_23_2020_v2.rda")
load("results/msid_avg_calib_results_3_23_2020_v2.rda")
load("results/het_calib_results_3_23_2020_v2.rda")

# run model with samples and no vaccination

all_sexid_prevs <- run_calib_results(all_sexid_calib_results$parms, so = 'msid',
                                     rep_df = all_sexid_rep, prop_df = all_sexid_props)
msid_avg_prevs <- run_calib_results(msid_avg_calib_results$parms, so = 'msid_avg',
                                    rep_df = sexid_tog_rep, prop_df = sexid_tog_props)
het_prevs <- run_calib_results(het_only_calib_results$parms, so = "het",
                               rep_df = het_rep, prop_df = het_props)

all_sexid_prevs_comb <- bind_rows(all_sexid_prevs)
all_sexid_prevs_comb$so <- "msid"

msid_avg_prevs_comb <- bind_rows(msid_avg_prevs)
msid_avg_prevs_comb$so <- "msid_avg"

het_prevs_comb <- bind_rows(het_prevs)
het_prevs_comb$so <- "het"

allmods_comb <- rbind(all_sexid_prevs_comb,
                      msid_avg_prevs_comb,
                      het_prevs_comb)

# plot results against targets
m_tar <- male_prevalence_target()
f_tar <- female_prevalence_target()

targets <- data.frame(rbind(m_tar, f_tar))
colnames(targets) <- c("mean", "sd")
targets$sex <- c("m", "w")

ggplot(allmods_comb) +
    geom_density(aes(x = mean_prev, fill = so)) +
    geom_vline(data = targets, aes(xintercept = mean),
               linetype = 1, size = 0.5) +
    geom_vline(data = targets, aes(xintercept = mean+1.96*sd),
               linetype = 2, size = 0.5) +
    geom_vline(data = targets, aes(xintercept = mean-1.96*sd),
               linetype = 2, size = 0.5) +
    facet_grid(sex~so) +
    theme_bw() +
    labs(x = "Prevalence", y = "Density")
ggsave(file = "plots/calib_results_03_23_2020_v2.png",
       width = 9, height = 4, units = "in",
       dpi = 500)

