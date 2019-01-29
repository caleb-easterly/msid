library(sincHPV)
library(ggplot2)
load('calib_results.rda')

## prevalence by sex and sexid
msid_reds$prevs %>%
    group_by(sex, sexid) %>%
    summarise(mean_prev = sum(prev * prop) / sum(prop))

## mSexID ##
# compare prevalence to targets
msid_cprev_wtar <- calc_prev_by_sex(msid_reds$prevs)

library(ggplot2)
ggplot(msid_cprev_wtar) +
    geom_density(aes(x = prev), fill="darkgreen", alpha = 0.3) +
    geom_vline(aes(xintercept = tar_mean), col = "pink", size = 2) +
    facet_grid(.~sex) +
    theme_bw()

# summarise reductions
ggplot(msid_reds$reds) +
    geom_density(aes(x = abs_red),fill="darkgreen", alpha = 0.3) +
    facet_grid(.~sex) +
    theme_bw()

#### HET ####

het_comps$prevs %>%
    group_by(sex, sexid) %>%
    summarise(mean_prev = sum(prev * prop) / sum(prop))

het_cprev_wtar <- calc_prev_by_sex(het_comps$prevs)

ggplot(het_cprev_wtar) +
    geom_density(aes(x = prev), fill="darkgreen", alpha = 0.3) +
    geom_vline(aes(xintercept = tar_mean), col = "pink", size = 2) +
    facet_grid(.~sex) +
    theme_bw()

# summarise reductions
ggplot(het_comps$reds) +
    geom_density(aes(x = abs_red),fill="darkgreen", alpha = 0.3) +
    facet_grid(.~sex, scales = "free_x") +
    theme_bw()

#### BOTH ####
# combine
het_reds <- het_comps$reds
het_reds$so <- 'het'

msid_red_sum <- msid_reds$reds
msid_red_sum$so <- 'msid'

both <- rbind(het_reds, msid_red_sum) %>%
    mutate(gsex = ifelse(sex == 'm', "Male", "Female"),
           gso = ifelse(so == 'het', "Het. Only", "mSexID"))

ggplot(both) +
    geom_density(aes(x = abs_red, fill = gso), alpha = 0.3) +
    facet_grid(.~gsex) +
    theme_bw(base_size = 16) +
    labs(x = "Reduction in Infections Per 1000 People Over 20 Years",
         y = "Density",
         title = "Absolute Reduction in HPV Infections") +
    scale_fill_discrete(name = "Model")
ggsave('../fullSO_SMDM/plots/abs_reduction.png', width = 9, height = 5, units = "in", dpi = 500)

ggplot(both) +
    geom_density(aes(x = rel_red, fill = so), alpha = 0.3) +
    facet_grid(.~sex, scales = "free_x") +
    theme_bw() +
    labs(x = "Relative Reduction in HPV Infections",
         y = "Density")

ggplot(both) +
    geom_boxplot(aes(x = so, y = abs_red)) +
    facet_grid(.~sex) +
    theme_bw() +
    labs(y = "Absolute Reduction in HPV Infections, Per 1000 People",
         x = "Model")

ggplot(both) +
    geom_boxplot(aes(x = so, y = rel_red)) +
    facet_grid(.~sex) +
    theme_bw() +
    labs(y = "Relative Reduction in HPV Infections",
         x = "Model")

results <- both %>%
    group_by(sex, so) %>%
    summarise(med_abs_red = median(abs_red),
              perc25 = quantile(abs_red, 0.25),
              perc75 = quantile(abs_red, 0.75),
              mean_rel_red = median(rel_red),
              sd_rel_red = sd(rel_red))
results
