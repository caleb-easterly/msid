library(msid)
library(ggplot2)

# all sexid
data("all_sexid_rep")
data("all_sexid_props")
all_bal <- het_rep_to_bal(all_sexid_rep, all_sexid_props)

# na's for plotting
all_bal$corrected_r[all_bal$corrected_r == 0] <- NA

midrange <- with(all_bal,
                 diff(range(corrected_r, na.rm = TRUE))/2 + min(corrected_r, na.rm = TRUE))

ggplot(all_bal) +
  geom_tile(aes(x = r_demo, y = rp_demo, fill=corrected_r), color="black", size = 0.2) +
  scale_fill_gradientn(name="Annual Number\nof New Partnerships",
                       colors = c("blue", "red", "yellow"),
                       na.value = "grey50") +
  labs(x = "Group of Partner 1",
       y = "Group of Partner 2") +
  theme_minimal(base_size = 14) + coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/all_sexid.png",
       width = 6, height = 6, units = "in")

# merged
data("sexid_tog_rep")
data("sexid_tog_props")
all_bal <- het_rep_to_bal(sexid_tog_rep, sexid_tog_props)

# na's for plotting
all_bal$corrected_r[all_bal$corrected_r == 0] <- NA

ggplot(all_bal) +
  geom_tile(aes(x = r_demo, y = rp_demo, fill=corrected_r), color="black", size = 0.2) +
  scale_fill_gradient2(name="Annual Number\nof New Partnerships",
                       low = "blue",
                       mid = "red",
                       high = "yellow",
                       midpoint = 1.5,
                       na.value = "grey50") +
  labs(x = "Group of Partner 1",
       y = "Group of Partner 2") +
  theme_minimal(base_size = 14) + coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/all_tog.png",
       width = 6, height = 6, units = "in")

# het only

data("het_props")
data("het_rep")
het_bal <- het_rep_to_bal(het_rep, het_props)

# na's for plotting
het_bal$corrected_r[het_bal$corrected_r == 0] <- NA

ggplot(het_bal) +
  geom_tile(aes(x = r_demo, y = rp_demo, fill=corrected_r), color="black", size = 0.2) +
  scale_fill_gradient2(name="Annual Number\nof New Partnerships",
                      breaks = seq(0, 4, by = 0.5),
                      low = "blue",
                      mid = "red",
                      high = "yellow",
                      midpoint = 2,
                      na.value = "grey50") +
  labs(x = "Group of Partner 1",
       y = "Group of Partner 2") +
  theme_minimal(base_size = 14) + coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/het_only.png",
       width = 6, height = 6, units = "in")
