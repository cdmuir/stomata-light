source("r/header.R")

m3 <- read_csv("ms/objects/model3-output.csv") %>% 
  mutate(sr = plogis(logit_sr))

m3_vars <- read_rds("ms/objects/m3_vars.rds")
m3_vars$cov_strength %<>% factor(levels = c("weak", "moderate", "strong"))

m4_vars <- read_rds("ms/objects/m4_vars.rds")
m3_vars$cov_strength %<>% factor(levels = c("weak", "moderate", "strong"))

m3$cov_strength <- factor(m3_vars$cov_strength, 
                          levels = c("weak", "moderate", "strong"))

# Supplemental figure on Cov(Cost, Light) ----
fS2 <- ggplot(m3_vars, aes(PPFD, SR, color = cov_strength)) +
  geom_line(size = 1.2, lineend = "round") +
  xlab(expression(paste("Sunlight [PPFD, ", mu, "mol quanta ", m^-2~s^-1, "]"))) +
  ylab(expression(lambda[italic(SR)]^-1)) +
  # scale_y_continuous(limits = c(0, 0.5)) +
  scale_color_manual(name = "Cov(Cost, Light)", values = palette()[c(5, 3, 1)]) +
  theme_bw() +
  theme(
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.justification = "center",
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5
  ) +
  NULL

fS2

ggsave("ms/figures/figS2.pdf", width = 5, height = 4, useDingbats = FALSE)

# Figure 5 ----
f5a <- ggplot(m3, aes(PPFD, sr, shape = cov_strength)) +
  geom_line() +
  geom_point(fill = "white", size = 2) +
  xlab(expression(paste("Sunlight [PPFD, ", mu, "mol quanta ", m^-2~s^-1, "]"))) +
  ylab("Optimal Stomatal Ratio") +
  scale_y_continuous(limits = c(0, 0.5)) +
  # scale_color_manual(name = "Cov(Cost, Light)", values = palette()[c(5, 3, 1)]) +
  scale_shape_manual(name = "Cov(Cost, Light)", values = c(24, 22, 21)) +
  theme_bw() +
  theme(
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(color = "white"),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.justification = "center",
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5
  ) +
  NULL

legend <- get_legend(f5a)

f5a <- f5a + theme(legend.position = "none")

f5a

f5b <- ggplot(m3, aes(PPFD, g_sc, shape = cov_strength)) +
  geom_line() +
  geom_point(fill = "white", size = 2) +
  # scale_color_manual(name = "Cov(Cost, Light)", values = palette()[c(5, 3, 1)]) +
  scale_shape_manual(name = "Cov(Cost, Light)", values = c(24, 22, 21)) +
  xlab(expression(paste("Sunlight [PPFD, ", mu, "mol quanta ", m^-2~s^-1, "]"))) +
  ylab(expression(paste("Optimal ", italic(g)[sw]))) +
  theme_bw() +
  theme(
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  NULL

plot_grid(legend, f5a, f5b, ncol = 1, align = 'h', axis = 'r', 
          rel_heights = c(0.1, 1, 1), labels = c("", LETTERS[1:2]))

ggsave("ms/figures/fig5-model3.pdf", width = 6.5, height = 6.5, 
       useDingbats = FALSE)
