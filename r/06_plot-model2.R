source("r/header.R")

m2 <- read_csv("ms/objects/model2-output.csv") %>% 
  mutate(sr = plogis(logit_sr),
         cost = case_when(
           SR == 0.5 ~ "low",
           SR == 1 ~ "medium",
           SR == 2 ~ "high"
         ),
         cost = factor(cost, levels = c("low", "medium", "high"))) %>%
  # Remove one aberrant optimization
  filter(!(cost == "high" & PPFD == 750))

f4a <- ggplot(m2, aes(PPFD, sr, shape = cost)) +
  geom_line() +
  geom_point(fill = "white", size = 2) +
  xlab(expression(paste("Sunlight [PPFD, ", mu, "mol quanta ", m^-2~s^-1, "]"))) +
  ylab("Optimal Stomatal Ratio") +
  scale_y_continuous(limits = c(0, 0.5)) +
  # scale_color_manual(name = "Cost of Amphistomy", 
  #                    values = palette()[c(1, 3, 5)]) + 
  scale_shape_manual(name = "Cost of Amphistomy", values = c(21, 22, 24)) + 
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

legend <- get_legend(f4a)

f4a <- f4a + theme(legend.position = "none")

f4b <- ggplot(m2, aes(PPFD, g_sc, shape = cost)) +
  geom_line() +
  geom_point(fill = "white", size = 2) +
  xlab(expression(paste("Sunlight [PPFD, ", mu, "mol quanta ", m^-2~s^-1, "]"))) +
  ylab(expression(paste("Optimal ", italic(g)[sw]))) +
  #scale_color_manual(name = "Cost of Amphistomy", values = palette()[c(1, 3, 5)]) + 
  scale_shape_manual(name = "Cost of Amphistomy", values = c(21, 22, 24)) + 
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

plot_grid(legend, f4a, f4b, ncol = 1, align = 'h', axis = 'r', 
          rel_heights = c(0.1, 1, 1), labels = c("", LETTERS[1:2]))

ggsave("ms/figures/fig4-model2.pdf", width = 6.5, height = 6.5, 
       useDingbats = FALSE)
