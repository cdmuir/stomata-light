source("r/header.R")

m1 <- read_csv("ms/objects/model1-output.csv") %>%
  mutate(
    sr = plogis(logit_sr),
    biochemistry = case_when(
      J_max25 == 75 ~ "low",
      J_max25 == 150 ~ "high"
      ),
    leafsize1 = case_when(
      leafsize == 0.004 ~ "small",
      leafsize == 0.04 ~ "medium",
      leafsize == 0.4 ~ "large"
    ),
    airtemp = case_when(
      T_air == 288.15 ~ "15 C",
      T_air == 298.15 ~ "25 C",
      T_air == 308.15 ~ "35 C"
    ),
    wind1 = case_when(
      wind == 0.2 ~ "still air",
      wind == 2 ~ "moving air"
    )
  ) %>%
  mutate(leafsize1 = factor(leafsize1, levels = c("small", "medium", "large")))

# Figures ----

## Figure 3A: SRopt across PPFD gradient ----
gpA <- ggplot(filter(m1, airtemp == "25 C", biochemistry == "low"), 
             aes(x = PPFD, y = sr, linetype = leafsize1)) +
  facet_grid(wind1 ~ .) +
  geom_line(size = 1, lineend = "round") +
  # geom_point() +
  scale_y_continuous(limits = c(0, 0.6), breaks = c(0, 0.25, 0.5)) + 
  # scale_color_manual(name = "Leaf size", values = palette()[c(1, 3, 5)]) + 
  scale_linetype_manual(name = "Leaf size", values = 1:3) + 
  xlab(expression(paste("Sunlight [PPFD, ", mu, "mol quanta ", m^-2~s^-1, "]"))) +
  ylab("Optimal Stomatal Ratio") +
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    legend.key.width = unit(0.5, "in"),
    strip.text = element_text(size = 12)
  ) +
  NULL

gpA

ggsave("ms/figures/fig3-model1A.pdf", width = 4.5, height = 4.5, 
       useDingbats = FALSE)

## Figure S3: SRopt across PPFD gradient ----
gpS3 <- ggplot(filter(m1, airtemp == "25 C", biochemistry == "high"), 
               aes(x = PPFD, y = sr, linetype = leafsize1)) +
  facet_grid(wind1 ~ .) +
  geom_line(size = 1, lineend = "round") +
  # geom_point() +
  scale_y_continuous(limits = c(0, 0.6), breaks = c(0, 0.25, 0.5)) + 
  # scale_color_manual(name = "Leaf size", values = palette()[c(1, 3, 5)]) + 
  scale_linetype_manual(name = "Leaf size", values = 1:3) + 
  xlab(expression(paste("Sunlight [PPFD, ", mu, "mol quanta ", m^-2~s^-1, "]"))) +
  ylab("Optimal Stomatal Ratio") +
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    legend.key.width = unit(0.5, "in"),
    strip.text = element_text(size = 12)
  ) +
  NULL

gpS3

ggsave("ms/figures/figS3.pdf", width = 4.5, height = 4.5, 
       useDingbats = FALSE)

## Figure 3B: gs_opt across PPFD gradient ----
gpB <- ggplot(filter(m1, airtemp == "25 C", biochemistry == "low"), 
              aes(x = PPFD, y = g_sw, linetype = leafsize1)) +
  facet_grid(wind1 ~ .) +
  geom_line(size = 1.1) +
  # geom_point() +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) + 
  # scale_color_manual(name = "Leaf size", values = palette()[c(1, 3, 5)]) + 
  scale_linetype_manual(name = "Leaf size", values = 1:3) + 
  xlab(expression(paste("Sunlight [PPFD, ", mu, "mol quanta ", m^-2~s^-1, "]"))) +
  ylab("Optimal Stomatal Conductance") +
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.justification = "center",
    legend.direction = "vertical",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    strip.text = element_text(size = 12)
  ) +
  NULL

gpB

ggsave("ms/figures/figX-model1B.pdf", width = 4.5, height = 4.5, 
       useDingbats = FALSE)

## Figures S3: Same as Figure with higher Vcmax/Jmax ----

m1 %<>% mutate(Ar = numeric(1), Gr = numeric(1), Re = numeric(1))
cs <- tealeaves::make_constants()
for (i in 1:nrow(m1)) {
  
  T_leaf <- m1$T_leaf[i]
  pars <- c(as.list(m1[i, ]), cs) %>%
    purrr::map_if(function(x) is(x, "units"), units::drop_units)
  m1[i, c("Ar", "Gr", "Re")] <- tealeaves::Ar(T_leaf, pars, unitless = TRUE)
  
}

# Filtered out high temperature because there appears to be some bistability at moderate Ar at this temperature. It would be interesting to figure out why.

df <- m1 %>% filter(convergence == 0, T_air < 308.15)
  
gp <- ggplot(df, aes(x = Ar, y = T_leaf - 273.15, color = leafsize1)) +
  facet_grid(airtemp ~ wind1) +
  scale_x_log10(breaks = 10 ^ seq(-3, 1, 2), 
                labels = c(expression(10 ^ -3),
                           expression(10 ^ -1),
                           expression(10 ^ 1))
                ) + 
  geom_hline(yintercept = 25) + 
  geom_vline(xintercept = 0.1) + 
  # geom_point(size = 1.1) +
  geom_line(size = 1.1) +
  scale_color_manual(name = "Leaf size", values = palette()[c(1, 3, 5)]) + 
  xlab("Archimedes number [unitless]") +
  ylab(expression(paste("Leaf temperature [", degree, "C]"))) +
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.direction = "vertical",
    legend.justification = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    strip.text = element_text(size = 12)
  ) +
  NULL  

gp

ggsave("ms/figures/figS2-model1C.pdf", width = 6, height = 4, 
       useDingbats = FALSE)
