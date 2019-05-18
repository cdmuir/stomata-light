source("r/header.R")

# run <- FALSE

library(photosynthesis)

safe_detach(package:leafoptimizer)
safe_detach(package:tealeaves)

# Parameter sets ----

bp <- photosynthesis::make_bakepar()
cs <- photosynthesis::make_constants(use_tealeaves = FALSE)
ep <- photosynthesis::make_enviropar(use_tealeaves = FALSE)
lp <- photosynthesis::make_leafpar(replace = list(
  g_mc25 = units::set_units(3, "umol/m^2/s/Pa"),
  g_sc = leafoptimizer::gw2gc(
    units::set_units(c(1, 4), "umol/m^2/s/Pa"), 
                              units::set_units(1.29e-05,"m^2/s"), 
                              units::set_units(2.12e-05,"m^2/s"), 
    unitless = FALSE
    ),
  J_max25 = units::set_units(150, "umol/m^2/s"),
  k_sc = units::set_units(c(0, 1/3, 1)),
  T_leaf = units::set_units(seq(278.15, 313.15, length.out = 1e2), "K"),
  V_cmax25 = units::set_units(100, "umol/m^2/s"),
  V_tpu25 = units::set_units(200, "umol/m^2/s")
), use_tealeaves = FALSE)

ph_pars <- list(cs = cs, lp = lp, ep = ep, bp = bp)
export2ms("ph_pars", "objects")

# Run photosynthesis or load saved ----

if (run) {
  ph <- photosynthesis::photosynthesis(
    leaf_par = lp, 
    enviro_par = ep, 
    bake_par = bp, 
    constants = cs, 
    use_tealeaves = FALSE,
    progress = TRUE,
    quiet = FALSE,
    set_units = TRUE, 
    parallel = TRUE
  )
  write_rds(ph, "ms/objects/ph.rds")
} else{
  ph <- read_rds("ms/objects/ph.rds")
}

# Plot ----

ph_plot <- ph %>% 
  mutate(T_leaf = set_units(T_leaf, degreeC)) %>%
  mutate_if(~ is(.x, "units"), drop_units) %>%
  mutate(
    `Stomatal conductance` = g_sc %>%
      is_less_than(1) %>%
      if_else("low", "high") %>%
      factor(levels = c("high", "low")),
    `Stomatal ratio` = factor(case_when(
      k_sc == 0 ~ "hypo",
      k_sc == 1/3 ~ "inter",
      k_sc == 1 ~ "amphi"),
      levels = c("amphi", "inter", "hypo"))
  )

Amax <- ph_plot %>% 
  ddply(.(`Stomatal conductance`, `Stomatal ratio`), summarize,
        x = T_leaf[which(A == max(A))],
        y = max(A))

gp <- ggplot(ph_plot, aes(T_leaf, A, color = `Stomatal conductance`,
               linetype = `Stomatal ratio`)) +
  # geom_segment(data = Amax, aes(x, y, xend = x, yend = 0)) +
  geom_line(size = 1.1) +
  xlab(expression(paste("Leaf Temperature [", degree, "C]"))) +
  ylab(expression(paste("Photosynthesis [", mu, "mol C", O[2]~m^-2, " ", s^-1, "]"))) +
  scale_color_manual(name = "Stomatal\nconductance", values = c("black", "grey")) + 
  scale_linetype_manual(name = "Stomatal\nratio", values = c(1, 2, 3)) + 
  theme_bw() + 
  theme_bw() + 
  theme(
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.direction = "vertical",
    legend.key.width = unit(.85, "in"),
    legend.justification = "center",
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.title.align = 0.5
  ) +
  NULL

gp %<>% plot_grid(ncol = 1, labels = "B")

ggsave("ms/figures/fig2B-photo.pdf", width = 6.5, height = 3.5)
