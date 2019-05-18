source("r/header.R")

# run <- FALSE

library(tealeaves)

safe_detach(package:leafoptimizer)
safe_detach(package:photosynthesis)

# Parameter sets ----

## 1. High and low light
cs <- leafoptimizer::make_constants()
S_sw <- leafoptimizer::ppfd2sun(set_units(c(500, 1500), "umol/m^2/s"), 
                                set_units(0.5), 
                                set_units(220, "kJ/mol"))

## 2. Vary free and forced convection via windspeed
# 2 m/s -> Ar < 1
# 0.02 m/s -> Ar > 10
wind <- set_units(seq(0, 2, 0.02), "m/s")

## 3. Stomatal ratio
logit_sr <- set_units(c(-Inf, qlogis(0.25), 0))

## 4. Stomatal conductance
g_sw <- set_units(2, "umol/m^2/s/Pa")

## Make parameters
cs <- tealeaves::make_constants()
lp <- tealeaves::make_leafpar(
  replace = list(
    g_sw = g_sw,
    logit_sr = logit_sr
  )
)
ep <- tealeaves::make_enviropar(
  replace = list(
    S_sw = S_sw,
    T_air = set_units(298.15, "K"),
    wind = wind
  )
)

tl_pars <- list(cs = cs, lp = lp, ep = ep)
export2ms("tl_pars", "objects")

# Run tleaves or load saved ----
if (run) {
  tl <- tleaves(lp, ep, cs, set_units = TRUE, parallel = TRUE)
  write_rds(tl, "ms/objects/tl.rds")
} else{
  tl <- read_rds("ms/objects/tl.rds")
}

# Plot results ----
tl_plot <- tl %>%

  # Drop units for plotting
  mutate_if(~ is(.x, "units"), drop_units) %>%
  
  # Factorize stomatal ratio 
  mutate(sr = case_when(
    logit_sr == -Inf ~ "hypostomatous",
    logit_sr == qlogis(0.25) ~ "intermediate",
    logit_sr == 0 ~ "amphistomatous"
  )) %>%
  
  mutate(
    sr = factor(sr, levels = c("amphistomatous", "intermediate", "hypostomatous"))
  ) %>%
  
  # Factorize light environment 
  mutate(Light = case_when(
    round(S_sw, 0) == 220 ~ "Shade (PPFD = 500)",
    round(S_sw, 0) == 660 ~ "Sun (PPFD = 1500)"
  )) %>%
  
  # Remove one aberrant point
  filter(wind != wind[E == max(E[.data$sr == "hypostomatous"])]) %>%

  # Calculate E ratio
  select(wind, Light, sr, E) %>%
  spread(sr, E) %>%
  mutate(
    intermediate = intermediate / hypostomatous,
    amphistomatous = amphistomatous / hypostomatous
  ) %>%
  select(-hypostomatous) %>%
  gather(sr, E_ratio, -wind, -Light)

# Determine approximate shift from free to mixed to forced convection
df <- tl %>%
  mutate_if(~ is(.x, "units"), drop_units) %>%
  mutate(
    log_Ar = log(Ar), 
    log_wind = log(wind), 
    light = as.factor(S_sw),
    flow = ifelse(Re > 4000, "turbulent", "laminar")
  ) 

fit_c <- lm(log_Ar ~ flow + log_wind * S_sw, data = filter(df, wind > 0)) 
fit_f <- lm(Re ~ wind * S_sw, data = df) 

conv_shift <- tibble(
  Light = c("Shade (PPFD = 500)", "Sun (PPFD = 1500)"),
  s0 = -0.125,
  s1 = exp(c(
    (log(10) - coef(fit_c)["(Intercept)"] - coef(fit_c)["flowturbulent"]) / 
      coef(fit_c)["log_wind"],
    (log(10) - coef(fit_c)["(Intercept)"] - coef(fit_c)["flowturbulent"] - 
       coef(fit_c)["S_sw"]) /
      (coef(fit_c)["log_wind"] + coef(fit_c)["log_wind:S_sw"])
  )),
  s2 = exp(c(
    (log(0.1) - coef(fit_c)["(Intercept)"] - coef(fit_c)["flowturbulent"]) / 
      coef(fit_c)["log_wind"],
    (log(0.1) - coef(fit_c)["(Intercept)"] - coef(fit_c)["flowturbulent"] - 
       coef(fit_c)["S_sw"]) /
      (coef(fit_c)["log_wind"] + coef(fit_c)["log_wind:S_sw"])
  )),
  s3 = 2,
  ymin = rep(-Inf, 2),
  ymax = rep(Inf, 2),
  E_ratio = 0.95,
  wind = 1,
  sr = NA
) %>%
  mutate(free = (s0 + s1) / 2, mixed = (s1 + s2) / 2, forced = (s2 + s3) / 2)

flow_shift <- tibble(
  Light = rep(c("Shade (PPFD = 500)", "Sun (PPFD = 1500)"), each = 2),
  wind = rep(c(
    (4000 - coef(fit_f)["(Intercept)"]) / coef(fit_f)["wind"],
    (4000 - coef(fit_f)["(Intercept)"] - coef(fit_f)["S_sw"]) / 
      (coef(fit_f)["wind"] + coef(fit_f)["wind:S_sw"])
  ), each = 2),
  E_ratio = c(Inf, -Inf, Inf, -Inf),
  sr = rep("amphistomatous", 4)
)

gp <- ggplot(tl_plot, aes(wind, E_ratio, linetype = sr)) +
  facet_grid(~ Light) +
  scale_x_continuous(limits = c(-0.125, 2), breaks = seq(0, 2, 0.5)) +
  #scale_linetype_manual(name = "Stomatal Ratio", values = c("solid", "dashed")) + 
  xlab(expression(paste("Wind speed [", m~s^-1, "]"))) +
  ylab(expression(frac(italic(E)[italic(j)], italic(E)[hypo]))) +
  geom_line(size = 1.2) +
  geom_rect(
    data = conv_shift, alpha = 0.5, show.legend = FALSE,
    mapping = aes(xmin = s1, xmax = s2, ymin = ymin, ymax = ymax)
  ) +
  geom_line(data = flow_shift, size = 1.2, alpha = 0.5, linetype = "dashed",
            lineend = "round", show.legend = FALSE) +
  geom_text(
    data = conv_shift %>%
      select(Light, E_ratio, sr, free, mixed, forced) %>%
      gather(s, wind, -Light, -E_ratio, -sr), 
    mapping = aes(label = s), color = "grey25", angle = 90, hjust = 0,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(flow_shift, E_ratio == Inf), 
    label = "atop(Shift~to,turbulence)", parse = TRUE,
    show.legend = FALSE, hjust = -0.25, vjust = 1.25
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  ) +
  NULL

gp %<>% plot_grid(ncol = 1, labels = "A")

ggsave("ms/figures/fig2A-tleaf.pdf", width = 6.5, height = 3.5, 
       useDingbats = FALSE)

safe_detach(package:tealeaves)
