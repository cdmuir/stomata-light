source("r/header.R")

# run <- FALSE

library(leafoptimizer)

safe_detach(package:tealeaves)
safe_detach(package:photosynthesis)

# Parameter sets ----

p <- c(0.001, 0.005, 0.025, 0.05, 0.1, 0.2, 0.4, 0.5, 0.6, 0.8, 0.9, 0.95, 0.975, 0.995, 0.999) 

# Variable levels of biochemical and diffusional constraints
g_mc25 <- 3
J_max25 <- 112.5

leafsize <- 0.1
PPFD <- qnorm(p, 1000, 300)
T_air <- 298.15 
wind <- 2

# Costs

H2O <- 1000
SR <- c(
  qunif(rev(p), 0, 3000),
  qunif(rev(p), 500, 2500),
  qunif(rev(p), 1000, 2000)
)

safely_optimize_leaf <- purrr::safely(optimize_leaf)

m3_vars <- data.frame(
  PPFD, 
  SR, 
  cov_strength = rep(c("strong", "moderate", "weak"), each = length(p)),
  stringsAsFactors = FALSE
)

export2ms("m3_vars", "objects")

m4_vars <- bind_rows(m3_vars, m3_vars, m3_vars) %>%
  mutate(PPFD = rep(c(400, 1000, 1600), each = nrow(m3_vars)))

export2ms("m4_vars", "objects")

# Run leafoptimizer ----

if (run) { 
  
  safely_optimize_leaf <- purrr::safely(optimize_leaf)
  
  nms <- c(
    "A", "abs_l", "abs_s", "C_air", "C_chl", "carbon_balance", "E", "E_q", 
    "f_par", "g_mc", "g_mc25", "g_sc", "g_sw", "g_uc", "g_uw", "gamma_star", 
    "gamma_star25", "H", "J_max", "J_max25", "K_C", "K_C25", "k_mc", "K_O", 
    "K_O25", "k_sc", "k_uc", "L", "leafsize", "logit_sr", "O", "P", "phi_J", 
    "PPFD", "r", "R_abs", "R_d", "R_d25", "RH", "S_r", "S_sw", "T_air", 
    "T_leaf", "theta_J", "V_cmax", "V_cmax25", "V_tpu", "V_tpu25", "wind", 
     "H2O", "SR", "i"
  )
  
  write_lines(str_c(nms, collapse = ","), "ms/objects/model3-output.csv")
  
  pb <- dplyr::progress_estimated(nrow(m3_vars))
  cs <- make_constants()
  bp <- make_bakepar()
  
  for (i in 1:nrow(m3_vars)) {
    
    # Variable levels of biochemical and diffusional constraints
    lp <- make_leafpar(cs, replace = list(
      g_mc25 = set_units(g_mc25, "umol/m^2/s/Pa"),
      J_max25 = set_units(J_max25, "umol/m^2/s"),
      leafsize = set_units(leafsize, "m"),
      V_cmax25 = set_units(2 / 3 * J_max25, "umol/m^2/s")
    ))
    
    ep <- make_enviropar(replace = list(
      PPFD = set_units(m3_vars$PPFD[i], "umol/m^2/s"),
      T_air = set_units(T_air, "K"),
      wind = set_units(wind, "m/s")
    ))
    
    carbon_costs <- list(H2O = H2O, SR = m3_vars$SR[i])
    
    ol <- safely_optimize_leaf(c("g_sc", "logit_sr"), carbon_costs, 
                               lp, ep, bp, cs, n_init = 1L, quiet = FALSE)
    
    if (!is.null(ol$result)) {
      ol$result$H2O <- carbon_costs$H2O
      ol$result$SR <- carbon_costs$SR
      ol$result$i <- i
      write_lines(str_c(ol$result, collapse = ","), 
                  "ms/objects/model3-output.csv", append = TRUE)
    }
    rm(ol)
    pb$tick()$print()
    
  }
  
}

if (run) { 
  
  safely_optimize_leaf <- purrr::safely(optimize_leaf)
  
  nms <- c(
    "A", "abs_l", "abs_s", "C_air", "C_chl", "carbon_balance", "E", "E_q", 
    "f_par", "g_mc", "g_mc25", "g_sc", "g_sw", "g_uc", "g_uw", "gamma_star", 
    "gamma_star25", "H", "J_max", "J_max25", "K_C", "K_C25", "k_mc", "K_O", 
    "K_O25", "k_sc", "k_uc", "L", "leafsize", "logit_sr", "O", "P", "phi_J", 
    "PPFD", "r", "R_abs", "R_d", "R_d25", "RH", "S_r", "S_sw", "T_air", 
    "T_leaf", "theta_J", "V_cmax", "V_cmax25", "V_tpu", "V_tpu25", "wind", 
    "H2O", "SR", "i"
  )
  
  write_lines(str_c(nms, collapse = ","), "ms/objects/model4-output.csv")
  
  pb <- dplyr::progress_estimated(nrow(m4_vars))
  cs <- make_constants()
  bp <- make_bakepar()
  
  for (i in 1:nrow(m4_vars)) {
    
    # Variable levels of biochemical and diffusional constraints
    lp <- make_leafpar(cs, replace = list(
      g_mc25 = set_units(g_mc25, "umol/m^2/s/Pa"),
      J_max25 = set_units(J_max25, "umol/m^2/s"),
      leafsize = set_units(leafsize, "m"),
      V_cmax25 = set_units(2 / 3 * J_max25, "umol/m^2/s")
    ))
    
    ep <- make_enviropar(replace = list(
      PPFD = set_units(m4_vars$PPFD[i], "umol/m^2/s"),
      T_air = set_units(T_air, "K"),
      wind = set_units(wind, "m/s")
    ))
    
    carbon_costs <- list(H2O = H2O, SR = m4_vars$SR[i])
    
    ol <- safely_optimize_leaf(c("g_sc", "logit_sr"), carbon_costs, 
                               lp, ep, bp, cs, n_init = 1L, quiet = FALSE)
    
    if (!is.null(ol$result)) {
      ol$result$H2O <- carbon_costs$H2O
      ol$result$SR <- carbon_costs$SR
      ol$result$i <- i
      write_lines(str_c(ol$result, collapse = ","), 
                  "ms/objects/model4-output.csv", append = TRUE)
    }
    rm(ol)
    pb$tick()$print()
    
  }
  
}
