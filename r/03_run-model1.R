source("r/header.R")

# run <- FALSE

library(leafoptimizer)

safe_detach(package:tealeaves)
safe_detach(package:photosynthesis)

# Parameter sets ----

g_mc25 <- 3
J_max25 <- c(75, 150)

k_mc <- c(1)
leafsize <- c(0.004, 0.04, 0.4)
PPFD <- c(seq(100, 800, 25), seq(900, 2000, 100))
T_air <- c(288.15, 298.15, 308.15)
wind <- c(0.2, 2)

m1_vars <- crossing(J_max25, k_mc, leafsize, PPFD, T_air, wind)

export2ms("m1_vars", "objects")

# Run leafoptimizer ----

if (run) { 
  
  set.seed(20181220)
  ordr <- sample(nrow(m1_vars), replace = FALSE)
  
  nms <- c(
    "A", "abs_l", "abs_s", "C_air", "C_chl", "carbon_balance", "convergence", 
    "E", "E_q", "f_par", "g_mc", "g_mc25", "g_sc", "g_sw", "g_uc", "g_uw",
    "gamma_star", "gamma_star25", "H", "J_max", "J_max25", "K_C", "K_C25", 
    "k_mc", "K_O", "K_O25", "k_sc", "k_uc", "L", "leafsize", "logit_sr", "O", 
    "P", "phi_J", "PPFD", "r", "R_abs", "R_d", "R_d25", "RH", "S_r", "S_sw", 
    "T_air", "T_leaf", "theta_J", "V_cmax", "V_cmax25", "V_tpu", "V_tpu25", 
    "wind", "H2O", "i"
  )
  
  write_lines(str_c(nms, collapse = ","), "ms/objects/model1-output.csv")
  
  pb <- dplyr::progress_estimated(nrow(m1_vars))
  cs <- leafoptimizer::make_constants()
  bp <- leafoptimizer::make_bakepar()
  carbon_costs <- list(H2O = 0.001, SR = 0)
  
  for (i in ordr) {
    
    # Variable levels of biochemical and diffusional constraints
    lp <- leafoptimizer::make_leafpar(replace = list(
      g_mc25 = set_units(g_mc25, "umol/m^2/s/Pa"),
      J_max25 = set_units(m1_vars$J_max25[i], "umol/m^2/s"),
      k_mc = set_units(m1_vars$k_mc[i]),
      leafsize = set_units(m1_vars$leafsize[i], "m"),
      V_cmax25 = set_units(2 / 3 * m1_vars$J_max25[i], "umol/m^2/s")
    ))
    
    ep <- make_enviropar(replace = list(
      PPFD = set_units(m1_vars$PPFD[i], "umol/m^2/s"),
      T_air = set_units(m1_vars$T_air[i], "K"),
      wind = set_units(m1_vars$wind[i], "m/s")
    ))
    
    ol <- safely_optimize_leaf(c("g_sc", "sr"), carbon_costs, 
                               bp, cs, ep, lp, n_init = 2L, 
                               check = TRUE, quiet = TRUE,
                               refit = TRUE, max_init = 4L)
    
    if (!is.null(ol$result)) {
      ol$result$H2O <- carbon_costs$H2O
      ol$result$i <- i
      write_lines(str_c(ol$result, collapse = ","), 
                  "ms/objects/model1-output.csv", append = TRUE)
    }
    rm(ol)
    pb$tick()$print()
    
  }
  
}
