
rm(list=ls())
library(tidyverse)
library(readstata13)
library(MASS)

get_model_parameters <- function(data_name, weights = TRUE, 
                                 weights_column, new_starting, dv){
  
  data_name$outcome <- data_name[, dv]
  data_name$use_weights <- data_name[, weights_column]

  if(weights == FALSE){
  mod <- tryCatch(nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                      data = data_name,
                      start = list(a = 1, 
                                   b1 = 2, 
                                   b2 = -0.1)), 
                  error = function(e) NULL)
  
  if(is.null(mod) == TRUE){ 
    
    mod <- tryCatch(nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                        data = data_name,
                        start = list(a = 1, 
                                     b1 = 2, 
                                     b2 = -0.1), 
                        algorithm = "port"),
                    error = function(e) NULL)
    pars <- as.data.frame(t(mod$m$getPars()))
    vcov_out <- as.data.frame(vcov(mod))
    pars$V_A <- vcov_out["a", "a"]
    pars$V_B1 <- vcov_out["b1", "b1"]
    pars$V_B2 <- vcov_out["b2", "b2"]
    pars$C_AB1 <- vcov_out["a", "b1"]
    pars$C_AB2 <- vcov_out["a", "b2"]
    pars$C_B1B2 <- vcov_out["b1", "b2"]
    
    pars$state <- unique(data_name$State)
    pars$algo <- "nl2sol"
    
    
  } else if (is.null(mod) == FALSE){
    pars <- as.data.frame(t(mod$m$getPars()))
    vcov_out <- as.data.frame(vcov(mod))
    pars$V_A <- vcov_out["a", "a"]
    pars$V_B1 <- vcov_out["b1", "b1"]
    pars$V_B2 <- vcov_out["b2", "b2"]
    pars$C_AB1 <- vcov_out["a", "b1"]
    pars$C_AB2 <- vcov_out["a", "b2"]
    pars$C_B1B2 <- vcov_out["b1", "b2"]
    
    pars$state <- unique(data_name$State)
    pars$algo <- "gnr"
  }
  
  } else if(weights == TRUE) {
    
    mod <- tryCatch(nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                        data = data_name,
                        start = list(a = 1, 
                                     b1 = 2, 
                                     b2 = -0.1),
                        weights = use_weights), 
                    error = function(e) NULL)
    
    if(is.null(mod) == TRUE){ 
      mod_port <- tryCatch(nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                               data = data_name,
                               start = list(a = 1, 
                                            b1 = 2, 
                                            b2 = -0.1), 
                               algorithm = "port", 
                               weights = use_weights),
                           error = function(e) NULL)
      
      if(is.null(mod_port) == TRUE){
        
        mod_port_newstart <- nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                                 data = data_name,
                                 start = list(a = as.numeric(new_starting["a"]), 
                                              b1 = as.numeric(new_starting["b1"]), 
                                              b2 = as.numeric(new_starting["b2"])),
                                 algorithm = "port",
                                 control = list(maxiter = 1000, tol = 1e-05, scaleOffset = 1),
                                 weights = use_weights)
        
        pars <- as.data.frame(t(mod_port_newstart$m$getPars()))
        vcov_out <- as.data.frame(vcov(mod_port_newstart))
        pars$V_A <- vcov_out["a", "a"]
        pars$V_B1 <- vcov_out["b1", "b1"]
        pars$V_B2 <- vcov_out["b2", "b2"]
        pars$C_AB1 <- vcov_out["a", "b1"]
        pars$C_AB2 <- vcov_out["a", "b2"]
        pars$C_B1B2 <- vcov_out["b1", "b2"]
        
        pars$state <- unique(data_name$State)
        pars$algo <- "nl2sol, new start init"        
        
      } else if (is.null(mod_port) == FALSE){
        pars <- as.data.frame(t(mod_port$m$getPars()))
        vcov_out <- as.data.frame(vcov(mod_port))
        pars$V_A <- vcov_out["a", "a"]
        pars$V_B1 <- vcov_out["b1", "b1"]
        pars$V_B2 <- vcov_out["b2", "b2"]
        pars$C_AB1 <- vcov_out["a", "b1"]
        pars$C_AB2 <- vcov_out["a", "b2"]
        pars$C_B1B2 <- vcov_out["b1", "b2"]
        
        pars$state <- unique(data_name$State)
        pars$algo <- "nl2sol"
      }
      
    } else if (is.null(mod) == FALSE){
      pars <- as.data.frame(t(mod$m$getPars()))
      vcov_out <- as.data.frame(vcov(mod))
      pars$V_A <- vcov_out["a", "a"]
      pars$V_B1 <- vcov_out["b1", "b1"]
      pars$V_B2 <- vcov_out["b2", "b2"]
      pars$C_AB1 <- vcov_out["a", "b1"]
      pars$C_AB2 <- vcov_out["a", "b2"]
      pars$C_B1B2 <- vcov_out["b1", "b2"]
      
      pars$state <- unique(data_name$State)
      pars$algo <- "gnr"
    }
    
    
  }
  
  return(pars)

}

get_uncertainty <- function(data_name, weights = TRUE, 
                            weights_column, new_starting, dv){
  
  data_name$outcome <- data_name[, dv]
  data_name$use_weights <- data_name[, weights_column]
  
  if(weights == FALSE){
    mod <- tryCatch(nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                        data = data_name,
                        start = list(a = 1, 
                                     b1 = 2, 
                                     b2 = -0.1)), 
                    error = function(e) NULL)
    
    if(is.null(mod) == TRUE){ 
      
      mod <- tryCatch(nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                          data = data_name,
                          start = list(a = 1, 
                                       b1 = 2, 
                                       b2 = -0.1), 
                          algorithm = "port"),
                      error = function(e) NULL)
      
      pars_out <- mod$m$getPars()
      vcov_out <- vcov(mod)
      
    } else if (is.null(mod) == FALSE){
      pars_out <- mod$m$getPars()
      vcov_out <- vcov(mod)
    }
    
  } else if(weights == TRUE) {
    
    mod <- tryCatch(nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                        data = data_name,
                        start = list(a = 1, 
                                     b1 = 2, 
                                     b2 = -0.1),
                        weights = use_weights), 
                    error = function(e) NULL)
    
    if(is.null(mod) == TRUE){ 
      mod_port <- tryCatch(nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                               data = data_name,
                               start = list(a = 1, 
                                            b1 = 2, 
                                            b2 = -0.1), 
                               algorithm = "port", 
                               weights = use_weights),
                           error = function(e) NULL)
      
      if(is.null(mod_port) == TRUE){
        
        mod_port_newstart <- nls(formula = outcome ~ a + b1 * exp((b2) * Y), 
                                 data = data_name,
                                 start = list(a = as.numeric(new_starting["a"]), 
                                              b1 = as.numeric(new_starting["b1"]), 
                                              b2 = as.numeric(new_starting["b2"])),
                                 algorithm = "port",
                                 control = list(maxiter = 1000, tol = 1e-05, scaleOffset = 1),
                                 weights = use_weights)
        
        pars_out <- mod_port_newstart$m$getPars()
        vcov_out <- vcov(mod_port_newstart)
        
      } else if (is.null(mod_port) == FALSE){
        pars_out <- mod_port$m$getPars()
        vcov_out <- vcov(mod_port)
      }
      
    } else if (is.null(mod) == FALSE){
      pars_out <- mod$m$getPars()
      vcov_out <- vcov(mod)
    }
  
    
}
  
  
  
  set.seed(02143)
  sims <- as.data.frame(MASS::mvrnorm(n = 1000, mu = pars_out, Sigma = vcov_out))
  sims$state <- unique(data_name$state_alpha)
  
  return(sims)
  
}

get_tau <- function(psi, sims_dataframe){
  tau <- 0.7* psi   # this is the starting value of tau before the search algorithm 
  
  tau_j <- matrix(NA, nrow = nrow(sims_dataframe), ncol = 100)
  test_j <- matrix(NA, nrow = nrow(sims_dataframe), ncol = 100)
  tau_new_j <- matrix(NA, nrow(sims_dataframe), ncol = 100)
  
  tau_j[,1] <- tau + .1
  test_j[,1] <- (psi - tau_j[1]) - (sims_dataframe$a+sims_dataframe$b1*exp(sims_dataframe$b2*tau_j[1]))
  tau_j[,1] <- ifelse( test_j[,1] < .1 &  test_j[,1] > 0,  tau_j[1], tau)  
  
  j <- 2 
  for(j in 2:100){
    tau_j[,j]  <- tau_j[, j-1] + j*.1
    test_j[,j] <- (psi - tau_j[,j]) - (sims_dataframe$a+sims_dataframe$b1*exp(sims_dataframe$b2*tau_j[,j] ))
    tau_j[,j] <- ifelse(test_j[,j] < .1 &  test_j[,j] > 0,  tau_j[,j], tau_j[,j-1])  
    j <- j + 1
  }
  
  tau_k <- matrix(NA, nrow = nrow(sims_dataframe), ncol = 100)
  test_k <- matrix(NA, nrow = nrow(sims_dataframe), ncol = 100)
  
  tau_k[,1] <- tau_j[, 100] + .01
  test_k[,1] <- (psi - tau_k[1]) - (sims_dataframe$a+sims_dataframe$b1*exp(sims_dataframe$b2*tau_k[,1]))
  tau_k[,1] <- ifelse( test_k[,1] < .01 &  test_k[,1] > 0,  tau_k[,1], tau_j[, 100])  
  
  k <- 2 
  for(k in 2:100){
    tau_k[,k]  <- tau_k[, k-1] + k*.01
    test_k[,k] <- (psi - tau_k[,k]) - (sims_dataframe$a+sims_dataframe$b1*exp(sims_dataframe$b2*tau_k[,k] ))
    tau_k[,k] <- ifelse(test_k[,k] < .01 &  test_k[,k] > 0,  tau_k[,k], tau_k[,k-1])  
    k <- k + 1
  } 
  
  
  tau_l <- matrix(NA, nrow = nrow(sims_dataframe), ncol = 100)
  test_l <- matrix(NA, nrow = nrow(sims_dataframe), ncol = 100)
  
  tau_l[,1] <- tau_k[, 100] + .001
  test_l[,1] <- (psi - tau_l[1]) - (sims_dataframe$a+sims_dataframe$b1*exp(sims_dataframe$b2*tau_l[,1]))
  tau_l[,1] <- ifelse( test_l[,1] < .001 &  test_l[,1] > 0,  tau_l[,1], tau_k[, 100])  
  
  l <- 2 
  for(l in 2:100){
    tau_l[,l]  <- tau_l[, l-1] + l*.001
    test_l[,l] <- (psi - tau_l[,l]) - (sims_dataframe$a+sims_dataframe$b1*exp(sims_dataframe$b2*tau_l[,l] ))
    tau_l[,l] <- ifelse(test_l[,l] < .001 &  test_l[,l] > 0,  tau_l[,l], tau_l[,l-1])  
    l <- l + 1
  } 
  
  return(tau_l)
  
}

get_r <- function(alpha, b1, b2, tau, psi){
  
  ((alpha * tau + 
      (b1 / b2) * exp(b2 * tau) - 
      (b1 / b2)) + 
     (psi^2 - (psi^2) / 2) - (psi * tau - (tau^2) / 2)) / 
    (psi^2 - (psi^2) / 2)
  
}

combine_all <- function(data, weights_value, set_weights, 
                        starting_values, dv_string, psi_value){
  
  pars <- get_model_parameters(data_name = data, 
                       weights = weights_value, 
                       weights_column = set_weights,
                       new_starting = starting_values, 
                       dv = dv_string)
  colnames(pars) <- paste(colnames(pars), "_Rnls", sep = "")
  
  sims <- get_uncertainty(data_name = data, 
                  weights = weights_value, 
                  weights_column = set_weights,
                  new_starting = starting_values, 
                  dv = dv_string)
  
  pars_sims <- cbind.data.frame(pars, sims)
  
  tau_search <- get_tau(psi = psi_value, 
                           sims_dataframe = pars_sims)
  
  estimates <- cbind.data.frame(pars_sims, tau_search[,100]) %>% 
    group_by(state_Rnls, state) %>% 
    summarize_if(is.numeric, mean) %>% 
    rename(tau = `tau_search[, 100]`)
  
  estimates$r_val <- get_r(alpha = estimates$a_Rnls, 
                       b1 = estimates$b1_Rnls,
                       b2 = estimates$b2_Rnls, 
                       tau = estimates$tau, 
                       psi = psi_value)
  
  return(estimates)
  
}



add_weights_transfers <- function(dataset, set_portfolio, set_level, set_year, state_specific){
  
  working_df <- dataset
  
  prop_served <- cfa_lift$proportion_inneed_served[cfa_lift$portfolio == set_portfolio & 
                                                     cfa_lift$level == set_level & 
                                                     cfa_lift$year == set_year]
  transfer_amounts <- cfa_lift$percapitaben[cfa_lift$portfolio == set_portfolio & 
                                              cfa_lift$level == set_level & 
                                              cfa_lift$year == set_year] / 1000
  
  weights_name <- paste("weights", set_portfolio, set_level, set_year, sep = "_")
  transfers_name <- paste("new_transfers", set_portfolio, set_level, set_year, sep = "_")
  
  if(state_specific == FALSE){
    
    working_df[, weights_name] <- ifelse(working_df$pair_id == 1, working_df$asecwt * prop_served, 
                                         ifelse(working_df$pair_id == 2, working_df$asecwt * (1 - prop_served), 
                                                working_df$asecwt))
    
    working_df[, transfers_name] <- ifelse(working_df$pair_id == 1, working_df$totalTransfers + transfer_amounts * working_df$hhsize, 
                                           working_df$totalTransfers)
    
  } else if(state_specific == TRUE){
    
    working_df[, weights_name] <- ifelse(working_df$pair_id == 1, working_df$asecwt * prop_served, 
                                         ifelse(working_df$pair_id == 2, working_df$asecwt * (1 - prop_served), 
                                                working_df$asecwt))
    working_df[, weights_name] <- ifelse(working_df$State == set_level, working_df[, weights_name], NA)
    
    working_df[, transfers_name] <- ifelse(working_df$pair_id == 1, working_df$totalTransfers + transfer_amounts * working_df$hhsize, 
                                           working_df$totalTransfers)  
    working_df[, transfers_name] <- ifelse(working_df$State == set_level, working_df[, transfers_name], NA)
    
  } 
  
  working_df <- working_df[! is.na(working_df[, weights_name]),]
  
  return(working_df)
  
}

cfa_colors <- cbind.data.frame(color = c("dark_purple", "med_purple", "light_purple", 
                                         "dark_red", "med_red", "light_red", 
                                         "dark_lilac", "med_lilac", "light_lilac", 
                                         "dark_yellow", "med_yellow", "light_yellow", 
                                         "dark_green", "med_green", "light_green", 
                                         "dark_cream", "med_cream", "light_cream", 
                                         "med_grey"), 
                               hex = c("#2B1A78", "#5650BE", "#C2C0E8", 
                                                "#AF121D", "#EF6C75", "#F9C8CB", 
                                                "#A1B4EA", "#C9D4F3", "#E6EBF9", 
                                                "#FFB446", "#FFD699", "#FFF3E0", 
                                                "#006152", "#00AD93", "#E2F9F6", 
                                                "#E9CCBE", "#F7EDE8", "#F5F0ED", 
                                                "#F3F3F3"))






