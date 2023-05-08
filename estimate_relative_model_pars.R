
# Before running this script, run: 
# 1. Cleaning Stata file 
# 2. make_funcs.R
# 3. calc_percap.R
# 4. get_psis.R

options(digits=8)

state_mods_rel_TMP <- lapply(loop_states, function(state){
  st_data <- cps[cps$state_alpha == state,]
  set_psi <- col_st$col[col_st$State == trimws(toupper(unique(st_data$State)))]
  
  ests <- combine_all(data = st_data, 
                      weights_value = FALSE, 
                      starting_values = NA, 
                      set_weights = "asecwt",
                      dv_string = "totalTransfers", 
                      psi_value = set_psi / 1000)
})

state_mods_rel <- do.call(rbind, state_mods_rel_TMP)

state_mods_rel_weights_TMP <- do.call(rbind, lapply(loop_states, function(state){
  st_data <- cps[cps$state_alpha == state,]
  start_pars <- state_mods_rel[state_mods_rel$state_Rnls == unique(st_data$State),]
  set_psi <- col_st$col[col_st$State == trimws(toupper(unique(st_data$State)))]
  
  ests <- combine_all(data = st_data, 
                      weights_value = TRUE, 
                      set_weights = "asecwt",
                      starting_values = start_pars, 
                      dv_string = "totalTransfers", 
                      psi_value = set_psi / 1000)
}))

state_mods_rel_weights <- as.data.frame(do.call(cbind, state_mods_rel_weights_TMP))
state_mods_rel_weights$r_val <- as.numeric(as.character(unlist(state_mods_rel_weights$r_val)))


###### Get National Estimates ######

cps_national <- cps
cps_national$State <- "National"
cps_national$state <- "National"
cps_national$state_alpha <- "National"

national_rel <- combine_all(data = cps_national, 
                        weights_value = TRUE, 
                        starting_values = NA, 
                        set_weights = "asecwt",
                        dv_string = "totalTransfers", 
                        psi_value = as.numeric(national_col) / 1000)


#############################################################################
#############################################################################
################## REPLICATE WITH CFA'S ADD-ON ##############################
#############################################################################
#############################################################################

loop_names <- names(new_copies_TMP)
allev_with_cfa_rel_TMP <- lapply(1:length(loop_names), function(x){
  
  id <- loop_names[x]
  call_weights <- paste("weights", id, sep = "_")
  call_transfers <- paste("new_transfers", id, sep = "_")
  
  use_data <- do.call(rbind, new_copies_TMP[x])
  use_data$model_id <- id
  use_data <- separate(data = use_data, col = model_id, into = c("portfolio", "level", "year"), sep = "\\_")
  
  if (grepl("National", id) == TRUE){
    
    d_national <- use_data
    d_national$State <- "National"
    d_national$state <- "National"
    d_national$state_alpha <- "National"
    
    national <- combine_all(data = d_national, 
                            weights_value = TRUE, 
                            starting_values = NA, 
                            set_weights = call_weights,
                            dv_string = call_transfers,
                            psi_value = as.numeric(national_col) / 1000)
    
    national$outcome <- id
    return(national)
    
  } else if (grepl("National", id) == FALSE){
    
    start_pars <- state_mods_rel[state_mods_rel$state_Rnls == unique(use_data$State),]
    set_psi <- col_st$col[col_st$State == trimws(toupper(unique(use_data$State)))]
    
    ests <- combine_all(data = use_data, 
                        weights_value = TRUE, 
                        set_weights = call_weights,
                        starting_values = start_pars, 
                        dv_string = call_transfers,
                        psi_value = set_psi / 1000)
    ests$outcome <- id
    return(ests)
  }
})


allev_with_rel_cfa <- do.call(rbind, allev_with_cfa_rel_TMP) %>% 
  dplyr::select(state_Rnls, state, r_val, outcome) %>% 
  separate(data = ., col = outcome, into = c("portfolio", "level", "year"), sep = "\\_") %>% 
  mutate(port_year = paste(portfolio, year, sep = "_")) %>% 
  dplyr::select(-c(level, year, portfolio)) %>% 
  pivot_wider(names_from = port_year, values_from = r_val)

baseline_models_rel <- rbind.data.frame(state_mods_rel_weights, national_rel) %>% 
  dplyr::select(-c(V_A_Rnls, V_B1_Rnls, V_B2_Rnls, C_AB1_Rnls, C_AB2_Rnls, C_B1B2_Rnls)) %>% 
  mutate(across(c(a_Rnls, b1_Rnls, b2_Rnls, a, b1, b2, tau, r_val), as.numeric))

all_models_rel <- left_join(allev_with_rel_cfa, baseline_models_rel) %>% 
  pivot_longer(-c(state_Rnls, state, a_Rnls, b1_Rnls, b2_Rnls, tau, r_val, a, b1, b2)) %>% 
  filter(! is.na(value)) %>% 
  rename(cfa_r = value, 
         portfolio_id = name, 
         baseline_r = r_val) %>% 
  mutate(difference = (cfa_r - baseline_r) * 10000) %>% 
  dplyr::select(-cfa_r) %>% 
  pivot_wider(names_from = portfolio_id, values_from = difference)


View(baseline_models_rel[, names(baseline_models_rel) %in% c("state_Rnls", "tau", "r_val")])
View(all_models_rel[, names(all_models_rel) %in% c("state_Rnls", "tax_2021", "tax_2022", "safety net_2022", "safety net_2021", "total_2022", "total_2021")])





