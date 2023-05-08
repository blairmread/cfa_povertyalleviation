
# Before running this script, run: 
# 1. Cleaning Stata file 
# 2. make_funcs.R
# 3. calc_percap.R
# 4. get_psis.R

options(digits=8)

loop_states <- as.list(unique(cps$state_al))

state_mods_TMP <- lapply(loop_states, function(state){
  st_data <- cps[cps$state_al == state,]
  ests <- combine_all(data = st_data, 
                      weights_value = FALSE, 
                      starting_values = NA, 
                      set_weights = "asecwt",
                      dv_string = "totalTransfers", 
                      psi_value = set_psi_national / 1000)
})

state_mods <- do.call(rbind, state_mods_TMP)

state_mods_weights_TMP <- do.call(rbind, lapply(loop_states, function(state){
  st_data <- cps[cps$state_al == state,]
  start_pars <- state_mods[state_mods$state_Rnls == unique(st_data$State),]
  
  ests <- combine_all(data = st_data, 
                      weights_value = TRUE, 
                      set_weights = "asecwt",
                      starting_values = start_pars, 
                      dv_string = "totalTransfers", 
                      psi_value = set_psi_national / 1000)
}))

state_mods_weights <- as.data.frame(do.call(cbind, state_mods_weights_TMP))
state_mods_weights$r_val <- as.numeric(as.character(unlist(state_mods_weights$r_val)))

sub_states <- c("California", "New York", "New Mexico", "Minnesota", 
                "Louisiana", "Connecticut", "Maryland", "Illinois", 
                "Colorado", "Alabama", "Arkansas", 
                "New Jersey", "Hawaii", "Nevada", 
                "Virginia", "Idaho", "South Carolina")

state_sub <- state_mods_weights %>% 
  filter(state_Rnls %in% sub_states) %>% 
  mutate(cfa = ifelse(state_Rnls %in% c("Alabama", "Arkansas", "New Jersey", 
                                        "Nevada", "Virginia", "Idaho", "South Carolina"), "No", "Yes"))


ggplot(state_sub, aes(x = reorder(state_Rnls, r_val), y = r_val, color = cfa)) + geom_point(size = 4) + 
  geom_segment( aes(x = state_Rnls, xend = state_Rnls, y = 0, yend = r_val), lwd = 1.5) + 
  coord_flip() + 
  scale_color_manual(values = c(cfa_colors$hex[cfa_colors$color == "dark_lilac"], 
                                cfa_colors$hex[cfa_colors$color == "dark_purple"])) + 
  theme_bw() + 
  labs(y = "Poverty Alleviation Rate", x = "", color = "CfA Cohort") + 
  theme(axis.text.x = element_text(size = 22), 
        axis.title.x = element_text(size = 22), 
        axis.text.y = element_text(size = 22), 
        legend.position = "top", 
        legend.text = element_text(size = 22), 
        legend.title = element_text(size = 22))
###### Get National Estimates ######

cps_national <- cps
cps_national$State <- "National"
cps_national$state <- "National"
cps_national$state_alpha <- "National"

national <- combine_all(data = cps_national, 
                        weights_value = TRUE, 
                        starting_values = NA, 
                        set_weights = "asecwt",
                        dv_string = "totalTransfers", 
                        psi_value = set_psi_national / 1000)




#############################################################################
#############################################################################
################## REPLICATE WITH CFA'S ADD-ON ##############################
#############################################################################
#############################################################################

loop_names <- names(new_copies_TMP)
allev_with_cfa_TMP <- lapply(1:length(loop_names), function(x){
  
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
                            psi_value = set_psi_national / 1000)
    
    national$outcome <- id
    return(national)
    
  } else if (grepl("National", id) == FALSE){
    
    start_pars <- state_mods[state_mods$state_Rnls == unique(use_data$State),]
    
    ests <- combine_all(data = use_data, 
                        weights_value = TRUE, 
                        set_weights = call_weights,
                        starting_values = start_pars, 
                        dv_string = call_transfers,
                        psi_value = set_psi_national / 1000)
    ests$outcome <- id
    return(ests)
  }
})


allev_with_cfa <- do.call(rbind, allev_with_cfa_TMP) %>% 
  dplyr::select(state_Rnls, state, r_val, outcome) %>% 
  separate(data = ., col = outcome, into = c("portfolio", "level", "year"), sep = "\\_") %>% 
  mutate(port_year = paste(portfolio, year, sep = "_")) %>% 
  dplyr::select(-c(level, year, portfolio)) %>% 
  pivot_wider(names_from = port_year, values_from = r_val)

baseline_models <- rbind.data.frame(state_mods_weights, national) %>% 
  dplyr::select(-c(V_A_Rnls, V_B1_Rnls, V_B2_Rnls, C_AB1_Rnls, C_AB2_Rnls, C_B1B2_Rnls)) %>% 
  mutate(across(c(a_Rnls, b1_Rnls, b2_Rnls, a, b1, b2, tau, r_val), as.numeric))

model_pars_baseline <- baseline_models[, names(baseline_models) %in% c("state_Rnls", "a_Rnls", "b1_Rnls", "b2_Rnls")] %>% 
  pivot_longer(-state_Rnls) %>% 
  mutate(name = gsub("_Rnls", "", name)) %>% 
  rename(value_estimated = value)

test_baseline <- baseline_models[, names(baseline_models) %in% c("state_Rnls", "a", "b1", "b2")] %>% 
  pivot_longer(-state_Rnls) %>% 
  rename(value_simulated = value) %>% 
  left_join(., model_pars_baseline)

test_baseline$problem_at_all <- ifelse(test_baseline$state_Rnls %in% 
                                         c("Hawaii", "Illinois", 
                                           "Indiana", "Iowa", 
                                           "Montana", "New Hampshire", 
                                           "New Jersey", "North Dakota", 
                                           "Rhode Island", "South Dakota", 
                                           "Vermont", "Washington"), 1, 0)

test_baseline$value_simulated <- as.numeric(as.character(unlist(test_baseline$value_simulated)))
test_baseline$value_estimated <- as.numeric(as.character(unlist(test_baseline$value_estimated)))
test_baseline$difference <- test_baseline$value_simulated - test_baseline$value_estimated

ggplot(test_baseline, aes(x = value_estimated, y = value_simulated, color = as.factor(problem_at_all))) + 
  geom_point() + 
  geom_abline(slope=1) + 
  facet_wrap(~ name, scales = "free") + 
  theme_bw()

ggplot(test_baseline[test_baseline$name == "a",], aes(x = reorder(state_Rnls, difference), y = difference, fill = as.factor(problem_at_all))) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  coord_flip() + 
  labs(title = "Difference for Alpha Estimate")

ggplot(test_baseline[test_baseline$name == "b1",], aes(x = reorder(state_Rnls, difference), y = difference, fill = as.factor(problem_at_all))) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  coord_flip() + 
  labs(title = "Difference for B1 Estimate")

ggplot(test_baseline[test_baseline$name == "b2",], aes(x = reorder(state_Rnls, difference), y = difference, fill = as.factor(problem_at_all))) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  coord_flip() + 
  labs(title = "Difference for B2 Estimate")

all_models <- left_join(allev_with_cfa, baseline_models) %>% 
  pivot_longer(-c(state_Rnls, state, a_Rnls, b1_Rnls, b2_Rnls, tau, r_val, a, b1, b2)) %>% 
  filter(! is.na(value)) %>% 
  rename(cfa_r = value, 
         portfolio_id = name, 
         baseline_r = r_val) %>% 
  mutate(difference = (cfa_r - baseline_r) * 10000) %>% 
  dplyr::select(-cfa_r) %>% 
  pivot_wider(names_from = portfolio_id, values_from = difference)







check_values <- do.call(rbind, allev_with_cfa_TMP) 

neg_list <- all_models %>% 
  filter(tax_2021 < 0 | tax_2022 < 0) %>% 
  mutate(negative_21 = ifelse(tax_2021 < 0, 1, 0), 
         negative_22 = ifelse(tax_2022 < 0, 1, 0))





neg_outcomes_cfa <- check_values[check_values$state_Rnls %in% neg_list$state_Rnls, ]
neg_outcomes_baseline <- baseline_models[baseline_models$state_Rnls %in% neg_list$state_Rnls, ]
colnames(neg_outcomes_baseline) <- paste(colnames(neg_outcomes_baseline), "baseline", sep = "_")

compare_outcomes <- left_join(neg_outcomes_cfa, neg_outcomes_baseline, by = c("state_Rnls" = "state_Rnls_baseline"))

# state_Rnls     tax_2021  tax_2022 negative_21 negative_22
# <chr>             <dbl>     <dbl>       <dbl>       <dbl>
# Hawaii        -0.0368   -0.00638            1           1
# Illinois       0.00189  -0.00126            0           1
# Indiana        0.00240  -0.000180           0           1
# Iowa          -0.000444 -0.000151           1           1
# Montana       -0.00507  -0.00859            1           1
# New Hampshire -0.0173   -0.0182             1           1
# New Jersey    -0.0144   -0.0160             1           1
# North Dakota   0.000695 -0.00142            0           1
# Rhode Island  -0.0118   -0.0123             1           1
# South Dakota   0.00116  -0.00313            0           1
# Vermont       -0.00404  -0.00476            1           1
# Washington     0.00483  -0.00490            0           1

all_names <- c(paste("tax_", all_models$state_Rnls, "_2021", sep = ""), paste("tax_", all_models$state_Rnls, "_2022", sep = ""))
neg_names <- c(paste("tax_", neg_list$state_Rnls, "_2021", sep = ""), paste("tax_", neg_list$state_Rnls, "_2022", sep = ""))

comp_valuesTMP <- lapply(as.list(all_names), function(x){
  get_d <- do.call(rbind, new_copies_TMP[x])
  transfers_name <- paste("new_transfers", x, sep = "_")
  
  get_d %>% 
    group_by(pair_id) %>% 
    summarize(sum_transfers_new = sum(get(transfers_name)), 
              sum_transfers_old = sum(totalTransfers)) %>% 
    mutate(transfers_id = x)
})

comp_values <- do.call(rbind, comp_valuesTMP) %>% 
  mutate(compare = (sum_transfers_new == sum_transfers_old),
         difference = sum_transfers_new - sum_transfers_old) %>% 
  separate(data = ., col = transfers_id, into = c("portfolio", "level", "year"), sep = "\\_") 

table(comp_values$pair_id, comp_values$compare) # This works 

comp_values$problem_21 <-  ifelse(comp_values$level %in% 
                                    c("Hawaii", "Iowa", 
                                      "Montana", "New Hampshire", 
                                      "New Jersey", "Rhode Island", 
                                      "Vermont"), 1, 0)
comp_values$problem_22 <- ifelse(comp_values$level %in% 
                                   c("Hawaii", "Illinois", 
                                     "Indiana", "Iowa", 
                                     "Montana", "New Hampshire", 
                                     "New Jersey", "North Dakota", 
                                     "Rhode Island", "South Dakota", 
                                     "Vermont", "Washington"), 1, 0)

comp_values$problem_obs <- ifelse(comp_values$year == 2021 & comp_values$problem_21 == 1, 1, 
                                  ifelse(comp_values$year == 2022 & comp_values$problem_22 == 1, 1, 0))

ggplot(comp_values[! comp_values$level == "National",], aes(x = reorder(level, difference), y = difference, fill = as.factor(problem_obs))) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ year) + 
  labs(y = "Difference in Transfers") + 
  coord_flip() + 
  theme_bw()


compare_pars_base <- baseline_models
colnames(compare_pars_base) <- paste(colnames(compare_pars_base), "_baseline", sep = "")

comp_pars <- check_values %>% 
  dplyr::select(-c(V_A_Rnls, V_B1_Rnls, V_B2_Rnls, C_AB1_Rnls, C_AB2_Rnls, C_B1B2_Rnls)) %>% 
  left_join(., compare_pars_base, by = c("state_Rnls" = "state_Rnls_baseline", "state" = "state_baseline")) %>% 
  dplyr::select(-c(r_val, r_val_baseline, a, b1, b2, a_baseline, b1_baseline, b2_baseline)) %>%  
  pivot_longer(-c(state_Rnls, state, outcome)) %>% 
  mutate(model = ifelse(grepl("_baseline", name) == TRUE, "Baseline", "CfA"), 
         name = gsub("_Rnls", "", name),
         name = gsub("_baseline", "", name)) %>% 
  pivot_wider(names_from = model, values_from = value) %>% 
  separate(data = ., col = outcome, into = c("portfolio", "level", "year"), sep = "\\_") 

comp_pars$problem_21 <-  ifelse(comp_pars$level %in% 
                                  c("Hawaii", "Iowa", 
                                    "Montana", "New Hampshire", 
                                    "New Jersey", "Rhode Island", 
                                    "Vermont"), 1, 0)
comp_pars$problem_22 <- ifelse(comp_pars$level %in% 
                                 c("Hawaii", "Illinois", 
                                   "Indiana", "Iowa", 
                                   "Montana", "New Hampshire", 
                                   "New Jersey", "North Dakota", 
                                   "Rhode Island", "South Dakota", 
                                   "Vermont", "Washington"), 1, 0)

comp_pars$problem_obs <- ifelse(comp_pars$year == 2021 & comp_pars$problem_21 == 1, 1, 
                                ifelse(comp_pars$year == 2022 & comp_pars$problem_22 == 1, 1, 0))


ggplot(comp_pars, aes(x = CfA, y = Baseline, color = as.factor(problem_obs))) + 
  geom_point() + 
  geom_abline(slope=1) + 
  facet_wrap(~ name, scales = "free") + 
  theme_bw()

comp_problems_only <- comp_pars[comp_pars$problem_obs == 1,]
comp_pars$diff_dir <- comp_pars$CfA > comp_pars$Baseline


