
# Before running this script, run: 
# 1. Cleaning Stata file 
# 2. make_funcs.R
# 3. calc_percap.R
# 4. get_psis.R

loop_states <- as.list(unique(cps$state_al))

state_mods_TMP <- lapply(loop_states, function(state){
  st_data <- cps[cps$state_al == state,]
  ests <- combine_all(data = st_data, 
                    weights_value = FALSE, 
                    starting_values = NA, 
                    dv_string = "totalTransfers", 
                    psi_value = set_psi_national / 1000)
})

state_mods <- do.call(rbind, state_mods_TMP)

state_mods_weights_TMP <- do.call(rbind, lapply(loop_states, function(state){
  st_data <- cps[cps$state_al == state,]
  start_pars <- state_mods[state_mods$state_Rnls == unique(st_data$State),]
  
  ests <- combine_all(data = st_data, 
                      weights_value = TRUE, 
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
  scale_color_manual(values = c(dark_lilac, dark_purple)) + 
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
                           dv_string = "totalTransfers", 
                        psi_value = set_psi_national / 1000)




#############################################################################
#############################################################################
################## REPLICATE WITH CFA'S ADD-ON ##############################
#############################################################################
#############################################################################

# First, run "calc_percap.R" to get the right outcome variables in there 

# Data is cps_cfa
# variables are totalTransfers_cfa22

national_outcomes <- colnames(cps_cfa)[grepl("totalTransfers_", colnames(cps_cfa))]

cps_cfa$State <- "National"
cps_cfa$state <- "National"
cps_cfa$state_alpha <- "National"

allcfa22 <- combine_all(data = cps_cfa, 
                        weights_value = TRUE, 
                        starting_values = NA, 
                        dv_string = "totalTransfers_cfa22", 
                        psi_value = set_psi_national / 1000) %>% 
  mutate(model = "All CfA", 
         year = 2022)

allcfa21 <- combine_all(data = cps_cfa, 
                        weights_value = TRUE, 
                        starting_values = NA, 
                        dv_string = "totalTransfers_cfa21", 
                        psi_value = set_psi_national / 1000) %>% 
  mutate(model = "All CfA", 
         year = 2021)

nsn22 <- combine_all(data = cps_national, 
                     weights_value = TRUE, 
                     starting_values = NA, 
                     dv_string = "totalTransfers_sn22", 
                     psi_value = 19.790) %>% 
  mutate(model = "Safety Net", 
         year = 2022)

nsn21 <- combine_all(data = cps_national, 
                     weights_value = TRUE, 
                     starting_values = NA, 
                     dv_string = "totalTransfers_csn21", 
                     psi_value = 19.790) %>% 
  mutate(model = "Safety Net", 
         year = 2021)

ntx22 <- combine_all(data = cps_national, 
                     weights_value = TRUE, 
                     starting_values = NA, 
                     dv_string = "new_lift_tx", 
                     psi_value = 19.790) %>% 
  mutate(model = "Tax Portfolio", 
         year = 2022)

ntx21 <- combine_all(data = cps_national, 
                     weights_value = TRUE, 
                     starting_values = NA, 
                     dv_string = "new_lift_tx21", 
                     psi_value = 19.790) %>% 
  mutate(model = "Tax Portfolio", 
         year = 2021)

ntx20 <- combine_all(data = cps_national, 
                     weights_value = TRUE, 
                     starting_values = NA, 
                     dv_string = "new_lift_tx20", 
                     psi_value = 19.790) %>% 
  mutate(model = "Tax Portfolio", 
         year = 2020)

national_out <- rbind.data.frame(allcfa20, allcfa21, allcfa22, 
                                 nsn22, nsn21, nsn20, 
                                 ntx22, ntx21, ntx20) 
national_out$lift <- (national_out$cfa_r - baseline_estimates_weights_national$R) * 100
national_out

ggplot(national_out, aes(x = year, y = lift, fill = model)) + geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "National Poverty Alleviation Metrics", x = "Year", y = "Lift\n(in % Points)", fill = "Portfolio") + 
  scale_fill_taylor() + 
  theme_bw() + 
  ylim(0, 1) + 
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        legend.position = "bottom", 
        legend.text = element_text(size = 18), 
        legend.title = element_text(size = 20), 
        plot.title = element_text(size = 30))



############################################################
################ state level results #######################
############################################################

cpstx21_TMP <- lapply(loop_states, function(state){
  
  st_data <- cps_tx[cps_tx$state_al == state,]
  start_pars <- state_mods[state_mods$state_Rnls == unique(st_data$State),]
  
  ests <- combine_all(data = st_data, 
                      weights_value = TRUE, 
                      starting_values = start_pars, 
                      dv_string = "newlift_st_tx21", 
                      psi_value = 19.790)
})

cpstx21 <- do.call(rbind, cpstx21_TMP) %>% 
  left_join(., baseline_estimates_weights[, names(baseline_estimates_weights) %in% c("state_Rnls", "R")], by = "state_Rnls") 

cpstx21$lift <- (cpstx21$cfa_r - cpstx21$R) * 100


cpstx20_TMP <- lapply(loop_states, function(state){
  
  st_data <- cps_tx[cps_tx$state_al == state,]
  start_pars <- state_mods[state_mods$state_Rnls == unique(st_data$State),]
  
  ests <- combine_all(data = st_data, 
                      weights_value = TRUE, 
                      starting_values = start_pars, 
                      dv_string = "newlift_st_tx20", 
                      psi_value = 19.790)
})

cpstx20 <- do.call(rbind, cpstx20_TMP) %>% 
  left_join(., baseline_estimates_weights[, names(baseline_estimates_weights) %in% c("state_Rnls", "R")], by = "state_Rnls") 

cpstx20$lift <- (cpstx20$cfa_r - cpstx20$R) * 100



############ MN SHIBA BENEFITS ##########################

n.m21 <- combine_all(data = cpsmn, 
                     weights_value = TRUE, 
                     starting_values = NA, 
                     dv_string = "mn21", 
                     psi_value = 19.790)
n.m21.lift <- n.m21$cfa_r - as.numeric(baseline_estimates_weights[baseline_estimates_weights$state_Rnls == "Minnesota", "R"])
n.m21.lift * 100


n.m22 <- combine_all(data = cpsmn, 
                     weights_value = TRUE, 
                     starting_values = NA, 
                     dv_string = "mn22", 
                     psi_value = 19.790)
n.m22.lift <- n.m22$cfa_r - as.numeric(baseline_estimates_weights[baseline_estimates_weights$state_Rnls == "Minnesota", "R"])
n.m22.lift * 100




############ CALFRESH BENEFITS ##########################

n.ca21 <- combine_all(data = cpsca, 
                     weights_value = TRUE, 
                     starting_values = NA, 
                     dv_string = "ca_new21", 
                     psi_value = 19.790)
n.ca21.lift <- n.ca21$cfa_r - as.numeric(baseline_estimates_weights[baseline_estimates_weights$state_Rnls == "California", "R"])
n.ca21.lift * 100


n.ca22 <- combine_all(data = cpsca, 
                     weights_value = TRUE, 
                     starting_values = NA, 
                     dv_string = "ca_new22", 
                     psi_value = 19.790)
n.ca22.lift <- n.ca22$cfa_r - as.numeric(baseline_estimates_weights[baseline_estimates_weights$state_Rnls == "California", "R"])
n.ca22.lift * 100




