
# This code calculates the CfA per capita improvements 
# We do this by: 
  # Recording the dollar amounts of benefits delivered and th number of people served by CfA per year and per portfolio 
  # Calculate the % of Americans who receive any benefits at all, based on the CPS-ASEC data 

library(readxl)
options(scipen = 100, digits = 4)


######################################################################################################
############################# Calculate CfA's lift ###################################################
######################################################################################################

# Step 1: Import CfA's Performance Numbers 

# Manually import the numbers from the Board Metrics and Metabase
cfa_national <- cbind.data.frame(portfolio = rep(c("safety net", "tax", "total"), times = 3), 
                              year = rep(c(2022, 2021, 2020), each = 3), 
                              benefits_delivered = c(2974347206, 370780052, 2974347206 + 370780052, 
                                                     2825646276, 787368213, 2825646276 + 787368213, 
                                                     2505213759, 62612974, 2505213759 + 62612974), 
                              people_helped = c(3853243, 277625, 3853243 + 277625,
                                                3234406, 590086, 3234406 + 590086,
                                                6046485, 488249, 6046485 + 488249), 
                              level = rep("National", length(rep(c("safety net", "tax", "total"), times = 3))))

cfa_sn_state <- cbind.data.frame(level = rep(c("Minnesota", "California"), each = 2), 
                                 year = rep(c(2022, 2021), times = 2), 
                                 benefits_delivered = c(90803378 + 89044278	+ 101904150,
                                                        9399997 + 12127116 + 18581165 + 35654598, 
                                                        704453200 + 644679200 + 702675600 + 640787400, 
                                                        635291800 + 600091800 + 848119800 + 666380000), 
                                 portfolio = rep("safety net", length(rep(c("Minnesota", "California"), each = 2))),
                                 people_helped = c(648698, 148102, 3204545, 3086304))

state_ids <- cps %>% 
  dplyr::select(state_alpha, State) %>% 
  distinct(., .keep_all = TRUE)

cfa_tax_state <- read.csv("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/new_taxdata.csv") %>% 
  dplyr::select(-X) %>% 
  filter(address_state %in% unique(cps$state_alpha)) %>% 
  rename(state_alpha = address_state, 
         year = impact_year, 
         people_helped = n_people,
         benefits_delivered = total_est_refund_amount) %>% 
  mutate(portfolio = "tax") %>% 
  left_join(., state_ids) %>% 
  rename(level = State) %>% 
  dplyr::select(-state_alpha)

cfa_all <- rbind.data.frame(cfa_national, cfa_sn_state, cfa_tax_state)

rm(cfa_national); rm(cfa_sn_state); rm(state_ids); rm(cfa_tax_state)
  # Clean things up a little bit 

ggplot(cfa_all[cfa_all$level == "National",], aes(x = portfolio, y = benefits_delivered / 100000, fill = as.character(year))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + 
  scale_fill_manual(values = c(cfa_colors$hex[cfa_colors$color == "dark_lilac"], 
                               cfa_colors$hex[cfa_colors$color == "med_lilac"], 
                               cfa_colors$hex[cfa_colors$color == "dark_purple"])) + 
  scale_x_discrete(labels=c("safety net" = "Safety Net", "tax" = "Tax",
                            "total" = "Total")) + 
  labs(y = "Benefits Amounts\n(100,000s USD)", x = "", fill = "Year") + 
  theme(axis.text.x = element_text(size = 26), 
        axis.title.x = element_text(size = 26), 
        axis.text.y = element_text(size = 26), 
        axis.title.y = element_text(size = 26),
        legend.position = "top", 
        legend.text = element_text(size = 26), 
        legend.title = element_text(size = 26))


######### Calculate the State and National Populations ############ 
# Step 2: Read in information about the national and state-level population 

cps$on_benefits <- ifelse(cps$totalTransfers > 0, 1, ifelse(is.na(cps$totalTransfers), NA, 0))
benefits_mean <- weighted.mean(cps$on_benefits, cps$asecwt)
# Calculated the weighted mean of how many people in the US receive any benefits at all 

us_population <- 333287557
# Record the US population (from Google) 

state_popTMP <- readxl::read_excel("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/NST-EST2022-POP.xlsx")
colnames(state_popTMP) <- c("State", "Apr2020", "Jul2020", "2021", "2022")

state_popTMP <- state_popTMP %>% 
  filter(grepl(".", State), 
         ! grepl("Annual", State), 
         ! grepl("Geographic", State), 
         ! grepl("Note", State), 
         ! grepl("Suggested", State), 
         ! grepl("Source", State), 
         ! grepl("Release", State)) %>% 
  filter(! State %in% c("United States", "Northeast", "Midwest", "South", "West", ".Puerto Rico", ".District of Columbia")) %>% 
  dplyr::select(c(State, Apr2020))

stids <- cps %>% 
  group_by(state_alpha) %>% 
  mutate(bens_yes = ifelse(totalTransfers == 0, 0, 1), 
         bens_mean = mean(bens_yes, na.rm=T)) %>% 
  dplyr::select(c(state_alpha, State, bens_mean)) %>% 
  distinct(., .keep_all = TRUE) 

state_popTMP$clState <- substr(state_popTMP$State, 2, nchar(state_popTMP$State)) 
state_popTMP.2 <- rbind.data.frame(state_popTMP, 
                                   c(".National", us_population, "National"))

######### Assign CfA amounts for each person in the CPS/ASEC sample ############ 
# Step 3: Figure out the per capita amounts of benefits that CfA unlocked
# Step 4: Assign those ammounts to households in the CPS-ASEC sample 

cfa_lift <- state_popTMP.2 %>% 
  dplyr::select(-State) %>% 
  rename(level = clState, 
         total_pop = Apr2020) %>% 
  right_join(., cfa_all) %>% 
  mutate(total_pop = as.numeric(as.character(unlist(total_pop))), 
         proportion_inneed_served = people_helped / (total_pop * benefits_mean), 
         percapitaben = benefits_delivered / (total_pop * benefits_mean))
  # proportion_inneed_served = how much of the benefits-receiving population did CfA reach? 
  # percapitaben = if CfA reached everyone who received benefits, how much would they have given them? 


cps_forpc <- cps %>% 
  mutate(hhsize = ifelse(mstat == 2, nchild + 2, 
                         nchild + 1)) %>% 
  dplyr::select(c(year, asecwt, State, state_alpha, totalTransfers, Y, depx, hhsize, on_benefits))
  # Make a new (and smaller) DF that only includes the questions we need to estimate the models 

nonclients <- cps_forpc[cps_forpc$on_benefits != 1, ]
nonclients$pair_id <- 3
nonclients$unique_id <- NA
  # Make a df for non-clients and hold them here 
  # Make it compatible with the clients DF by adding the necessary ID fields 

clients <- cps_forpc[cps_forpc$on_benefits == 1,]
clients$unique_id <- seq(1, nrow(clients), by = 1)
  # Split the sample up into those who received any benefits, and those who did not receive any benefits 
  # This is because we will only "award" CfA benefits to those who received benefits in the sample
  # Then, create a unique ID because we will "duplicate" these households into one partition that received benefits 
    # and one that did not 

clients_cfa <- rbind.data.frame(clients, clients) 
clients_cfa <- clients_cfa[order(clients_cfa$unique_id),]
clients_cfa$pair_id <- rep(c(1, 2), times = nrow(clients))
  # Make two identical data frames and sort by ID so each member of the pair appears together 
  # Assign each member of the pair an ID -- 1 or 2 

cps_cfa <- rbind.data.frame(clients_cfa, nonclients)
# In this dataframe, if "pairid" = 1, you received CfA's benefits & government benefits 
# If pairid = 2, you only received government benefits 
# If pairid = 3, you received neither 

get_queries <- cfa_lift %>% 
  dplyr::select(c(portfolio, level, year)) %>% 
  distinct(., .keep_all = TRUE) %>% 
  mutate(specific = ifelse(level == "National", FALSE, TRUE)) %>% 
  filter(! year %in% c(2020, 2023))
  # This gives you all the different inputs you need for the function to work 

new_copies_TMP <- lapply(as.list(1:nrow(get_queries)), function(x){
  run_func <- add_weights_transfers(dataset = cps_cfa, 
                                    set_portfolio = as.character(get_queries[x, "portfolio"]), 
                                    set_level = as.character(get_queries[x, "level"]), 
                                    set_year = as.character(get_queries[x, "year"]), 
                                    state_specific = as.character(get_queries[x, "specific"]))
  
  return(run_func)
})
  # This gives you a new dataframe with the variables that we'll need to re-estimate 
    # the functions with the "plus CfA" transfers amount 

names(new_copies_TMP) <- paste(get_queries$portfolio, get_queries$level, get_queries$year, sep = "_")
  # Save the names so we can back out what we're looking at with the 

gc()
rm(state_popTMP); rm(state_popTMP.2); rm(stids)
rm(clients); rm(clients_cfa); rm(nonclients)
rm(cps_forpc)
rm(cfa_all)






