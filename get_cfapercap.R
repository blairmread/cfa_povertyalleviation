
library(tidyverse)
library(readstata13)
library(MASS)
library(readxl)

state_pop <- readxl::read_excel("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/NST-EST2022-POP.xlsx")
colnames(state_pop) <- c("State", "Apr2020", "Jul2020", "2021", "2022")

# National

# Manually import the numbers from the Board Metrics 
cfa_lifts <- cbind.data.frame(portfolio = rep(c("safety net", "tax", "total"), times = 3), 
                              year = rep(c(2022, 2021, 2020), each = 3), 
                              benefits_delivered = c(2974347206, 370780052, 2974347206 + 370780052, 
                                                     2825646276, 787368213, 2825646276 + 787368213, 
                                                     2505213759, 62612974, 2505213759 + 62612974), 
                              people_served = c(3853243, 277625, 3853243 + 277625,
                                                3234406, 590086, 3234406 + 590086,
                                                6046485, 488249, 6046485 + 488249)) 

cps$on_benefits <- ifelse(cps$totalTransfers > 0, 1, ifelse(is.na(cps$totalTransfers), NA, 0))
benefits_mean <- weighted.mean(cps$on_benefits, cps$asecwt)
benefits_mean # the weighted mean is that 51% of households are on benefits 

us_population <- 333287557

cfa_lifts$proportion_served <- cfa_lifts$people_served / (us_population * benefits_mean)
cfa_lifts$percapitaben <- cfa_lifts$benefits_delivered / (us_population * benefits_mean)
cfa_lifts

ggplot(cfa_lifts, aes(x = portfolio, y = benefits_delivered / 100000, fill = as.character(year))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + 
  scale_fill_manual(values = c(dark_lilac, med_lilac, dark_purple)) + 
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





# Assign CfA's benefits delivered to the CPS-ASEC sample 

cps_forpc <- cps %>% 
  mutate(hhsize = ifelse(mstat == 2, nchild + 2, 
                         nchild + 1)) %>% 
  dplyr::select(c(year, asecwt, State, state_alpha, totalTransfers, Y, depx, hhsize, on_benefits))

clients <- cps_forpc[cps_forpc$on_benefits == 1,]
clients$unique_id <- seq(1, nrow(clients), by = 1)

clients_cfa <- rbind.data.frame(clients, clients)
clients_cfa <- clients_cfa[order(clients_cfa$unique_id),]
clients_cfa$pair_id <- rep(c(1, 2), times = nrow(clients))
clients_cfa$new_weights <- ifelse(clients_cfa$pair_id == 1, clients_cfa$asecwt * 0.024, 
                                  clients_cfa$asecwt * (1 - 0.024))


nonclients <- cps_forpc[cps_forpc$on_benefits != 1, ]
nonclients$pair_id <- 3
nonclients$new_weights <- nonclients$asecwt
nonclients$unique_id <- NA

cps_cfa <- rbind.data.frame(clients_cfa, nonclients)

cps_cfa$totalTransfers_cfa22 <- ifelse(cps_cfa$pair_id == 1, cps_cfa$totalTransfers + (cfa_lifts[cfa_lifts$portfolio == "total" & cfa_lifts$year == 2022, "percapitaben"] / 1000) * cps_cfa$hhsize, 
                                       cps_cfa$totalTransfers)

cps_cfa$totalTransfers_tax22 <- ifelse(cps_cfa$pair_id == 1, cps_cfa$totalTransfers + (cfa_lifts[cfa_lifts$portfolio == "tax" & cfa_lifts$year == 2022, "percapitaben"] / 1000) * cps_cfa$hhsize, 
                                       cps_cfa$totalTransfers)

cps_cfa$totalTransfers_sn22 <- ifelse(cps_cfa$pair_id == 1, cps_cfa$totalTransfers + (cfa_lifts[cfa_lifts$portfolio == "safety net" & cfa_lifts$year == 2022, "percapitaben"] / 1000) * cps_cfa$hhsize, 
                                      cps_cfa$totalTransfers)

cps_cfa$totalTransfers_cfa21 <- ifelse(cps_cfa$pair_id == 1, cps_cfa$totalTransfers + (cfa_lifts[cfa_lifts$portfolio == "total" & cfa_lifts$year == 2021, "percapitaben"] / 1000) * cps_cfa$hhsize, 
                                       cps_cfa$totalTransfers)

cps_cfa$totalTransfers_tax21 <- ifelse(cps_cfa$pair_id == 1, cps_cfa$totalTransfers + (cfa_lifts[cfa_lifts$portfolio == "tax" & cfa_lifts$year == 2021, "percapitaben"] / 1000) * cps_cfa$hhsize, 
                                       cps_cfa$totalTransfers)

cps_cfa$totalTransfers_sn21 <- ifelse(cps_cfa$pair_id == 1, cps_cfa$totalTransfers + (cfa_lifts[cfa_lifts$portfolio == "safety net" & cfa_lifts$year == 2021, "percapitaben"] / 1000) * cps_cfa$hhsize, 
                                      cps_cfa$totalTransfers)

cps_cfa$asecwt <- cps_cfa$new_weights
  # BAD CODING PRACTICE! Change this back later 

























sn_states <- cbind.data.frame(state = rep(c("Minnesota", "California"), each = 2), 
                              year = rep(c(2022, 2021), times = 2), 
                              benefits_delivered = c(90803378 + 89044278	+ 101904150,
                                                     9399997 + 12127116 + 18581165 + 35654598, 
                                                     704453200 + 644679200 + 702675600 + 640787400, 
                                                     635291800 + 600091800 + 848119800 + 666380000))


state_pop <- state_pop %>% 
  filter(grepl(".", State), 
         ! grepl("Annual", State), 
         ! grepl("Geographic", State), 
         ! grepl("Note", State), 
         ! grepl("Suggested", State), 
         ! grepl("Source", State), 
         ! grepl("Release", State)) %>% 
  filter(! State %in% c("United States", "Northeast", "Midwest", "South", "West", ".Puerto Rico", ".District of Columbia"))

stids <- cps %>% 
  group_by(state_alpha) %>% 
  mutate(bens_yes = ifelse(totalTransfers == 0, 0, 1), 
         bens_mean = mean(bens_yes, na.rm=T)) %>% 
  dplyr::select(c(state_alpha, State, bens_mean)) %>% 
  distinct(., .keep_all = TRUE)

state_pop$clState <- substr(state_pop$State, 2, nchar(state_pop$State)) 








