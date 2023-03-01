
library(tidyverse)
library(readstata13)
library(MASS)
library(readxl)

# Manually import the numbers from the Board Metrics 
cfa_lifts <- cbind.data.frame(portfolio = rep(c("safety net", "tax", "total"), times = 3), 
                              year = rep(c(2022, 2021, 2020), each = 3), 
                              benefits_delivered = c(2974347206, 370780052, 2974347206 + 370780052, 
                                                     2825646276, 787368213, 2825646276 + 787368213, 
                                                     2505213759, 62612974, 2505213759 + 62612974)) 
cfa_lifts$ben_sm <- cfa_lifts$benefits_delivered / 100000

ggplot(cfa_lifts, aes(x = portfolio, y = ben_sm, fill = as.character(year))) + 
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


sn_states <- cbind.data.frame(state = rep(c("Minnesota", "California"), each = 2), 
                              year = rep(c(2022, 2021), times = 2), 
                              benefits_delivered = c(90803378 + 89044278	+ 101904150,
                                                     9399997 + 12127116 + 18581165 + 35654598, 
                                                     704453200 + 644679200 + 702675600 + 640787400, 
                                                     635291800 + 600091800 + 848119800 + 666380000))

state_pop <- readxl::read_excel("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/NST-EST2022-POP.xlsx")
colnames(state_pop) <- c("State", "Apr2020", "Jul2020", "2021", "2022")

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


# placeholder for tax data # 



# Assign CfA's benefits delivered to the CPS-ASEC sample 

cps_forpc <- cps %>% 
  mutate(hhsize = ifelse(mstat == 2, nchild + 2, 
                         nchild + 1)) %>% 
  dplyr::select(c(year, asecwt, State, state_alpha, totalTransfers, Y, depx, hhsize))

test_df <- cps_forpc[cps_forpc$year == 2017,]
  
# Steps: 
# Calculate how much CfA gave to each person it helped (Total Amount / Total # of Clients) 
# Calculate what proportion of Americans in a year: e.g. SN in 2020, this is 1.84% -- 6 million / 329 million
# Expand the CPS-ASEC dataset so that each household row appears "n" times, where n is the weight value 
# For any household that received transfers (totalTransfers > 0), add the CfA per capita amount to 1.84% of those household copies TIMES the number of people in that household 
# Do not touch the transfers amount for the other 92% of that household copies 
# For any household that did not receive transfers, do not touch their totalTransfers amount 
# Rerun the model 




