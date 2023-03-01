
library(tidyverse)
library(readstata13)
library(MASS)
library(tayloRswift)

col <- read.csv("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/col_cl.csv")
col$Total_annual_num <- as.numeric(unlist(gsub("[^0-9.-]", "", col$Total_annual)))
col$nfam <- ifelse(col$Family == "1p0c", 1, 
                   ifelse(col$Family == "1p1c", 2,
                          ifelse(col$Family == "1p2c", 3,
                                 ifelse(col$Family == "1p3c", 4, 
                                        ifelse(col$Family == "1p4c", 5,
                                               ifelse(col$Family == "2p0c", 2,
                                                      ifelse(col$Family == "2p1c", 3,
                                                             ifelse(col$Family == "2p2c", 4,
                                                                    ifelse(col$Family == "2p3c", 5, 
                                                                           ifelse(col$Family == "2p4c", 6, NA))))))))))

col_nfam_sum <- col %>% 
  mutate(nfam_code = paste("Family Size: ", nfam, sep = "")) %>% 
  group_by(nfam_code, Areaname) %>% 
  summarize(avg_cost = mean(Total_annual_num, na.rm=T)) 


col_nfam_sum$psi <- ifelse(col_nfam_sum$nfam_code == "Family Size: 1", 12488,
                           ifelse(col_nfam_sum$nfam_code == "Family Size: 2", 15877, 
                                  ifelse(col_nfam_sum$nfam_code == "Family Size: 3", 19515,
                                         ifelse(col_nfam_sum$nfam_code == "Family Size: 4", 25094,
                                                ifelse(col_nfam_sum$nfam_code == "Family Size: 5", 29714,
                                                       ifelse(col_nfam_sum$nfam_code == "Family Size: 6", 33618, NA))))))


# https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-thresholds.html

ggplot(col_nfam_sum, aes(x = avg_cost, fill = nfam_code)) + geom_histogram(bins = 100) + 
  facet_wrap(~ nfam_code) + 
  geom_vline(aes(xintercept = psi, color = nfam_code), linewidth = 1, lty = "dashed") + 
  theme_bw() + 
  scale_fill_taylor() + 
  scale_color_taylor() + 
  labs(x = "Average Cost of Living", y = "Frequency Across Counties") + 
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18), 
        strip.text.x = element_text(size = 18),
        legend.position = "none")



cps <- readstata13::read.dta13("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/cps17_21_Working_hh_new.dta")

psi_rates <- cbind.data.frame(hhsize = c(1, 2, 3, 4, 5, 6), 
                              poverty_rate = c(12488, 15877, 19515, 25094, 29714, 33618))
psi_rates

cps$hhsize <- ifelse(cps$mstat == 2, cps$nchild + 2, 
                     cps$nchild + 1)

table(cps$hhsize)

mean_hhsize <- weighted.mean(cps$hhsize, cps$asecwt)
mean_hhsize
# (Weighted) mean hhsize is 2.58

set_psi_national <-  psi_rates$poverty_rate[psi_rates$hhsize == round(mean_hhsize, digits = 0)]
set_psi_national
# take the average of the federal poverty level for HHs size 2 and 3

mean_statehhsize <- cps %>%  
  group_by(state_alpha) %>% 
  summarize(mean_hhsize = weighted.mean(hhsize, asecwt), 
            mean_rounded = round(mean_hhsize, digits = 0))

get_state_psi <- left_join(mean_statehhsize, psi_rates, by = c("mean_rounded" = "hhsize"))
get_state_psi


co_sizes_TMP <- read.csv("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/co-est2019-annres.csv") 
colnames(co_sizes_TMP) <- c("place", "pop")

co_sizes <- co_sizes_TMP %>% 
  separate(., col = place, into = c("county", "state"), sep = "\\,") %>% 
  filter(! pop == "") %>% 
  filter(! pop == "Population") %>% 
  mutate(pop = as.numeric(gsub(",", "", pop))) %>% 
  mutate(county = toupper(trimws(gsub("\\.", "", county))),
         state = toupper(trimws(state)))

head(co_sizes)

state_ids <- cps %>% 
  dplyr::select(State, state_alpha) %>% 
  distinct(., .keep_all = TRUE)

col_ids <- col %>% 
  mutate(County = trimws(toupper(County))) %>% 
  dplyr::select(c(County, State.abv., Total_annual_num, nfam)) %>% 
  left_join(., state_ids, by = c("State.abv." = "state_alpha")) %>% 
  mutate(State = trimws(toupper(State)),
         County = gsub("\\.", "", County))

co_sizes$county[co_sizes$county == "PETERSBURG BOROUGH" & co_sizes$state == "ALASKA"] <- "PETERSBURG CENSUS AREA"
co_sizes$county[co_sizes$county == "LASALLE COUNTY" & co_sizes$state == "ILLINOIS"] <- "LA SALLE COUNTY"
co_sizes$county[co_sizes$county == "LASALLE PARISH" & co_sizes$state == "LOUISIANA"] <- "LA SALLE PARISH"
co_sizes$county[co_sizes$county == "DOÑA ANA COUNTY" & co_sizes$state == "NEW MEXICO"] <- "DONA ANA COUNTY"

county_data <- left_join(col_ids, co_sizes, by = c("County" = "county", 
                                                   "State" = "state")) %>% 
  filter(! County == "DISTRICT OF COLUMBIA") %>% 
  filter(! is.na(pop)) %>% 
  left_join(., mean_statehhsize, by = c("State.abv." = "state_alpha")) %>% 
  filter(mean_rounded == nfam) %>% 
  distinct(., .keep_all = TRUE) %>% 
  group_by(nfam, County, State, State.abv., mean_rounded, pop) %>%
  summarize(avg_col = mean(Total_annual_num)) %>% 
  group_by(State) %>% 
  mutate(state_pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(pop_prop = pop / state_pop) 

col_st <- county_data %>% 
  mutate(col_prop = avg_col * pop_prop) %>% 
  group_by(State, nfam) %>% 
  summarize(col = sum(col_prop))

head(col_st)

rm(state_ids)
rm(county_data)
rm(col)
rm(col_ids)
rm(col_nfam_sum)
rm(mean_statehhsize)
rm(co_sizes_TMP)
rm(co_sizes)
rm(mean_hhsize)










