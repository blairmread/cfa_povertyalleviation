
list.files("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/")

income <- readstata13::read.dta13("/Users/blair@codeforamerica.org/Documents/GitHub/cfa_povertyalleviation/income_cps2017_2021.dta")

# Step 1: Merge in the race and ethnicity data from IPUMS

# Step 2: Rerun the baseline models separately for each race/ethnic group at the national level

# Step 3: Output the baseline alleviation rates by race / ethnicity