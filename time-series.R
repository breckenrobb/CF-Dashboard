library(dplyr)
library(forcats)

## Rewrite to loop through parks
## Add df reordering and renaming

# Read in monthly data
kova.month <- read.csv("C:\\Users\\brobb\\OneDrive - DOI\\Projects\\AKR_CFs\\AKR-CFs\\KOVA\\Data\\monthly_data.csv")

# Read in annual data
kova.year <- kova.month %>%
  select(-X,-time,-month,-season) %>%
  group_by(GCM,year) %>%
  summarize(across(everything(),mean,na.rm=TRUE),.groups="drop")

write.csv(kova.year, "C:\\Users\\brobb\\OneDrive - DOI\\Projects\\AKR_CFs\\AKR-CFs\\KOVA\\Data\\annual_tmin_tmax.csv", 
          row.names = FALSE)




cakr.month <- read.csv("C:\\Users\\brobb\\OneDrive - DOI\\Projects\\AKR_CFs\\AKR-CFs\\CAKR\\Data\\monthly_data.csv")

cakr.year <- cakr.month %>%
  select(-X,-time,-month,-season) %>%
  group_by(GCM,year) %>%
  summarize(across(everything(),mean,na.rm=TRUE),.groups="drop")

write.csv(cakr.year, "C:\\Users\\brobb\\OneDrive - DOI\\Projects\\AKR_CFs\\AKR-CFs\\CAKR\\Data\\annual_tmin_tmax.csv",
          row.names = FALSE)
