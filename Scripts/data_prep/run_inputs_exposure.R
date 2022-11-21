
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
rm(list = ls())


### Generate fixed inputs for Melbourne for mslt_code and ithim-r

### Trips data used in mslt_code and ithim-r
source("Scripts/data_prep/trips_prep.R")
trips_melbourne <- calculateVistaTrips(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"
)
write.csv(trips_melbourne, "Data/processed/trips_melbourne.csv", row.names=F, quote=F)


### Generate fixed inputs for Brisbane

## Before using the function, as a preliminary once-only step, run  
## Scripts/trips_prep_bris_db_to_csv.R,to extract tables from the 
## QTS MS access file and save as .csv files.
## That script needs to run in 32 bit - see notes in script.

source("Scripts/data_prep/trips_prep.R")
trips_brisbane <- calculateQtsTrips(
  QTS_location = "Data/Travelsurvey/QTS/"
)
write.csv(trips_brisbane, "Data/processed/trips_brisbane.csv", row.names=F, quote=F)


### BZ: not used, default speeds used of 4.8 for walking and 14.5 for cycling.
### SP note: 'calculateShortTrips' in functions_tripsReplace.R has default speeds
###   of 4 for walking and 11 for cycling.  Which is correct?

### Speed walking and cycling Melbourne 
# source("Scripts/data_prep/trips_prep.R")
# speed_trips_melbourne <- CalculateAgeSexSpeed(
#   in_data="Data/processed/trips_melbourne.csv"
# )
# write.csv(speed_trips_melbourne, "Data/processed/speed_trips_melbourne.csv", row.names=F, quote=F)

### Speed walking and cycling Brisbane
# source("Scripts/data_prep/trips_prep.R")
# speed_trips_brisbane <- CalculateAgeSexSpeed(
#   in_data="Data/processed/trips_brisbane.csv"
# )
# write.csv(speed_trips_brisbane, "Data/processed/speed_trips_brisbane.csv", row.names=F, quote=F)


### Travel data people used in mslt_code to generate matched population - Melbourne
source("Scripts/data_prep/synthetic_pop.R")
travel_data_melbourne <- calculateVistaTravelData(
  hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv",
  person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv",
  ses_index_location="Data/Travelsurvey/ABS SEIFA/ses.csv"
)

write.csv(travel_data_melbourne, "Data/processed/travel_data_melbourne.csv", row.names=F, quote=T)


### Travel data people used in mslt_code to generate matched population - Brisbane
source("Scripts/data_prep/synthetic_pop.R")
travel_data_brisbane <- calculateQtsTravelData(
  QTS_location = "Data/Travelsurvey/QTS/",
  ses_index_location = "Data/Travelsurvey/ABS SEIFA/2033055001 - sa1 indexes.xls",
  SA1_2016_2021_location = "Data/Travelsurvey/ABS SEIFA/CG_SA1_2016_SA1_2021.csv"
)

write.csv(travel_data_brisbane, "Data/processed/travel_data_brisbane.csv", row.names=F, quote=T)



### PA data people used in mslt_code to generate matched population (same for Melbourne and Brisbane)
source("Scripts/data_prep/synthetic_pop.R")

persons_pa <- calculatePersonsPA(
  pa_location="Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv",
  hh_location="Data/Physical activity/NHS2017-18_CSV/NHS17HHB.csv"
)
write.csv(persons_pa, "Data/processed/persons_pa.csv", row.names=F, quote=F)
