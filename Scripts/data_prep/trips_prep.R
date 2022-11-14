##### Generate trips data - Melbourne and Brisbane
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data


options(scipen=999)

### 1) calculateVistaTrips (Melbourne) & calculateQtsTrips (Brisbane): 
###  each row is a stage
###  data used for: ithimr (https://github.com/ITHIM/ITHIM-R), scenarios DoT Melbourne and descriptives stages

### 2) calculateVistaTripsDescriptives (Melbourne) & calculateQtsTripsDescriptives (Brisbane): 
###  each row is main mode of travel
#### data used for descriptive statistics only

### 3) Calculate speed walking and cycling by age and sex (quantiles and mean (sd))


######################  1) calculateVistaTrips (Melbourne) & calculateQtsTrips (Brisbane)  ############################

### calculateVistaTrips (Melbourne) - notes
#-------------------------------------------------#
### Columns ITHIMR: 
#### -One row per trip (or stage of trip)
#### -Minimal columns: participant_id, age, sex, trip_mode, trip_duration (or trip_distance)
#### -Other columns: stage_mode, stage_duration (or stage_distance)
### Should include people without trips. FOR NOW INCLUDES ONLY THOSE WITH TRIPS, NOT SURE IF IT INCLUDES PEOPLE WITHOUT TRIPS, I DELETED SOME NAS WHICH MAY REPRESENT PEOPLE
### WITHOUT TRIPS
### [SP note: Vista trips, used here, does not include people without trips (that is, there are people in 
##   Vista persons who have no trips in Vista trips).  However those non-travelling people do ultimately appear
###  in the scenarios, which are based on the Vista persons.]
### VISTA Survey for Melbourne greater area for time period 2017-18
### One day (weekday or weekend)
### Final product trips_melbourne where each row is a stage


calculateVistaTrips <- function(hh_VISTA_location,person_VISTA_location,trip_VISTA_location) {
   # hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv"
   # person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv"
   # trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"


  hh_VISTA <- read.csv(hh_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID,SurveyPeriod,DayType,WDHHWGT,WEHHWGT,HomeSubRegion,HOMELGA) %>%
    filter(HHID!="") # some rows were completely blank
  person_VISTA <- read.csv(person_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID,HHID,AGE,SEX,WDPERSWGT,WEPERSWGT)
  trip_VISTA <- read.csv(trip_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%

    dplyr::select(TRIPID,PERSID,HHID,TRIPNO,CUMDIST,TRAVTIME,ORIGLGA,DESTLGA,
                  TRIPPURP,LINKMODE,
                  MODE1,MODE2,MODE3,MODE4,MODE5,MODE6,MODE7,MODE8,MODE9,
                  DIST1,DIST2,DIST3,DIST4,DIST5,DIST6,DIST7,DIST8,DIST9,
                  TIME1,TIME2,TIME3,TIME4,TIME5,TIME6,TIME7,TIME8,TIME9,
                  WDTRIPWGT,WETRIPWGT)


  hh_person <- left_join(person_VISTA, hh_VISTA, by = "HHID")
  
  
  trips_melbourne  <- left_join(trip_VISTA, hh_person, by = c("PERSID","HHID") ) %>%
    dplyr::filter(SurveyPeriod == "2017-18" &
                    (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%
    dplyr::select(HHID, PERSID, AGE, SEX, TRIPID, DayType, SurveyPeriod, 
                  HomeSubRegion, TRIPNO, LINKMODE, MODE1, MODE2, MODE3, MODE4, 
                  MODE5, MODE6, MODE7, MODE8, MODE9, TIME1, TIME2, TIME3, TIME4, 
                  TIME5, TIME6, TIME7, TIME8, TIME9, DIST1, DIST2, DIST3, DIST4,
                  DIST5, DIST6, DIST7, DIST8, DIST9, TRAVTIME, TRIPPURP, 
                  WDPERSWGT, WEPERSWGT, CUMDIST, DESTLGA, ORIGLGA, HOMELGA, WDTRIPWGT,WETRIPWGT) %>% 
    dplyr::filter(AGE>15) %>%
    rowwise() %>% # want to sum across rows, not down columns %>%
    mutate(participant_wt = sum(as.numeric(WDPERSWGT),as.numeric(WEPERSWGT),na.rm=T)) %>%
    mutate(trips_wt = sum(as.numeric(WDTRIPWGT),as.numeric(WETRIPWGT),na.rm=T)) %>%
    dplyr::select(-WDPERSWGT,-WEPERSWGT, -WDTRIPWGT, -WETRIPWGT) %>%
    as.data.frame()
  
  ### Replace all character "N/A" with NA
  trips_melbourne[ trips_melbourne == "N/A" ] <- NA 
  
  # all of the data that isn't MODE,TIME,DIST
  trips_melbourne_tripid <- trips_melbourne %>%
    dplyr::select(HHID, PERSID, AGE, SEX, TRIPID, DayType, SurveyPeriod, 
                  HomeSubRegion, TRIPNO, LINKMODE, TRAVTIME, TRIPPURP, 
                  participant_wt, trips_wt, CUMDIST, DESTLGA, ORIGLGA, HOMELGA) %>%
    ### Keep only if complete cases for MODE, DIST and TIME and participant_wt(I AM UNSURE WHY THERE ARE NA WEIGHTS)
    filter(!is.na(participant_wt))
  
  # pivot MODE,TIME,DIST from wide to long and create a stop number
  trips_melbourne_mode <- trips_melbourne %>%
    dplyr::select(TRIPID,MODE1:MODE9) %>%
    pivot_longer(MODE1:MODE9,names_to="STOP",values_to="MODE") %>%
    filter(!is.na(MODE)) %>%
    mutate(STOP=as.numeric(gsub("MODE", "",STOP)))
  trips_melbourne_time <- trips_melbourne %>%
    dplyr::select(TRIPID,TIME1:TIME9) %>%
    mutate_at(vars(starts_with("TIME")),as.numeric) %>%
    pivot_longer(TIME1:TIME9,names_to="STOP",values_to="TIME") %>%
    filter(!is.na(TIME)) %>%
    mutate(STOP=as.numeric(gsub("TIME", "",STOP)))
  trips_melbourne_dist <- trips_melbourne %>%
    dplyr::select(TRIPID,DIST1:DIST9) %>%
    mutate_at(vars(starts_with("DIST")),as.numeric) %>%
    pivot_longer(DIST1:DIST9,names_to="STOP",values_to="DIST") %>%
    filter(!is.na(DIST)) %>%
    mutate(STOP=as.numeric(gsub("DIST", "",STOP)))
  
  # merge all of the data back into a single long table.
  trips_melbourne_long <- trips_melbourne_tripid %>%
    inner_join(trips_melbourne_mode, by="TRIPID") %>%
    inner_join(trips_melbourne_time, by=c("TRIPID","STOP")) %>%
    inner_join(trips_melbourne_dist, by=c("TRIPID","STOP"))
  
  
  trips_melbourne <- trips_melbourne_long %>%
    ### Replace mode 13 and 14 with NA
    filter(MODE != "13" & MODE != "14") %>%
    ### Sort by person id
    dplyr::arrange(PERSID,TRIPID,STOP) %>%
    ### Name to match ITHIMR
    dplyr::rename(household_id=HHID,
                  age=AGE,
                  sex=SEX,
                  trip_id=TRIPID,
                  trip_mode=MODE,
                  trip_duration=TIME,
                  trip_distance=DIST,
                  trip_purpose=TRIPPURP,
                  day_type=DayType) %>%
    dplyr::mutate(participant_id=PERSID) %>%
    ### Separate participant id into year, hh_id and participant_id
    separate(participant_id, into=c(NA,"year",NA,"cluster_id","household_id",NA,
                                    "participant_id"), sep = c(1, 3, 4, 6, -3,-2)) %>%
    # assign a unique id for each trip
    mutate(trip_id=row_number())
  
  
  ### Create age groups to match with PA data
  trips_melbourne <- trips_melbourne %>%
    dplyr::mutate(age_cat = case_when(age <   5             ~  1,
                                      age >=  5 & age <=  9 ~  2,
                                      age >= 10 & age <= 14 ~  3,
                                      age >= 15 & age <= 17 ~  4, 
                                      age >= 18 & age <= 19 ~  5,
                                      age >= 20 & age <= 24 ~  6,
                                      age >= 25 & age <= 29 ~  7, 
                                      age >= 30 & age <= 34 ~  8, 
                                      age >= 35 & age <= 39 ~  9, 
                                      age >= 40 & age <= 44 ~ 10,
                                      age >= 45 & age <= 49 ~ 11, 
                                      age >= 50 & age <= 54 ~ 12, 
                                      age >= 55 & age <= 59 ~ 13, 
                                      age >= 60 & age <= 64 ~ 14, 
                                      age >= 65 & age <= 69 ~ 15,
                                      age >= 70 & age <= 74 ~ 16, 
                                      age >= 75 & age <= 79 ~ 17,
                                      age >= 80 & age <= 84 ~ 18,
                                      age >= 85             ~ 19))
  
  trips_melbourne <- trips_melbourne %>%
    #### Only keep adults to match NHS_pa data, drop categories 1,2 and 3
    dplyr::filter(age_cat>3) %>%
    ### Do not include age_cat as ithim is doing its own synthetic population and this causes issues to have age_cat
    dplyr::select(PERSID, cluster_id, household_id, participant_id, age, sex, year, trip_id,
                  trip_purpose, participant_wt, trips_wt, trip_mode, trip_duration, trip_distance, 
                  day_type) %>%
    dplyr::mutate(PERSID=tolower(PERSID)) %>%
    dplyr::mutate(year=as.numeric(year)) %>%
    dplyr::mutate(trip_purpose=tolower(trip_purpose)) %>%
    dplyr::mutate(day_type=tolower(day_type)) %>%
    dplyr::mutate(sex=case_when(sex=="M" ~ 'male', sex=="F" ~ 'female')) %>%
    dplyr::mutate(trip_mode=case_when(trip_mode=="Vehicle Driver" ~ 'car', 
                                      trip_mode=="Vehicle Passenger" ~ 'car', 
                                      trip_mode=="Taxi" ~ 'car', 
                                      trip_mode=="School Bus" ~ 'bus', 
                                      trip_mode=="Public Bus" ~ 'bus', 
                                      trip_mode=="Walking" ~ 'pedestrian',
                                      # if meeting none of these criteria, keep original value
                                      TRUE ~ tolower(trip_mode)))
  
  
  ### Create numeric id
  trips_melbourne <- trips_melbourne %>%
    dplyr::rename(persid=PERSID) %>%
    group_by(persid) %>%
    mutate(participant_id=group_indices()) %>%
    ungroup()
  ### Create number of trips per person
  trips_melbourne <- trips_melbourne %>%
  group_by(persid) %>%
    dplyr::mutate(trip_id_2 = 1:dplyr::n()) %>%
    ungroup() %>% ## Add age groups c( "15 to 19", "20 to 39", "40 to 64", "65 plus", "all"),
    mutate(age_group = as.factor(case_when(age >=  15 & age <=  19 ~  "15 to 19",
                                           age >=  20 & age <=  39 ~  "20 to 39",
                                           age >= 40 & age <= 64 ~  "40 to 64",
                                           age >= 65             ~ "65 plus"))) %>%
    dplyr::mutate(sex =as.factor(sex)) %>%
    dplyr::mutate(age_group=as.factor(age_group)) %>%
    dplyr::mutate(dist_cat=as.factor(case_when(trip_distance < 1 ~ "<1km",
                                               trip_distance >= 1 & trip_distance <= 2 ~ "1-2km", 
                                                trip_distance <= 5 & trip_distance > 2 ~ "3-5km", 
                                                trip_distance <=10 & trip_distance > 5 ~ "6-10km",
                                                trip_distance > 10 ~ ">10km"))) %>%
    dplyr::mutate(trip_purpose=as.factor(case_when(trip_purpose=="social" ~ "Leisure",
                                                   trip_purpose=="recreational" ~ "Leisure",
                                                   trip_purpose=="buy something" ~ "Shopping",
                                                   trip_purpose=="education" ~ "Education",
                                                   trip_purpose=="pick-up or drop-off someone"  ~ "Other",
                                                   trip_purpose=="pick-up or deliver something"  ~ "Other",
                                                   trip_purpose=="unknown purpose (at start of day)" ~ "Other",
                                                   trip_purpose=="other purpose" ~ "Other",
                                                   trip_purpose=="at or go Home"  ~ "Other",
                                                   trip_purpose=="change mode"  ~ "Other",
                                                   trip_purpose=="accompany someone"   ~ "Other",
                                                   trip_purpose=="personal business"  ~ "Other",
                                                   trip_purpose=="at or go home"  ~ "Other",
                                                   trip_purpose=="not stated"   ~ "Other",
                                                   trip_purpose=="work related"   ~ "Work",
                                                   TRUE ~ trip_purpose))) %>%
    
    dplyr::mutate(day_type =as.factor(day_type))
  
  
  return(trips_melbourne)
}


### calculateQtsTrips (Brisbane) - notes
#-------------------------------------------------#
## Based on equivalent 'calculateVistaTrips' function for Melbourne.
## Qld Travel Survey for Greater Brisbane area for time period 2020-21.
## Does not include people without trips (that is, there are people in 
##   QTS_person who have no stops/trips in QTS_stops and QTS_trips). However 
##   those non-travelling people do ultimately appear in the scenarios, which 
##   are based  on QTS_person.
## Survey period per person is one day.  In Brisbane, unlike Melbourne, they are 
##   all weekdays.

## Final product is 'trips_brisbane' where each row is a trip stage - and so
##  'trip_duration' and 'trip_distance' are for the stage only (not overall trip).

## Brisbane output differs from Melbourne output as follows.
## - Brisbane has no 'cluster id' data.  Cluster id forms part of Melbourne PERSID field,
##      but not equivalent Brisbane field.  'cluster_id' is included as a NA field.
## - There is no 'age' column.  Instead, there is an 'age_band' column, 
##     in 5 year bands. The Brisbane data does not include exact age. Also, the 
##     Melbourne output excludes persons aged 15 and under, but this is not 
##     possible for Brisbane, and so persons aged under 15 are excluded instead.
## - The 'participant_wt' and 'trips_wt' columns are the same.  No separate 
##     Brisbane trip weight field is provided, but the QTS '_Readme' file says 
##     person weights should be used for trip weights.
## - 'day_type' (weekday or weekend day) is included, but all the Brisbane days 
##     are weekdays.
## - 'trip_mode' differs from Melbourne by omitting 'tram' but including 'ferry' 
##     and 'light rail'. Also, 'truck' and 'mobility scooter' (not mentioned in  
##     Melbourne) are treated as 'car' and 'other' respectively.

## Before using the function, as a preliminary once-only step, run  
## Scripts/trips_prep_bris_db_to_csv.R,to extract tables from the 
## QTS MS access file and save as .csv files.
## That script needs to run in 32 bit - see notes in script.

calculateQtsTrips <- function(QTS_location) {
  
  # QTS_location = "Data/Travelsurvey/QTS/"
  
  region_QTS <- read.csv(paste0(QTS_location, "R_REGION.csv"), as.is=T, fileEncoding="UTF-8-BOM")
  lga_QTS <- read.csv(paste0(QTS_location, "R_LGA.csv"), as.is=T, fileEncoding="UTF-8-BOM")
  agegroup_QTS <- read.csv(paste0(QTS_location, "RP_AGE_GROUP.csv"), as.is=T, fileEncoding="UTF-8-BOM")
  
  hh_QTS <- read.csv(paste0(QTS_location, "1_QTS_HOUSEHOLDS.csv"), as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID, HHWGT_20, TRAVDATE, TRAVMONTH, TRAVYEAR, stratagroupid = STRATA_LGA) %>%
    
    # determine weekday/weekend days (but note that they are all weekdays)
    mutate(date = paste0(TRAVYEAR, "-", TRAVMONTH, "-", TRAVDATE),
           day = weekdays(as.Date(date)),
           DayType = as.factor(
             case_when(day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "weekday",
                       day %in% c("Saturday", "Sunday") ~ "weekend day"))) %>%
    
    # add regions and LGAs
    left_join(region_QTS, by = "stratagroupid") %>%
    left_join(lga_QTS, by = "stratagroupid")
  
  person_QTS <- read.csv(paste0(QTS_location, "2_QTS_PERSONS.csv"), as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID, HHID, AGEGROUP_CODE = AGEGROUP, SEX, PERSWGT20) %>%
    
    # add ages
    left_join(agegroup_QTS %>% dplyr::select(AGEGROUP_CODE, age_band = DESCRIPTION) %>% distinct(),
              by = "AGEGROUP_CODE")
  
  stops_QTS <- read.csv(paste0(QTS_location, "4_QTS_STOPS.csv"), as.is=T, fileEncoding="UTF-8-BOM") %>%
    
    # Rename 'TRAVTIME' as 'TIME', to avoid conflict with the trips_QTS field with the same name:
    # the stops_QTS TRAVTIME field is the time for the trip stage, whereas the trips_QTS TRAVTIME field is cumulative time for entire trip
    rename(TIME = TRAVTIME)
  
  trips_QTS <- read.csv(paste0(QTS_location, "5_QTS_TRIPS.csv"), as.is=T, fileEncoding="UTF-8-BOM") %>%
    
    # rename 'TRAVTIME' as 'total_time'
    # Note that 'total_time' differs from the Melbourne field 'TRAVTIME': 
    # - Melbourne 'TRAVTIME' seems to be an exact summary of the travel times for the trip stages,
    # - whereas  Brisbane 'total_time' can be longer - presumably including some waiting time in between stages
    # However, the field is not used in the final output anyway
    rename(total_time = TRAVTIME) %>%
    
    # add TRIPNO (sequential number of trips taken by person)
    group_by(PERSID) %>%
    arrange(STARTSTOP, by_group = TRUE) %>%
    mutate(TRIPNO = row_number()) %>%
    ungroup()
  
  # filter trips_QTS to only TRIPID, and fields not already contained in stops_QTS (for joining to stops_QTS)
  trips_QTS <- trips_QTS %>%  
    dplyr::select(TRIPID, names(trips_QTS)[!(names(trips_QTS) %in% names(stops_QTS))])
  
  
  hh_person_QTS <- left_join(person_QTS, hh_QTS, by = "HHID")
  
  trips_brisbane <- left_join(stops_QTS, trips_QTS, by = "TRIPID") %>%
    left_join(hh_person_QTS, by = c("PERSID", "HHID")) %>%
    
    # add a stop field (sequential number of stops within a trip, whereas 'STOPNO' is sequential 
    # number of stops within a day)
    group_by(TRIPID) %>%
    arrange(STOPNO, by_group = TRUE) %>%
    mutate(STOP = row_number()) %>%
    ungroup() %>%
    
    # confine to Greater Brisbane (excluding Gold Coast and Sunshine Coast)
    dplyr::filter(REGION == "Greater Brisbane") %>%
    
    # select required fields
    dplyr::select(HHID, PERSID, AGEGROUP_CODE, age_band, SEX, TRIPID, DayType, TRAVYEAR, STOP,
                  REGION, TRIPNO, MAINMODE, MODE, TIME, DISTANCE, total_time, OVERALL_PURPOSE, 
                  PERSWGT20, CUMDIST, ORIGSA1_2021, DESTSA1_2021, homeLGA = LGA) %>%
    
    # filter to age >= 15 (AGEGROUP_CODE 4 or higher)
    # note that unlike Melbourne, it can't be age > 15, because age is only specified in 5 year blocks
    filter(AGEGROUP_CODE > 3) %>%
    
    # sort by person id
    dplyr::arrange(PERSID, TRIPID, STOP) %>%
    
    # create participant_id (sequential numeric id for each person in survey) 
    group_by(PERSID) %>%
    mutate(participant_id = group_indices()) %>%
    ungroup() %>%
    
    # assign a unique id for each trip stage 
    mutate(trip_id = row_number()) %>%
    
    # categorise trip modes (outcome is car, motorcycle, pedestrian, bicycle, bus, train, 
    # ferry, light rail, other)
    mutate(MODE = 
             case_when(MODE == "Car driver"                     ~ "car",
                       MODE == "Car passenger"                  ~ "car",
                       MODE == "Taxi"                           ~ "car",
                       MODE == "Truck driver"                   ~ "car",
                       MODE == "Truck passenger"                ~ "car",
                       MODE == "Uber / Other Ride Share"        ~ "car",
                       MODE == "Motorcycle driver"              ~ "motorcycle",
                       MODE == "Motorcycle passenger"           ~ "motorcycle",
                       MODE == "Charter/Courtesy/Other bus"     ~ "bus",
                       MODE == "Public bus"                     ~ "bus",
                       MODE == "School bus (private/chartered)" ~ "bus",
                       MODE == "School bus (with route number)" ~ "bus",
                       MODE == "Walking"                        ~ "pedestrian",
                       MODE == "Mobility scooter"               ~ "other",
                       MODE == "Other method"                   ~ "other",
                       # if meeting none of these criteria, keep original value (bicycle, train, ferry, light rail)
                       TRUE                                     ~ tolower(MODE))) %>%
    
    # categorise age groups (note that groups 1:3 have already been excluded above)
    mutate(age_group = as.factor(
      case_when(AGEGROUP_CODE == 4         ~ "15 to 19",
                AGEGROUP_CODE %in% c(5:8)  ~ "20 to 39",
                AGEGROUP_CODE %in% c(9:13) ~ "40 to 64",
                AGEGROUP_CODE > 13         ~ "65 plus"))) %>%
    
    # categorise trip distances
    mutate(dist_cat = as.factor(
      case_when(DISTANCE < 1                  ~ "<1km",
                DISTANCE >= 1 & DISTANCE <= 2 ~ "1-2km", 
                DISTANCE <= 5 & DISTANCE > 2  ~ "3-5km", 
                DISTANCE <=10 & DISTANCE > 5  ~ "6-10km",
                DISTANCE > 10                 ~ ">10km"))) %>%
    
    # categorise trip purposes
    mutate(OVERALL_PURPOSE = as.factor(
      case_when(OVERALL_PURPOSE == "Recreation"               ~ "Leisure",
                OVERALL_PURPOSE == "Social"                   ~ "Leisure",
                OVERALL_PURPOSE == "Direct Work Commute"      ~ "Work",
                OVERALL_PURPOSE == "Work Related"             ~ "Work",
                OVERALL_PURPOSE == "Accompany Someone"        ~ "Other",
                OVERALL_PURPOSE == "Other Purpose"            ~ "Other",
                OVERALL_PURPOSE == "Personal Business"        ~ "Other",
                OVERALL_PURPOSE == "Pickup/Deliver Something" ~ "Other",
                OVERALL_PURPOSE == "Pickup/Dropoff Someone"   ~ "Other",
                # if meeting none of these criteria, keep original value (Education, Shopping)
                TRUE                                          ~ OVERALL_PURPOSE))) %>%
    
    # reformat fields where required to match Melbourne output, and add empty cluster-id field
    dplyr::mutate(household_id = as.character(HHID),
                  age_band = as.factor(age_band),
                  sex = as.factor(SEX),
                  year = TRAVYEAR %% 100, ## last 2 digits of year only
                  trip_duration = as.numeric(TIME),
                  day_type = as.factor(DayType),
                  cluster_id = NA) %>%
    
    
    # select required final fields
    dplyr::select(persid = PERSID,
                  cluster_id,
                  household_id,
                  participant_id,
                  age_band,
                  sex,
                  year,
                  trip_id,
                  trip_purpose = OVERALL_PURPOSE,
                  participant_wt = PERSWGT20,
                  trips_wt = PERSWGT20,
                  trip_mode = MODE,
                  trip_duration,
                  trip_distance = DISTANCE,
                  day_type,
                  trip_id_2 = TRIPNO,
                  age_group,
                  dist_cat)
  
  return(trips_brisbane)
}



######################  2) calculateVistaTripsDescriptives (Melbourne) & calculateQtsTripsDescriptive (Brisbane) ######
### Uses LINMODE (main mode of trip) instead of stages of the trip.

### calculateVistaTripsDescriptives (Melbourne)
#-------------------------------------------------#

calculateVistaTripsDescriptives <- function(hh_VISTA_location,person_VISTA_location,trip_VISTA_location) {

  # hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv"
  # person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv"
  # trip_VISTA_location="Data/Travelsurvey/VISTA12-18/T_VISTA1218_V1.csv"

  hh_VISTA <- read.csv(hh_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID,SurveyPeriod,DayType,WDHHWGT,WEHHWGT,HomeSubRegion,HOMELGA) %>%
    filter(HHID!="") # some rows were completely blank
  person_VISTA <- read.csv(person_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID,HHID,AGE,SEX,WDPERSWGT,WEPERSWGT)
  trip_VISTA <- read.csv(trip_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    
    dplyr::select(TRIPID,PERSID,HHID,TRIPNO,CUMDIST,TRAVTIME,ORIGLGA,DESTLGA,
                  TRIPPURP,LINKMODE,
                  MODE1,MODE2,MODE3,MODE4,MODE5,MODE6,MODE7,MODE8,MODE9,
                  DIST1,DIST2,DIST3,DIST4,DIST5,DIST6,DIST7,DIST8,DIST9,
                  TIME1,TIME2,TIME3,TIME4,TIME5,TIME6,TIME7,TIME8,TIME9,
                  WDTRIPWGT,WETRIPWGT) 
  
  trip_VISTA$WDTRIPWGT <- as.numeric(trip_VISTA$WDTRIPWGT)
  
  trip_VISTA$WETRIPWGT <- as.numeric(trip_VISTA$WETRIPWGT)
  
  hh_person <- left_join(person_VISTA, hh_VISTA, by = "HHID")
  
  
  trips_melbourne  <- left_join(trip_VISTA, hh_person, by = c("PERSID","HHID") ) %>%
    dplyr::filter(SurveyPeriod == "2017-18" &
                    (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%
    dplyr::select(HHID, PERSID, AGE, SEX, TRIPID, DayType, SurveyPeriod, 
                  HomeSubRegion, TRIPNO, LINKMODE, MODE1, MODE2, MODE3, MODE4, 
                  MODE5, MODE6, MODE7, MODE8, MODE9, TIME1, TIME2, TIME3, TIME4, 
                  TIME5, TIME6, TIME7, TIME8, TIME9, DIST1, DIST2, DIST3, DIST4,
                  DIST5, DIST6, DIST7, DIST8, DIST9, TRAVTIME, TRIPPURP, 
                  WDPERSWGT, WEPERSWGT, CUMDIST, DESTLGA, ORIGLGA, HOMELGA, WDTRIPWGT,WETRIPWGT) %>% 
    dplyr::filter(AGE>15) %>%
    rowwise() %>% # want to sum across rows, not down columns %>%
    mutate(trips_wt = sum(as.numeric(WDTRIPWGT),as.numeric(WETRIPWGT),na.rm=T)) %>%  ## Add age groups
    mutate(age_group = as.factor(case_when(AGE >=  15 & AGE <=  19 ~  "15 to 19",
                                            AGE >=  20 & AGE <=  39 ~  "20 to 39",
                                            AGE >= 40 & AGE <= 64 ~  "40 to 64",
                                            AGE >= 65  ~ "65 plus"))) %>%
    dplyr::rename(sex=SEX) %>%
    mutate(sex=case_when(sex=="M" ~ 'male', sex=="F" ~ 'female')) %>% # group modes as per trips file
    dplyr::mutate(trip_mode=as.factor(case_when(LINKMODE=="Vehicle Driver" ~ 'car', 
                                               LINKMODE=="Vehicle Passenger" ~ 'car', 
                                               LINKMODE=="Taxi" ~ 'car', 
                                               LINKMODE=="School Bus" ~ 'public.transport', 
                                               LINKMODE=="Public Bus" ~ 'public.transport', 
                                               LINKMODE=="Train" ~ 'public.transport',
                                               LINKMODE=="Tram" ~ 'public.transport',
                                               LINKMODE=="Motorcycle" ~ 'other',
                                               # if meeting none of these criteria, keep original value
                                               TRUE ~ tolower(LINKMODE)))) %>%
    dplyr::mutate(day_type =as.factor(DayType)) %>%
    dplyr::mutate(sex =as.factor(sex)) %>%
    dplyr::mutate(age_group=as.factor(age_group)) %>%
    dplyr::mutate(dist_cat=as.factor(case_when(CUMDIST < 1 ~ "<1km",
                                                CUMDIST >= 1 & CUMDIST <= 2 ~ "1-2km", 
                                                CUMDIST <= 5 & CUMDIST > 2 ~ "3-5km", 
                                                CUMDIST <=10 & CUMDIST > 5 ~ "6-10km",
                                                CUMDIST > 10 ~ ">10km"))) %>%
    dplyr::mutate(TRIPPURP=as.factor(case_when(TRIPPURP=="Social" ~ "Leisure",
                                                   TRIPPURP=="Recreational" ~ "Leisure",
                                                   TRIPPURP=="Buy Something" ~ "Shopping",
                                                   TRIPPURP=="Education" ~ "Education",
                                                   TRIPPURP=="Pick-up or Drop-off Someone"  ~ "Other",
                                                   TRIPPURP=="Pick-up or Deliver Something"  ~ "Other",
                                                   TRIPPURP=="Unknown purpose (at start of day)" ~ "Other",
                                                   TRIPPURP=="Other Purpose" ~ "Other",
                                                   TRIPPURP=="At or Go Home"  ~ "Other",
                                                   TRIPPURP=="Change Mode"  ~ "Other",
                                                   TRIPPURP=="Accompany Someone"   ~ "Other",
                                                   TRIPPURP=="Personal Business"  ~ "Other",
                                                   TRIPPURP=="Not Stated"   ~ "Other",
                                                   TRIPPURP=="Work Related"   ~ "Work",
                                                   TRUE ~ TRIPPURP))) %>%
    dplyr::rename(trip_distance = CUMDIST, trip_duration = TRAVTIME, trip_purpose = TRIPPURP, persid=PERSID, trip_id_2=TRIPNO)
  
  return(trips_melbourne)
}


### calculateQtsTripsDescritives (Brisbane) - notes
#-------------------------------------------------#
## Based on equivalent function for Melbourne.

## Brisbane output differs from Melbourne output as follows.
## - There is no 'age' column.  Instead, there is an 'age_band' column, 
##     in 5 year bands. The Brisbane data does not include exact age. Also, the
##     Melbourne output excludes persons aged 15 and under, but this is not 
##     possible for Brisbane, and so persons aged under 15 are excluded instead.
## - The 'participant_wt' and 'trips_wt' columns are the same.  No separate 
##     Brisbane trip weight field is provided, but the QTS '_Readme' file says 
##     person weights should be used for trip weights.  Also, they appear as 
##     ‘participant_wt’ and ‘trips_wt’, whereas Melbourne uses ‘WDPERSWGT’, 
##     ‘WEPERSWGT’, ‘WDTRIPWGT’ and WETRIPWGT’.
## - 'day_type' (weekday or weekend day) is included, but all the Brisbane days 
##     are weekdays.  The field ‘DayType’ is not included (in Melbourne, 
##     this is the original field from which ‘day_type’ is derived, whereas in Brisbane, 
##     ‘day_type’ is derived from the date fields).
## - 'trip_mode' differs from Melbourne by omitting 'tram' but including 'ferry' 
##     and 'light rail'. Also, 'truck' and 'mobility scooter' (not mentioned in 
##     Melbourne) are treated as 'car' and 'other' respectively.
## - Brisbane includes ‘TRAVYEAR’ which specifies the year of travel (2020 or 
##     2021), whereas Melbourne includes ‘SurveyPeriod’ (2017-18).
## - Brisbane includes ‘REGION’ (which is always Greater Brisbane) whereas 
##     Melbourne includes ‘HomeSubRegion’.
## - Brisbane ‘MAINMODE’ corresponds to Melbourne ‘LINKMODE’ (that is, the main 
##     mode of travel, from which the categorised field ‘trip_mode’ is derived).
## - TIME1 to TIME8 and DIST1 to DIST8 are not included.  These are the times 
##     and distances for individual trip stages, which would need to be obtained 
##     from stops_QTS if needed.
## - Brisbane includes ‘total_time’ (which seems to be end-to-end travel time, 
##     including waiting) whereas Melbourne includes ‘trip duration’ (which seems 
##     to be the sum of individual trip stage travelling times only).
## - Brisbane includes ‘ORIGSA1_2021’ and ‘DESTSA1_2021’ in place of ‘ORIGLGA’ and ‘DESTLGA’.


calculateTripsDescriptives <- function(QTS_location) {
  
  # QTS_location = "Data/Travelsurvey/QTS/"
  
  region_QTS <- read.csv(paste0(QTS_location, "R_REGION.csv"), as.is=T, fileEncoding="UTF-8-BOM")
  lga_QTS <- read.csv(paste0(QTS_location, "R_LGA.csv"), as.is=T, fileEncoding="UTF-8-BOM")
  agegroup_QTS <- read.csv(paste0(QTS_location, "RP_AGE_GROUP.csv"), as.is=T, fileEncoding="UTF-8-BOM")
  
  hh_QTS <- read.csv(paste0(QTS_location, "1_QTS_HOUSEHOLDS.csv"), as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID, HHWGT_20, TRAVDATE, TRAVMONTH, TRAVYEAR, stratagroupid = STRATA_LGA) %>%
    
    # determine weekday/weekend days (but note that they are all weekdays)
    mutate(date = paste0(TRAVYEAR, "-", TRAVMONTH, "-", TRAVDATE),
           day = weekdays(as.Date(date)),
           day_type = as.factor(
             case_when(day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "weekday",
                       day %in% c("Saturday", "Sunday") ~ "weekend day"))) %>%
    
    # add regions and LGAs
    left_join(region_QTS, by = "stratagroupid") %>%
    left_join(lga_QTS, by = "stratagroupid")
  
  person_QTS <- read.csv(paste0(QTS_location, "2_QTS_PERSONS.csv"), as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID, HHID, AGEGROUP_CODE = AGEGROUP, SEX, PERSWGT20) %>%
    
    # add ages
    left_join(agegroup_QTS %>% dplyr::select(AGEGROUP_CODE, age_band = DESCRIPTION) %>% distinct(),
              by = "AGEGROUP_CODE")
  
  trips_QTS <- read.csv(paste0(QTS_location, "5_QTS_TRIPS.csv"), as.is=T, fileEncoding="UTF-8-BOM") %>%
    
    # rename 'TRAVTIME' as 'total_time'
    # Note that 'total_time' differs from the Melbourne field 'TRAVTIME': 
    # - Melbourne 'TRAVTIME' seems to be an exact summary of the travel times for the trip stages,
    # - whereas  Brisbane 'total_time' can be longer - presumably including some waiting time in between stages
    # However, the field is not used in the final output anyway
    rename(total_time = TRAVTIME) %>%
    
    # add TRIPNO (sequential number of trips taken by person)
    group_by(PERSID) %>%
    arrange(STARTSTOP, by_group = TRUE) %>%
    mutate(TRIPNO = row_number()) %>%
    ungroup()
  
  hh_person_QTS <- left_join(person_QTS, hh_QTS, by = "HHID")
  
  trips_brisbane <- left_join(trips_QTS, hh_person_QTS, by = c("PERSID", "HHID")) %>%
    
    # confine to Greater Brisbane (excluding Gold Coast and Sunshine Coast)
    dplyr::filter(REGION == "Greater Brisbane") %>%
    
    # select required fields
    dplyr::select(HHID, PERSID, AGEGROUP_CODE, age_band, SEX, TRIPID, day_type, TRAVYEAR,
                  REGION, TRIPNO, MAINMODE, MODE1, MODE2, MODE3, MODE4, 
                  MODE5, MODE6, MODE7, MODE8, total_time, OVERALL_PURPOSE, 
                  PERSWGT20, CUMDIST, ORIGSA1_2021, DESTSA1_2021, HOMELGA = LGA) %>%
    
    # filter to age >= 15 (AGEGROUP_CODE 4 or higher)
    # note that unlike Melbourne, it can't be age > 15, because age is only specified in 5 year blocks
    filter(AGEGROUP_CODE > 3) %>%
    
    # categorise trip modes (outcome is car, motorcycle, pedestrian, bicycle, bus, train, 
    # ferry, other - unlike section 1, there is no 'light rail')
    mutate(trip_mode = as.factor(
      case_when(MAINMODE == "Car driver"                     ~ "car",
                MAINMODE == "Car passenger"                  ~ "car",
                MAINMODE == "Taxi"                           ~ "car",
                MAINMODE == "Truck driver"                   ~ "car",
                MAINMODE == "Truck passenger"                ~ "car",
                MAINMODE == "Uber / Other Ride Share"        ~ "car",
                MAINMODE == "Motorcycle driver"              ~ "motorcycle",
                MAINMODE == "Motorcycle passenger"           ~ "motorcycle",
                MAINMODE == "Charter/Courtesy/Other bus"     ~ "bus",
                tolower(MAINMODE) == "public bus"            ~ "bus",  # in MAINMODE, appears both upper and lower
                MAINMODE == "School bus (private/chartered)" ~ "bus",
                MAINMODE == "School bus (with route number)" ~ "bus",
                MAINMODE == "Walking"                        ~ "pedestrian",
                MAINMODE == "Mobility scooter"               ~ "other",
                MAINMODE == "Other method"                   ~ "other",
                # if meeting none of these criteria, keep original value (bicycle, train, ferry)
                TRUE                                         ~ tolower(MAINMODE)))) %>%
    
    # categorise age groups (note that groups 1:3 have already been excluded above)
    mutate(age_group = as.factor(
      case_when(AGEGROUP_CODE == 4         ~ "15 to 19",
                AGEGROUP_CODE %in% c(5:8)  ~ "20 to 39",
                AGEGROUP_CODE %in% c(9:13) ~ "40 to 64",
                AGEGROUP_CODE > 13         ~ "65 plus"))) %>%
    
    # categorise trip distances
    mutate(dist_cat = as.factor(
      case_when(CUMDIST < 1                 ~ "<1km",
                CUMDIST >= 1 & CUMDIST <= 2 ~ "1-2km", 
                CUMDIST <= 5 & CUMDIST > 2  ~ "3-5km", 
                CUMDIST <=10 & CUMDIST > 5  ~ "6-10km",
                CUMDIST > 10                ~ ">10km"))) %>%
    
    # categorise trip purposes
    mutate(OVERALL_PURPOSE = as.factor(
      case_when(OVERALL_PURPOSE == "Recreation"               ~ "Leisure",
                OVERALL_PURPOSE == "Social"                   ~ "Leisure",
                OVERALL_PURPOSE == "Direct Work Commute"      ~ "Work",
                OVERALL_PURPOSE == "Work Related"             ~ "Work",
                OVERALL_PURPOSE == "Accompany Someone"        ~ "Other",
                OVERALL_PURPOSE == "Other Purpose"            ~ "Other",
                OVERALL_PURPOSE == "Personal Business"        ~ "Other",
                OVERALL_PURPOSE == "Pickup/Deliver Something" ~ "Other",
                OVERALL_PURPOSE == "Pickup/Dropoff Someone"   ~ "Other",
                # if meeting none of these criteria, keep original value (Education, Shopping)
                TRUE                                          ~ OVERALL_PURPOSE))) %>%
    
    # reformat fields where required to match Melbourne output
    dplyr::mutate(HHID = as.character(HHID),
                  age_band = as.factor(age_band),
                  sex = as.factor(SEX),
                  TRIPID = as.character(TRIPID),
                  day_type = as.factor(day_type)) %>%
    
    # select required final fields
    dplyr::select(HHID,
                  persid = PERSID,
                  age_band,
                  sex,
                  TRIPID,
                  TRAVYEAR,
                  REGION,
                  trip_id_2 = TRIPNO,
                  MAINMODE,
                  MODE1, MODE2, MODE3, MODE4, MODE5, MODE6, MODE7, MODE8,
                  total_time,
                  trip_purpose = OVERALL_PURPOSE,
                  participant_wt = PERSWGT20,
                  trip_distance = CUMDIST,
                  ORIGSA1_2021,
                  DESTSA1_2021,
                  HOMELGA,
                  trips_wt = PERSWGT20,
                  age_group,
                  trip_mode,
                  day_type,
                  trip_id_2 = TRIPNO,
                  dist_cat)
  
  return(trips_brisbane)
}



###################### 3) Calculate speeds walking and cycling ########################################################
#### To calculate time spent walking and cycling
CalculateAgeSexSpeed <- function(in_data){
  
# in_data="Data/processed/trips_melbourne.csv"
# in_data="Data/processed/trips_brisbane.csv"

trips_city <- read.csv(in_data,as.is=T,fileEncoding="UTF-8-BOM") ## Add age groups to facilitate selection above and matching  

### Use weighted data


#### SPEEDs by age and sex groups
SPEED_WALK <- dplyr::filter(trips_city, trip_mode == "pedestrian") %>%
  mutate(speed_walk=trip_distance*60/trip_duration)

# Exclude 0 speed, some values have time but not distance

SPEED_WALK <- dplyr::filter(SPEED_WALK, trip_distance != 0)

# Check distribution
density_walk <- ggplot(SPEED_WALK, aes(speed_walk)) + geom_density()
# density_walk


cum_density_walk <- ggplot(SPEED_WALK, aes(speed_walk)) + stat_ecdf(geom = "step")
# cum_density_walk

SPEED_WALK <-  SPEED_WALK  %>%
  srvyr::as_survey_design(weights = trips_wt)

SPEED_CYCLE <- dplyr::filter(trips_city, trip_mode == "bicycle") %>%
  mutate(speed_cycle=trip_distance*60/trip_duration)

# Exclude 0 speed, some values have time but not distance

SPEED_CYCLE <- dplyr::filter(SPEED_CYCLE, trip_distance != 0)
# Check distribution
density_cycle <- ggplot(SPEED_CYCLE, aes(speed_cycle)) + geom_density()
# density_cycle

cum_density_cycle <- ggplot(SPEED_CYCLE, aes(speed_cycle)) + stat_ecdf(geom = "step")
# cum_density_cycle

SPEED_CYCLE <-  SPEED_CYCLE  %>%
  srvyr::as_survey_design(weights = trips_wt)

### Calculate weighted statistics

SPEED_WALK <- SPEED_WALK %>% 
  group_by(sex, age_group,
           .drop = FALSE) %>%
  dplyr::summarize(mean= srvyr::survey_mean(speed_walk),
                   quantiles= srvyr::survey_quantile(speed_walk,  c(.25,.5,.75),ci=TRUE)) %>% 
  mutate(activity = "walking")

SPEED_CYCLE <- SPEED_CYCLE %>% 
  group_by(sex, age_group,
           .drop = FALSE) %>%
  dplyr::summarize(mean= srvyr::survey_mean(speed_cycle),
                   quantiles= srvyr::survey_quantile(speed_cycle,  c(.25,.5,.75),ci=TRUE)) %>% 
  mutate(activity = "bicycle")

SPEEDS <- rbind(SPEED_WALK, SPEED_CYCLE)

return(SPEEDS)
}

