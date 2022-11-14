# SYNTHETIC POPULATION OF VISTA (MELBOURNE) / QTS (BRISBANE) PERSONS AND NHS PERSONS
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
suppressPackageStartupMessages(library(readxl)) # for reading SEIFA data for Brisbane


######################  1) calculateVistaTravelData (Melbourne) & calculateQtsTravelData (Brisbane)  ############################
# TRAVEL DATA PERSON FILE

### calculateVistaTravelData (Melbourne)
#-------------------------------------------------#
## Join VISTA persons to VISTA Households
calculateVistaTravelData <- function(hh_VISTA_location,person_VISTA_location,ses_index_location) {
  # hh_VISTA_location="Data/Travelsurvey/VISTA12-18/H_VISTA_1218_V1.csv"
  # person_VISTA_location="Data/Travelsurvey/VISTA12-18/P_VISTA1218_V1.csv"
  # ses_index_location="Data/Travelsurvey/ABS SEIFA/ses.csv"
  
  hh_VISTA <- read.csv(hh_VISTA_location,as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID,SurveyPeriod,HomeSubRegion,HOMEPC) %>%
    filter(HHID!="") # some rows were completely blank
  
  person_VISTA <- read.csv(person_VISTA_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID, HHID, AGE, SEX, FULLTIMEWORK, ANYWORK, STUDYING, ANZSCO1,
                  ANZSIC1, WDPERSWGT, WEPERSWGT)
  ## Add SEIFA-IRSD
  ses_index <- read.csv(ses_index_location,as.is=T, fileEncoding="UTF-8-BOM") %>% 
    rename_all(~c("HOMEPC","ses")) %>%
    filter(!is.na(HOMEPC))
  
  ## Join persons and household, keep data for greater Melbourne only and create unique weights
  persons_travel <- left_join(person_VISTA, hh_VISTA, by = "HHID") %>% 
    filter(SurveyPeriod == "2017-18" &
             (HomeSubRegion != "Geelong" | HomeSubRegion != "Other")) %>%
    rowwise() %>% # want to sum across rows, not down columns
    mutate(participant_wt = sum(as.numeric(WDPERSWGT),as.numeric(WEPERSWGT),na.rm=T)) %>%
    dplyr::select(-WDPERSWGT,-WEPERSWGT) %>%
    as.data.frame() %>%
    inner_join(ses_index, by="HOMEPC") %>%
    ### Create age category as persons_pa (from NHS) is only available by age groups
    rename(age=AGE) %>%
    mutate(age_group = case_when(age <   5             ~  1,
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
                                 age >= 85             ~ 19)) %>%
    mutate(age_group_2 = case_when(age <   5             ~  2, ### for pifs calculations Melbourne model
                                 age >=  5 & age <=  9 ~  7,
                                 age >= 10 & age <= 14 ~  12,
                                 age >= 15 & age <= 17 ~  17, 
                                 age >= 18 & age <= 19 ~  17,
                                 age >= 20 & age <= 24 ~  22,
                                 age >= 25 & age <= 29 ~  27, 
                                 age >= 30 & age <= 34 ~  32, 
                                 age >= 35 & age <= 39 ~  37, 
                                 age >= 40 & age <= 44 ~ 42,
                                 age >= 45 & age <= 49 ~ 47, 
                                 age >= 50 & age <= 54 ~ 52, 
                                 age >= 55 & age <= 59 ~ 57, 
                                 age >= 60 & age <= 64 ~ 62, 
                                 age >= 65 & age <= 69 ~ 67,
                                 age >= 70 & age <= 74 ~ 72, 
                                 age >= 75 & age <= 79 ~ 77,
                                 age >= 80 & age <= 84 ~ 82,
                                 age >= 85 & age <= 89 ~ 87,
                                 age >= 90 & age <= 94 ~ 92,
                                 age >=95 ~ 97)) %>%
    #### Only keep adults over 18
    filter(age_group>4) %>%
    ### Sex to match persons_pa sex variable
    rename(sex=SEX) %>%
    mutate(sex=case_when(sex=="M" ~ 'male', sex=="F" ~ 'female')) %>%
    ### Employment status to match persons_pa work_status variable (LFSBC)
    mutate(work_status = ifelse(ANYWORK=="Yes",'employed','unemployed')) %>%
    ### Classification of occupation (ANZSCO1)
    mutate(occupation_cat = ifelse(ANZSCO1=="Missing/Refused",NA,ANZSCO1)) %>%
    ### Classification for industry
    mutate(industry_cat = ifelse(ANZSIC1=="Missing/Refused",NA,ANZSIC1)) %>%
    ### Change observations to match pa data
    mutate(study_full = case_when(STUDYING=="Part-time TAFE/Uni" ~ "Part time",
                                  STUDYING=="Full-time TAFE/Uni" ~ "Full time")) %>%
    ### Change observations to match pa data
    rename(work_full = FULLTIMEWORK) %>%
    rename(persid = PERSID) %>%
    rename(hhid = HHID) %>%
    dplyr::select(persid,hhid,age,age_group, age_group_2, sex,work_status,work_full,study_full,
                  occupation_cat,industry_cat,SurveyPeriod,HomeSubRegion,HOMEPC,
                  participant_wt,ses)
  
  return(persons_travel)
  
}


### calculateQtsTravelData (Brisbane) - notes
#-------------------------------------------------#
## Based on equivalent function for Melbourne.

## Brisbane output differs from Melbourne output as follows.
## - SEIFA IRSD deciles are based on SA1s rather than Postcodes.  As IRSD is 
##     from 2016 and SA1s are from 2021, they based on table of SA1 correspondences. 
## - There is no 'age' column.  Instead, there is an 'age_band' column, 
##     in 5 year bands. The Brisbane data does not include exact age. Also:
##     -- the Melbourne output excludes persons aged 15 and under, but this is not
##     possible for Brisbane, and so persons aged under 15 are excluded instead;
##     -- in 'age_group' ages 15-17 should be category 4, and ages 18-19 category 5, 
##        but these cannot be distinguished in the QTS data, so ages 15-19 are all 
##        categorised as category 5.  Alternative would be to exclude all 15-19 year olds.
## - 'TRAVYEAR' (which is a single year rather than a period) replaces 'SurveyPeriod'.
## - 'REGION' replaces 'HomeSubRegion.
## - 'SA1_CODE_2021' replaces 'PCODE'.
## - In 'occupation_cat', Brisbane has 'Professional', 'Technicians and Trades Worker' 
##     and 'Clerical and Administrative Worker' (in each case, matching the 
##     categories in persons_pa and NHS CURF), whereas Melbourne has plurals
##     'Professionals' and 'Workers'.

## Join QTS persons to QTS Households
calculateQtsTravelData <- function(QTS_location, ses_index_location, SA1_2016_2021_location) {
  
  # QTS_location = "Data/Travelsurvey/QTS/"
  # ses_index_location = "Data/Travelsurvey/ABS SEIFA/2033055001 - sa1 indexes.xls"
  # SA1_2016_2021_location = "Data/Travelsurvey/ABS SEIFA/CG_SA1_2016_SA1_2021.csv"
  
  region_QTS <- read.csv(paste0(QTS_location, "R_REGION.csv"), as.is=T, fileEncoding="UTF-8-BOM")
  agegroup_QTS <- read.csv(paste0(QTS_location, "RP_AGE_GROUP.csv"), as.is=T, fileEncoding="UTF-8-BOM")
  anzsco_QTS <- read.csv(paste0(QTS_location, "R_ANZSCO.csv"), as.is=T, fileEncoding="UTF-8-BOM")
  
  hh_QTS <- read.csv(paste0(QTS_location, "1_QTS_HOUSEHOLDS.csv"), as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(HHID, TRAVYEAR,  HOME_SA1_2021, stratagroupid = STRATA_LGA) %>%
    mutate(SA1_CODE_2021 = as.character(HOME_SA1_2021)) %>%
    left_join(region_QTS, by = "stratagroupid")
  
  person_QTS <- person_QTS <- read.csv(paste0(QTS_location, "2_QTS_PERSONS.csv"), as.is=T, fileEncoding="UTF-8-BOM") %>%
    dplyr::select(PERSID, HHID, AGEGROUP_CODE = AGEGROUP, SEX, WORKSTATUS, STUDYING, ANZSCO_1 = ANZSCO_1.digit, 
                  INDUSTRY, PERSWGT20) %>%
    
    # add ages
    left_join(agegroup_QTS %>% dplyr::select(AGEGROUP_CODE, age_band = DESCRIPTION) %>% distinct(),
              by = "AGEGROUP_CODE") %>%
    
    # add ANZSCO
    left_join(anzsco_QTS %>% dplyr::select(ANZSCO_1, Description_1) %>% distinct(),
              by = "ANZSCO_1")
  
  # 2016-2021 SA1 correspondences (needed for 2016 SEIFA IRSD)
  SA1_2016_2021 <- read.csv(SA1_2016_2021_location, as.is=T, fileEncoding="UTF-8-BOM")
  
  ## Add SEIFA-IRSD
  # SEIFA data by SA1: omit headings (rows 1-5); retain only cols 2 (SA1 code) and 4 (IRSD decile) 
  ses_index <- readxl::read_xls(ses_index_location, sheet = "Table 1", range = cell_rows(c(6, NA))) %>%
    dplyr::select(c(2, 4)) %>%
    rename_all(~c("SA1_MAINCODE_2016", "ses")) %>%
    filter(!is.na(SA1_MAINCODE_2016)) %>%
    # join table with corresponding 2021 codes
    left_join(SA1_2016_2021, by = "SA1_MAINCODE_2016") %>%
    
    # where 2021 SA1 corresponds to more than one 2016 SA1, pick the one with the highest correspondence ratio
    group_by(SA1_CODE_2021) %>%
    filter(RATIO_FROM_TO == max(RATIO_FROM_TO, na.rm = TRUE)) %>%
    ungroup() %>%
    
    # select required fields
    dplyr::select(SA1_CODE_2021, SA1_MAINCODE_2016, ses)
  
  
  ## Join persons and household, keep data for greater Brisbane only
  persons_travel <- left_join(person_QTS, hh_QTS, by = "HHID") %>%
    filter(REGION == "Greater Brisbane") %>%  # that is, excluding Gold Coast and Sunshine Coast
    
    # join SEIFA IRSD
    inner_join(ses_index, by = "SA1_CODE_2021") %>%
    
    # create age category as persons_pa (from NSH) is only available by age groups
    ## NOTE - ages 15-17 should be category 4, and ages 18-19 category 5, but these
    ## cannot be distinguished in the QTS data, so ages 15-19 are all categoriesed
    ## as category 5.  Alternative is to exclude all 15-19 year olds.
    mutate(age_group = case_when(age_band == "0-4 years"          ~  1,
                                 age_band == "5-9 years"          ~  2,
                                 age_band == "10-14 years"        ~  3,
                                 age_band == "15-19 years"        ~  5,  # see note above
                                 age_band == "20-24 years"        ~  6,
                                 age_band == "25-29 years"        ~  7,
                                 age_band == "30-34 years"        ~  8,
                                 age_band == "35-39 years"        ~  9,
                                 age_band == "40-44 years"        ~ 10,
                                 age_band == "45-49 years"        ~ 11,
                                 age_band == "50-54 years"        ~ 12,
                                 age_band == "55-59 years"        ~ 13,
                                 age_band == "60-64 years"        ~ 14,
                                 age_band == "65-69 years"        ~ 15,
                                 age_band == "70-74 years"        ~ 16,
                                 age_band == "75-79 years"        ~ 17,
                                 age_band == "80-84 years"        ~ 18,
                                 age_band == "85-89 years"        ~ 19,
                                 age_band == "90-94 years"        ~ 19,
                                 age_band == "95-99 years"        ~ 19,
                                 age_band == "100 years and over" ~ 19)) %>%
    
    mutate(age_group_2 = case_when(age_band == "0-4 years"         ~  2,  ### [for pif calculations [Melbourne?] model]?
                                   age_band == "5-9 years"          ~  7,
                                   age_band == "10-14 years"        ~ 12,
                                   age_band == "15-19 years"        ~ 17,
                                   age_band == "20-24 years"        ~ 22,
                                   age_band == "25-29 years"        ~ 27,
                                   age_band == "30-34 years"        ~ 32,
                                   age_band == "35-39 years"        ~ 37,
                                   age_band == "40-44 years"        ~ 42,
                                   age_band == "45-49 years"        ~ 47,
                                   age_band == "50-54 years"        ~ 52,
                                   age_band == "55-59 years"        ~ 57,
                                   age_band == "60-64 years"        ~ 62,
                                   age_band == "65-69 years"        ~ 67,
                                   age_band == "70-74 years"        ~ 72,
                                   age_band == "75-79 years"        ~ 77,
                                   age_band == "80-84 years"        ~ 82,
                                   age_band == "85-89 years"        ~ 87,
                                   age_band == "90-94 years"        ~ 92,
                                   age_band == "95-99 years"        ~ 97,
                                   age_band == "100 years and over" ~ 97)) %>%
    
    # only keep people 15 and over
    # NOTE - Melbourne code at this point only keeps people 18 and over
    filter(age_group > 4) %>%
    
    # sex to match persons_pa sex variable
    rename(sex = SEX) %>%
    
    # employment status to match persons_pa work_status and work_full variables
    mutate(work_status = ifelse(WORKSTATUS %in% c("workFullTime", "workPartTime"),
                                "employed",
                                "unemployed"),
           work_full = ifelse(WORKSTATUS %in% c("workFullTime"),
                              "Yes",
                              "No")) %>%
    
    # classification of occupation (ANZSO1)
    mutate(occupation_cat = 
             case_when(work_status == "unemployed"      ~ "Not in Work Force",
                       # others are where work_status is "employed" - 
                       Description_1 == "MANAGERS"      ~ "Managers",
                       Description_1 == "PROFESSIONALS" ~ "Professional",  # cf Melbourne - 'Professionals'
                       Description_1 == "TECHNICIANS AND TRADES WORKERS" ~ "Technicians and Trades Worker", # cf Melbourne - 'Workers'
                       Description_1 == "COMMUNITY AND PERSONAL SERVICE WORKERS" ~ "Community and Personal Service Workers",
                       Description_1 == "CLERICAL AND ADMINISTRATIVE WORKERS" ~ "Clerical and Administrative Worker", # cf Melbourne - 'Workers'
                       Description_1 == "SALES WORKERS" ~ "Sales Workers", 
                       Description_1 == "MACHINERY OPERATORS AND DRIVERS" ~ "Machinery Operators and Drivers", 
                       Description_1 == "LABOURERS"     ~ "Labourers", 
                       Description_1 == "MISCELLANEOUS" ~ as.character(NA),
                       is.na(Description_1)             ~ as.character(NA))) %>%
    
    # classification for industry
    mutate(industry_cat = 
             case_when(work_status == "unemployed" ~ "Not in Work Force",
                       # others are where work_status is "employed" - 
                       INDUSTRY == "agriculture"   ~ "Agriculture, Forestry and Fishing",
                       INDUSTRY == "mining"        ~ "Mining",
                       INDUSTRY == "manufacturing" ~ "Manufacturing",
                       INDUSTRY == "utilities"     ~ "Electricity, Gas, Water and Waste Services",
                       INDUSTRY == "construction"  ~ "Construction",
                       INDUSTRY == "wholesale"     ~ "Wholesale Trade",
                       INDUSTRY == "retail"        ~ "Retail Trade",
                       INDUSTRY == "accom"         ~ "Accommodation and Food Services",
                       INDUSTRY == "transport"     ~ "Transport, Postal and Warehousing",
                       INDUSTRY == "it"            ~ "Information Media and Telecommunications",
                       INDUSTRY == "financial"     ~ "Financial and Insurance Services",
                       INDUSTRY == "realEstate"    ~ "Rental, Hiring and Real Estate Services",
                       INDUSTRY == "professional"  ~ "Professional, Scientific and Technical Services",
                       INDUSTRY == "admin"         ~ "Administrative and Support Services",
                       INDUSTRY == "publicAdmin"   ~ "Public Administration and Safety",
                       INDUSTRY == "education"     ~ "Education and Training",
                       INDUSTRY == "health"        ~ "Health Care and Social Assistance",
                       INDUSTRY == "arts"          ~ "Arts and Recreation Services",
                       INDUSTRY == "other"         ~ "Other Services",
                       # fallback for any others (but there are none)
                       TRUE                        ~ "Inadequately described")) %>%
    
    # study status to match persons_pa study_full variable
    mutate(study_full = 
             case_when(STUDYING == "tertiaryPartTime" ~ "Part time",
                       STUDYING == "tertiaryFullTime" ~ "Full time")) %>%
    
    # select required fields
    dplyr::select(persid = PERSID,
                  hhid = HHID,
                  age_band, 
                  age_group,
                  age_group_2,
                  sex,
                  work_status,
                  work_full,
                  study_full,
                  occupation_cat,
                  industry_cat,
                  TRAVYEAR, 
                  REGION, 
                  SA1_CODE_2021, 
                  participant_wt = PERSWGT20, 
                  ses)
  
  return(persons_travel)
  
}

######################  2) calculatePersonsTravelScenario  ############################

calculatePersonsTravelScenario <- function(travel_data_location,scenario_location) {
  # travel_data_location="Data/processed/travel_data_melbourne.csv"
  # scenario_location="scenarios/melbourne_scenarios/scenarioTrips/all_0_2.csv"
  # scenario_location=scenario_trips
    
  # travel_data_location="Data/processed/travel_data_brisbane.csv"
  # scenario_location="scenarios/brisbane_scenarios/scenarioTrips/all_0_2.csv"
  # scenario_location=scenario_trips
  
  
  ### Original set
    # "Data/processed/trips_melbourne_scenarios.csv"

  
  travel_data <- read.csv(travel_data_location,as.is=T, fileEncoding="UTF-8-BOM")
  
  # if scenario_location is a file location, read the csv. If not, then use it
  # as a dataframe.
  trips_city <- NULL
   if(is.character(scenario_location)) {
     trips_city <- read.csv(scenario_location,as.is=T,fileEncoding="UTF-8-BOM")
  }
  if(!is.character(scenario_location)) {
    trips_city <- scenario_location
  }
  trips_city <- trips_city %>% mutate(persid=toupper(persid))
  
  ### Create total duration and distance for all modes, rather long process here. 
  ### The intervention will change the trips file (scenario trips file) which in 
  ### turn will feed onto the persons file. 
  
  #### Scenario
  total_trips_time_dist_base <- trips_city %>%
    dplyr::select(persid, trip_mode_base, trip_duration_base_hrs, trip_distance_base) %>%
    # group by person and travel mode
    dplyr::group_by(persid, trip_mode_base) %>%
    # find the total distance and time traveled for each person by travel mode
    dplyr::summarise(time_base=sum(trip_duration_base_hrs),
                     distance_base=sum(trip_distance_base)) %>%
    # expand time_base and distance_base to separate pairs of columns for each
    # travel mode
    pivot_wider(names_from=trip_mode_base,values_from=c(time_base, distance_base)) %>%
    # rearrange the columns
    dplyr::select(persid,time_base_car,distance_base_car,
                  time_base_walking,distance_base_walking,
                  time_base_public.transport,distance_base_public.transport,
                  time_base_other,distance_base_other,
                  time_base_bicycle,distance_base_bicycle)
  
  #### Scenario
  total_trips_time_dist_scen <- trips_city %>%
    dplyr::select(persid, trip_mode_scen, trip_duration_scen_hrs, trip_distance_scen) %>%
    # group by person and travel mode
    dplyr::group_by(persid, trip_mode_scen) %>%
    # find the total distance and time traveled for each person by travel mode
    dplyr::summarise(time_scen=sum(trip_duration_scen_hrs),
                     distance_scen=sum(trip_distance_scen)) %>%
    # expand time_scen and distance_scen to separate pairs of columns for each
    # travel mode
    pivot_wider(names_from=trip_mode_scen,values_from=c(time_scen, distance_scen)) %>%
    # rearrange the columns
    dplyr::select(persid,time_scen_car,distance_scen_car,
                  time_scen_walking,distance_scen_walking,
                  time_scen_public.transport,distance_scen_public.transport,
                  time_scen_other,distance_scen_other,
                  time_scen_bicycle,distance_scen_bicycle)
  
  
  ### Do this last appending all
  persons_travel <- travel_data %>%
    left_join(total_trips_time_dist_base, by = "persid") %>%
    left_join(total_trips_time_dist_scen, by = "persid")
  
  # ### Create walking yes or no
  persons_travel <- persons_travel %>%
    mutate(walk_base = case_when(is.na(time_base_walking) ~ "No",
                                 time_base_walking > 0  ~ "Yes")) %>%
    mutate(walk_scen = case_when(is.na(time_scen_walking) ~ "No",
                                 time_scen_walking > 0  ~ "Yes"))  %>%
    
  ### Create walking min of 2 (to improve matching)
  mutate(walk_base_min = case_when(time_base_walking <=2 ~ "No",
                                   is.na(time_base_walking) ~ "No",
                                   time_base_walking > 2  ~ "Yes")) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  #not needed
  # names(persons_travel)[1:95] <- RemoveAllWs(tolower(names(persons_travel)[1:95]))
  
  return(persons_travel)
}


######################  3) calculatePersonsPA  ################################
# PA PERSON AND HOUSEHOLD FILE

### Create variables to match with travel survey and for pa analysis
calculatePersonsPA <- function(pa_location,hh_location) {
  # pa_location="Data/Physical activity/NHS2017-18_CSV/NHS17SPB.csv"
  # hh_location="Data/Physical activity/NHS2017-18_CSV/NHS17HHB.csv"

  pa <- read.csv(pa_location,as.is=T, fileEncoding="UTF-8-BOM") %>%
    
    dplyr::select(ABSHIDB, SEX, LFSBC, OCCUP13B, ANZSICBC, USHRWKB, STDYFTPT, AGEB,
                  EXTRAMIN, EXLWMMIN, EXLWVMIN, WPAMMIN, WPAVMIN, MODMINS, VIGMINS, EXFSRMIN,
                  EXLWKTNO, EXNUDAYW, EXNUDST, EXWLKTME, EXNUDTH, NHIFINWT) %>%
    mutate_all(funs(type.convert(replace(., .== 99997, NA)))) %>%
    mutate_all(funs(type.convert(replace(., .== 99998, NA))))
  
 hh <- read.csv(hh_location,as.is=T, fileEncoding="UTF-8-BOM") %>% 
   dplyr::select(ABSHIDB, STATE16, SA1SF2DN, INCDECU1)
  
  persons_pa <- left_join(pa, hh, by="ABSHIDB") %>%
    ### Sex to match persons_pa
    dplyr::rename(sex = SEX) %>%
    mutate(sex = case_when(sex == 1 ~ "male",
                           sex == 2 ~ "female")) %>%
    ### Work status (reclassify "Not in the workforce" coded as 3 and NA coded 
    ### as 0 to "No" to match NHS PA)
    mutate(work_status = case_when(LFSBC == 0 ~ "unemployed",
                                   LFSBC == 1 ~ "employed",
                                   LFSBC == 2 ~ "unemployed",
                                   LFSBC == 3 ~ "unemployed")) %>%
    ### Classification of occupation (OCCUP13B) see Physical Activity-Basic 
    ### CURF.xls for codes references
    mutate(occupation_cat = case_when(OCCUP13B == "0" ~ "Not in Work Force",
                                      OCCUP13B == "1" ~ "Managers",
                                      OCCUP13B == "2" ~ "Professional",
                                      OCCUP13B == "3" ~ "Technicians and Trades Worker", 
                                      OCCUP13B == "4" ~ "Community and Personal Service Workers",
                                      OCCUP13B == "5" ~ "Clerical and Administrative Worker",
                                      OCCUP13B == "6" ~ "Sales Workers", 
                                      OCCUP13B == "7" ~ "Machinery Operators and Drivers", 
                                      OCCUP13B == "8" ~ "Labourers", 
                                      OCCUP13B == "9" ~ "NA")) %>%
    mutate(occupation_cat = ifelse(occupation_cat=="NA",NA,occupation_cat)) %>%
    ### Classification of industry (ANZSICBC) see Physical Activity-Basic CURF.xls
    ### for codes references
    mutate(industry_cat = case_when(ANZSICBC == 1 ~ "Agriculture, Forestry and Fishing",
                                    ANZSICBC == 2 ~ "Mining",
                                    ANZSICBC == 3 ~ "Manufacturing",
                                    ANZSICBC == 4 ~ "Electricity, Gas, Water and Waste Services",
                                    ANZSICBC == 5 ~ "Construction",
                                    ANZSICBC == 6 ~ "Wholesale Trade",
                                    ANZSICBC == 7 ~ "Retail Trade",
                                    ANZSICBC == 8 ~ "Accommodation and Food Services",
                                    ANZSICBC == 9 ~ "Transport, Postal and Warehousing",
                                    ANZSICBC == 10 ~ "Information Media and Telecommunications",
                                    ANZSICBC == 11 ~ "Financial and Insurance Services",
                                    ANZSICBC == 12 ~ "Rental, Hiring and Real Estate Services",
                                    ANZSICBC == 13 ~ "Professional, Scientific and Technical Services",
                                    ANZSICBC == 14 ~ "Administrative and Support Services",
                                    ANZSICBC == 15 ~ "Public Administration and Safety",
                                    ANZSICBC == 16 ~ "Education and Training",
                                    ANZSICBC == 17 ~ "Health Care and Social Assistance",
                                    ANZSICBC == 18 ~ "Arts and Recreation Services",
                                    ANZSICBC == 19 ~ "Other Services",
                                    ANZSICBC == 25 ~ "Inadequately described",
                                    ANZSICBC == 26 ~ "Not in Work Force")) %>%
    ### Create work_full (yes or no) to indicate whether ind work full time or 
    ### not. Derive from USHRWKB
    mutate(work_full = case_when(USHRWKB == 0 ~ "NA",
                                 USHRWKB == 1 | USHRWKB == 2 | USHRWKB == 3 ~ "No",
                                 USHRWKB >= 4 ~ "Yes")) %>%
    mutate(work_full = ifelse(work_full=="NA",NA,work_full)) %>%
    ### Original variable values: 0. Not applicable 1. Studying full-time
    ### 2. Studying part-time 3. Not studying
    mutate(study_full = case_when(STDYFTPT  == 0 | STDYFTPT  == 3 ~ "NA",
                                  STDYFTPT == 1 ~ "Full time",
                                  STDYFTPT  == 2 ~ "Part time")) %>%
    mutate(study_full = ifelse(study_full=="NA",NA,study_full)) %>%
    dplyr::rename(age_group = AGEB) %>%
    dplyr::rename(ses = SA1SF2DN) %>%
    dplyr::rename(state = STATE16) %>%
    mutate(mod_total_hr = MODMINS/60) %>%
    mutate(vig_total_hr = VIGMINS/60) %>%
    mutate(mod_leis_hr = EXLWMMIN/60) %>% 
    mutate(vig_leis_hr = EXLWVMIN/60) %>%
    mutate(mod_work_hr = WPAMMIN/60) %>% 
    mutate(vig_work_hr = WPAVMIN/60) %>%
    mutate(walk_rc = EXFSRMIN/60) %>%
    mutate(walk_trans = EXTRAMIN/60) %>%
    mutate(walk_base = case_when(EXTRAMIN == 0 ~ "No",
                                 EXTRAMIN > 0  ~ "Yes")) %>%
    mutate(walk_base_min = case_when(walk_trans < 2 ~ "No",
                                     walk_trans >= 2  ~ "Yes")) %>%
    ### Add whether participants meet PA guidelines (difference for adults and 
    ### older adults)
    mutate(pa_guide_adults =ifelse((EXNUDAYW >=5 & EXNUDST >=2 & 
                                      (EXWLKTME + EXLWMMIN + EXLWVMIN*2) >= 150),
                                   "Yes", "No")) %>%
    mutate(pa_guide_older_adults = ifelse(EXNUDAYW >= 5 & EXNUDTH >=5, "Yes", "No")) %>%
    
    dplyr::filter(age_group >4) %>%
    
    
    ## Add age group variable
    mutate(age_group_scen = case_when(age_group == 5 ~ "15 to 19",
                                      age_group == 6 ~ "20 to 24",
                                      age_group == 7 ~ "25 to 29", 
                                      age_group == 8 ~ "30 to 34", 
                                      age_group == 9 ~ "35 to 39",
                                      age_group == 10 ~ "40 to 44",
                                      age_group == 11 ~ "45 to 49", 
                                      age_group == 12 ~ "50 to 54",
                                      age_group == 13 ~ "55 to 59",
                                      age_group == 14 ~ "60 to 64", 
                                      age_group == 15 ~ "65 to 69",
                                      age_group == 16 ~ "70 to 74",
                                      age_group == 17 ~ "75 to 79", 
                                      age_group == 18 ~ "80 to 84",
                                      age_group == 19 ~ "85 +")) %>%
    
    mutate(dem_index = case_when(age_group == 5 & sex == "male" ~  1,
                                 age_group == 6 & sex == "male" ~  3,
                                 age_group == 7 & sex == "male" ~  5,
                                 age_group == 8 & sex == "male" ~  7,
                                 age_group == 9 & sex == "male" ~  9,
                                 age_group == 10 & sex == "male" ~ 11,
                                 age_group == 11 & sex == "male" ~  13, 
                                 age_group == 12 & sex == "male" ~  15,
                                 age_group == 13 & sex == "male" ~ 17,
                                 age_group == 14 & sex == "male" ~ 19, 
                                 age_group == 15 & sex == "male" ~ 21,
                                 age_group == 16 & sex == "male" ~ 23, 
                                 age_group == 17 & sex == "male" ~ 25, 
                                 age_group == 18 & sex == "male" ~ 27,
                                 age_group == 19 & sex == "male" ~ 29,
                                 age_group == 5 & sex == "female" ~  2,
                                 age_group == 6 & sex == "female" ~  4,
                                 age_group == 7 & sex == "female" ~  6,
                                 age_group == 8 & sex == "female" ~  8,
                                 age_group == 9 & sex == "female" ~  10,
                                 age_group == 10 & sex == "female" ~  12,
                                 age_group == 11 & sex == "female" ~  14, 
                                 age_group == 12 & sex == "female" ~  16,
                                 age_group == 13 & sex == "female" ~ 18,
                                 age_group == 14 & sex == "female" ~ 20, 
                                 age_group == 15 & sex == "female" ~ 22,
                                 age_group == 16 & sex == "female" ~ 24, 
                                 age_group == 17 & sex == "female" ~ 26, 
                                 age_group == 18 & sex == "female" ~ 28,
                                 age_group == 19 & sex == "female" ~ 30)) %>%  
    
    
    dplyr::rename(participant_wt = NHIFINWT) %>%
    
    dplyr::select(ABSHIDB, age_group, age_group_scen, sex, ses, walk_base, work_status, participant_wt, dem_index, age_group_scen, 
                  mod_total_hr, vig_total_hr, mod_leis_hr, vig_leis_hr, mod_work_hr, vig_work_hr, walk_rc, walk_trans, EXTRAMIN, walk_base_min,
                  pa_guide_adults, pa_guide_older_adults) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) ### Replace NAs with zeros, then in descriptive stats all taken into acccount, not only those with values
  
  return(persons_pa)
}


######################  4) calculatePersonsMatch  ################################

##### Randomly allocate leisure time, work time and walk for transport variables from persons_pa to persons_travel

# I am aiming to randomly assign the following variables from dataset persons_pa
# to dataset persons_travel (both datasets are attached): 
### mod_total_hr
###  vig_total_hr
### mod_leis_hr
### vig_leis_hr
### mod_work_hr
### vig_work_hr
### walk_rc
### walk_trans
### walk_base

## Same function applies to Melbourne and Brisbane, but
## Brisbane output differs from Melbourne output as follows.
## - There is no 'age' column.  Instead, there is an 'age_band' column, 
##     in 5 year bands. The Brisbane data does not include exact age. As
##     a consequence, all 15-19 year olds from the travel data are treated
##     as 'category 5' (which, in the pa data, is 18-19 year olds), and
##     15-19 year olds from the travel data are all matched with 18-19 year
##     olds from the NHS data.


calculatePersonsMatch <- function(pa_location,persons_travel_location) {
  pa_location="Data/processed/persons_pa.csv"
  # persons_travel_location="Data/processed/persons_travel.csv"
  # persons_travel_location=persons_travel
  persons_pa <- read.csv(pa_location,as.is=T, fileEncoding="UTF-8-BOM")

  # if persons_travel_location is a file location, read the csv. If not, then
  # use it as a dataframe.
  persons_travel <- NULL
  if(is.character(persons_travel_location)) {
    persons_travel <- read.csv(persons_travel_location,as.is=T, fileEncoding="UTF-8-BOM")
  }
  if(!is.character(persons_travel_location)) {
    persons_travel <- persons_travel_location
  }

  # This joins the two tables based on the match variables. This is all of the 
  # possible group combinations. We then assign a unique per group number.
  # Some people have no viable matches, so using a left join means that these
  # people will have NA for their values. I'd suggest using wider age ranges.
  
  persons_matched <- left_join(persons_travel, persons_pa,
                               by=c("age_group", "sex", "ses", "walk_base", "walk_base_min", "work_status")) %>% 
    group_by(persid) %>%
    # group_number is a unique number (1:n) for each of a persid's possible matches
    dplyr::mutate(group_number=row_number()) %>%
    ungroup()
  
  # This we then find the group size for each person and pick a random number
  # between 1 and the group size.
  # Some groups are large, some only have one member so they're not really random
  # set.seed ensures that the randomization will produce the same results each
  # time, MUST BE REMOVED FOR PRODUCTION!
  set.seed(12)
  persons_matched_random<- persons_matched %>%
    dplyr::group_by(persid) %>%
    dplyr::summarize(group_size=dplyr::n()) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(random_sample=round(runif(1, min=1, max=group_size)))
  
  # Making the final table
  persons_matched_final <- persons_matched_random %>%
    left_join(persons_matched,
              by=c("persid"="persid","random_sample"="group_number")) 
  
  # Check matches
  
  check_match <- 1-(sum(is.na(persons_matched_final$walk_trans))/nrow(persons_matched_final))
  
  cat(paste0("The percentage of sucessful matches is: ",
             round(check_match*100,2),"%"))
 
  
  ### Add demographic groups to match with ITHIMR style code
  ### for Melbourne - based on 'age'; for Brisbane - based on 'age_band'
  if ("age" %in% colnames(persons_matched_final)) {  # Melbourne has 'age'
    persons_matched_final<- persons_matched_final %>%
      mutate(dem_index = case_when(age <= 19              & sex ==   "male" ~  1,
                                   age >= 20 & age <=  24 & sex ==   "male" ~  3,
                                   age >= 25 & age <=  29 & sex ==   "male" ~  5,
                                   age >= 30 & age <=  34 & sex ==   "male" ~  7,
                                   age >= 35 & age <=  39 & sex ==   "male" ~  9,
                                   age >= 40 & age <=  44 & sex ==   "male" ~ 11,
                                   age >= 45 & age <=  49 & sex ==   "male" ~ 13,
                                   age >= 50 & age <=  54 & sex ==   "male" ~ 15, 
                                   age >= 55 & age <=  59 & sex ==   "male" ~ 17,
                                   age >= 60 & age <=  64 & sex ==   "male" ~ 19,
                                   age >= 65 & age <=  69 & sex ==   "male" ~ 21, 
                                   age >= 70 & age <=  74 & sex ==   "male" ~ 23,
                                   age >= 75 & age <=  79 & sex ==   "male" ~ 25, 
                                   age >= 80 & age <=  84 & sex ==   "male" ~ 27, 
                                   age >= 85 & age <=  89 & sex ==   "male" ~ 29,
                                   age >= 90 & age <=  94 & sex ==   "male" ~ 31,
                                   age >= 95 & age <= 120 & sex ==   "male" ~ 33,
                                   age <= 19              & sex == "female" ~  2,
                                   age >= 20 & age <=  24 & sex == "female" ~  4,
                                   age >= 25 & age <=  29 & sex == "female" ~  6,
                                   age >= 30 & age <=  34 & sex == "female" ~  8,
                                   age >= 35 & age <=  39 & sex == "female" ~ 10,
                                   age >= 40 & age <=  44 & sex == "female" ~ 12,
                                   age >= 45 & age <=  49 & sex == "female" ~ 14,
                                   age >= 50 & age <=  54 & sex == "female" ~ 16,
                                   age >= 55 & age <=  59 & sex == "female" ~ 18,
                                   age >= 60 & age <=  64 & sex == "female" ~ 20,
                                   age >= 65 & age <=  69 & sex == "female" ~ 22,
                                   age >= 70 & age <=  74 & sex == "female" ~ 24,
                                   age >= 75 & age <=  79 & sex == "female" ~ 26,
                                   age >= 80 & age <=  84 & sex == "female" ~ 28,
                                   age >= 85 & age <=  89 & sex == "female" ~ 30,
                                   age >= 90 & age <=  94 & sex == "female" ~ 32,
                                   age >= 95 & age <= 120 & sex == "female" ~ 34))
  } else if ("age_band" %in% colnames(persons_matched_final)) {  # Brisbane has 'age_band'
    persons_matched_final<- persons_matched_final %>%
      mutate(dem_index = case_when(age_band == "15-19 years" & sex == "male" ~  1,
                                   age_band == "20-24 years" & sex == "male" ~  3,
                                   age_band == "25-29 years" & sex == "male" ~  5,
                                   age_band == "30-34 years" & sex == "male" ~  7,
                                   age_band == "35-39 years" & sex == "male" ~  9,
                                   age_band == "40-44 years" & sex == "male" ~ 11,
                                   age_band == "45-49 years" & sex == "male" ~ 13,
                                   age_band == "50-54 years" & sex == "male" ~ 15,
                                   age_band == "55-59 years" & sex == "male" ~ 17,
                                   age_band == "60-64 years" & sex == "male" ~ 19,
                                   age_band == "65-69 years" & sex == "male" ~ 21,
                                   age_band == "70-74 years" & sex == "male" ~ 23,
                                   age_band == "75-79 years" & sex == "male" ~ 25,
                                   age_band == "80-84 years" & sex == "male" ~ 27,
                                   age_band == "85-89 years" & sex == "male" ~ 29,
                                   age_band == "90-94 years" & sex == "male" ~ 31,
                                   age_band == "95-99 years" & sex == "male" ~ 33,
                                   age_band == "100 years and over" & sex == "male" ~ 33,
                                   age_band == "15-19 years" & sex == "female" ~  2,
                                   age_band == "20-24 years" & sex == "female" ~  4,
                                   age_band == "25-29 years" & sex == "female" ~  6,
                                   age_band == "30-34 years" & sex == "female" ~  8,
                                   age_band == "35-39 years" & sex == "female" ~ 10,
                                   age_band == "40-44 years" & sex == "female" ~ 12,
                                   age_band == "45-49 years" & sex == "female" ~ 14,
                                   age_band == "50-54 years" & sex == "female" ~ 16,
                                   age_band == "55-59 years" & sex == "female" ~ 18,
                                   age_band == "60-64 years" & sex == "female" ~ 20,
                                   age_band == "65-69 years" & sex == "female" ~ 22,
                                   age_band == "70-74 years" & sex == "female" ~ 24,
                                   age_band == "75-79 years" & sex == "female" ~ 26,
                                   age_band == "80-84 years" & sex == "female" ~ 28,
                                   age_band == "85-89 years" & sex == "female" ~ 30,
                                   age_band == "90-94 years" & sex == "female" ~ 32,
                                   age_band == "95-99 years" & sex == "female" ~ 34,
                                   age_band == "100 years and over" & sex == "female" ~ 34))
  }
  
  ### participant_w present in both PA and travel data frame, we are interested in the travel weights
  persons_matched_final <- persons_matched_final %>%
    dplyr::rename(participant_wt = participant_wt.x)
  
    
  ### Select variables ### Keep participant_wt for travel survey
  
  # select appropriate age field ('age' for Melbourne; 'age_band' for Brisbane)
  if ("age" %in% colnames(persons_matched_final)) {
    age_field <- "age"
  } else if ("age_band" %in% colnames(persons_matched_final)) {
    age_field <- "age_band"
  }

  persons_matched_final <- persons_matched_final %>%
    dplyr::select(persid, participant_wt, all_of(age_field), sex, ses, dem_index, work_status, age_group_scen, age_group_2,
                  occupation_cat, industry_cat, work_full, study_full,
                  mod_total_hr, vig_total_hr, mod_leis_hr, vig_leis_hr, mod_work_hr, vig_work_hr, walk_rc, walk_trans, walk_base,
                  pa_guide_adults, pa_guide_older_adults,
                  time_base_car , distance_base_car,
                  time_base_walking, distance_base_walking, 
                  time_base_public.transport, distance_base_public.transport,
                  time_base_other, distance_base_other,
                  time_base_bicycle, distance_base_bicycle,
                  time_scen_car, distance_scen_car,
                  time_scen_walking, distance_scen_walking, 
                  time_scen_public.transport, distance_scen_public.transport,
                  time_scen_other, distance_scen_other,
                  time_scen_bicycle, distance_scen_bicycle,
                  walk_base, walk_scen) %>% 
                  mutate(mod_leis_hr = ifelse(mod_leis_hr > 30, 30, mod_leis_hr), #### Limit PA values to 30 hours
                              vig_leis_hr = ifelse(vig_leis_hr > 30, 30, vig_leis_hr), 
                              walk_rc= ifelse(walk_rc > 30, 30, walk_rc), 
                              walk_trans= ifelse(walk_trans > 30, 30, walk_trans), 
                              time_base_walking = ifelse(time_base_walking > 30, 30, time_base_walking), 
                              time_base_bicycle = ifelse(time_base_bicycle > 30, 30, time_base_bicycle))
  
  return(persons_matched_final)
}



