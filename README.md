# Transport Health Assessment Tool for Melbourne 
(THAT-Melbourne)

## Citation

Zapata-Diomedi Belen, Ali Abbas and Alan Both (2021). Transport Health Assessment Tool for Melbourne (THAT-Melbourne). R code.

## Introduction 

The Transport Health Assessment Tool for Melbourne (THAT-Melbourne) quantifies physical activity-related health impacts arising from a range of scenarios where short car trips are replaced by walking, cycling or a combination of both, based on data from metropolitan Melbourne.

The Transport Health Assessment Tool for Melbourne models scenarios for replacing short car trips with walking, cycling or a combination of both, accommodating options for trip purpose; age groups; and sex. The model always replaces car trips. The maximum possible distance to be replaced with walking is 2 kilometres and 5 kilometres for cycling. Note that distances replaced correspond to stages of a trip, for example, a trip to work may include multiple stages including walking to a train station, train ride itself, and walking from the station to the place of employment. Trip purposes were grouped into: 1) all trips, which includes work-related, education, leisure, shopping, pick-up or drop-off someone/something, personal business, other, accompanying someone, and at or going home; 2) commuting, which includes work-related and education trips.
The model had an older version with results presented in the Australian Urban Observatory and the latest version with an associated manuscript. The code here refers to the latest version.

## Run the model

See below for steps for running the model and generating outputs

1) model_script_trips_replace.R: runs the model with two options, probabilistic and deterministic
2) presentation_paper: functions to visualise outcomes

## Data

The data to run the model is available in the data folder. 

## Results
Complete results for the latest version of the model can be found here ( https://belenzapata-diomedi.shinyapps.io/presentation_dashboard_MD/).