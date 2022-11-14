# Script to be run once only, to extract tables from the QTS MS access 
# database file and save as .csv files

# Script must be run in 32-bit version of R, as RODBC library only works in 32-bit
# To select 32-bit R in RStudio, go to Tools > Global Options > General, and then exit and restart.
library(RODBC)

# location of the MS access file
QTS_location = "Data/Travelsurvey/QTS/2020-21_seq_qts_erv1.0.accdb"

# open database connection (see https://leowong.ca/blog/connect-to-microsoft-access-database-via-r/ )
driverinfo <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
path <- paste0(driverinfo, "DBQ=", QTS_location)
channel <- odbcDriverConnect(path)

# get vector of table names
tables <- sqlTables(channel)
tables <- tables[tables$TABLE_TYPE == "TABLE", "TABLE_NAME"]

# for each table, create variable 'table1', 'table2' etc, which is the relevant table from the db file
for (i in 1:length(tables)) {
  assign(paste0("table", i),
         sqlFetch(channel, tables[i]))
}

# for each table, save as a .csv file
for (i in 1:length(tables)) {
  write.csv(get(paste0("table", i)), # retrieves the variable named 'table1', 'table2', etc
            paste0("Data/Travelsurvey/QTS/", tables[i], ".csv"),
            row.names = FALSE)
}


