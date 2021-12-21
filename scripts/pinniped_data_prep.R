#######################################################################
###### Data Prep for Pinniped Mgmt Summary #####
######################################################################


library(gt)        # nice tables
library(viridis)   # color palette
library(lubridate) # dates/times
library(rnoaa)     # pull noaa abiotic data
library(readxl)    # read excel files
library(janitor)   # clean data
library(tidyverse) # life

##### sd bay tide data (obs + predictions) #####
# station website
# https://www.ndbc.noaa.gov/station_history.php?station=sdbc1

# data pulled from site below - not available on rnoaa b/c data not qa/qc'ed
# https://tidesandcurrents.noaa.gov/waterlevels.html?id=9410170&units=standard&bdate=20190101&edate=20191231&timezone=GMT&datum=MLLW&interval=h&action=data

tide_data <- list.files(path = '../data/abiotic/sd_tide/',
                        pattern = '*.csv') %>%
  map(~ read_csv(file.path('../data/abiotic/sd_tide/', .))) %>%
  reduce(rbind)

tide_data <- tide_data %>%
  clean_names() %>%
  select(-preliminary_ft) %>%
  # change time zone from gmt to pst to match temp data
  mutate(dtime = with_tz(force_tz(as_datetime(paste(date, time_gmt)), 
                                  tzone = 'UTC'), tzone = 'US/Pacific')) %>%
  select(dtime, predicted_ft, verified_ft)

write_csv(tide_data, '../data/abiotic/sd_bay_tides_assembled.csv')

##### sio pier water temp data #####
# station website
# https://www.ndbc.noaa.gov/station_page.php?station=ljac1

# pull data 2021
years <- 2021

for(i in 1:length(years)) {
  lj <- buoy(buoyid = 'LJAC1', dataset = 'stdmet', year = years[i])$data
  write_csv(lj, paste('../data/abiotic/sio_pier/pier', years[i], '.csv', sep = ''))
}

remove(lj)

# assemble data
pier_data <- list.files(path = '../data/abiotic/sio_pier/',
                        pattern = '*.csv') %>%
  map(~ read_csv(file.path('../data/abiotic/sio_pier/', .))) %>%
  reduce(rbind)

# tidy
pier_data <- pier_data %>%
  # remove columnns with all NAs
  remove_empty(which = c('cols')) %>%
  # make all col names snake case
  clean_names() %>%
  # rename time column
  rename(dtime = time,
         sio_air_temp = air_temperature,
         sio_sst = sea_surface_temperature) %>%
  select(dtime, sio_sst, sio_air_temp)


# write csv
write_csv(pier_data, '../data/abiotic/sio_pier_assembled.csv')

##### North Island Naval Air Station Air Temp Data #####
for (i in 1:length(years)){
  # pull data for a given study year, name it 'weather'
  weather <- lcd(station = '72290023188',
                 year = years[i])
  # write a csv with a file name that contains the year 
  # note: change the part of the path before the first "/" to desired file location
  write_csv(weather, paste('../data/abiotic/navy_north_island_air/SD_Intl_Airport_', years[i], '.csv', sep= ''))
}

# assemble data
air_data <- list.files(path = '../data/abiotic/navy_north_island_air/',
                       pattern = '*.csv') %>%
  map(~ read_csv(file.path('../data/abiotic/navy_north_island_air/', .))) %>%
  reduce(rbind)

air_data <- air_data %>%
  # remove columnns with all NAs
  remove_empty(which = c('cols')) %>%
  # make all col names snake case
  clean_names() %>%
  select(date, hourlydrybulbtemperature) %>%
  # round datetime to nearest hr
  mutate(dtime = round_date(date, unit = 'hour')) %>%
  rename(air_temp = hourlydrybulbtemperature) %>%
  select(-date) %>%
  # get avg air temp for each hr
  group_by(dtime) %>%
  summarize(air_temp = mean(air_temp))

write_csv(air_data, '../data/abiotic/sd_airport_air_data.csv')
remove(weather, years, i)


pin <- read_excel('../data/Pinnniped_Monitoring_CABR_Data2.xlsx', 
                  col_types = c("date", "text", "numeric", 
                                "numeric", "text", "skip", "date", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "text"))

# tidy pinniped
pin <- pin %>%
  select(vista:humans, cloud_cover, wind) %>%
  # make time nearest hr
  mutate(dtime = round_date(datetime, unit = 'hour')) %>%
  select(-datetime)

# join with tide + sio temp data
pin <- pin %>%
  left_join(., pier_data, by = c('dtime')) %>%
  left_join(., select(tide_data, dtime:predicted_ft))

# remove other data
remove(air_data, pier_data, tide_data)

write.csv(pin, '../data/pinniped_summary.csv')