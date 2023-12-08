Data Preparation 01 - 1
================

Clean full Familinx data, reduce the full data down to working size

## Setup and read profile data

Load necessary packages and read raw profile data file

``` r
# Load packages
library(data.table)
library(dplyr)
library(maps)

# Read full raw profile data
data <- fread(file = params$path_data, na.strings = c("*", ""), encoding = "UTF-8")
```

### Check NAs

Percentage of missing values for all columns in raw data

``` r
na_frac <- sapply(data, function(y) sum(length(which(is.na(y))))/nrow(data)*100)
na_frac <- data.frame(na_frac)

print(round(na_frac, 1))
```

                                                   na_frac
    profileid                                          0.0
    gender                                            17.2
    is_alive                                           0.0
    current_residence_location_city                   95.5
    current_residence_location_state                  96.0
    current_residence_location_county                 98.1
    current_residence_location_country                95.1
    current_residence_location_country_code           97.7
    current_residence_location_latitude               95.1
    current_residence_location_longitude              95.1
    current_residence_location_place_name             96.5
    current_residence_location_resolved_externally     0.0
    current_residence_resolved_extern_confidence       0.0
    current_residence_resolved_extern_type            98.5
    birth_year                                        60.2
    birth_month                                       76.6
    birth_day                                         77.5
    birth_date_circa                                   0.0
    birth_date_text                                   99.0
    birth_location_city                               83.5
    birth_location_state                              85.1
    birth_location_country                            80.7
    birth_location_country_code                       89.8
    birth_location_latitude                           80.9
    birth_location_longitude                          80.9
    birth_location_place_name                         89.5
    birth_location_resolved_externally                 0.0
    birth_location_resolved_extern_confidence          0.0
    birth_location_resolved_extern_type               95.5
    death_year                                        74.3
    death_month                                       82.4
    death_day                                         83.4
    death_date_circa                                   0.0
    death_date_text                                  100.0
    death_location_city                               88.6
    death_location_state                              89.8
    death_location_country                            87.5
    death_location_country_code                       93.4
    death_location_latitude                           87.5
    death_location_longitude                          87.5
    death_location_place_name                         94.3
    death_location_resolved_externally                 0.0
    death_location_resolved_extern_confidence          0.0
    death_location_resolved_extern_type               97.2
    burial_year                                       97.5
    burial_month                                      97.9
    burial_day                                        98.2
    burial_date_circa                                  0.0
    burial_date_text                                 100.0
    burial_location_city                              96.2
    burial_location_state                             96.4
    burial_location_country                           95.8
    burial_location_country_code                      97.7
    burial_location_latitude                          95.8
    burial_location_longitude                         95.8
    burial_location_place_name                        97.4
    burial_location_resolved_externally                0.0
    burial_location_resolved_extern_confidence         0.0
    burial_location_resolved_extern_type              99.1
    baptism_year                                      98.9
    baptism_month                                     98.9
    baptism_day                                       98.9
    baptism_date_circa                                 0.0
    baptism_date_text                                100.0
    baptism_location_city                             99.3
    baptism_location_state                            99.4
    baptism_location_country                          99.2
    baptism_location_country_code                     99.5
    baptism_location_latitude                         99.3
    baptism_location_longitude                        99.3
    baptism_location_place_name                       99.5
    baptism_location_resolved_externally               0.0
    baptism_location_resolved_extern_confidence        0.0
    baptism_location_resolved_extern_type             99.8
    cause_of_death                                    98.5

### Remove uniformative columns

Columns with no relevant information is removed from the data

``` r
del_cols <- c("current_residence_location_resolved_externally",
              "current_residence_resolved_extern_confidence", 
              "current_residence_resolved_extern_type", "birth_date_circa",
              "birth_location_resolved_externally", 
              "birth_location_resolved_extern_confidence", 
              "birth_location_resolved_extern_type", "death_date_circa", 
              "death_location_resolved_externally", 
              "death_location_resolved_extern_confidence", 
              "death_location_resolved_extern_type", "burial_date_circa", 
              "burial_location_resolved_externally", 
              "burial_location_resolved_extern_confidence", 
              "burial_location_resolved_extern_type", "baptism_date_circa", 
              "baptism_location_resolved_externally", 
              "baptism_location_resolved_extern_confidence", 
              "baptism_location_resolved_extern_type")

data[, (del_cols) := NULL] 
```

## Function to only keep profiles with relations

Function by A\*\*\*\*\* C\*\*\*\*\*\*

- Profile is the dataset with all the individual information
- Links is the dataset with all the child-parent relations select only
  the observations that are present (as a child or as a parent) in the
  links dataset

``` r
subset_profiles <- function(links, profile) {
  
  ids <- unique(unlist(links))
  prof_sub <- profile[ profile$profileid %in% ids, ]
  
  return(prof_sub)
  
}
```

Read links data and subset the full data

``` r
links <- fread(file = params$path_links, na.strings = c("*", ""))
data <- subset_profiles(links, data)
```

## Match coordinates

Function to extract location from coordinates by A\*\*\*\*\*
C\*\*\*\*\*\* (rewritten to use data.table)

- Profile is the dataset with all the individual information
- Links is the dataset with all the child-parent relations select only
  the observations that are present (as a child or as a parent) in the
  links dataset

``` r
read_coordinates <- function(profiles, birth_coord=T,
                             baptism_coord=T,burial_coord=T,death_coord=T){
  
  if(birth_coord){
    profiles$birth_location_latitude = fifelse(is.na(profiles$birth_location_longitude) |
                                                profiles$birth_location_latitude==0,
                                              NA_real_,
                                              profiles$birth_location_latitude)
    
    profiles$birth_location_longitude = fifelse(is.na(profiles$birth_location_latitude) |
                                                 profiles$birth_location_longitude==0,NA_real_,
                                               profiles$birth_location_longitude)
    
    data_temp = profiles[!(is.na(birth_location_longitude)) &
                         !(is.na(birth_location_latitude))]
    
    data_temp$birth_coord_country = map.where(database = "world",
                                              data_temp$birth_location_longitude,
                                              data_temp$birth_location_latitude)
    
    data_temp <- data_temp[, .(profileid,birth_coord_country)]
    profiles <-  data_temp[profiles, on="profileid"]
    
  }
  if(baptism_coord){
    profiles$baptism_location_latitude = fifelse(is.na(profiles$baptism_location_longitude) |
                                                  profiles$baptism_location_latitude==0,
                                                NA_real_,
                                                profiles$baptism_location_latitude)
    
    profiles$baptism_location_longitude = fifelse(is.na(profiles$baptism_location_latitude) |
                                                   profiles$baptism_location_longitude==0,
                                                 NA_real_,
                                                 profiles$baptism_location_longitude)
    
    
    data_temp = profiles[!(is.na(baptism_location_longitude)) &
                         !(is.na(baptism_location_latitude))]
    
    data_temp$baptism_coord_country = map.where(database = "world",
                                                data_temp$baptism_location_longitude,
                                                data_temp$baptism_location_latitude)

    data_temp <- data_temp[, .(profileid,baptism_coord_country)]
    profiles <-  data_temp[profiles, on="profileid"]
  }
  
  if(death_coord){
    profiles$death_location_latitude = fifelse(is.na(profiles$death_location_longitude) |
                                                profiles$death_location_latitude==0,
                                              NA_real_,
                                              profiles$death_location_latitude)
    
    profiles$death_location_longitude = fifelse(is.na(profiles$death_location_latitude) |
                                                 profiles$death_location_longitude==0,
                                               NA_real_,
                                               profiles$death_location_longitude)
    
    data_temp = profiles[!(is.na(death_location_longitude)) &
                         !(is.na(death_location_latitude))]
  
    
    data_temp$death_coord_country = map.where(database = "world",
                                              data_temp$death_location_longitude,
                                              data_temp$death_location_latitude)

    data_temp <- data_temp[, .(profileid,death_coord_country)]
    profiles <-  data_temp[profiles, on="profileid"]
    
  }
  if(burial_coord){
    profiles$burial_location_latitude = fifelse(is.na(profiles$burial_location_longitude) |
                                                 profiles$burial_location_latitude==0,
                                               NA_real_,
                                               profiles$burial_location_latitude)
    
    profiles$burial_location_longitude = fifelse(is.na(profiles$burial_location_latitude) |
                                                  profiles$burial_location_longitude==0,
                                                NA_real_,
                                                profiles$burial_location_longitude)
    
    data_temp = profiles[!(is.na(burial_location_longitude)) &
                         !(is.na(burial_location_latitude))]
    
    data_temp$burial_coord_country = map.where(database = "world",
                                               data_temp$burial_location_longitude,
                                               data_temp$burial_location_latitude)

    data_temp <- data_temp[, .(profileid,burial_coord_country)]
    profiles <-  data_temp[profiles, on="profileid"]
  }
    
  return(profiles)
}
```

### Run the coordinate matching

Extract the locations from the coordinates, keep only the country name
from the strings with locations

``` r
data_recoded = read_coordinates(data)

data_recoded$birth_coord = sub(":.*", "", data_recoded$birth_coord_country)
data_recoded$death_coord = sub(":.*", "", data_recoded$death_coord_country)
data_recoded$burial_coord = sub(":.*", "", data_recoded$burial_coord_country)
data_recoded$baptism_coord = sub(":.*", "", data_recoded$baptism_coord_country)
```

Drop extra columns: is_alive, month, text and day of birth, death,
burial and baptism

``` r
data_recoded <- data_recoded %>% 
  select(
    "profileid", "gender", ends_with("country"), ends_with("city"),
     ends_with("state"), ends_with("county"), ends_with("country_code"),
     ends_with("latitude"), ends_with("longitude"), ends_with("place_name"),
     ends_with("year"), ends_with("coord")
    )
```

## Save data to file

Data with coordinate matching for next step - 01 - Data Imputation

``` r
fwrite(data_recoded, params$save_path)
```
