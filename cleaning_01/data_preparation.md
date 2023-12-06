Data Preparation
================

## Clean full familinx data

Reduce the full data down to working size

### Setup

Load necessary packages and paths to raw data and save location of
cleaned file

``` r
# Load packages
library(data.table)
library(mapdata)
```

## Reading the full data

``` r
data <- fread(file = params$path_data, na.strings = c("*", ""), encoding = "UTF-8")
```

### Check NAs

Percentage of missing values for all columns in raw data

``` r
na_frac <-sapply(data, function(y) sum(length(which(is.na(y))))/nrow(data)*100)
na_frac <- data.frame(na_frac)
na_frac
```

                                                    na_frac
    profileid                                       0.00000
    gender                                         17.24127
    is_alive                                        0.00000
    current_residence_location_city                95.47801
    current_residence_location_state               95.96379
    current_residence_location_county              98.10403
    current_residence_location_country             95.14065
    current_residence_location_country_code        97.69219
    current_residence_location_latitude            95.10509
    current_residence_location_longitude           95.10510
    current_residence_location_place_name          96.46670
    current_residence_location_resolved_externally  0.00000
    current_residence_resolved_extern_confidence    0.00000
    current_residence_resolved_extern_type         98.45622
    birth_year                                     60.19650
    birth_month                                    76.56994
    birth_day                                      77.53975
    birth_date_circa                                0.00000
    birth_date_text                                99.04681
    birth_location_city                            83.47271
    birth_location_state                           85.12981
    birth_location_country                         80.69647
    birth_location_country_code                    89.80251
    birth_location_latitude                        80.91629
    birth_location_longitude                       80.91628
    birth_location_place_name                      89.49951
    birth_location_resolved_externally              0.00000
    birth_location_resolved_extern_confidence       0.00000
    birth_location_resolved_extern_type            95.47401
    death_year                                     74.30932
    death_month                                    82.36623
    death_day                                      83.38667
    death_date_circa                                0.00000
    death_date_text                                99.97932
    death_location_city                            88.64736
    death_location_state                           89.75614
    death_location_country                         87.51093
    death_location_country_code                    93.40282
    death_location_latitude                        87.52455
    death_location_longitude                       87.52455
    death_location_place_name                      94.33114
    death_location_resolved_externally              0.00000
    death_location_resolved_extern_confidence       0.00000
    death_location_resolved_extern_type            97.23015
    burial_year                                    97.47557
    burial_month                                   97.92349
    burial_day                                     98.24074
    burial_date_circa                               0.00000
    burial_date_text                               99.99756
    burial_location_city                           96.17300
    burial_location_state                          96.39329
    burial_location_country                        95.83470
    burial_location_country_code                   97.69393
    burial_location_latitude                       95.80039
    burial_location_longitude                      95.80040
    burial_location_place_name                     97.37738
    burial_location_resolved_externally             0.00000
    burial_location_resolved_extern_confidence      0.00000
    burial_location_resolved_extern_type           99.14925
    baptism_year                                   98.85600
    baptism_month                                  98.89893
    baptism_day                                    98.90589
    baptism_date_circa                              0.00000
    baptism_date_text                              99.99721
    baptism_location_city                          99.28558
    baptism_location_state                         99.38405
    baptism_location_country                       99.22062
    baptism_location_country_code                  99.48135
    baptism_location_latitude                      99.25280
    baptism_location_longitude                     99.25280
    baptism_location_place_name                    99.51240
    baptism_location_resolved_externally            0.00000
    baptism_location_resolved_extern_confidence     0.00000
    baptism_location_resolved_extern_type          99.84215
    cause_of_death                                 98.50092

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

Function by Andrea

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
# Read links data and run subset function

links <- fread(file = params$path_links, na.strings = c("*", ""))
data <- subset_profiles(links, data)
```

## Match coordinates

Function to extract location from coordinates by Andrea (rewritten to
use data.table)

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

## Save data to file

Cleaned data with coordinate matching

``` r
fwrite(data_recoded, params$save_path)
```
