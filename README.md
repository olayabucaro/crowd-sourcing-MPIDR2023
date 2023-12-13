# Leaving for Life: Using Online Crowd-Sourced Genealogies to Estimate the Migrant Mortality Advantage for the United Kingdom and Ireland During the 18th and 19th Centuries

Repository for the article Leaving for Life:

All the code is available in a stable environment in the Binder running R version 4.2.3 and R packages from the Posit Public Package Manager CRAN snapshot [2023-07-31](https://packagemanager.posit.co/cran/2023-07-31/src/contrib/PACKAGES) 

Due to processing power limitations in Binder not all computations can be executed 

Press the Binder badge to launch the Binder Rstudio environment:

<!-- badges: start -->
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/olayabucaro/leaving4life/main?urlpath=rstudio)
<!-- badges: end -->

Environment also avalibe in a docker container.

## Code and data for Leaving for Life 

Raw Familinx data avalible at: [https://osf.io/fd25c/](https://osf.io/fd25c/)

## Data

| File name   | Description |
| ----------- | ----------- |
| data.csv   | Final clean dataset for analysis |

### cleaning_01

| File name   | Description |
| ----------- | ----------- |
| [data_preparation-1.qmd](https://github.com/olayabucaro/leaving4life/blob/e96f57be6c62146be93b9d5c0c55af59ebb5e31f/cleaning_01/data_preparation-1.md)  | Cleaned raw data and coordinate match |
| [data_imputation-2.qmd](https://github.com/olayabucaro/leaving4life/blob/e96f57be6c62146be93b9d5c0c55af59ebb5e31f/cleaning_01/data_imputation-2.md)   | Impute location information from other columns |


### filter_02

| File name   | Description |
| ----------- | ----------- |
| [data_uk-ie_birth_filter-1.qmd](https://github.com/olayabucaro/leaving4life/blob/e96f57be6c62146be93b9d5c0c55af59ebb5e31f/filter_02/data_uk-ie_birth_filter-1.md)  | Filter data to only include UK/IE births |


### kinship_03

| File name   | Description |
| ----------- | ----------- |
| [data_add_kinship-1.qmd](https://github.com/olayabucaro/leaving4life/blob/e96f57be6c62146be93b9d5c0c55af59ebb5e31f/kinship_03/data_add_kinship-1.md)  | Add kinship information to datas | 
| [data_final_var-2.qmd](https://github.com/olayabucaro/leaving4life/blob/e96f57be6c62146be93b9d5c0c55af59ebb5e31f/kinship_03/data_final_var-2.md)  | Create final dataset for analysis | 

## Analysis

| File name   | Description |
| ----------- | ----------- |
| [analysis.qmd](https://github.com/olayabucaro/leaving4life/blob/e96f57be6c62146be93b9d5c0c55af59ebb5e31f/analysis/analysis.md)    | Analysis script |
| [online_supplement.qmd](https://github.com/olayabucaro/leaving4life/blob/e96f57be6c62146be93b9d5c0c55af59ebb5e31f/analysis/online_supplement.md)    | Online supplement analysis script |

