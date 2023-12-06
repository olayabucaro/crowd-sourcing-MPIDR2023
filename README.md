# Leaving for Life: Using Online Crowd-Sourced Genealogies to Estimate the Migrant Mortality Advantage for the United Kingdom and Ireland During the 18th and 19th Centuries

Repository for the article Leaving for Life:

All the code is available in a stable environment in the Binder running R version 4.3.1 and R packages from the Posit Public Package Manager CRAN snapshot [2023-07-31](https://packagemanager.posit.co/cran/2023-07-31/src/contrib/PACKAGES) 

Due to processing power limitations in Binder not all computations can be executed 

Press the Binder badge to launch the Binder Rstudio environment:

<!-- badges: start -->
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/olayabucaro/crowd-sourcing-MPIDR2023/main?urlpath=rstudio)
<!-- badges: end -->

## Code and data for Leaving for Life 

Raw Familinx data avalible at: [https://osf.io/fd25c/](https://osf.io/fd25c/)

## Data

| File name   | Description |
| ----------- | ----------- |
| data.csv   | Final clean dataset for analysis |

### cleaning_01

| File name   | Description |
| ----------- | ----------- |
| [data_preparation.qmd](https://github.com/olayabucaro/crowd-sourcing-MPIDR2023/blob/1bdd62fd995957aaf0fd4544f98429b3dd2e8eab/cleaning_01/data_preparation.md)  | Cleaned raw data and coordinate match |
| [data_imputation.qmd](https://github.com/olayabucaro/crowd-sourcing-MPIDR2023/blob/1bdd62fd995957aaf0fd4544f98429b3dd2e8eab/cleaning_01/data_imputation.md)   | Impute location information from other columns |


### filter_02

| File name   | Description |
| ----------- | ----------- |
| [data_uk-ie_birth_filter.qmd](https://github.com/olayabucaro/crowd-sourcing-MPIDR2023/blob/1bdd62fd995957aaf0fd4544f98429b3dd2e8eab/filter_02/data_uk-ie_birth_filter.md)  | Filter data to only include UK/IE births |


### kinship_03

| File name   | Description |
| ----------- | ----------- |
| [data_add_kinship.qmd](https://github.com/olayabucaro/crowd-sourcing-MPIDR2023/blob/1bdd62fd995957aaf0fd4544f98429b3dd2e8eab/kinship_03/data_add_kinship.md)  | Add kinship information to datas | 
| [data_final_var.qmd](https://github.com/olayabucaro/crowd-sourcing-MPIDR2023/blob/1bdd62fd995957aaf0fd4544f98429b3dd2e8eab/kinship_03/data_final_var.md)  | Create final dataset for analysis | 

## Analysis

| File name   | Description |
| ----------- | ----------- |
| [analysis.qmd](https://github.com/olayabucaro/crowd-sourcing-MPIDR2023/blob/07e939bfd3ac9f545aaebdae92ccbd866081b8fb/analysis/analysis.md)    | Analysis script |
| [online_supplement.qmd](https://github.com/olayabucaro/crowd-sourcing-MPIDR2023/blob/abfe306e41f9ec9d9173660f6f8671e4e3ea164a/analysis/online_supplement.md)    | Online supplement analysis script |

