Analysis
================

## Load packages & read data

``` r
library(dplyr)
library(data.table)
library(ggplot2)
library(lme4)
library(stargazer)

data <- fread("../data/moddatafinal_LASTTIMEIPROMISE.csv", 
              drop  = c("ego", "birth_country", "birth_year", "death_year"))
```

## Format data

``` r
data <- data %>% 
  mutate(
    migrant = factor(migrant, levels = c(0, 1),
                     labels = c("nonmigrant", "migrant")),
    death_country = factor(death_country, levels = c("UK/Ireland", "Canada",
                                                     "South Africa", "Australia",
                                                     "New Zealand",
                                                     "United States of America")),
    birth10 = as.factor(birth10),
    death10 = as.factor(death10),
    gender = as.factor(gender),
    sib_size_cat = as.factor(sib_size_cat),
    birthorder_cat = as.factor(birthorder_cat),
    firstborn = factor(firstborn, levels = c('missing', '0', '1'))
    )
```

## Desctiptive statistics data

``` r
summary(data$deathage)
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      15.00   56.00   70.00   66.57   80.00  109.00 

``` r
summary(data$migrant)
```

    nonmigrant    migrant 
         83722      54774 

``` r
table(data$death_country)
```


                  UK/Ireland                   Canada             South Africa 
                       83722                     8008                     1872 
                   Australia              New Zealand United States of America 
                       12271                     3907                    28716 

``` r
summary(data$birth10)
```

     1730  1740  1750  1760  1770  1780  1790  1800  1810  1820  1830  1840  1850 
     2085  4451  5140  5284  6205  7514  9396 11067 12773 13745 13539 11891  9521 
     1860  1870  1880  1890 
     8042  7226  6771  3846 

``` r
summary(data$death10)
```

     1750  1760  1770  1780  1790  1800  1810  1820  1830  1840  1850  1860  1870 
       31   153   420   910  1399  2292  3431  4882  5879  7079  8618 10140 12052 
     1880  1890  1900  1910  1920  1930  1940  1950  1960  1970  1980  1990 
    12861 13298 12441 10503  8116  6750  5500  4702  3831  2286   815   107 

``` r
summary(data$gender)
```

    female   male 
     59276  79220 

``` r
summary(data$sib_size_cat)
```

          0       1       2     3-5      6+ missing 
      48794   14375   10002   17446    7440   40439 

``` r
summary(data$famid)
```

        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
          10   134130 19245780 26429179 47148115 86166860 

``` r
table(data$firstborn)
```


    missing       0       1 
      40439   32987   65070 

## Family effects models

### Model 1 - Migrant

``` r
fe1 <- lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
            data = data) 

summary(fe1)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ migrant * birth10 + gender + sib_size_cat + (1 | famid)
       Data: data

    REML criterion at convergence: 1182518

    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -3.6345 -0.5567  0.1827  0.6884  2.9060 

    Random effects:
     Groups   Name        Variance Std.Dev.
     famid    (Intercept)  35.87    5.989  
     Residual             264.73   16.271  
    Number of obs: 138496, groups:  famid, 108200

    Fixed effects:
                               Estimate Std. Error t value
    (Intercept)                66.09320    0.49443 133.676
    migrantmigrant              3.95375    0.78278   5.051
    birth101740                -0.24692    0.58488  -0.422
    birth101750                 0.07112    0.57515   0.124
    birth101760                 1.67540    0.56728   2.953
    birth101770                 2.79262    0.55268   5.053
    birth101780                 2.19218    0.54280   4.039
    birth101790                 1.12500    0.53539   2.101
    birth101800                 0.26507    0.53154   0.499
    birth101810                -0.77564    0.53088  -1.461
    birth101820                -2.16204    0.52988  -4.080
    birth101830                -4.04431    0.53335  -7.583
    birth101840                -4.81088    0.53606  -8.975
    birth101850                -4.70524    0.54094  -8.698
    birth101860                -3.11019    0.54589  -5.697
    birth101870                -0.66817    0.54897  -1.217
    birth101880                 2.93422    0.55349   5.301
    birth101890                 4.80807    0.59740   8.048
    gendermale                 -0.03273    0.09510  -0.344
    sib_size_cat1              -1.94853    0.17135 -11.372
    sib_size_cat2              -3.01671    0.20533 -14.692
    sib_size_cat3-5            -2.99463    0.17224 -17.387
    sib_size_cat6+             -2.56002    0.26961  -9.495
    sib_size_catmissing         0.41124    0.11751   3.500
    migrantmigrant:birth101740  1.03531    0.94843   1.092
    migrantmigrant:birth101750  0.04491    0.93005   0.048
    migrantmigrant:birth101760 -1.32043    0.93704  -1.409
    migrantmigrant:birth101770 -1.86679    0.92642  -2.015
    migrantmigrant:birth101780 -1.06945    0.89964  -1.189
    migrantmigrant:birth101790  0.52605    0.87011   0.605
    migrantmigrant:birth101800  0.89988    0.85374   1.054
    migrantmigrant:birth101810  1.24582    0.84185   1.480
    migrantmigrant:birth101820  2.39067    0.83753   2.854
    migrantmigrant:birth101830  3.57713    0.83849   4.266
    migrantmigrant:birth101840  4.08161    0.84630   4.823
    migrantmigrant:birth101850  3.86735    0.86355   4.478
    migrantmigrant:birth101860  1.83430    0.88217   2.079
    migrantmigrant:birth101870  1.55466    0.89764   1.732
    migrantmigrant:birth101880 -0.93577    0.90283  -1.036
    migrantmigrant:birth101890 -0.38229    0.98357  -0.389


    Correlation matrix not shown by default, as p = 40 > 12.
    Use print(x, correlation=TRUE)  or
        vcov(x)        if you need it

### Model 2 - Death country

``` r
fe2 <- lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
         data = data)
```

    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients

``` r
summary(fe2)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ death_country * birth10 + gender + sib_size_cat +  
        (1 | famid)
       Data: data

    REML criterion at convergence: 1182020

    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -3.6353 -0.5570  0.1815  0.6893  2.9003 

    Random effects:
     Groups   Name        Variance Std.Dev.
     famid    (Intercept)  35.59    5.966  
     Residual             264.41   16.261  
    Number of obs: 138496, groups:  famid, 108200

    Fixed effects:
                                                       Estimate Std. Error t value
    (Intercept)                                        66.09715    0.49394 133.815
    death_countryCanada                                 4.20242    2.57219   1.634
    death_countrySouth Africa                           1.88675    2.00131   0.943
    death_countryAustralia                             24.95473   17.10922   1.459
    death_countryNew Zealand                            2.93817    1.80436   1.628
    death_countryUnited States of America               3.91100    0.79698   4.907
    birth101740                                        -0.24775    0.58431  -0.424
    birth101750                                         0.07174    0.57457   0.125
    birth101760                                         1.67515    0.56671   2.956
    birth101770                                         2.79129    0.55212   5.056
    birth101780                                         2.19058    0.54225   4.040
    birth101790                                         1.12300    0.53486   2.100
    birth101800                                         0.26535    0.53101   0.500
    birth101810                                        -0.77879    0.53035  -1.468
    birth101820                                        -2.16677    0.52935  -4.093
    birth101830                                        -4.04661    0.53281  -7.595
    birth101840                                        -4.81363    0.53552  -8.989
    birth101850                                        -4.70618    0.54039  -8.709
    birth101860                                        -3.11264    0.54535  -5.708
    birth101870                                        -0.66991    0.54842  -1.222
    birth101880                                         2.93362    0.55294   5.306
    birth101890                                         4.80628    0.59680   8.053
    gendermale                                         -0.04186    0.09506  -0.440
    sib_size_cat1                                      -1.95428    0.17126 -11.411
    sib_size_cat2                                      -3.01077    0.20518 -14.674
    sib_size_cat3-5                                    -2.99003    0.17212 -17.372
    sib_size_cat6+                                     -2.52244    0.26921  -9.370
    sib_size_catmissing                                 0.41407    0.11752   3.523
    death_countryCanada:birth101740                     4.10182    2.96457   1.384
    death_countryAustralia:birth101740                -24.02113   17.68651  -1.358
    death_countryNew Zealand:birth101740              -17.78758   12.38438  -1.436
    death_countryUnited States of America:birth101740   0.82158    0.97056   0.847
    death_countryCanada:birth101750                     1.45986    2.89219   0.505
    death_countrySouth Africa:birth101750              -1.74271    8.89388  -0.196
    death_countryAustralia:birth101750                -21.00639   17.19769  -1.221
    death_countryNew Zealand:birth101750               23.62636   17.29537   1.366
    death_countryUnited States of America:birth101750  -0.10426    0.95700  -0.109
    death_countryCanada:birth101760                     1.99789    2.83079   0.706
    death_countrySouth Africa:birth101760               2.68331    4.55082   0.590
    death_countryAustralia:birth101760                -23.61649   17.14681  -1.377
    death_countryNew Zealand:birth101760               15.49990    8.85113   1.751
    death_countryUnited States of America:birth101760  -1.78578    0.98944  -1.805
    death_countryCanada:birth101770                     3.02376    2.75263   1.098
    death_countrySouth Africa:birth101770              -0.56337    2.77814  -0.203
    death_countryAustralia:birth101770                -25.32601   17.14139  -1.477
    death_countryNew Zealand:birth101770               -3.40809   10.16495  -0.335
    death_countryUnited States of America:birth101770  -2.78100    1.00529  -2.766
    death_countryCanada:birth101780                     2.24645    2.70157   0.832
    death_countrySouth Africa:birth101780              -4.95267    2.38380  -2.078
    death_countryAustralia:birth101780                -22.35348   17.13347  -1.305
    death_countryNew Zealand:birth101780                7.02635    3.55712   1.975
    death_countryUnited States of America:birth101780  -1.70024    0.98750  -1.722
    death_countryCanada:birth101790                     3.35027    2.65982   1.260
    death_countrySouth Africa:birth101790              -2.52130    2.33888  -1.078
    death_countryAustralia:birth101790                -20.96534   17.12372  -1.224
    death_countryNew Zealand:birth101790                3.98226    2.41560   1.649
    death_countryUnited States of America:birth101790  -0.22414    0.94792  -0.236
    death_countryCanada:birth101800                     1.91088    2.65024   0.721
    death_countrySouth Africa:birth101800              -1.46843    2.35833  -0.623
    death_countryAustralia:birth101800                -21.13759   17.11872  -1.235
    death_countryNew Zealand:birth101800                3.37140    2.10282   1.603
    death_countryUnited States of America:birth101800   1.26349    0.91939   1.374
    death_countryCanada:birth101810                     3.34504    2.64234   1.266
    death_countrySouth Africa:birth101810               0.01595    2.28510   0.007
    death_countryAustralia:birth101810                -20.79027   17.11620  -1.215
    death_countryNew Zealand:birth101810                4.61000    2.02596   2.275
    death_countryUnited States of America:birth101810   1.05623    0.89036   1.186
    death_countryCanada:birth101820                     4.14646    2.64277   1.569
    death_countrySouth Africa:birth101820               4.12211    2.71116   1.520
    death_countryAustralia:birth101820                -19.85213   17.11520  -1.160
    death_countryNew Zealand:birth101820                4.81589    1.98446   2.427
    death_countryUnited States of America:birth101820   2.39107    0.87963   2.718
    death_countryCanada:birth101830                     5.14316    2.66636   1.929
    death_countrySouth Africa:birth101830               4.06944    2.67753   1.520
    death_countryAustralia:birth101830                -18.76082   17.11472  -1.096
    death_countryNew Zealand:birth101830                4.80441    1.93558   2.482
    death_countryUnited States of America:birth101830   4.12301    0.88000   4.685
    death_countryCanada:birth101840                     4.64254    2.70249   1.718
    death_countrySouth Africa:birth101840               3.01505    2.59847   1.160
    death_countryAustralia:birth101840                -17.56794   17.11692  -1.026
    death_countryNew Zealand:birth101840                5.94725    1.94732   3.054
    death_countryUnited States of America:birth101840   4.24710    0.88717   4.787
    death_countryCanada:birth101850                     3.10728    2.75112   1.129
    death_countrySouth Africa:birth101850               0.41585    2.65016   0.157
    death_countryAustralia:birth101850                -17.08899   17.12054  -0.998
    death_countryNew Zealand:birth101850                7.42982    1.98019   3.752
    death_countryUnited States of America:birth101850   3.63212    0.91693   3.961
    death_countryCanada:birth101860                     2.32533    2.73383   0.851
    death_countrySouth Africa:birth101860              -0.06774    2.71928  -0.025
    death_countryAustralia:birth101860                -19.14123   17.12794  -1.118
    death_countryNew Zealand:birth101860                5.91947    2.05300   2.883
    death_countryUnited States of America:birth101860   1.25493    0.94494   1.328
    death_countryCanada:birth101870                     3.05643    2.72072   1.123
    death_countrySouth Africa:birth101870               1.59743    2.43297   0.657
    death_countryAustralia:birth101870                -19.95532   17.13763  -1.164
    death_countryNew Zealand:birth101870                4.20699    2.14587   1.961
    death_countryUnited States of America:birth101870   1.08125    0.97706   1.107
    death_countryCanada:birth101880                    -0.75087    2.69484  -0.279
    death_countrySouth Africa:birth101880               2.25577    2.45224   0.920
    death_countryAustralia:birth101880                -21.55117   17.13532  -1.258
    death_countryNew Zealand:birth101880                1.33060    2.32699   0.572
    death_countryUnited States of America:birth101880  -1.55678    0.99315  -1.568
    death_countryCanada:birth101890                    -0.07745    2.77033  -0.028
    death_countryAustralia:birth101890                -21.02057   17.15905  -1.225
    death_countryUnited States of America:birth101890  -0.42643    1.12308  -0.380


    Correlation matrix not shown by default, as p = 105 > 12.
    Use print(x, correlation=TRUE)  or
        vcov(x)        if you need it

    fit warnings:
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients


    ======================================================================================
                                                        Dependent variable: Age at Death  
                                                      ------------------------------------
                                                                                          
                                                      Migrant Model Country of Death Model
                                                           (1)               (2)          
    --------------------------------------------------------------------------------------
    Migrant                                             3.954***                          
                                                         (0.783)                          
                                                                                          
    Country of Death: Canada                                                4.202         
                                                                           (2.572)        
                                                                                          
    Country of Death: South Africa                                          1.887         
                                                                           (2.001)        
                                                                                          
    Country of Death: Australia                                             24.955        
                                                                           (17.109)       
                                                                                          
    Country of Death: New Zealand                                           2.938         
                                                                           (1.804)        
                                                                                          
    Country of Death: United States of America                             3.911***       
                                                                           (0.797)        
                                                                                          
    birth101740                                          -0.247             -0.248        
                                                         (0.585)           (0.584)        
                                                                                          
    birth101750                                           0.071             0.072         
                                                         (0.575)           (0.575)        
                                                                                          
    birth101760                                         1.675***           1.675***       
                                                         (0.567)           (0.567)        
                                                                                          
    birth101770                                         2.793***           2.791***       
                                                         (0.553)           (0.552)        
                                                                                          
    birth101780                                         2.192***           2.191***       
                                                         (0.543)           (0.542)        
                                                                                          
    birth101790                                          1.125**           1.123**        
                                                         (0.535)           (0.535)        
                                                                                          
    birth101800                                           0.265             0.265         
                                                         (0.532)           (0.531)        
                                                                                          
    birth101810                                          -0.776             -0.779        
                                                         (0.531)           (0.530)        
                                                                                          
    birth101820                                         -2.162***         -2.167***       
                                                         (0.530)           (0.529)        
                                                                                          
    birth101830                                         -4.044***         -4.047***       
                                                         (0.533)           (0.533)        
                                                                                          
    birth101840                                         -4.811***         -4.814***       
                                                         (0.536)           (0.536)        
                                                                                          
    birth101850                                         -4.705***         -4.706***       
                                                         (0.541)           (0.540)        
                                                                                          
    birth101860                                         -3.110***         -3.113***       
                                                         (0.546)           (0.545)        
                                                                                          
    birth101870                                          -0.668             -0.670        
                                                         (0.549)           (0.548)        
                                                                                          
    birth101880                                         2.934***           2.934***       
                                                         (0.553)           (0.553)        
                                                                                          
    birth101890                                         4.808***           4.806***       
                                                         (0.597)           (0.597)        
                                                                                          
    gendermale                                           -0.033             -0.042        
                                                         (0.095)           (0.095)        
                                                                                          
    sib_size_cat1                                       -1.949***         -1.954***       
                                                         (0.171)           (0.171)        
                                                                                          
    sib_size_cat2                                       -3.017***         -3.011***       
                                                         (0.205)           (0.205)        
                                                                                          
    sib_size_cat3-5                                     -2.995***         -2.990***       
                                                         (0.172)           (0.172)        
                                                                                          
    sib_size_cat6+                                      -2.560***         -2.522***       
                                                         (0.270)           (0.269)        
                                                                                          
    sib_size_catmissing                                 0.411***           0.414***       
                                                         (0.118)           (0.118)        
                                                                                          
    migrantmigrant:birth101740                            1.035                           
                                                         (0.948)                          
                                                                                          
    migrantmigrant:birth101750                            0.045                           
                                                         (0.930)                          
                                                                                          
    migrantmigrant:birth101760                           -1.320                           
                                                         (0.937)                          
                                                                                          
    migrantmigrant:birth101770                          -1.867**                          
                                                         (0.926)                          
                                                                                          
    migrantmigrant:birth101780                           -1.069                           
                                                         (0.900)                          
                                                                                          
    migrantmigrant:birth101790                            0.526                           
                                                         (0.870)                          
                                                                                          
    migrantmigrant:birth101800                            0.900                           
                                                         (0.854)                          
                                                                                          
    migrantmigrant:birth101810                            1.246                           
                                                         (0.842)                          
                                                                                          
    migrantmigrant:birth101820                          2.391***                          
                                                         (0.838)                          
                                                                                          
    migrantmigrant:birth101830                          3.577***                          
                                                         (0.838)                          
                                                                                          
    migrantmigrant:birth101840                          4.082***                          
                                                         (0.846)                          
                                                                                          
    migrantmigrant:birth101850                          3.867***                          
                                                         (0.864)                          
                                                                                          
    migrantmigrant:birth101860                           1.834**                          
                                                         (0.882)                          
                                                                                          
    migrantmigrant:birth101870                           1.555*                           
                                                         (0.898)                          
                                                                                          
    migrantmigrant:birth101880                           -0.936                           
                                                         (0.903)                          
                                                                                          
    migrantmigrant:birth101890                           -0.382                           
                                                         (0.984)                          
                                                                                          
    death_countryCanada:birth101740                                         4.102         
                                                                           (2.965)        
                                                                                          
    death_countryAustralia:birth101740                                     -24.021        
                                                                           (17.687)       
                                                                                          
    death_countryNew Zealand:birth101740                                   -17.788        
                                                                           (12.384)       
                                                                                          
    death_countryUnited States of America:birth101740                       0.822         
                                                                           (0.971)        
                                                                                          
    death_countryCanada:birth101750                                         1.460         
                                                                           (2.892)        
                                                                                          
    death_countrySouth Africa:birth101750                                   -1.743        
                                                                           (8.894)        
                                                                                          
    death_countryAustralia:birth101750                                     -21.006        
                                                                           (17.198)       
                                                                                          
    death_countryNew Zealand:birth101750                                    23.626        
                                                                           (17.295)       
                                                                                          
    death_countryUnited States of America:birth101750                       -0.104        
                                                                           (0.957)        
                                                                                          
    death_countryCanada:birth101760                                         1.998         
                                                                           (2.831)        
                                                                                          
    death_countrySouth Africa:birth101760                                   2.683         
                                                                           (4.551)        
                                                                                          
    death_countryAustralia:birth101760                                     -23.616        
                                                                           (17.147)       
                                                                                          
    death_countryNew Zealand:birth101760                                   15.500*        
                                                                           (8.851)        
                                                                                          
    death_countryUnited States of America:birth101760                      -1.786*        
                                                                           (0.989)        
                                                                                          
    death_countryCanada:birth101770                                         3.024         
                                                                           (2.753)        
                                                                                          
    death_countrySouth Africa:birth101770                                   -0.563        
                                                                           (2.778)        
                                                                                          
    death_countryAustralia:birth101770                                     -25.326        
                                                                           (17.141)       
                                                                                          
    death_countryNew Zealand:birth101770                                    -3.408        
                                                                           (10.165)       
                                                                                          
    death_countryUnited States of America:birth101770                     -2.781***       
                                                                           (1.005)        
                                                                                          
    death_countryCanada:birth101780                                         2.246         
                                                                           (2.702)        
                                                                                          
    death_countrySouth Africa:birth101780                                  -4.953**       
                                                                           (2.384)        
                                                                                          
    death_countryAustralia:birth101780                                     -22.353        
                                                                           (17.133)       
                                                                                          
    death_countryNew Zealand:birth101780                                   7.026**        
                                                                           (3.557)        
                                                                                          
    death_countryUnited States of America:birth101780                      -1.700*        
                                                                           (0.988)        
                                                                                          
    death_countryCanada:birth101790                                         3.350         
                                                                           (2.660)        
                                                                                          
    death_countrySouth Africa:birth101790                                   -2.521        
                                                                           (2.339)        
                                                                                          
    death_countryAustralia:birth101790                                     -20.965        
                                                                           (17.124)       
                                                                                          
    death_countryNew Zealand:birth101790                                    3.982*        
                                                                           (2.416)        
                                                                                          
    death_countryUnited States of America:birth101790                       -0.224        
                                                                           (0.948)        
                                                                                          
    death_countryCanada:birth101800                                         1.911         
                                                                           (2.650)        
                                                                                          
    death_countrySouth Africa:birth101800                                   -1.468        
                                                                           (2.358)        
                                                                                          
    death_countryAustralia:birth101800                                     -21.138        
                                                                           (17.119)       
                                                                                          
    death_countryNew Zealand:birth101800                                    3.371         
                                                                           (2.103)        
                                                                                          
    death_countryUnited States of America:birth101800                       1.263         
                                                                           (0.919)        
                                                                                          
    death_countryCanada:birth101810                                         3.345         
                                                                           (2.642)        
                                                                                          
    death_countrySouth Africa:birth101810                                   0.016         
                                                                           (2.285)        
                                                                                          
    death_countryAustralia:birth101810                                     -20.790        
                                                                           (17.116)       
                                                                                          
    death_countryNew Zealand:birth101810                                   4.610**        
                                                                           (2.026)        
                                                                                          
    death_countryUnited States of America:birth101810                       1.056         
                                                                           (0.890)        
                                                                                          
    death_countryCanada:birth101820                                         4.146         
                                                                           (2.643)        
                                                                                          
    death_countrySouth Africa:birth101820                                   4.122         
                                                                           (2.711)        
                                                                                          
    death_countryAustralia:birth101820                                     -19.852        
                                                                           (17.115)       
                                                                                          
    death_countryNew Zealand:birth101820                                   4.816**        
                                                                           (1.984)        
                                                                                          
    death_countryUnited States of America:birth101820                      2.391***       
                                                                           (0.880)        
                                                                                          
    death_countryCanada:birth101830                                         5.143*        
                                                                           (2.666)        
                                                                                          
    death_countrySouth Africa:birth101830                                   4.069         
                                                                           (2.678)        
                                                                                          
    death_countryAustralia:birth101830                                     -18.761        
                                                                           (17.115)       
                                                                                          
    death_countryNew Zealand:birth101830                                   4.804**        
                                                                           (1.936)        
                                                                                          
    death_countryUnited States of America:birth101830                      4.123***       
                                                                           (0.880)        
                                                                                          
    death_countryCanada:birth101840                                         4.643*        
                                                                           (2.702)        
                                                                                          
    death_countrySouth Africa:birth101840                                   3.015         
                                                                           (2.598)        
                                                                                          
    death_countryAustralia:birth101840                                     -17.568        
                                                                           (17.117)       
                                                                                          
    death_countryNew Zealand:birth101840                                   5.947***       
                                                                           (1.947)        
                                                                                          
    death_countryUnited States of America:birth101840                      4.247***       
                                                                           (0.887)        
                                                                                          
    death_countryCanada:birth101850                                         3.107         
                                                                           (2.751)        
                                                                                          
    death_countrySouth Africa:birth101850                                   0.416         
                                                                           (2.650)        
                                                                                          
    death_countryAustralia:birth101850                                     -17.089        
                                                                           (17.121)       
                                                                                          
    death_countryNew Zealand:birth101850                                   7.430***       
                                                                           (1.980)        
                                                                                          
    death_countryUnited States of America:birth101850                      3.632***       
                                                                           (0.917)        
                                                                                          
    death_countryCanada:birth101860                                         2.325         
                                                                           (2.734)        
                                                                                          
    death_countrySouth Africa:birth101860                                   -0.068        
                                                                           (2.719)        
                                                                                          
    death_countryAustralia:birth101860                                     -19.141        
                                                                           (17.128)       
                                                                                          
    death_countryNew Zealand:birth101860                                   5.919***       
                                                                           (2.053)        
                                                                                          
    death_countryUnited States of America:birth101860                       1.255         
                                                                           (0.945)        
                                                                                          
    death_countryCanada:birth101870                                         3.056         
                                                                           (2.721)        
                                                                                          
    death_countrySouth Africa:birth101870                                   1.597         
                                                                           (2.433)        
                                                                                          
    death_countryAustralia:birth101870                                     -19.955        
                                                                           (17.138)       
                                                                                          
    death_countryNew Zealand:birth101870                                   4.207**        
                                                                           (2.146)        
                                                                                          
    death_countryUnited States of America:birth101870                       1.081         
                                                                           (0.977)        
                                                                                          
    death_countryCanada:birth101880                                         -0.751        
                                                                           (2.695)        
                                                                                          
    death_countrySouth Africa:birth101880                                   2.256         
                                                                           (2.452)        
                                                                                          
    death_countryAustralia:birth101880                                     -21.551        
                                                                           (17.135)       
                                                                                          
    death_countryNew Zealand:birth101880                                    1.331         
                                                                           (2.327)        
                                                                                          
    death_countryUnited States of America:birth101880                       -1.557        
                                                                           (0.993)        
                                                                                          
    death_countryCanada:birth101890                                         -0.077        
                                                                           (2.770)        
                                                                                          
    death_countryAustralia:birth101890                                     -21.021        
                                                                           (17.159)       
                                                                                          
    death_countryUnited States of America:birth101890                       -0.426        
                                                                           (1.123)        
                                                                                          
    Constant                                            66.093***         66.097***       
                                                         (0.494)           (0.494)        
                                                                                          
    --------------------------------------------------------------------------------------
    Sibling random effects                                 Yes               Yes          
    Observations                                         138,496           138,496        
    ======================================================================================
    Note:                                                      *p<0.1; **p<0.05; ***p<0.01

## Models with modified sample

``` r
# Make df where everyone has a parent in the data
newdata <- data %>% 
  filter(miss.famid == F) %>% 
  select(-miss.famid)

summary(newdata$sib_size_cat)
```

          0       1       2     3-5      6+ missing 
      48794   14375   10002   17446    7440       0 

``` r
rm(data)
```

``` r
omit.vars <- c("birth10", "sib_size_cat", "gender")

fe1.lim <- lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
         data = newdata) 
summary(fe1.lim)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ migrant * birth10 + gender + sib_size_cat + (1 | famid)
       Data: newdata

    REML criterion at convergence: 842641.5

    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -3.5660 -0.5587  0.1892  0.6871  2.8467 

    Random effects:
     Groups   Name        Variance Std.Dev.
     famid    (Intercept)  40.25    6.344  
     Residual             278.41   16.686  
    Number of obs: 98057, groups:  famid, 67785

    Fixed effects:
                                 Estimate Std. Error t value
    (Intercept)                66.7852475  0.6075791 109.920
    migrantmigrant              3.3502698  1.0580859   3.166
    birth101740                -0.7427914  0.7182734  -1.034
    birth101750                -0.7442009  0.7096462  -1.049
    birth101760                 0.9571018  0.6981016   1.371
    birth101770                 2.0681542  0.6797209   3.043
    birth101780                 1.4128193  0.6688203   2.112
    birth101790                 0.0418959  0.6597679   0.064
    birth101800                -0.6674083  0.6548747  -1.019
    birth101810                -1.5563454  0.6535988  -2.381
    birth101820                -2.9077263  0.6498306  -4.475
    birth101830                -4.9176605  0.6533662  -7.527
    birth101840                -5.6547379  0.6564004  -8.615
    birth101850                -5.8308817  0.6603116  -8.831
    birth101860                -4.3150505  0.6653597  -6.485
    birth101870                -1.9707319  0.6679218  -2.951
    birth101880                 1.4143964  0.6745623   2.097
    birth101890                 3.2202939  0.7330239   4.393
    gendermale                  0.0003405  0.1169433   0.003
    sib_size_cat1              -1.8596191  0.1771402 -10.498
    sib_size_cat2              -2.9270636  0.2126288 -13.766
    sib_size_cat3-5            -2.8937211  0.1789106 -16.174
    sib_size_cat6+             -2.4682579  0.2808085  -8.790
    migrantmigrant:birth101740  0.9948129  1.2859409   0.774
    migrantmigrant:birth101750  0.2331316  1.2658956   0.184
    migrantmigrant:birth101760 -0.0577445  1.2784970  -0.045
    migrantmigrant:birth101770 -0.1535921  1.2575232  -0.122
    migrantmigrant:birth101780  0.3564673  1.2144065   0.294
    migrantmigrant:birth101790  2.2807377  1.1684896   1.952
    migrantmigrant:birth101800  2.2007335  1.1433492   1.925
    migrantmigrant:birth101810  2.4258565  1.1259649   2.154
    migrantmigrant:birth101820  3.4416268  1.1185095   3.077
    migrantmigrant:birth101830  4.7328808  1.1185359   4.231
    migrantmigrant:birth101840  5.1718527  1.1269513   4.589
    migrantmigrant:birth101850  5.0141750  1.1435489   4.385
    migrantmigrant:birth101860  3.0946055  1.1649842   2.656
    migrantmigrant:birth101870  2.5113722  1.1802705   2.128
    migrantmigrant:birth101880 -0.0617160  1.1927386  -0.052
    migrantmigrant:birth101890  0.0339968  1.3041557   0.026


    Correlation matrix not shown by default, as p = 39 > 12.
    Use print(x, correlation=TRUE)  or
        vcov(x)        if you need it

``` r
newdata %>% 
  ggplot(aes(
    x = predict(fe1.lim),
    y = deathage)) + 
  geom_point() + 
  geom_abline(
    intercept = 0,
    slope = 1) +
  scale_x_continuous(limits = c(15, 110)) +
  scale_y_continuous(limits = c(15, 110)) +
  labs(x='Predicted Values', 
       y='Actual Values', 
       title='Predicted vs. Actual Values',
       subtitle= 'migrant - model 2 - have parents')
```

![](analysis_binder_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe1-1.png)

``` r
fe2.lim <- lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
         data = newdata) 
```

    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients

``` r
summary(fe2.lim)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ death_country * birth10 + gender + sib_size_cat +  
        (1 | famid)
       Data: newdata

    REML criterion at convergence: 842216.2

    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -3.5669 -0.5597  0.1906  0.6876  2.8380 

    Random effects:
     Groups   Name        Variance Std.Dev.
     famid    (Intercept)  39.99    6.324  
     Residual             278.13   16.677  
    Number of obs: 98057, groups:  famid, 67785

    Fixed effects:
                                                        Estimate Std. Error t value
    (Intercept)                                        66.784288   0.607069 110.011
    death_countryCanada                                 1.242098   4.637175   0.268
    death_countrySouth Africa                           1.992094   2.716106   0.733
    death_countryAustralia                             23.949661  17.595312   1.361
    death_countryNew Zealand                            4.728123   2.275619   2.078
    death_countryUnited States of America               3.376720   1.071097   3.153
    birth101740                                        -0.744088   0.717675  -1.037
    birth101750                                        -0.743524   0.709036  -1.049
    birth101760                                         0.955905   0.697496   1.370
    birth101770                                         2.067350   0.679130   3.044
    birth101780                                         1.411311   0.668240   2.112
    birth101790                                         0.038858   0.659196   0.059
    birth101800                                        -0.665502   0.654307  -1.017
    birth101810                                        -1.559630   0.653033  -2.388
    birth101820                                        -2.913749   0.649268  -4.488
    birth101830                                        -4.919457   0.652801  -7.536
    birth101840                                        -5.657455   0.655831  -8.626
    birth101850                                        -5.831602   0.659739  -8.839
    birth101860                                        -4.319569   0.664783  -6.498
    birth101870                                        -1.973808   0.667342  -2.958
    birth101880                                         1.414338   0.673977   2.098
    birth101890                                         3.218980   0.732391   4.395
    gendermale                                          0.000230   0.116917   0.002
    sib_size_cat1                                      -1.860519   0.177129 -10.504
    sib_size_cat2                                      -2.926088   0.212582 -13.764
    sib_size_cat3-5                                    -2.888647   0.178904 -16.146
    sib_size_cat6+                                     -2.438861   0.280530  -8.694
    death_countryCanada:birth101740                     6.812302   5.070977   1.343
    death_countryAustralia:birth101740                -27.467945  18.693266  -1.469
    death_countryNew Zealand:birth101740              -19.768323  12.822140  -1.542
    death_countryUnited States of America:birth101740   0.716503   1.314442   0.545
    death_countryCanada:birth101750                     3.461942   4.989024   0.694
    death_countrySouth Africa:birth101750              -9.602714  12.906810  -0.744
    death_countryAustralia:birth101750                -20.242075  17.823479  -1.136
    death_countryNew Zealand:birth101750               21.868317  17.843770   1.226
    death_countryUnited States of America:birth101750   0.051248   1.300026   0.039
    death_countryCanada:birth101760                     4.671562   4.918245   0.950
    death_countrySouth Africa:birth101760               8.050004   6.256263   1.287
    death_countryAustralia:birth101760                -20.349056  17.718426  -1.148
    death_countryNew Zealand:birth101760                3.531453  17.984075   0.196
    death_countryUnited States of America:birth101760  -0.784717   1.340336  -0.585
    death_countryCanada:birth101770                     6.418051   4.826022   1.330
    death_countrySouth Africa:birth101770              -1.006530   3.788418  -0.266
    death_countryAustralia:birth101770                -20.676300  17.668672  -1.170
    death_countryNew Zealand:birth101770               12.420008  17.983375   0.691
    death_countryUnited States of America:birth101770  -1.662819   1.364538  -1.219
    death_countryCanada:birth101780                     5.179588   4.765001   1.087
    death_countrySouth Africa:birth101780               0.522528   3.429146   0.152
    death_countryAustralia:birth101780                -19.386271  17.642290  -1.099
    death_countryNew Zealand:birth101780                6.123055   4.294928   1.426
    death_countryUnited States of America:birth101780  -1.398122   1.329048  -1.052
    death_countryCanada:birth101790                     7.662560   4.726174   1.621
    death_countrySouth Africa:birth101790               0.372758   3.257261   0.114
    death_countryAustralia:birth101790                -18.345005  17.618544  -1.041
    death_countryNew Zealand:birth101790                3.631076   3.076181   1.180
    death_countryUnited States of America:birth101790   0.767362   1.267410   0.605
    death_countryCanada:birth101800                     5.397962   4.713038   1.145
    death_countrySouth Africa:birth101800              -0.017125   3.117283  -0.005
    death_countryAustralia:birth101800                -19.143310  17.610111  -1.087
    death_countryNew Zealand:birth101800                2.872806   2.662667   1.079
    death_countryUnited States of America:birth101800   2.301547   1.223940   1.880
    death_countryCanada:birth101810                     6.841808   4.701182   1.455
    death_countrySouth Africa:birth101810              -0.009224   3.025153  -0.003
    death_countryAustralia:birth101810                -19.260517  17.605571  -1.094
    death_countryNew Zealand:birth101810                3.639065   2.545227   1.430
    death_countryUnited States of America:birth101810   2.309173   1.182619   1.953
    death_countryCanada:birth101820                     7.709246   4.700955   1.640
    death_countrySouth Africa:birth101820               2.711394   3.572073   0.759
    death_countryAustralia:birth101820                -18.469989  17.603790  -1.049
    death_countryNew Zealand:birth101820                3.912585   2.493643   1.569
    death_countryUnited States of America:birth101820   3.431379   1.165151   2.945
    death_countryCanada:birth101830                     9.520183   4.723306   2.016
    death_countrySouth Africa:birth101830               1.410341   3.663448   0.385
    death_countryAustralia:birth101830                -17.703172  17.603008  -1.006
    death_countryNew Zealand:birth101830                2.964802   2.429243   1.220
    death_countryUnited States of America:birth101830   5.659650   1.163604   4.864
    death_countryCanada:birth101840                     7.928557   4.751722   1.669
    death_countrySouth Africa:birth101840               1.879716   3.417915   0.550
    death_countryAustralia:birth101840                -16.297282  17.605879  -0.926
    death_countryNew Zealand:birth101840                4.338003   2.453729   1.768
    death_countryUnited States of America:birth101840   5.562698   1.171299   4.749
    death_countryCanada:birth101850                     7.068794   4.785108   1.477
    death_countrySouth Africa:birth101850               0.987066   3.452082   0.286
    death_countryAustralia:birth101850                -15.636526  17.610306  -0.888
    death_countryNew Zealand:birth101850                6.152552   2.485079   2.476
    death_countryUnited States of America:birth101850   4.700138   1.201974   3.910
    death_countryCanada:birth101860                     4.712175   4.773853   0.987
    death_countrySouth Africa:birth101860              -2.003529   3.472678  -0.577
    death_countryAustralia:birth101860                -17.647066  17.619735  -1.002
    death_countryNew Zealand:birth101860                5.005966   2.549582   1.963
    death_countryUnited States of America:birth101860   2.889528   1.240344   2.330
    death_countryCanada:birth101870                     6.443764   4.759462   1.354
    death_countrySouth Africa:birth101870              -1.099358   3.193475  -0.344
    death_countryAustralia:birth101870                -18.830667  17.631944  -1.068
    death_countryNew Zealand:birth101870                3.599961   2.659284   1.354
    death_countryUnited States of America:birth101870   2.253580   1.271106   1.773
    death_countryCanada:birth101880                     2.450110   4.745027   0.516
    death_countrySouth Africa:birth101880               2.230004   3.246716   0.687
    death_countryAustralia:birth101880                -20.162333  17.632486  -1.143
    death_countryNew Zealand:birth101880                0.615447   2.894171   0.213
    death_countryUnited States of America:birth101880  -0.863907   1.299053  -0.665
    death_countryCanada:birth101890                     2.156076   4.830770   0.446
    death_countryAustralia:birth101890                -19.512861  17.673668  -1.104
    death_countryUnited States of America:birth101890  -0.415424   1.483874  -0.280


    Correlation matrix not shown by default, as p = 104 > 12.
    Use print(x, correlation=TRUE)  or
        vcov(x)        if you need it

    fit warnings:
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients

``` r
newdata %>% 
  ggplot(aes(
    x = predict(fe2.lim),
    y = deathage)) + 
  geom_point() + 
  geom_abline(
    intercept = 0,
    slope = 1) +
  scale_x_continuous(limits = c(15, 110)) +
  scale_y_continuous(limits = c(15, 110)) +
  labs(x='Predicted Values', 
       y='Actual Values', 
       title='Predicted vs. Actual Values',
       subtitle= 'death country - model 2 - have parents')
```

![](analysis_binder_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe1-2.png)

``` r
# make df where only people with siblings are in data
newerdata <- newdata %>% 
  filter(sib.ct > 0)

rm(newdata)
```

``` r
fe1.sibs <- lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
         data = newerdata) 
summary(fe1.sibs)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ migrant * birth10 + gender + sib_size_cat + (1 | famid)
       Data: newerdata

    REML criterion at convergence: 429579.4

    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -3.4123 -0.5689  0.1999  0.6964  2.7281 

    Random effects:
     Groups   Name        Variance Std.Dev.
     famid    (Intercept)  53.54    7.317  
     Residual             313.44   17.704  
    Number of obs: 49263, groups:  famid, 18995

    Fixed effects:
                               Estimate Std. Error t value
    (Intercept)                 63.8376     1.0290  62.039
    migrantmigrant               2.8807     2.1394   1.346
    birth101740                 -0.8208     1.1979  -0.685
    birth101750                 -1.4859     1.2067  -1.231
    birth101760                  0.8390     1.1799   0.711
    birth101770                  2.5014     1.1463   2.182
    birth101780                  1.1246     1.1282   0.997
    birth101790                 -0.1256     1.1027  -0.114
    birth101800                 -0.4412     1.0891  -0.405
    birth101810                 -0.8515     1.0824  -0.787
    birth101820                 -2.2261     1.0756  -2.070
    birth101830                 -4.4653     1.0779  -4.143
    birth101840                 -5.0505     1.0795  -4.679
    birth101850                 -4.8398     1.0871  -4.452
    birth101860                 -3.3543     1.0899  -3.077
    birth101870                 -0.2773     1.0945  -0.253
    birth101880                  2.7366     1.1027   2.482
    birth101890                  4.4808     1.1700   3.830
    gendermale                   0.2206     0.1765   1.249
    sib_size_cat2               -1.0755     0.2715  -3.961
    sib_size_cat3-5             -1.0458     0.2421  -4.321
    sib_size_cat6+              -0.6991     0.3413  -2.048
    migrantmigrant:birth101740   2.4335     2.4924   0.976
    migrantmigrant:birth101750   1.4060     2.5299   0.556
    migrantmigrant:birth101760   3.1582     2.5352   1.246
    migrantmigrant:birth101770   0.6392     2.5456   0.251
    migrantmigrant:birth101780   1.7868     2.4178   0.739
    migrantmigrant:birth101790   4.9154     2.3218   2.117
    migrantmigrant:birth101800   3.5270     2.2605   1.560
    migrantmigrant:birth101810   4.2725     2.2280   1.918
    migrantmigrant:birth101820   5.1418     2.2116   2.325
    migrantmigrant:birth101830   6.3859     2.2049   2.896
    migrantmigrant:birth101840   6.9744     2.2129   3.152
    migrantmigrant:birth101850   6.1129     2.2329   2.738
    migrantmigrant:birth101860   4.6199     2.2555   2.048
    migrantmigrant:birth101870   2.8824     2.2733   1.268
    migrantmigrant:birth101880   1.3744     2.2829   0.602
    migrantmigrant:birth101890   1.0198     2.4038   0.424


    Correlation matrix not shown by default, as p = 38 > 12.
    Use print(x, correlation=TRUE)  or
        vcov(x)        if you need it

``` r
newerdata %>% 
  ggplot(aes(
    x = predict(fe1.sibs),
    y = deathage)) + 
  geom_point() + 
  geom_abline(
    intercept = 0,
    slope = 1) +
  scale_x_continuous(limits = c(15, 110)) +
  scale_y_continuous(limits = c(15, 110)) +
  labs(x='Predicted Values', 
       y='Actual Values', 
       title='Predicted vs. Actual Values',
       subtitle= 'migrant - model 3 - have parents + siblings')
```

![](analysis_binder_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe2-1.png)

``` r
stargazer(fe1, fe1.lim, fe1.sibs,
          type = "text",
          column.labels = c("full", "have parents", "have siblings"),
          dep.var.caption = "Dependent variable: Age at Death",
          dep.var.labels = "",
          omit = omit.vars)
```


    ============================================================
                            Dependent variable: Age at Death    
                        ----------------------------------------
                                                                
                            full      have parents have siblings
                             (1)          (2)           (3)     
    ------------------------------------------------------------
    migrantmigrant        3.954***      3.350***       2.881    
                           (0.783)      (1.058)       (2.139)   
                                                                
    Constant              66.093***    66.785***     63.838***  
                           (0.494)      (0.608)       (1.029)   
                                                                
    ------------------------------------------------------------
    Observations           138,496       98,057       49,263    
    Log Likelihood      -591,258.700  -421,320.700 -214,789.700 
    Akaike Inf. Crit.   1,182,601.000 842,723.500   429,659.400 
    Bayesian Inf. Crit. 1,183,015.000 843,112.700   430,011.600 
    ============================================================
    Note:                            *p<0.1; **p<0.05; ***p<0.01

``` r
fe2.sibs <- lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
         data = newerdata) 
```

    fixed-effect model matrix is rank deficient so dropping 6 columns / coefficients

``` r
summary(fe2.sibs)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ death_country * birth10 + gender + sib_size_cat +  
        (1 | famid)
       Data: newerdata

    REML criterion at convergence: 429229.2

    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -3.4143 -0.5685  0.1990  0.6954  2.7232 

    Random effects:
     Groups   Name        Variance Std.Dev.
     famid    (Intercept)  53.41    7.308  
     Residual             313.22   17.698  
    Number of obs: 49263, groups:  famid, 18995

    Fixed effects:
                                                       Estimate Std. Error t value
    (Intercept)                                        63.82165    1.02853  62.052
    death_countryCanada                                12.40403   18.60843   0.667
    death_countrySouth Africa                           2.80960    4.02538   0.698
    death_countryAustralia                             24.67380   18.82042   1.311
    death_countryNew Zealand                            8.16427    3.18227   2.566
    death_countryUnited States of America               2.55311    2.15348   1.186
    birth101740                                        -0.81923    1.19736  -0.684
    birth101750                                        -1.48216    1.20614  -1.229
    birth101760                                         0.84054    1.17936   0.713
    birth101770                                         2.50921    1.14573   2.190
    birth101780                                         1.12396    1.12762   0.997
    birth101790                                        -0.13231    1.10217  -0.120
    birth101800                                        -0.43379    1.08860  -0.398
    birth101810                                        -0.85319    1.08188  -0.789
    birth101820                                        -2.23179    1.07504  -2.076
    birth101830                                        -4.46460    1.07740  -4.144
    birth101840                                        -5.05220    1.07895  -4.683
    birth101850                                        -4.83982    1.08656  -4.454
    birth101860                                        -3.36333    1.08942  -3.087
    birth101870                                        -0.28484    1.09393  -0.260
    birth101880                                         2.73683    1.10213   2.483
    birth101890                                         4.48154    1.16946   3.832
    gendermale                                          0.23121    0.17659   1.309
    sib_size_cat2                                      -1.07177    0.27162  -3.946
    sib_size_cat3-5                                    -1.03045    0.24217  -4.255
    sib_size_cat6+                                     -0.66226    0.34139  -1.940
    death_countryCanada:birth101740                    -8.38314   18.93771  -0.443
    death_countryAustralia:birth101740                 -8.51532   26.70923  -0.319
    death_countryUnited States of America:birth101740   2.83759    2.53797   1.118
    death_countryCanada:birth101750                    -4.97487   18.98056  -0.262
    death_countrySouth Africa:birth101750              -9.38030   19.57784  -0.479
    death_countryAustralia:birth101750                -16.13566   21.74536  -0.742
    death_countryNew Zealand:birth101750               19.89719   19.21763   1.035
    death_countryUnited States of America:birth101750   1.18463    2.58007   0.459
    death_countryCanada:birth101760                    -2.62763   18.88130  -0.139
    death_countrySouth Africa:birth101760               2.94313   13.91375   0.212
    death_countryAustralia:birth101760                -18.87078   19.54618  -0.965
    death_countryUnited States of America:birth101760   2.71771    2.62568   1.035
    death_countryCanada:birth101770                    -1.80548   18.84110  -0.096
    death_countrySouth Africa:birth101770              -5.82776    7.23300  -0.806
    death_countryAustralia:birth101770                -16.00542   19.17056  -0.835
    death_countryUnited States of America:birth101770  -1.76994    2.72852  -0.649
    death_countryCanada:birth101780                    -3.46553   18.71843  -0.185
    death_countrySouth Africa:birth101780              -0.63685    5.53216  -0.115
    death_countryAustralia:birth101780                -18.64119   19.02844  -0.980
    death_countryNew Zealand:birth101780               -0.66002    8.35330  -0.079
    death_countryUnited States of America:birth101780  -0.24442    2.62235  -0.093
    death_countryCanada:birth101790                    -1.26024   18.67159  -0.067
    death_countrySouth Africa:birth101790               3.79041    5.04248   0.752
    death_countryAustralia:birth101790                -17.37413   18.90236  -0.919
    death_countryNew Zealand:birth101790                3.32751    5.22557   0.637
    death_countryUnited States of America:birth101790   2.95127    2.53620   1.164
    death_countryCanada:birth101800                    -5.37082   18.65255  -0.288
    death_countrySouth Africa:birth101800               1.70422    4.50734   0.378
    death_countryAustralia:birth101800                -18.14869   18.86605  -0.962
    death_countryNew Zealand:birth101800                2.99548    4.24103   0.706
    death_countryUnited States of America:birth101800   3.20595    2.41817   1.326
    death_countryCanada:birth101810                    -3.88872   18.64418  -0.209
    death_countrySouth Africa:birth101810               1.42130    4.41841   0.322
    death_countryAustralia:birth101810                -18.96619   18.84802  -1.006
    death_countryNew Zealand:birth101810                2.19555    3.86919   0.567
    death_countryUnited States of America:birth101810   5.05583    2.32604   2.174
    death_countryCanada:birth101820                    -2.63422   18.64511  -0.141
    death_countrySouth Africa:birth101820               4.83277    5.32001   0.908
    death_countryAustralia:birth101820                -17.38025   18.83921  -0.923
    death_countryNew Zealand:birth101820                1.49647    3.59145   0.417
    death_countryUnited States of America:birth101820   5.17928    2.28091   2.271
    death_countryCanada:birth101830                    -1.10989   18.65981  -0.059
    death_countrySouth Africa:birth101830               3.62875    5.46193   0.664
    death_countryAustralia:birth101830                -16.40268   18.83557  -0.871
    death_countryNew Zealand:birth101830               -0.36457    3.44118  -0.106
    death_countryUnited States of America:birth101830   7.48248    2.26065   3.310
    death_countryCanada:birth101840                    -1.74968   18.68769  -0.094
    death_countrySouth Africa:birth101840              -0.81156    5.21362  -0.156
    death_countryAustralia:birth101840                -15.71028   18.84078  -0.834
    death_countryNew Zealand:birth101840                2.39271    3.47765   0.688
    death_countryUnited States of America:birth101840   7.84334    2.26447   3.464
    death_countryCanada:birth101850                    -2.40265   18.70806  -0.128
    death_countrySouth Africa:birth101850               0.45903    5.29780   0.087
    death_countryAustralia:birth101850                -15.65150   18.85196  -0.830
    death_countryNew Zealand:birth101850                3.41866    3.51128   0.974
    death_countryUnited States of America:birth101850   5.96416    2.29745   2.596
    death_countryCanada:birth101860                    -5.55319   18.69028  -0.297
    death_countrySouth Africa:birth101860              -5.40045    5.09409  -1.060
    death_countryAustralia:birth101860                -17.34505   18.86890  -0.919
    death_countryNew Zealand:birth101860                2.41215    3.57101   0.675
    death_countryUnited States of America:birth101860   5.10958    2.35041   2.174
    death_countryCanada:birth101870                    -4.06013   18.67773  -0.217
    death_countrySouth Africa:birth101870              -4.28845    4.75809  -0.901
    death_countryAustralia:birth101870                -18.43008   18.89111  -0.976
    death_countryNew Zealand:birth101870                0.02444    3.71076   0.007
    death_countryUnited States of America:birth101870   2.50469    2.38909   1.048
    death_countryCanada:birth101880                    -8.67402   18.66206  -0.465
    death_countrySouth Africa:birth101880               1.65844    4.93037   0.336
    death_countryAustralia:birth101880                -18.60069   18.88881  -0.985
    death_countryNew Zealand:birth101880               -3.82689    4.00189  -0.956
    death_countryUnited States of America:birth101880   1.17602    2.41636   0.487
    death_countryCanada:birth101890                    -9.37119   18.72117  -0.501
    death_countryAustralia:birth101890                -18.41600   18.97189  -0.971
    death_countryUnited States of America:birth101890   0.24765    2.60598   0.095


    Correlation matrix not shown by default, as p = 100 > 12.
    Use print(x, correlation=TRUE)  or
        vcov(x)        if you need it

    fit warnings:
    fixed-effect model matrix is rank deficient so dropping 6 columns / coefficients

``` r
newerdata %>% 
  ggplot(aes(
    x = predict(fe2.sibs),
    y = deathage)) + 
  geom_point() + 
  geom_abline(
    intercept = 0,
    slope = 1) +
  scale_x_continuous(limits = c(15, 110)) +
  scale_y_continuous(limits = c(15, 110)) +
  labs(x='Predicted Values', 
       y='Actual Values', 
       title='Predicted vs. Actual Values',
       subtitle= 'death country - model 3 - have parents + siblings')
```

![](analysis_binder_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe2-2.png)

``` r
stargazer(fe1, fe1.lim, fe1.sibs, fe2, fe2.lim, fe2.sibs,
          type = "text",
          column.labels = c("full", "have parents", "have siblings",
                            "full", "have parents", "have siblings"),
          dep.var.caption = "Dependent variable: Age at Death",
          dep.var.labels = "")
```


    ===================================================================================================================================
                                                                              Dependent variable: Age at Death                         
                                                      ---------------------------------------------------------------------------------
                                                                                                                                       
                                                          full      have parents have siblings     full      have parents have siblings
                                                           (1)          (2)           (3)           (4)          (5)           (6)     
    -----------------------------------------------------------------------------------------------------------------------------------
    migrantmigrant                                      3.954***      3.350***       2.881                                             
                                                         (0.783)      (1.058)       (2.139)                                            
                                                                                                                                       
    death_countryCanada                                                                            4.202        1.242        12.404    
                                                                                                  (2.572)      (4.637)      (18.608)   
                                                                                                                                       
    death_countrySouth Africa                                                                      1.887        1.992         2.810    
                                                                                                  (2.001)      (2.716)       (4.025)   
                                                                                                                                       
    death_countryAustralia                                                                        24.955        23.950       24.674    
                                                                                                 (17.109)      (17.595)     (18.820)   
                                                                                                                                       
    death_countryNew Zealand                                                                       2.938       4.728**       8.164**   
                                                                                                  (1.804)      (2.276)       (3.182)   
                                                                                                                                       
    death_countryUnited States of America                                                        3.911***      3.377***       2.553    
                                                                                                  (0.797)      (1.071)       (2.153)   
                                                                                                                                       
    birth101740                                          -0.247        -0.743       -0.821        -0.248        -0.744       -0.819    
                                                         (0.585)      (0.718)       (1.198)       (0.584)      (0.718)       (1.197)   
                                                                                                                                       
    birth101750                                           0.071        -0.744       -1.486         0.072        -0.744       -1.482    
                                                         (0.575)      (0.710)       (1.207)       (0.575)      (0.709)       (1.206)   
                                                                                                                                       
    birth101760                                         1.675***       0.957         0.839       1.675***       0.956         0.841    
                                                         (0.567)      (0.698)       (1.180)       (0.567)      (0.697)       (1.179)   
                                                                                                                                       
    birth101770                                         2.793***      2.068***      2.501**      2.791***      2.067***      2.509**   
                                                         (0.553)      (0.680)       (1.146)       (0.552)      (0.679)       (1.146)   
                                                                                                                                       
    birth101780                                         2.192***      1.413**        1.125       2.191***      1.411**        1.124    
                                                         (0.543)      (0.669)       (1.128)       (0.542)      (0.668)       (1.128)   
                                                                                                                                       
    birth101790                                          1.125**       0.042        -0.126        1.123**       0.039        -0.132    
                                                         (0.535)      (0.660)       (1.103)       (0.535)      (0.659)       (1.102)   
                                                                                                                                       
    birth101800                                           0.265        -0.667       -0.441         0.265        -0.666       -0.434    
                                                         (0.532)      (0.655)       (1.089)       (0.531)      (0.654)       (1.089)   
                                                                                                                                       
    birth101810                                          -0.776       -1.556**      -0.852        -0.779       -1.560**      -0.853    
                                                         (0.531)      (0.654)       (1.082)       (0.530)      (0.653)       (1.082)   
                                                                                                                                       
    birth101820                                         -2.162***    -2.908***     -2.226**      -2.167***    -2.914***     -2.232**   
                                                         (0.530)      (0.650)       (1.076)       (0.529)      (0.649)       (1.075)   
                                                                                                                                       
    birth101830                                         -4.044***    -4.918***     -4.465***     -4.047***    -4.919***     -4.465***  
                                                         (0.533)      (0.653)       (1.078)       (0.533)      (0.653)       (1.077)   
                                                                                                                                       
    birth101840                                         -4.811***    -5.655***     -5.050***     -4.814***    -5.657***     -5.052***  
                                                         (0.536)      (0.656)       (1.079)       (0.536)      (0.656)       (1.079)   
                                                                                                                                       
    birth101850                                         -4.705***    -5.831***     -4.840***     -4.706***    -5.832***     -4.840***  
                                                         (0.541)      (0.660)       (1.087)       (0.540)      (0.660)       (1.087)   
                                                                                                                                       
    birth101860                                         -3.110***    -4.315***     -3.354***     -3.113***    -4.320***     -3.363***  
                                                         (0.546)      (0.665)       (1.090)       (0.545)      (0.665)       (1.089)   
                                                                                                                                       
    birth101870                                          -0.668      -1.971***      -0.277        -0.670      -1.974***      -0.285    
                                                         (0.549)      (0.668)       (1.094)       (0.548)      (0.667)       (1.094)   
                                                                                                                                       
    birth101880                                         2.934***      1.414**       2.737**      2.934***      1.414**       2.737**   
                                                         (0.553)      (0.675)       (1.103)       (0.553)      (0.674)       (1.102)   
                                                                                                                                       
    birth101890                                         4.808***      3.220***     4.481***      4.806***      3.219***     4.482***   
                                                         (0.597)      (0.733)       (1.170)       (0.597)      (0.732)       (1.169)   
                                                                                                                                       
    gendermale                                           -0.033        0.0003        0.221        -0.042        0.0002        0.231    
                                                         (0.095)      (0.117)       (0.177)       (0.095)      (0.117)       (0.177)   
                                                                                                                                       
    sib_size_cat1                                       -1.949***    -1.860***                   -1.954***    -1.861***                
                                                         (0.171)      (0.177)                     (0.171)      (0.177)                 
                                                                                                                                       
    sib_size_cat2                                       -3.017***    -2.927***     -1.076***     -3.011***    -2.926***     -1.072***  
                                                         (0.205)      (0.213)       (0.271)       (0.205)      (0.213)       (0.272)   
                                                                                                                                       
    sib_size_cat3-5                                     -2.995***    -2.894***     -1.046***     -2.990***    -2.889***     -1.030***  
                                                         (0.172)      (0.179)       (0.242)       (0.172)      (0.179)       (0.242)   
                                                                                                                                       
    sib_size_cat6+                                      -2.560***    -2.468***     -0.699**      -2.522***    -2.439***      -0.662*   
                                                         (0.270)      (0.281)       (0.341)       (0.269)      (0.281)       (0.341)   
                                                                                                                                       
    sib_size_catmissing                                 0.411***                                 0.414***                              
                                                         (0.118)                                  (0.118)                              
                                                                                                                                       
    migrantmigrant:birth101740                            1.035        0.995         2.433                                             
                                                         (0.948)      (1.286)       (2.492)                                            
                                                                                                                                       
    migrantmigrant:birth101750                            0.045        0.233         1.406                                             
                                                         (0.930)      (1.266)       (2.530)                                            
                                                                                                                                       
    migrantmigrant:birth101760                           -1.320        -0.058        3.158                                             
                                                         (0.937)      (1.278)       (2.535)                                            
                                                                                                                                       
    migrantmigrant:birth101770                          -1.867**       -0.154        0.639                                             
                                                         (0.926)      (1.258)       (2.546)                                            
                                                                                                                                       
    migrantmigrant:birth101780                           -1.069        0.356         1.787                                             
                                                         (0.900)      (1.214)       (2.418)                                            
                                                                                                                                       
    migrantmigrant:birth101790                            0.526        2.281*       4.915**                                            
                                                         (0.870)      (1.168)       (2.322)                                            
                                                                                                                                       
    migrantmigrant:birth101800                            0.900        2.201*        3.527                                             
                                                         (0.854)      (1.143)       (2.261)                                            
                                                                                                                                       
    migrantmigrant:birth101810                            1.246       2.426**       4.272*                                             
                                                         (0.842)      (1.126)       (2.228)                                            
                                                                                                                                       
    migrantmigrant:birth101820                          2.391***      3.442***      5.142**                                            
                                                         (0.838)      (1.119)       (2.212)                                            
                                                                                                                                       
    migrantmigrant:birth101830                          3.577***      4.733***     6.386***                                            
                                                         (0.838)      (1.119)       (2.205)                                            
                                                                                                                                       
    migrantmigrant:birth101840                          4.082***      5.172***     6.974***                                            
                                                         (0.846)      (1.127)       (2.213)                                            
                                                                                                                                       
    migrantmigrant:birth101850                          3.867***      5.014***     6.113***                                            
                                                         (0.864)      (1.144)       (2.233)                                            
                                                                                                                                       
    migrantmigrant:birth101860                           1.834**      3.095***      4.620**                                            
                                                         (0.882)      (1.165)       (2.256)                                            
                                                                                                                                       
    migrantmigrant:birth101870                           1.555*       2.511**        2.882                                             
                                                         (0.898)      (1.180)       (2.273)                                            
                                                                                                                                       
    migrantmigrant:birth101880                           -0.936        -0.062        1.374                                             
                                                         (0.903)      (1.193)       (2.283)                                            
                                                                                                                                       
    migrantmigrant:birth101890                           -0.382        0.034         1.020                                             
                                                         (0.984)      (1.304)       (2.404)                                            
                                                                                                                                       
    death_countryCanada:birth101740                                                                4.102        6.812        -8.383    
                                                                                                  (2.965)      (5.071)      (18.938)   
                                                                                                                                       
    death_countryAustralia:birth101740                                                            -24.021      -27.468       -8.515    
                                                                                                 (17.687)      (18.693)     (26.709)   
                                                                                                                                       
    death_countryNew Zealand:birth101740                                                          -17.788      -19.768                 
                                                                                                 (12.384)      (12.822)                
                                                                                                                                       
    death_countryUnited States of America:birth101740                                              0.822        0.717         2.838    
                                                                                                  (0.971)      (1.314)       (2.538)   
                                                                                                                                       
    death_countryCanada:birth101750                                                                1.460        3.462        -4.975    
                                                                                                  (2.892)      (4.989)      (18.981)   
                                                                                                                                       
    death_countrySouth Africa:birth101750                                                         -1.743        -9.603       -9.380    
                                                                                                  (8.894)      (12.907)     (19.578)   
                                                                                                                                       
    death_countryAustralia:birth101750                                                            -21.006      -20.242       -16.136   
                                                                                                 (17.198)      (17.823)     (21.745)   
                                                                                                                                       
    death_countryNew Zealand:birth101750                                                          23.626        21.868       19.897    
                                                                                                 (17.295)      (17.844)     (19.218)   
                                                                                                                                       
    death_countryUnited States of America:birth101750                                             -0.104        0.051         1.185    
                                                                                                  (0.957)      (1.300)       (2.580)   
                                                                                                                                       
    death_countryCanada:birth101760                                                                1.998        4.672        -2.628    
                                                                                                  (2.831)      (4.918)      (18.881)   
                                                                                                                                       
    death_countrySouth Africa:birth101760                                                          2.683        8.050         2.943    
                                                                                                  (4.551)      (6.256)      (13.914)   
                                                                                                                                       
    death_countryAustralia:birth101760                                                            -23.616      -20.349       -18.871   
                                                                                                 (17.147)      (17.718)     (19.546)   
                                                                                                                                       
    death_countryNew Zealand:birth101760                                                          15.500*       3.531                  
                                                                                                  (8.851)      (17.984)                
                                                                                                                                       
    death_countryUnited States of America:birth101760                                             -1.786*       -0.785        2.718    
                                                                                                  (0.989)      (1.340)       (2.626)   
                                                                                                                                       
    death_countryCanada:birth101770                                                                3.024        6.418        -1.805    
                                                                                                  (2.753)      (4.826)      (18.841)   
                                                                                                                                       
    death_countrySouth Africa:birth101770                                                         -0.563        -1.007       -5.828    
                                                                                                  (2.778)      (3.788)       (7.233)   
                                                                                                                                       
    death_countryAustralia:birth101770                                                            -25.326      -20.676       -16.005   
                                                                                                 (17.141)      (17.669)     (19.171)   
                                                                                                                                       
    death_countryNew Zealand:birth101770                                                          -3.408        12.420                 
                                                                                                 (10.165)      (17.983)                
                                                                                                                                       
    death_countryUnited States of America:birth101770                                            -2.781***      -1.663       -1.770    
                                                                                                  (1.005)      (1.365)       (2.729)   
                                                                                                                                       
    death_countryCanada:birth101780                                                                2.246        5.180        -3.466    
                                                                                                  (2.702)      (4.765)      (18.718)   
                                                                                                                                       
    death_countrySouth Africa:birth101780                                                        -4.953**       0.523        -0.637    
                                                                                                  (2.384)      (3.429)       (5.532)   
                                                                                                                                       
    death_countryAustralia:birth101780                                                            -22.353      -19.386       -18.641   
                                                                                                 (17.133)      (17.642)     (19.028)   
                                                                                                                                       
    death_countryNew Zealand:birth101780                                                          7.026**       6.123        -0.660    
                                                                                                  (3.557)      (4.295)       (8.353)   
                                                                                                                                       
    death_countryUnited States of America:birth101780                                             -1.700*       -1.398       -0.244    
                                                                                                  (0.988)      (1.329)       (2.622)   
                                                                                                                                       
    death_countryCanada:birth101790                                                                3.350        7.663        -1.260    
                                                                                                  (2.660)      (4.726)      (18.672)   
                                                                                                                                       
    death_countrySouth Africa:birth101790                                                         -2.521        0.373         3.790    
                                                                                                  (2.339)      (3.257)       (5.042)   
                                                                                                                                       
    death_countryAustralia:birth101790                                                            -20.965      -18.345       -17.374   
                                                                                                 (17.124)      (17.619)     (18.902)   
                                                                                                                                       
    death_countryNew Zealand:birth101790                                                          3.982*        3.631         3.328    
                                                                                                  (2.416)      (3.076)       (5.226)   
                                                                                                                                       
    death_countryUnited States of America:birth101790                                             -0.224        0.767         2.951    
                                                                                                  (0.948)      (1.267)       (2.536)   
                                                                                                                                       
    death_countryCanada:birth101800                                                                1.911        5.398        -5.371    
                                                                                                  (2.650)      (4.713)      (18.653)   
                                                                                                                                       
    death_countrySouth Africa:birth101800                                                         -1.468        -0.017        1.704    
                                                                                                  (2.358)      (3.117)       (4.507)   
                                                                                                                                       
    death_countryAustralia:birth101800                                                            -21.138      -19.143       -18.149   
                                                                                                 (17.119)      (17.610)     (18.866)   
                                                                                                                                       
    death_countryNew Zealand:birth101800                                                           3.371        2.873         2.995    
                                                                                                  (2.103)      (2.663)       (4.241)   
                                                                                                                                       
    death_countryUnited States of America:birth101800                                              1.263        2.302*        3.206    
                                                                                                  (0.919)      (1.224)       (2.418)   
                                                                                                                                       
    death_countryCanada:birth101810                                                                3.345        6.842        -3.889    
                                                                                                  (2.642)      (4.701)      (18.644)   
                                                                                                                                       
    death_countrySouth Africa:birth101810                                                          0.016        -0.009        1.421    
                                                                                                  (2.285)      (3.025)       (4.418)   
                                                                                                                                       
    death_countryAustralia:birth101810                                                            -20.790      -19.261       -18.966   
                                                                                                 (17.116)      (17.606)     (18.848)   
                                                                                                                                       
    death_countryNew Zealand:birth101810                                                          4.610**       3.639         2.196    
                                                                                                  (2.026)      (2.545)       (3.869)   
                                                                                                                                       
    death_countryUnited States of America:birth101810                                              1.056        2.309*       5.056**   
                                                                                                  (0.890)      (1.183)       (2.326)   
                                                                                                                                       
    death_countryCanada:birth101820                                                                4.146        7.709        -2.634    
                                                                                                  (2.643)      (4.701)      (18.645)   
                                                                                                                                       
    death_countrySouth Africa:birth101820                                                          4.122        2.711         4.833    
                                                                                                  (2.711)      (3.572)       (5.320)   
                                                                                                                                       
    death_countryAustralia:birth101820                                                            -19.852      -18.470       -17.380   
                                                                                                 (17.115)      (17.604)     (18.839)   
                                                                                                                                       
    death_countryNew Zealand:birth101820                                                          4.816**       3.913         1.496    
                                                                                                  (1.984)      (2.494)       (3.591)   
                                                                                                                                       
    death_countryUnited States of America:birth101820                                            2.391***      3.431***      5.179**   
                                                                                                  (0.880)      (1.165)       (2.281)   
                                                                                                                                       
    death_countryCanada:birth101830                                                               5.143*       9.520**       -1.110    
                                                                                                  (2.666)      (4.723)      (18.660)   
                                                                                                                                       
    death_countrySouth Africa:birth101830                                                          4.069        1.410         3.629    
                                                                                                  (2.678)      (3.663)       (5.462)   
                                                                                                                                       
    death_countryAustralia:birth101830                                                            -18.761      -17.703       -16.403   
                                                                                                 (17.115)      (17.603)     (18.836)   
                                                                                                                                       
    death_countryNew Zealand:birth101830                                                          4.804**       2.965        -0.365    
                                                                                                  (1.936)      (2.429)       (3.441)   
                                                                                                                                       
    death_countryUnited States of America:birth101830                                            4.123***      5.660***     7.482***   
                                                                                                  (0.880)      (1.164)       (2.261)   
                                                                                                                                       
    death_countryCanada:birth101840                                                               4.643*        7.929*       -1.750    
                                                                                                  (2.702)      (4.752)      (18.688)   
                                                                                                                                       
    death_countrySouth Africa:birth101840                                                          3.015        1.880        -0.812    
                                                                                                  (2.598)      (3.418)       (5.214)   
                                                                                                                                       
    death_countryAustralia:birth101840                                                            -17.568      -16.297       -15.710   
                                                                                                 (17.117)      (17.606)     (18.841)   
                                                                                                                                       
    death_countryNew Zealand:birth101840                                                         5.947***       4.338*        2.393    
                                                                                                  (1.947)      (2.454)       (3.478)   
                                                                                                                                       
    death_countryUnited States of America:birth101840                                            4.247***      5.563***     7.843***   
                                                                                                  (0.887)      (1.171)       (2.264)   
                                                                                                                                       
    death_countryCanada:birth101850                                                                3.107        7.069        -2.403    
                                                                                                  (2.751)      (4.785)      (18.708)   
                                                                                                                                       
    death_countrySouth Africa:birth101850                                                          0.416        0.987         0.459    
                                                                                                  (2.650)      (3.452)       (5.298)   
                                                                                                                                       
    death_countryAustralia:birth101850                                                            -17.089      -15.637       -15.652   
                                                                                                 (17.121)      (17.610)     (18.852)   
                                                                                                                                       
    death_countryNew Zealand:birth101850                                                         7.430***      6.153**        3.419    
                                                                                                  (1.980)      (2.485)       (3.511)   
                                                                                                                                       
    death_countryUnited States of America:birth101850                                            3.632***      4.700***     5.964***   
                                                                                                  (0.917)      (1.202)       (2.297)   
                                                                                                                                       
    death_countryCanada:birth101860                                                                2.325        4.712        -5.553    
                                                                                                  (2.734)      (4.774)      (18.690)   
                                                                                                                                       
    death_countrySouth Africa:birth101860                                                         -0.068        -2.004       -5.400    
                                                                                                  (2.719)      (3.473)       (5.094)   
                                                                                                                                       
    death_countryAustralia:birth101860                                                            -19.141      -17.647       -17.345   
                                                                                                 (17.128)      (17.620)     (18.869)   
                                                                                                                                       
    death_countryNew Zealand:birth101860                                                         5.919***      5.006**        2.412    
                                                                                                  (2.053)      (2.550)       (3.571)   
                                                                                                                                       
    death_countryUnited States of America:birth101860                                              1.255       2.890**       5.110**   
                                                                                                  (0.945)      (1.240)       (2.350)   
                                                                                                                                       
    death_countryCanada:birth101870                                                                3.056        6.444        -4.060    
                                                                                                  (2.721)      (4.759)      (18.678)   
                                                                                                                                       
    death_countrySouth Africa:birth101870                                                          1.597        -1.099       -4.288    
                                                                                                  (2.433)      (3.193)       (4.758)   
                                                                                                                                       
    death_countryAustralia:birth101870                                                            -19.955      -18.831       -18.430   
                                                                                                 (17.138)      (17.632)     (18.891)   
                                                                                                                                       
    death_countryNew Zealand:birth101870                                                          4.207**       3.600         0.024    
                                                                                                  (2.146)      (2.659)       (3.711)   
                                                                                                                                       
    death_countryUnited States of America:birth101870                                              1.081        2.254*        2.505    
                                                                                                  (0.977)      (1.271)       (2.389)   
                                                                                                                                       
    death_countryCanada:birth101880                                                               -0.751        2.450        -8.674    
                                                                                                  (2.695)      (4.745)      (18.662)   
                                                                                                                                       
    death_countrySouth Africa:birth101880                                                          2.256        2.230         1.658    
                                                                                                  (2.452)      (3.247)       (4.930)   
                                                                                                                                       
    death_countryAustralia:birth101880                                                            -21.551      -20.162       -18.601   
                                                                                                 (17.135)      (17.632)     (18.889)   
                                                                                                                                       
    death_countryNew Zealand:birth101880                                                           1.331        0.615        -3.827    
                                                                                                  (2.327)      (2.894)       (4.002)   
                                                                                                                                       
    death_countryUnited States of America:birth101880                                             -1.557        -0.864        1.176    
                                                                                                  (0.993)      (1.299)       (2.416)   
                                                                                                                                       
    death_countryCanada:birth101890                                                               -0.077        2.156        -9.371    
                                                                                                  (2.770)      (4.831)      (18.721)   
                                                                                                                                       
    death_countryAustralia:birth101890                                                            -21.021      -19.513       -18.416   
                                                                                                 (17.159)      (17.674)     (18.972)   
                                                                                                                                       
    death_countryUnited States of America:birth101890                                             -0.426        -0.415        0.248    
                                                                                                  (1.123)      (1.484)       (2.606)   
                                                                                                                                       
    Constant                                            66.093***    66.785***     63.838***     66.097***    66.784***     63.822***  
                                                         (0.494)      (0.608)       (1.029)       (0.494)      (0.607)       (1.029)   
                                                                                                                                       
    -----------------------------------------------------------------------------------------------------------------------------------
    Observations                                         138,496       98,057       49,263        138,496       98,057       49,263    
    Log Likelihood                                    -591,258.700  -421,320.700 -214,789.700  -591,010.200  -421,108.100 -214,614.600 
    Akaike Inf. Crit.                                 1,182,601.000 842,723.500   429,659.400  1,182,234.000 842,428.200   429,433.200 
    Bayesian Inf. Crit.                               1,183,015.000 843,112.700   430,011.600  1,183,287.000 843,434.500   430,331.300 
    ===================================================================================================================================
    Note:                                                                                                   *p<0.1; **p<0.05; ***p<0.01
