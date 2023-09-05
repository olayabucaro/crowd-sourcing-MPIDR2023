Analysis
================

## Analysis

``` r
library(dplyr)
library(stargazer)
library(marginaleffects)
library(lme4)
library(margins)
library(data.table)
library(ggplot2)

data <- fread("../data/moddatafinal_LASTTIMEIPROMISE.csv")
```

## Format and summarize variables

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
    famid = as.numeric(famid),
    firstborn = factor(firstborn, levels = c('missing', '0', '1'))
    )
```

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

``` r
# make df where everyone has a parent in the data
newdata <- data %>% 
  filter(miss.famid == F) %>% 
  select(-miss.famid)

summary(newdata$sib_size_cat)
```

          0       1       2     3-5      6+ missing 
      48794   14375   10002   17446    7440       0 

``` r
# make df where only people with siblings are in data
newerdata <- newdata %>% 
  filter(sib.ct > 0)
```

## Family effects models

``` r
# model 1 - migrant
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

``` r
# plots here are kind of boring unless you do them by birth10 which isn't part
# of our RQ

fe1.plot <- plot_predictions(fe1, by = "migrant", draw = F)

# fe1.mm <- margins(lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
#             data = data), variables = "migrant")
# print(fe1.mm)
# 
# fe1.mm.bc <- margins(lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
#             data = data), variables = c("migrant", "birth10"))
# print(fe1.mm.bc)
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

![](analysis_render_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe1-1.png)

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

![](analysis_render_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe1-2.png)

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

## Death country

``` r
# model 2 - destination country
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

``` r
# fe2.mm <- margins(lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
#             data = data), variables = "death_country")
# print(fe2.mm)
```

``` r
fe2.plot <- plot_predictions(fe1, by = "death_country", draw = F)

fe2.plot %>% 
  ggplot(aes(
    x = death_country,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  )) + 
  geom_point(size = 3.5,
             aes(stroke = death_country)) +
  geom_errorbar(width = .15) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.title.y.left = element_text(size = 13)) +
  scale_x_discrete(labels = c("UK/Ireland", "Canada", "South Africa",
                              "Australia", "New Zealand", "USA")) +
  labs(x = "country of death", y = "estimated age at death") 
```

    Warning in `[<-.factor`(`*tmp*`, is.na(stroke_size), value = 0): ungültiges
    Faktorniveau, NA erzeugt

    Warning in Ops.factor(stroke_size, .stroke): '*' ist nicht sinnvoll für
    Faktoren

    Warning in Ops.factor(coords$stroke, .stroke): '*' ist nicht sinnvoll für
    Faktoren

![](analysis_render_files/figure-commonmark/death%20country%20plots-1.png)

``` r
# save graph
# ggsave(
#   plot = last_plot(),
#   filename = "graphs/fe2_countries.png",
#   width = 8.5,
#   height = 5,
#   units = "in"
# )
```

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
plot_predictions(fe2.lim, by = "death_country") 
```

![](analysis_render_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe2-1.png)

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

![](analysis_render_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe2-2.png)

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
plot_predictions(fe2.sibs, by = "death_country")
```

![](analysis_render_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe2-3.png)

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

![](analysis_render_files/figure-commonmark/alt%20migrant%20-%20modified%20samples%20fe2-4.png)

``` r
tbl <- stargazer(fe1, fe1.lim, fe1.sibs, fe2, fe2.lim, fe2.sibs,
          type = "html",
          column.labels = c("full", "have parents", "have siblings",
                            "full", "have parents", "have siblings"),
          dep.var.caption = "Dependent variable: Age at Death",
          dep.var.labels = "")
```


    <table style="text-align:center"><tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="6">Dependent variable: Age at Death</td></tr>
    <tr><td></td><td colspan="6" style="border-bottom: 1px solid black"></td></tr>
    <tr><td style="text-align:left"></td><td colspan="6"></td></tr>
    <tr><td style="text-align:left"></td><td>full</td><td>have parents</td><td>have siblings</td><td>full</td><td>have parents</td><td>have siblings</td></tr>
    <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td></tr>
    <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">migrantmigrant</td><td>3.954<sup>***</sup></td><td>3.350<sup>***</sup></td><td>2.881</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.783)</td><td>(1.058)</td><td>(2.139)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada</td><td></td><td></td><td></td><td>4.202</td><td>1.242</td><td>12.404</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.572)</td><td>(4.637)</td><td>(18.608)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa</td><td></td><td></td><td></td><td>1.887</td><td>1.992</td><td>2.810</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.001)</td><td>(2.716)</td><td>(4.025)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia</td><td></td><td></td><td></td><td>24.955</td><td>23.950</td><td>24.674</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.109)</td><td>(17.595)</td><td>(18.820)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand</td><td></td><td></td><td></td><td>2.938</td><td>4.728<sup>**</sup></td><td>8.164<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(1.804)</td><td>(2.276)</td><td>(3.182)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America</td><td></td><td></td><td></td><td>3.911<sup>***</sup></td><td>3.377<sup>***</sup></td><td>2.553</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.797)</td><td>(1.071)</td><td>(2.153)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101740</td><td>-0.247</td><td>-0.743</td><td>-0.821</td><td>-0.248</td><td>-0.744</td><td>-0.819</td></tr>
    <tr><td style="text-align:left"></td><td>(0.585)</td><td>(0.718)</td><td>(1.198)</td><td>(0.584)</td><td>(0.718)</td><td>(1.197)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101750</td><td>0.071</td><td>-0.744</td><td>-1.486</td><td>0.072</td><td>-0.744</td><td>-1.482</td></tr>
    <tr><td style="text-align:left"></td><td>(0.575)</td><td>(0.710)</td><td>(1.207)</td><td>(0.575)</td><td>(0.709)</td><td>(1.206)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101760</td><td>1.675<sup>***</sup></td><td>0.957</td><td>0.839</td><td>1.675<sup>***</sup></td><td>0.956</td><td>0.841</td></tr>
    <tr><td style="text-align:left"></td><td>(0.567)</td><td>(0.698)</td><td>(1.180)</td><td>(0.567)</td><td>(0.697)</td><td>(1.179)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101770</td><td>2.793<sup>***</sup></td><td>2.068<sup>***</sup></td><td>2.501<sup>**</sup></td><td>2.791<sup>***</sup></td><td>2.067<sup>***</sup></td><td>2.509<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.553)</td><td>(0.680)</td><td>(1.146)</td><td>(0.552)</td><td>(0.679)</td><td>(1.146)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101780</td><td>2.192<sup>***</sup></td><td>1.413<sup>**</sup></td><td>1.125</td><td>2.191<sup>***</sup></td><td>1.411<sup>**</sup></td><td>1.124</td></tr>
    <tr><td style="text-align:left"></td><td>(0.543)</td><td>(0.669)</td><td>(1.128)</td><td>(0.542)</td><td>(0.668)</td><td>(1.128)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101790</td><td>1.125<sup>**</sup></td><td>0.042</td><td>-0.126</td><td>1.123<sup>**</sup></td><td>0.039</td><td>-0.132</td></tr>
    <tr><td style="text-align:left"></td><td>(0.535)</td><td>(0.660)</td><td>(1.103)</td><td>(0.535)</td><td>(0.659)</td><td>(1.102)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101800</td><td>0.265</td><td>-0.667</td><td>-0.441</td><td>0.265</td><td>-0.666</td><td>-0.434</td></tr>
    <tr><td style="text-align:left"></td><td>(0.532)</td><td>(0.655)</td><td>(1.089)</td><td>(0.531)</td><td>(0.654)</td><td>(1.089)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101810</td><td>-0.776</td><td>-1.556<sup>**</sup></td><td>-0.852</td><td>-0.779</td><td>-1.560<sup>**</sup></td><td>-0.853</td></tr>
    <tr><td style="text-align:left"></td><td>(0.531)</td><td>(0.654)</td><td>(1.082)</td><td>(0.530)</td><td>(0.653)</td><td>(1.082)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101820</td><td>-2.162<sup>***</sup></td><td>-2.908<sup>***</sup></td><td>-2.226<sup>**</sup></td><td>-2.167<sup>***</sup></td><td>-2.914<sup>***</sup></td><td>-2.232<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.530)</td><td>(0.650)</td><td>(1.076)</td><td>(0.529)</td><td>(0.649)</td><td>(1.075)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101830</td><td>-4.044<sup>***</sup></td><td>-4.918<sup>***</sup></td><td>-4.465<sup>***</sup></td><td>-4.047<sup>***</sup></td><td>-4.919<sup>***</sup></td><td>-4.465<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.533)</td><td>(0.653)</td><td>(1.078)</td><td>(0.533)</td><td>(0.653)</td><td>(1.077)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101840</td><td>-4.811<sup>***</sup></td><td>-5.655<sup>***</sup></td><td>-5.050<sup>***</sup></td><td>-4.814<sup>***</sup></td><td>-5.657<sup>***</sup></td><td>-5.052<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.536)</td><td>(0.656)</td><td>(1.079)</td><td>(0.536)</td><td>(0.656)</td><td>(1.079)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101850</td><td>-4.705<sup>***</sup></td><td>-5.831<sup>***</sup></td><td>-4.840<sup>***</sup></td><td>-4.706<sup>***</sup></td><td>-5.832<sup>***</sup></td><td>-4.840<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.541)</td><td>(0.660)</td><td>(1.087)</td><td>(0.540)</td><td>(0.660)</td><td>(1.087)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101860</td><td>-3.110<sup>***</sup></td><td>-4.315<sup>***</sup></td><td>-3.354<sup>***</sup></td><td>-3.113<sup>***</sup></td><td>-4.320<sup>***</sup></td><td>-3.363<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.546)</td><td>(0.665)</td><td>(1.090)</td><td>(0.545)</td><td>(0.665)</td><td>(1.089)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101870</td><td>-0.668</td><td>-1.971<sup>***</sup></td><td>-0.277</td><td>-0.670</td><td>-1.974<sup>***</sup></td><td>-0.285</td></tr>
    <tr><td style="text-align:left"></td><td>(0.549)</td><td>(0.668)</td><td>(1.094)</td><td>(0.548)</td><td>(0.667)</td><td>(1.094)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101880</td><td>2.934<sup>***</sup></td><td>1.414<sup>**</sup></td><td>2.737<sup>**</sup></td><td>2.934<sup>***</sup></td><td>1.414<sup>**</sup></td><td>2.737<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.553)</td><td>(0.675)</td><td>(1.103)</td><td>(0.553)</td><td>(0.674)</td><td>(1.102)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101890</td><td>4.808<sup>***</sup></td><td>3.220<sup>***</sup></td><td>4.481<sup>***</sup></td><td>4.806<sup>***</sup></td><td>3.219<sup>***</sup></td><td>4.482<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.597)</td><td>(0.733)</td><td>(1.170)</td><td>(0.597)</td><td>(0.732)</td><td>(1.169)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">gendermale</td><td>-0.033</td><td>0.0003</td><td>0.221</td><td>-0.042</td><td>0.0002</td><td>0.231</td></tr>
    <tr><td style="text-align:left"></td><td>(0.095)</td><td>(0.117)</td><td>(0.177)</td><td>(0.095)</td><td>(0.117)</td><td>(0.177)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_cat1</td><td>-1.949<sup>***</sup></td><td>-1.860<sup>***</sup></td><td></td><td>-1.954<sup>***</sup></td><td>-1.861<sup>***</sup></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.171)</td><td>(0.177)</td><td></td><td>(0.171)</td><td>(0.177)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_cat2</td><td>-3.017<sup>***</sup></td><td>-2.927<sup>***</sup></td><td>-1.076<sup>***</sup></td><td>-3.011<sup>***</sup></td><td>-2.926<sup>***</sup></td><td>-1.072<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.205)</td><td>(0.213)</td><td>(0.271)</td><td>(0.205)</td><td>(0.213)</td><td>(0.272)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_cat3-5</td><td>-2.995<sup>***</sup></td><td>-2.894<sup>***</sup></td><td>-1.046<sup>***</sup></td><td>-2.990<sup>***</sup></td><td>-2.889<sup>***</sup></td><td>-1.030<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.172)</td><td>(0.179)</td><td>(0.242)</td><td>(0.172)</td><td>(0.179)</td><td>(0.242)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_cat6+</td><td>-2.560<sup>***</sup></td><td>-2.468<sup>***</sup></td><td>-0.699<sup>**</sup></td><td>-2.522<sup>***</sup></td><td>-2.439<sup>***</sup></td><td>-0.662<sup>*</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.270)</td><td>(0.281)</td><td>(0.341)</td><td>(0.269)</td><td>(0.281)</td><td>(0.341)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_catmissing</td><td>0.411<sup>***</sup></td><td></td><td></td><td>0.414<sup>***</sup></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.118)</td><td></td><td></td><td>(0.118)</td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101740</td><td>1.035</td><td>0.995</td><td>2.433</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.948)</td><td>(1.286)</td><td>(2.492)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101750</td><td>0.045</td><td>0.233</td><td>1.406</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.930)</td><td>(1.266)</td><td>(2.530)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101760</td><td>-1.320</td><td>-0.058</td><td>3.158</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.937)</td><td>(1.278)</td><td>(2.535)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101770</td><td>-1.867<sup>**</sup></td><td>-0.154</td><td>0.639</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.926)</td><td>(1.258)</td><td>(2.546)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101780</td><td>-1.069</td><td>0.356</td><td>1.787</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.900)</td><td>(1.214)</td><td>(2.418)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101790</td><td>0.526</td><td>2.281<sup>*</sup></td><td>4.915<sup>**</sup></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.870)</td><td>(1.168)</td><td>(2.322)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101800</td><td>0.900</td><td>2.201<sup>*</sup></td><td>3.527</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.854)</td><td>(1.143)</td><td>(2.261)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101810</td><td>1.246</td><td>2.426<sup>**</sup></td><td>4.272<sup>*</sup></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.842)</td><td>(1.126)</td><td>(2.228)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101820</td><td>2.391<sup>***</sup></td><td>3.442<sup>***</sup></td><td>5.142<sup>**</sup></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.838)</td><td>(1.119)</td><td>(2.212)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101830</td><td>3.577<sup>***</sup></td><td>4.733<sup>***</sup></td><td>6.386<sup>***</sup></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.838)</td><td>(1.119)</td><td>(2.205)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101840</td><td>4.082<sup>***</sup></td><td>5.172<sup>***</sup></td><td>6.974<sup>***</sup></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.846)</td><td>(1.127)</td><td>(2.213)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101850</td><td>3.867<sup>***</sup></td><td>5.014<sup>***</sup></td><td>6.113<sup>***</sup></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.864)</td><td>(1.144)</td><td>(2.233)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101860</td><td>1.834<sup>**</sup></td><td>3.095<sup>***</sup></td><td>4.620<sup>**</sup></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.882)</td><td>(1.165)</td><td>(2.256)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101870</td><td>1.555<sup>*</sup></td><td>2.511<sup>**</sup></td><td>2.882</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.898)</td><td>(1.180)</td><td>(2.273)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101880</td><td>-0.936</td><td>-0.062</td><td>1.374</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.903)</td><td>(1.193)</td><td>(2.283)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101890</td><td>-0.382</td><td>0.034</td><td>1.020</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.984)</td><td>(1.304)</td><td>(2.404)</td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101740</td><td></td><td></td><td></td><td>4.102</td><td>6.812</td><td>-8.383</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.965)</td><td>(5.071)</td><td>(18.938)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101740</td><td></td><td></td><td></td><td>-24.021</td><td>-27.468</td><td>-8.515</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.687)</td><td>(18.693)</td><td>(26.709)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101740</td><td></td><td></td><td></td><td>-17.788</td><td>-19.768</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(12.384)</td><td>(12.822)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101740</td><td></td><td></td><td></td><td>0.822</td><td>0.717</td><td>2.838</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.971)</td><td>(1.314)</td><td>(2.538)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101750</td><td></td><td></td><td></td><td>1.460</td><td>3.462</td><td>-4.975</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.892)</td><td>(4.989)</td><td>(18.981)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101750</td><td></td><td></td><td></td><td>-1.743</td><td>-9.603</td><td>-9.380</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(8.894)</td><td>(12.907)</td><td>(19.578)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101750</td><td></td><td></td><td></td><td>-21.006</td><td>-20.242</td><td>-16.136</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.198)</td><td>(17.823)</td><td>(21.745)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101750</td><td></td><td></td><td></td><td>23.626</td><td>21.868</td><td>19.897</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.295)</td><td>(17.844)</td><td>(19.218)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101750</td><td></td><td></td><td></td><td>-0.104</td><td>0.051</td><td>1.185</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.957)</td><td>(1.300)</td><td>(2.580)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101760</td><td></td><td></td><td></td><td>1.998</td><td>4.672</td><td>-2.628</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.831)</td><td>(4.918)</td><td>(18.881)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101760</td><td></td><td></td><td></td><td>2.683</td><td>8.050</td><td>2.943</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(4.551)</td><td>(6.256)</td><td>(13.914)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101760</td><td></td><td></td><td></td><td>-23.616</td><td>-20.349</td><td>-18.871</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.147)</td><td>(17.718)</td><td>(19.546)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101760</td><td></td><td></td><td></td><td>15.500<sup>*</sup></td><td>3.531</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(8.851)</td><td>(17.984)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101760</td><td></td><td></td><td></td><td>-1.786<sup>*</sup></td><td>-0.785</td><td>2.718</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.989)</td><td>(1.340)</td><td>(2.626)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101770</td><td></td><td></td><td></td><td>3.024</td><td>6.418</td><td>-1.805</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.753)</td><td>(4.826)</td><td>(18.841)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101770</td><td></td><td></td><td></td><td>-0.563</td><td>-1.007</td><td>-5.828</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.778)</td><td>(3.788)</td><td>(7.233)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101770</td><td></td><td></td><td></td><td>-25.326</td><td>-20.676</td><td>-16.005</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.141)</td><td>(17.669)</td><td>(19.171)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101770</td><td></td><td></td><td></td><td>-3.408</td><td>12.420</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(10.165)</td><td>(17.983)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101770</td><td></td><td></td><td></td><td>-2.781<sup>***</sup></td><td>-1.663</td><td>-1.770</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(1.005)</td><td>(1.365)</td><td>(2.729)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101780</td><td></td><td></td><td></td><td>2.246</td><td>5.180</td><td>-3.466</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.702)</td><td>(4.765)</td><td>(18.718)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101780</td><td></td><td></td><td></td><td>-4.953<sup>**</sup></td><td>0.523</td><td>-0.637</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.384)</td><td>(3.429)</td><td>(5.532)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101780</td><td></td><td></td><td></td><td>-22.353</td><td>-19.386</td><td>-18.641</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.133)</td><td>(17.642)</td><td>(19.028)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101780</td><td></td><td></td><td></td><td>7.026<sup>**</sup></td><td>6.123</td><td>-0.660</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(3.557)</td><td>(4.295)</td><td>(8.353)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101780</td><td></td><td></td><td></td><td>-1.700<sup>*</sup></td><td>-1.398</td><td>-0.244</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.988)</td><td>(1.329)</td><td>(2.622)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101790</td><td></td><td></td><td></td><td>3.350</td><td>7.663</td><td>-1.260</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.660)</td><td>(4.726)</td><td>(18.672)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101790</td><td></td><td></td><td></td><td>-2.521</td><td>0.373</td><td>3.790</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.339)</td><td>(3.257)</td><td>(5.042)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101790</td><td></td><td></td><td></td><td>-20.965</td><td>-18.345</td><td>-17.374</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.124)</td><td>(17.619)</td><td>(18.902)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101790</td><td></td><td></td><td></td><td>3.982<sup>*</sup></td><td>3.631</td><td>3.328</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.416)</td><td>(3.076)</td><td>(5.226)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101790</td><td></td><td></td><td></td><td>-0.224</td><td>0.767</td><td>2.951</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.948)</td><td>(1.267)</td><td>(2.536)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101800</td><td></td><td></td><td></td><td>1.911</td><td>5.398</td><td>-5.371</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.650)</td><td>(4.713)</td><td>(18.653)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101800</td><td></td><td></td><td></td><td>-1.468</td><td>-0.017</td><td>1.704</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.358)</td><td>(3.117)</td><td>(4.507)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101800</td><td></td><td></td><td></td><td>-21.138</td><td>-19.143</td><td>-18.149</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.119)</td><td>(17.610)</td><td>(18.866)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101800</td><td></td><td></td><td></td><td>3.371</td><td>2.873</td><td>2.995</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.103)</td><td>(2.663)</td><td>(4.241)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101800</td><td></td><td></td><td></td><td>1.263</td><td>2.302<sup>*</sup></td><td>3.206</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.919)</td><td>(1.224)</td><td>(2.418)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101810</td><td></td><td></td><td></td><td>3.345</td><td>6.842</td><td>-3.889</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.642)</td><td>(4.701)</td><td>(18.644)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101810</td><td></td><td></td><td></td><td>0.016</td><td>-0.009</td><td>1.421</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.285)</td><td>(3.025)</td><td>(4.418)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101810</td><td></td><td></td><td></td><td>-20.790</td><td>-19.261</td><td>-18.966</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.116)</td><td>(17.606)</td><td>(18.848)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101810</td><td></td><td></td><td></td><td>4.610<sup>**</sup></td><td>3.639</td><td>2.196</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.026)</td><td>(2.545)</td><td>(3.869)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101810</td><td></td><td></td><td></td><td>1.056</td><td>2.309<sup>*</sup></td><td>5.056<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.890)</td><td>(1.183)</td><td>(2.326)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101820</td><td></td><td></td><td></td><td>4.146</td><td>7.709</td><td>-2.634</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.643)</td><td>(4.701)</td><td>(18.645)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101820</td><td></td><td></td><td></td><td>4.122</td><td>2.711</td><td>4.833</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.711)</td><td>(3.572)</td><td>(5.320)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101820</td><td></td><td></td><td></td><td>-19.852</td><td>-18.470</td><td>-17.380</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.115)</td><td>(17.604)</td><td>(18.839)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101820</td><td></td><td></td><td></td><td>4.816<sup>**</sup></td><td>3.913</td><td>1.496</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(1.984)</td><td>(2.494)</td><td>(3.591)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101820</td><td></td><td></td><td></td><td>2.391<sup>***</sup></td><td>3.431<sup>***</sup></td><td>5.179<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.880)</td><td>(1.165)</td><td>(2.281)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101830</td><td></td><td></td><td></td><td>5.143<sup>*</sup></td><td>9.520<sup>**</sup></td><td>-1.110</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.666)</td><td>(4.723)</td><td>(18.660)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101830</td><td></td><td></td><td></td><td>4.069</td><td>1.410</td><td>3.629</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.678)</td><td>(3.663)</td><td>(5.462)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101830</td><td></td><td></td><td></td><td>-18.761</td><td>-17.703</td><td>-16.403</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.115)</td><td>(17.603)</td><td>(18.836)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101830</td><td></td><td></td><td></td><td>4.804<sup>**</sup></td><td>2.965</td><td>-0.365</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(1.936)</td><td>(2.429)</td><td>(3.441)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101830</td><td></td><td></td><td></td><td>4.123<sup>***</sup></td><td>5.660<sup>***</sup></td><td>7.482<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.880)</td><td>(1.164)</td><td>(2.261)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101840</td><td></td><td></td><td></td><td>4.643<sup>*</sup></td><td>7.929<sup>*</sup></td><td>-1.750</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.702)</td><td>(4.752)</td><td>(18.688)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101840</td><td></td><td></td><td></td><td>3.015</td><td>1.880</td><td>-0.812</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.598)</td><td>(3.418)</td><td>(5.214)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101840</td><td></td><td></td><td></td><td>-17.568</td><td>-16.297</td><td>-15.710</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.117)</td><td>(17.606)</td><td>(18.841)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101840</td><td></td><td></td><td></td><td>5.947<sup>***</sup></td><td>4.338<sup>*</sup></td><td>2.393</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(1.947)</td><td>(2.454)</td><td>(3.478)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101840</td><td></td><td></td><td></td><td>4.247<sup>***</sup></td><td>5.563<sup>***</sup></td><td>7.843<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.887)</td><td>(1.171)</td><td>(2.264)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101850</td><td></td><td></td><td></td><td>3.107</td><td>7.069</td><td>-2.403</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.751)</td><td>(4.785)</td><td>(18.708)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101850</td><td></td><td></td><td></td><td>0.416</td><td>0.987</td><td>0.459</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.650)</td><td>(3.452)</td><td>(5.298)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101850</td><td></td><td></td><td></td><td>-17.089</td><td>-15.637</td><td>-15.652</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.121)</td><td>(17.610)</td><td>(18.852)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101850</td><td></td><td></td><td></td><td>7.430<sup>***</sup></td><td>6.153<sup>**</sup></td><td>3.419</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(1.980)</td><td>(2.485)</td><td>(3.511)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101850</td><td></td><td></td><td></td><td>3.632<sup>***</sup></td><td>4.700<sup>***</sup></td><td>5.964<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.917)</td><td>(1.202)</td><td>(2.297)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101860</td><td></td><td></td><td></td><td>2.325</td><td>4.712</td><td>-5.553</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.734)</td><td>(4.774)</td><td>(18.690)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101860</td><td></td><td></td><td></td><td>-0.068</td><td>-2.004</td><td>-5.400</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.719)</td><td>(3.473)</td><td>(5.094)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101860</td><td></td><td></td><td></td><td>-19.141</td><td>-17.647</td><td>-17.345</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.128)</td><td>(17.620)</td><td>(18.869)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101860</td><td></td><td></td><td></td><td>5.919<sup>***</sup></td><td>5.006<sup>**</sup></td><td>2.412</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.053)</td><td>(2.550)</td><td>(3.571)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101860</td><td></td><td></td><td></td><td>1.255</td><td>2.890<sup>**</sup></td><td>5.110<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.945)</td><td>(1.240)</td><td>(2.350)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101870</td><td></td><td></td><td></td><td>3.056</td><td>6.444</td><td>-4.060</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.721)</td><td>(4.759)</td><td>(18.678)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101870</td><td></td><td></td><td></td><td>1.597</td><td>-1.099</td><td>-4.288</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.433)</td><td>(3.193)</td><td>(4.758)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101870</td><td></td><td></td><td></td><td>-19.955</td><td>-18.831</td><td>-18.430</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.138)</td><td>(17.632)</td><td>(18.891)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101870</td><td></td><td></td><td></td><td>4.207<sup>**</sup></td><td>3.600</td><td>0.024</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.146)</td><td>(2.659)</td><td>(3.711)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101870</td><td></td><td></td><td></td><td>1.081</td><td>2.254<sup>*</sup></td><td>2.505</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.977)</td><td>(1.271)</td><td>(2.389)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101880</td><td></td><td></td><td></td><td>-0.751</td><td>2.450</td><td>-8.674</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.695)</td><td>(4.745)</td><td>(18.662)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101880</td><td></td><td></td><td></td><td>2.256</td><td>2.230</td><td>1.658</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.452)</td><td>(3.247)</td><td>(4.930)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101880</td><td></td><td></td><td></td><td>-21.551</td><td>-20.162</td><td>-18.601</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.135)</td><td>(17.632)</td><td>(18.889)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101880</td><td></td><td></td><td></td><td>1.331</td><td>0.615</td><td>-3.827</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.327)</td><td>(2.894)</td><td>(4.002)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101880</td><td></td><td></td><td></td><td>-1.557</td><td>-0.864</td><td>1.176</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.993)</td><td>(1.299)</td><td>(2.416)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101890</td><td></td><td></td><td></td><td>-0.077</td><td>2.156</td><td>-9.371</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(2.770)</td><td>(4.831)</td><td>(18.721)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101890</td><td></td><td></td><td></td><td>-21.021</td><td>-19.513</td><td>-18.416</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.159)</td><td>(17.674)</td><td>(18.972)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101890</td><td></td><td></td><td></td><td>-0.426</td><td>-0.415</td><td>0.248</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(1.123)</td><td>(1.484)</td><td>(2.606)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td style="text-align:left">Constant</td><td>66.093<sup>***</sup></td><td>66.785<sup>***</sup></td><td>63.838<sup>***</sup></td><td>66.097<sup>***</sup></td><td>66.784<sup>***</sup></td><td>63.822<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.494)</td><td>(0.608)</td><td>(1.029)</td><td>(0.494)</td><td>(0.607)</td><td>(1.029)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
    <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>138,496</td><td>98,057</td><td>49,263</td><td>138,496</td><td>98,057</td><td>49,263</td></tr>
    <tr><td style="text-align:left">Log Likelihood</td><td>-591,258.700</td><td>-421,320.700</td><td>-214,789.700</td><td>-591,010.200</td><td>-421,108.100</td><td>-214,614.600</td></tr>
    <tr><td style="text-align:left">Akaike Inf. Crit.</td><td>1,182,601.000</td><td>842,723.500</td><td>429,659.400</td><td>1,182,234.000</td><td>842,428.200</td><td>429,433.200</td></tr>
    <tr><td style="text-align:left">Bayesian Inf. Crit.</td><td>1,183,015.000</td><td>843,112.700</td><td>430,011.600</td><td>1,183,287.000</td><td>843,434.500</td><td>430,331.300</td></tr>
    <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="6" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
    </table>

``` r
#write(tbl, file = "tables/Results_table_compsamples.html", sep = ",")
```

``` r
cov.labs <- c("Migrant", "Canada", "South Africa", "Australia", 
              "New Zealand", "United States of America")
omit.vars <- c("birth10", "sib_size_cat", "gender")

tbl <- stargazer(fe1, fe2,
                 type = "html",
                 column.labels = c("Migrant", "Country of Death"),
                 covariate.labels = cov.labs,
                 dep.var.caption = "Dependent variable: Age at Death",
                 dep.var.labels = "",
                 keep.stat = c("n"),
                 add.lines=list(c("Sibling random effects", "Yes", "Yes"))
)
```


    <table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2">Dependent variable: Age at Death</td></tr>
    <tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
    <tr><td style="text-align:left"></td><td colspan="2"></td></tr>
    <tr><td style="text-align:left"></td><td>Migrant</td><td>Country of Death</td></tr>
    <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
    <tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Migrant</td><td>3.954<sup>***</sup></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.783)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">Canada</td><td></td><td>4.202</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.572)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">South Africa</td><td></td><td>1.887</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.001)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">Australia</td><td></td><td>24.955</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.109)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">New Zealand</td><td></td><td>2.938</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(1.804)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">United States of America</td><td></td><td>3.911<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.797)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101740</td><td>-0.247</td><td>-0.248</td></tr>
    <tr><td style="text-align:left"></td><td>(0.585)</td><td>(0.584)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101750</td><td>0.071</td><td>0.072</td></tr>
    <tr><td style="text-align:left"></td><td>(0.575)</td><td>(0.575)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101760</td><td>1.675<sup>***</sup></td><td>1.675<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.567)</td><td>(0.567)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101770</td><td>2.793<sup>***</sup></td><td>2.791<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.553)</td><td>(0.552)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101780</td><td>2.192<sup>***</sup></td><td>2.191<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.543)</td><td>(0.542)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101790</td><td>1.125<sup>**</sup></td><td>1.123<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.535)</td><td>(0.535)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101800</td><td>0.265</td><td>0.265</td></tr>
    <tr><td style="text-align:left"></td><td>(0.532)</td><td>(0.531)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101810</td><td>-0.776</td><td>-0.779</td></tr>
    <tr><td style="text-align:left"></td><td>(0.531)</td><td>(0.530)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101820</td><td>-2.162<sup>***</sup></td><td>-2.167<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.530)</td><td>(0.529)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101830</td><td>-4.044<sup>***</sup></td><td>-4.047<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.533)</td><td>(0.533)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101840</td><td>-4.811<sup>***</sup></td><td>-4.814<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.536)</td><td>(0.536)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101850</td><td>-4.705<sup>***</sup></td><td>-4.706<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.541)</td><td>(0.540)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101860</td><td>-3.110<sup>***</sup></td><td>-3.113<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.546)</td><td>(0.545)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101870</td><td>-0.668</td><td>-0.670</td></tr>
    <tr><td style="text-align:left"></td><td>(0.549)</td><td>(0.548)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101880</td><td>2.934<sup>***</sup></td><td>2.934<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.553)</td><td>(0.553)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">birth101890</td><td>4.808<sup>***</sup></td><td>4.806<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.597)</td><td>(0.597)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">gendermale</td><td>-0.033</td><td>-0.042</td></tr>
    <tr><td style="text-align:left"></td><td>(0.095)</td><td>(0.095)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_cat1</td><td>-1.949<sup>***</sup></td><td>-1.954<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.171)</td><td>(0.171)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_cat2</td><td>-3.017<sup>***</sup></td><td>-3.011<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.205)</td><td>(0.205)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_cat3-5</td><td>-2.995<sup>***</sup></td><td>-2.990<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.172)</td><td>(0.172)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_cat6+</td><td>-2.560<sup>***</sup></td><td>-2.522<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.270)</td><td>(0.269)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">sib_size_catmissing</td><td>0.411<sup>***</sup></td><td>0.414<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.118)</td><td>(0.118)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101740</td><td>1.035</td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.948)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101750</td><td>0.045</td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.930)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101760</td><td>-1.320</td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.937)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101770</td><td>-1.867<sup>**</sup></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.926)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101780</td><td>-1.069</td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.900)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101790</td><td>0.526</td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.870)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101800</td><td>0.900</td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.854)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101810</td><td>1.246</td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.842)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101820</td><td>2.391<sup>***</sup></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.838)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101830</td><td>3.577<sup>***</sup></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.838)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101840</td><td>4.082<sup>***</sup></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.846)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101850</td><td>3.867<sup>***</sup></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.864)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101860</td><td>1.834<sup>**</sup></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.882)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101870</td><td>1.555<sup>*</sup></td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.898)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101880</td><td>-0.936</td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.903)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">migrantmigrant:birth101890</td><td>-0.382</td><td></td></tr>
    <tr><td style="text-align:left"></td><td>(0.984)</td><td></td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101740</td><td></td><td>4.102</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.965)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101740</td><td></td><td>-24.021</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.687)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101740</td><td></td><td>-17.788</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(12.384)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101740</td><td></td><td>0.822</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.971)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101750</td><td></td><td>1.460</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.892)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101750</td><td></td><td>-1.743</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(8.894)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101750</td><td></td><td>-21.006</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.198)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101750</td><td></td><td>23.626</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.295)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101750</td><td></td><td>-0.104</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.957)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101760</td><td></td><td>1.998</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.831)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101760</td><td></td><td>2.683</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(4.551)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101760</td><td></td><td>-23.616</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.147)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101760</td><td></td><td>15.500<sup>*</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(8.851)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101760</td><td></td><td>-1.786<sup>*</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.989)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101770</td><td></td><td>3.024</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.753)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101770</td><td></td><td>-0.563</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.778)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101770</td><td></td><td>-25.326</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.141)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101770</td><td></td><td>-3.408</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(10.165)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101770</td><td></td><td>-2.781<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(1.005)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101780</td><td></td><td>2.246</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.702)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101780</td><td></td><td>-4.953<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.384)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101780</td><td></td><td>-22.353</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.133)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101780</td><td></td><td>7.026<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(3.557)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101780</td><td></td><td>-1.700<sup>*</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.988)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101790</td><td></td><td>3.350</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.660)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101790</td><td></td><td>-2.521</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.339)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101790</td><td></td><td>-20.965</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.124)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101790</td><td></td><td>3.982<sup>*</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.416)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101790</td><td></td><td>-0.224</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.948)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101800</td><td></td><td>1.911</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.650)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101800</td><td></td><td>-1.468</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.358)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101800</td><td></td><td>-21.138</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.119)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101800</td><td></td><td>3.371</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.103)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101800</td><td></td><td>1.263</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.919)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101810</td><td></td><td>3.345</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.642)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101810</td><td></td><td>0.016</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.285)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101810</td><td></td><td>-20.790</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.116)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101810</td><td></td><td>4.610<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.026)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101810</td><td></td><td>1.056</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.890)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101820</td><td></td><td>4.146</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.643)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101820</td><td></td><td>4.122</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.711)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101820</td><td></td><td>-19.852</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.115)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101820</td><td></td><td>4.816<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(1.984)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101820</td><td></td><td>2.391<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.880)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101830</td><td></td><td>5.143<sup>*</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.666)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101830</td><td></td><td>4.069</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.678)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101830</td><td></td><td>-18.761</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.115)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101830</td><td></td><td>4.804<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(1.936)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101830</td><td></td><td>4.123<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.880)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101840</td><td></td><td>4.643<sup>*</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.702)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101840</td><td></td><td>3.015</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.598)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101840</td><td></td><td>-17.568</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.117)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101840</td><td></td><td>5.947<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(1.947)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101840</td><td></td><td>4.247<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.887)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101850</td><td></td><td>3.107</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.751)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101850</td><td></td><td>0.416</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.650)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101850</td><td></td><td>-17.089</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.121)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101850</td><td></td><td>7.430<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(1.980)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101850</td><td></td><td>3.632<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.917)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101860</td><td></td><td>2.325</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.734)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101860</td><td></td><td>-0.068</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.719)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101860</td><td></td><td>-19.141</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.128)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101860</td><td></td><td>5.919<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.053)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101860</td><td></td><td>1.255</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.945)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101870</td><td></td><td>3.056</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.721)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101870</td><td></td><td>1.597</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.433)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101870</td><td></td><td>-19.955</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.138)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101870</td><td></td><td>4.207<sup>**</sup></td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.146)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101870</td><td></td><td>1.081</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.977)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101880</td><td></td><td>-0.751</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.695)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countrySouth Africa:birth101880</td><td></td><td>2.256</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.452)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101880</td><td></td><td>-21.551</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.135)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryNew Zealand:birth101880</td><td></td><td>1.331</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.327)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101880</td><td></td><td>-1.557</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(0.993)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryCanada:birth101890</td><td></td><td>-0.077</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(2.770)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryAustralia:birth101890</td><td></td><td>-21.021</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(17.159)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">death_countryUnited States of America:birth101890</td><td></td><td>-0.426</td></tr>
    <tr><td style="text-align:left"></td><td></td><td>(1.123)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td style="text-align:left">Constant</td><td>66.093<sup>***</sup></td><td>66.097<sup>***</sup></td></tr>
    <tr><td style="text-align:left"></td><td>(0.494)</td><td>(0.494)</td></tr>
    <tr><td style="text-align:left"></td><td></td><td></td></tr>
    <tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Sibling random effects</td><td>Yes</td><td>Yes</td></tr>
    <tr><td style="text-align:left">Observations</td><td>138,496</td><td>138,496</td></tr>
    <tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
    </table>

``` r
#write(tbl, file = "tables/Results_table.html", sep = ",")
```
