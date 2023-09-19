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
# Make df where everyone has a parent in the data
data_par_complete <- data %>% 
  filter(miss.famid == F) %>% 
  select(-miss.famid)

summary(data_par_complete$sib_size_cat)
```

          0       1       2     3-5      6+ missing 
      48794   14375   10002   17446    7440       0 

``` r
# Make df where only people with siblings are in data
data_only_siblings <- data_par_complete %>% 
  filter(sib.ct > 0)
```

## Family effects models

``` r
# Model 1 - migrant
fe1 <- lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
            data = data_par_complete) 

summary(fe1)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ migrant * birth10 + gender + sib_size_cat + (1 | famid)
       Data: data_par_complete

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
fe1.mm <- margins(fe1, variables = "migrant")
print(fe1.mm)
```

    Average marginal effects

     migrantmigrant
              5.934

``` r
p <- summary(fe1.mm) # AME = 5.93, p < 0.001
p
```

             factor    AME     SE       z      p  lower  upper
     migrantmigrant 5.9335 0.1259 47.1451 0.0000 5.6868 6.1802

``` r
print(signif(mean(fe1.mm$dydx_migrantmigrant)), 4) # same as above
```

    [1] 5.934

``` r
#plot(fe1.mm, xlab = "migrant")


plot.p <- p %>% 
  ggplot(aes(
    x = factor,
    y = AME
  )) + 
  geom_point() +
  geom_errorbar(aes(
    ymin = lower,
    ymax = upper),
    width = 0.15) + 
  xlab("Migrant") + 
  ylab("Average Marginal Effect") + 
  ylim(c(5.5,6.5)) +
  theme_bw() + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

# save graph
ggsave(
  plot = plot.p,
  filename = "./analysis_output/fe1_base.png",
  width = 8.5,
  height = 5,
  units = "in"
)
```

``` r
fe1.sibs <- lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
         data = data_only_siblings) 
summary(fe1.sibs)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ migrant * birth10 + gender + sib_size_cat + (1 | famid)
       Data: data_only_siblings

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
fe1.sibs.mm <- margins(fe1.sibs, variables = "migrant")
p.sibs <- summary(fe1.sibs.mm) # AME = 5.9335, p < 0.001
p.sibs
```

             factor    AME     SE       z      p  lower  upper
     migrantmigrant 7.1471 0.2066 34.6002 0.0000 6.7423 7.5520

``` r
plot.p.sibs <- p.sibs %>% 
  ggplot(aes(
    x = factor,
    y = AME
  )) + 
  geom_point() +
  geom_errorbar(aes(
    ymin = lower,
    ymax = upper),
    width = 0.15) + 
  xlab("Migrant") + 
  ylab("Average Marginal Effect") + 
  ylim(c(5.5,6.5)) +
  theme_bw() + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

# save graph
ggsave(
  plot = plot.p.sibs,
  filename = "./analysis_output/fe1_sibs.png",
  width = 8.5,
  height = 5,
  units = "in"
)
```

    Warning: Removed 1 rows containing missing values (`geom_point()`).

## Death country

``` r
# Model 2 - destination country
fe2 <- lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
         data = data_par_complete)
```

    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients

``` r
summary(fe2)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ death_country * birth10 + gender + sib_size_cat +  
        (1 | famid)
       Data: data_par_complete

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
fe2.mm <- margins(fe2, variables = "death_country")
q <- summary(fe2.mm)
q
```

                                    factor    AME     SE       z      p  lower
                    death_countryAustralia 5.4902 0.3846 14.2737 0.0000 4.7363
                       death_countryCanada 7.6079 0.2828 26.9049 0.0000 7.0537
                  death_countryNew Zealand 8.7105 1.2454  6.9944 0.0000 6.2696
                 death_countrySouth Africa 2.5506 0.7314  3.4871 0.0005 1.1170
     death_countryUnited States of America 5.6918 0.1594 35.7123 0.0000 5.3794
       upper
      6.2441
      8.1621
     11.1513
      3.9842
      6.0041

``` r
q <- q %>% 
  mutate(factor = ifelse(factor == "death_countryAustralia", "Australia", factor),
         factor = ifelse(factor == "death_countrySouth Africa", "South Africa", factor),
         factor = ifelse(factor == "death_countryCanada", "Canada", factor),
         factor = ifelse(factor == "death_countryNew Zealand", "New Zealand", factor),
         factor = ifelse(factor == "death_countryUnited States of America", "USA", factor)) 

plot.q <- q %>% 
  ggplot(aes(
    x = factor,
    y = AME
  )) +
  geom_point() +
  geom_errorbar(aes(
    ymin = lower,
    ymax = upper), 
    width = 0.25) + 
  ylim(c(1,11.5)) +
  xlab("Destination Country") + 
  ylab("Average Marginal Effect") + 
  theme_bw()

# save graph
ggsave(
  plot = plot.q,
  filename = "./analysis_output/fe2_base.png",
  width = 8.5,
  height = 5,
  units = "in"
)
```

``` r
fe2.sibs <- lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
         data = data_only_siblings) 
```

    fixed-effect model matrix is rank deficient so dropping 6 columns / coefficients

``` r
summary(fe2.sibs)
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: deathage ~ death_country * birth10 + gender + sib_size_cat +  
        (1 | famid)
       Data: data_only_siblings

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
fe2.sibs.mm <- margins(fe2.sibs, variables = "death_country")
q.sibs <- summary(fe2.sibs.mm) # AME = , p < 0.001
q.sibs
```

                                    factor    AME     SE       z      p  lower
                    death_countryAustralia 7.6977 0.6518 11.8090 0.0000 6.4201
                       death_countryCanada 8.7746 0.5022 17.4705 0.0000 7.7902
                  death_countryNew Zealand 9.7821 0.8687 11.2612 0.0000 8.0796
                 death_countrySouth Africa 3.1939 1.1005  2.9022 0.0037 1.0370
     death_countryUnited States of America 6.7653 0.2724 24.8355 0.0000 6.2314
       upper
      8.9753
      9.7589
     11.4846
      5.3509
      7.2992

``` r
q.sibs <- q.sibs %>% 
  mutate(factor = ifelse(factor == "death_countryAustralia", "Australia", factor),
         factor = ifelse(factor == "death_countrySouth Africa", "South Africa", factor),
         factor = ifelse(factor == "death_countryCanada", "Canada", factor),
         factor = ifelse(factor == "death_countryNew Zealand", "New Zealand", factor),
         factor = ifelse(factor == "death_countryUnited States of America", "USA", factor)) 

plot.q.sibs <- q.sibs %>% 
  ggplot(aes(
    x = factor,
    y = AME
  )) +
  geom_point() +
  geom_errorbar(aes(
    ymin = lower,
    ymax = upper), 
    width = 0.25) + 
  ylim(c(1,11.5)) +
  xlab("Destination Country") + 
  ylab("Average Marginal Effect") + 
  theme_bw()

# save graph
ggsave(
  plot = plot.q.sibs,
  filename = "./analysis_output/fe2_base.png",
  width = 8.5,
  height = 5,
  units = "in"
)
```

``` r
cov.labs <- c("Migrant", "Canada", "South Africa", "Australia", 
              "New Zealand", "United States of America")

tbl <- stargazer(fe1, fe2,
                 type = "text",
                 column.labels = c("Migrant", "Country of Death"),
                 covariate.labels = cov.labs,
                 dep.var.caption = "Dependent variable: Age at Death",
                 dep.var.labels = "",
                 keep.stat = c("n"),
                 add.lines=list(c("Sibling random effects", "Yes", "Yes"))
)
```


    ===================================================================================
                                                      Dependent variable: Age at Death 
                                                      ---------------------------------
                                                                                       
                                                          Migrant      Country of Death
                                                            (1)              (2)       
    -----------------------------------------------------------------------------------
    Migrant                                               3.350***                     
                                                          (1.058)                      
                                                                                       
    Canada                                                                  1.242      
                                                                           (4.637)     
                                                                                       
    South Africa                                                            1.992      
                                                                           (2.716)     
                                                                                       
    Australia                                                               23.950     
                                                                           (17.595)    
                                                                                       
    New Zealand                                                            4.728**     
                                                                           (2.276)     
                                                                                       
    United States of America                                               3.377***    
                                                                           (1.071)     
                                                                                       
    birth101740                                            -0.743           -0.744     
                                                          (0.718)          (0.718)     
                                                                                       
    birth101750                                            -0.744           -0.744     
                                                          (0.710)          (0.709)     
                                                                                       
    birth101760                                            0.957            0.956      
                                                          (0.698)          (0.697)     
                                                                                       
    birth101770                                           2.068***         2.067***    
                                                          (0.680)          (0.679)     
                                                                                       
    birth101780                                           1.413**          1.411**     
                                                          (0.669)          (0.668)     
                                                                                       
    birth101790                                            0.042            0.039      
                                                          (0.660)          (0.659)     
                                                                                       
    birth101800                                            -0.667           -0.666     
                                                          (0.655)          (0.654)     
                                                                                       
    birth101810                                           -1.556**         -1.560**    
                                                          (0.654)          (0.653)     
                                                                                       
    birth101820                                          -2.908***        -2.914***    
                                                          (0.650)          (0.649)     
                                                                                       
    birth101830                                          -4.918***        -4.919***    
                                                          (0.653)          (0.653)     
                                                                                       
    birth101840                                          -5.655***        -5.657***    
                                                          (0.656)          (0.656)     
                                                                                       
    birth101850                                          -5.831***        -5.832***    
                                                          (0.660)          (0.660)     
                                                                                       
    birth101860                                          -4.315***        -4.320***    
                                                          (0.665)          (0.665)     
                                                                                       
    birth101870                                          -1.971***        -1.974***    
                                                          (0.668)          (0.667)     
                                                                                       
    birth101880                                           1.414**          1.414**     
                                                          (0.675)          (0.674)     
                                                                                       
    birth101890                                           3.220***         3.219***    
                                                          (0.733)          (0.732)     
                                                                                       
    gendermale                                             0.0003           0.0002     
                                                          (0.117)          (0.117)     
                                                                                       
    sib_size_cat1                                        -1.860***        -1.861***    
                                                          (0.177)          (0.177)     
                                                                                       
    sib_size_cat2                                        -2.927***        -2.926***    
                                                          (0.213)          (0.213)     
                                                                                       
    sib_size_cat3-5                                      -2.894***        -2.889***    
                                                          (0.179)          (0.179)     
                                                                                       
    sib_size_cat6+                                       -2.468***        -2.439***    
                                                          (0.281)          (0.281)     
                                                                                       
    migrantmigrant:birth101740                             0.995                       
                                                          (1.286)                      
                                                                                       
    migrantmigrant:birth101750                             0.233                       
                                                          (1.266)                      
                                                                                       
    migrantmigrant:birth101760                             -0.058                      
                                                          (1.278)                      
                                                                                       
    migrantmigrant:birth101770                             -0.154                      
                                                          (1.258)                      
                                                                                       
    migrantmigrant:birth101780                             0.356                       
                                                          (1.214)                      
                                                                                       
    migrantmigrant:birth101790                             2.281*                      
                                                          (1.168)                      
                                                                                       
    migrantmigrant:birth101800                             2.201*                      
                                                          (1.143)                      
                                                                                       
    migrantmigrant:birth101810                            2.426**                      
                                                          (1.126)                      
                                                                                       
    migrantmigrant:birth101820                            3.442***                     
                                                          (1.119)                      
                                                                                       
    migrantmigrant:birth101830                            4.733***                     
                                                          (1.119)                      
                                                                                       
    migrantmigrant:birth101840                            5.172***                     
                                                          (1.127)                      
                                                                                       
    migrantmigrant:birth101850                            5.014***                     
                                                          (1.144)                      
                                                                                       
    migrantmigrant:birth101860                            3.095***                     
                                                          (1.165)                      
                                                                                       
    migrantmigrant:birth101870                            2.511**                      
                                                          (1.180)                      
                                                                                       
    migrantmigrant:birth101880                             -0.062                      
                                                          (1.193)                      
                                                                                       
    migrantmigrant:birth101890                             0.034                       
                                                          (1.304)                      
                                                                                       
    death_countryCanada:birth101740                                         6.812      
                                                                           (5.071)     
                                                                                       
    death_countryAustralia:birth101740                                     -27.468     
                                                                           (18.693)    
                                                                                       
    death_countryNew Zealand:birth101740                                   -19.768     
                                                                           (12.822)    
                                                                                       
    death_countryUnited States of America:birth101740                       0.717      
                                                                           (1.314)     
                                                                                       
    death_countryCanada:birth101750                                         3.462      
                                                                           (4.989)     
                                                                                       
    death_countrySouth Africa:birth101750                                   -9.603     
                                                                           (12.907)    
                                                                                       
    death_countryAustralia:birth101750                                     -20.242     
                                                                           (17.823)    
                                                                                       
    death_countryNew Zealand:birth101750                                    21.868     
                                                                           (17.844)    
                                                                                       
    death_countryUnited States of America:birth101750                       0.051      
                                                                           (1.300)     
                                                                                       
    death_countryCanada:birth101760                                         4.672      
                                                                           (4.918)     
                                                                                       
    death_countrySouth Africa:birth101760                                   8.050      
                                                                           (6.256)     
                                                                                       
    death_countryAustralia:birth101760                                     -20.349     
                                                                           (17.718)    
                                                                                       
    death_countryNew Zealand:birth101760                                    3.531      
                                                                           (17.984)    
                                                                                       
    death_countryUnited States of America:birth101760                       -0.785     
                                                                           (1.340)     
                                                                                       
    death_countryCanada:birth101770                                         6.418      
                                                                           (4.826)     
                                                                                       
    death_countrySouth Africa:birth101770                                   -1.007     
                                                                           (3.788)     
                                                                                       
    death_countryAustralia:birth101770                                     -20.676     
                                                                           (17.669)    
                                                                                       
    death_countryNew Zealand:birth101770                                    12.420     
                                                                           (17.983)    
                                                                                       
    death_countryUnited States of America:birth101770                       -1.663     
                                                                           (1.365)     
                                                                                       
    death_countryCanada:birth101780                                         5.180      
                                                                           (4.765)     
                                                                                       
    death_countrySouth Africa:birth101780                                   0.523      
                                                                           (3.429)     
                                                                                       
    death_countryAustralia:birth101780                                     -19.386     
                                                                           (17.642)    
                                                                                       
    death_countryNew Zealand:birth101780                                    6.123      
                                                                           (4.295)     
                                                                                       
    death_countryUnited States of America:birth101780                       -1.398     
                                                                           (1.329)     
                                                                                       
    death_countryCanada:birth101790                                         7.663      
                                                                           (4.726)     
                                                                                       
    death_countrySouth Africa:birth101790                                   0.373      
                                                                           (3.257)     
                                                                                       
    death_countryAustralia:birth101790                                     -18.345     
                                                                           (17.619)    
                                                                                       
    death_countryNew Zealand:birth101790                                    3.631      
                                                                           (3.076)     
                                                                                       
    death_countryUnited States of America:birth101790                       0.767      
                                                                           (1.267)     
                                                                                       
    death_countryCanada:birth101800                                         5.398      
                                                                           (4.713)     
                                                                                       
    death_countrySouth Africa:birth101800                                   -0.017     
                                                                           (3.117)     
                                                                                       
    death_countryAustralia:birth101800                                     -19.143     
                                                                           (17.610)    
                                                                                       
    death_countryNew Zealand:birth101800                                    2.873      
                                                                           (2.663)     
                                                                                       
    death_countryUnited States of America:birth101800                       2.302*     
                                                                           (1.224)     
                                                                                       
    death_countryCanada:birth101810                                         6.842      
                                                                           (4.701)     
                                                                                       
    death_countrySouth Africa:birth101810                                   -0.009     
                                                                           (3.025)     
                                                                                       
    death_countryAustralia:birth101810                                     -19.261     
                                                                           (17.606)    
                                                                                       
    death_countryNew Zealand:birth101810                                    3.639      
                                                                           (2.545)     
                                                                                       
    death_countryUnited States of America:birth101810                       2.309*     
                                                                           (1.183)     
                                                                                       
    death_countryCanada:birth101820                                         7.709      
                                                                           (4.701)     
                                                                                       
    death_countrySouth Africa:birth101820                                   2.711      
                                                                           (3.572)     
                                                                                       
    death_countryAustralia:birth101820                                     -18.470     
                                                                           (17.604)    
                                                                                       
    death_countryNew Zealand:birth101820                                    3.913      
                                                                           (2.494)     
                                                                                       
    death_countryUnited States of America:birth101820                      3.431***    
                                                                           (1.165)     
                                                                                       
    death_countryCanada:birth101830                                        9.520**     
                                                                           (4.723)     
                                                                                       
    death_countrySouth Africa:birth101830                                   1.410      
                                                                           (3.663)     
                                                                                       
    death_countryAustralia:birth101830                                     -17.703     
                                                                           (17.603)    
                                                                                       
    death_countryNew Zealand:birth101830                                    2.965      
                                                                           (2.429)     
                                                                                       
    death_countryUnited States of America:birth101830                      5.660***    
                                                                           (1.164)     
                                                                                       
    death_countryCanada:birth101840                                         7.929*     
                                                                           (4.752)     
                                                                                       
    death_countrySouth Africa:birth101840                                   1.880      
                                                                           (3.418)     
                                                                                       
    death_countryAustralia:birth101840                                     -16.297     
                                                                           (17.606)    
                                                                                       
    death_countryNew Zealand:birth101840                                    4.338*     
                                                                           (2.454)     
                                                                                       
    death_countryUnited States of America:birth101840                      5.563***    
                                                                           (1.171)     
                                                                                       
    death_countryCanada:birth101850                                         7.069      
                                                                           (4.785)     
                                                                                       
    death_countrySouth Africa:birth101850                                   0.987      
                                                                           (3.452)     
                                                                                       
    death_countryAustralia:birth101850                                     -15.637     
                                                                           (17.610)    
                                                                                       
    death_countryNew Zealand:birth101850                                   6.153**     
                                                                           (2.485)     
                                                                                       
    death_countryUnited States of America:birth101850                      4.700***    
                                                                           (1.202)     
                                                                                       
    death_countryCanada:birth101860                                         4.712      
                                                                           (4.774)     
                                                                                       
    death_countrySouth Africa:birth101860                                   -2.004     
                                                                           (3.473)     
                                                                                       
    death_countryAustralia:birth101860                                     -17.647     
                                                                           (17.620)    
                                                                                       
    death_countryNew Zealand:birth101860                                   5.006**     
                                                                           (2.550)     
                                                                                       
    death_countryUnited States of America:birth101860                      2.890**     
                                                                           (1.240)     
                                                                                       
    death_countryCanada:birth101870                                         6.444      
                                                                           (4.759)     
                                                                                       
    death_countrySouth Africa:birth101870                                   -1.099     
                                                                           (3.193)     
                                                                                       
    death_countryAustralia:birth101870                                     -18.831     
                                                                           (17.632)    
                                                                                       
    death_countryNew Zealand:birth101870                                    3.600      
                                                                           (2.659)     
                                                                                       
    death_countryUnited States of America:birth101870                       2.254*     
                                                                           (1.271)     
                                                                                       
    death_countryCanada:birth101880                                         2.450      
                                                                           (4.745)     
                                                                                       
    death_countrySouth Africa:birth101880                                   2.230      
                                                                           (3.247)     
                                                                                       
    death_countryAustralia:birth101880                                     -20.162     
                                                                           (17.632)    
                                                                                       
    death_countryNew Zealand:birth101880                                    0.615      
                                                                           (2.894)     
                                                                                       
    death_countryUnited States of America:birth101880                       -0.864     
                                                                           (1.299)     
                                                                                       
    death_countryCanada:birth101890                                         2.156      
                                                                           (4.831)     
                                                                                       
    death_countryAustralia:birth101890                                     -19.513     
                                                                           (17.674)    
                                                                                       
    death_countryUnited States of America:birth101890                       -0.415     
                                                                           (1.484)     
                                                                                       
    Constant                                             66.785***        66.784***    
                                                          (0.608)          (0.607)     
                                                                                       
    -----------------------------------------------------------------------------------
    Sibling random effects                                  Yes              Yes       
    Observations                                           98,057           98,057     
    ===================================================================================
    Note:                                                   *p<0.1; **p<0.05; ***p<0.01

``` r
#write(tbl, file = "./analysis_output/results_table.html", sep = ",")
```
