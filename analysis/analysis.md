Analysis - Main Results
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
library(ggpubr)

data <- fread(params$path_data)
```

## Format variables

``` r
data <- data %>% 
  mutate(
    migrant = factor(migrant, levels = c(0, 1),
                     labels = c("nonmigrant", "migrant")),
    death_country = factor(death_country, levels = c("UK/Ireland", "Canada",
                                                     "South Africa", 
                                                     "Australia", "New Zealand",
                                                     "United States of America")),
    birth10 = as.factor(birth10),
    death10 = as.factor(death10),
    gender = as.factor(gender),
    sib_size_cat = as.factor(sib_size_cat),
    famid = as.numeric(famid),
    firstborn = factor(firstborn, levels = c('missing', '0', '1'))
    )

# Make df where everyone has a parent in the data
data_par_complete <- data[!is.na(famid)]
```

## Migrant Model - A1

``` r
# Model A1 - migrant

a1 <- lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
            data = data_par_complete) 
```

### Plot Figure 1

``` r
birth_mm <- margins(a1, variables = "migrant", at = list(birth10 = 
                                                            c("1735","1745", 
                                                              "1755", "1765",
                                                              "1775", "1785", 
                                                              "1795", "1805", 
                                                              "1815", "1825", 
                                                              "1835", "1845", 
                                                              "1855", "1865", 
                                                              "1875", "1885")))
birth_ame <- summary(birth_mm) # AME = 5.93, p < 0.001

figure1 <- birth_ame %>% 
  ggplot(aes(
    x = birth10,
    y = AME
  )) + 
  geom_point() +
  geom_errorbar(aes(
    ymin = lower,
    ymax = upper),
    width = 0.15) + 
  xlab("Birth Cohort") + 
  ylab("Average Marginal Effect") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=35, margin = margin(t = 2, unit = "mm"))) + 
  scale_x_discrete(labels=c("1730", "1740", "1750", "1760", "1770", "1780",
                            "1790","1800", "1810", "1820", "1830", "1840", 
                            "1850", "1860", "1870", "1880", "1890")) +
  scale_y_continuous(breaks = seq(1, 9, by = 1), limits = c(1.1,9.4))

ggsave(
  plot = figure1,
  filename = paste0(params$save_path, "figure1.pdf"),
  width = 150,
  height = 100,
  units = "mm"
  )

ggsave(
  plot = figure1,
  filename = paste0(params$save_path, "figure1.tiff"),
  width = 150,
  height = 100,
  units = "mm",
  compression = "lzw"
)
```

## Death country Model - B1

``` r
# Model b1 - destination country
b1 <- lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
         data = data_par_complete)
```

    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients

``` r
b1_mm <- margins(b1, variables = "death_country")
b1_ame <- summary(b1_mm)

b1_ame <- b1_ame %>% 
  mutate(factor = ifelse(factor == "death_countryAustralia", "Australia", factor),
         factor = ifelse(factor == "death_countrySouth Africa", "South Africa", factor),
         factor = ifelse(factor == "death_countryCanada", "Canada", factor),
         factor = ifelse(factor == "death_countryNew Zealand", "New Zealand", factor),
         factor = ifelse(factor == "death_countryUnited States of America", "USA", factor)) 
```

### Plot Figure 2

``` r
x2 <- data.frame(factor = 
                 c(" ", "Australia", "Canada", "New Zealand", 
                   "South Africa", "USA", "UU" ), 
                 AME = rep(5.93, 7), gp = rep(1, 7))

figure2 <- ggplot(x2, aes(x=factor, y=AME, group=gp)) + 
  geom_line(colour="#116656", linetype="dashed") +
  geom_ribbon(aes(ymin = 5.68, ymax = 6.18), fill = "#116656", alpha = 0.3) +
  geom_point(data = b1_ame, aes(x=factor, y=AME, group=factor)) + 
  geom_errorbar(data = b1_ame, aes(
    ymin = lower,
    ymax = upper, group=factor), 
    width = 0.06) +
  coord_cartesian(xlim=c(2, 6)) +
  labs(x = "Destination Country", 
       y = "Average Marginal Effect") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(1, 11, by = 1), limits = c(1.1,11.2))
 
ggsave(
  plot = figure2,
  filename = paste0(params$save_path, "figure2.pdf"),
  width = 150,
  height = 150,
  units = "mm"
)

ggsave(
  plot = figure2,
  filename = paste0(params$save_path, "figure2.tiff"),
  width = 150,
  height = 150,
  units = "mm",
  compression = "lzw"
)
```

``` r
# Cut ages

x <- seq(0, 50, 5)
j = 1


results <- vector("list", length(x)*2)
margins <- vector("list", length(x)*2)

for (i in x) {
  
  ## Cut data
  
  data_par_complete_mod <- data_par_complete[deathage >= i]

  # Model a1 - migrant
  results[[j]] <- lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
              data = data_par_complete_mod) 
  
  margins[[j]] <- margins(results[[j]], variables = "migrant")
  
  j = j+1
  
  # Model b1 - destination country
  results[[j]] <- lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
              data = data_par_complete_mod)
  
  margins[[j]] <- margins(results[[j]], variables = "death_country")
  
  j = j+1
  
}
```

    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients
    fixed-effect model matrix is rank deficient so dropping 3 columns / coefficients

### Plot Figure 3

``` r
# Model a1 - migrant

mig_mar <- margins[c(1,3,5,7,9,11,13,15,17,19,21)]

mig_mar_av <- list()

for (k in 1:length(mig_mar)) {
  mig_mar_av[[k]] <- summary(mig_mar[[k]])
}

# unlist and rename factor into dataframe

mig_mar_av <- do.call(rbind.data.frame, mig_mar_av)

mig_mar_av <- cbind(mig_mar_av, x)

figure3 <- mig_mar_av %>% 
  ggplot(aes(
    x = x,
    y = AME
  )) +
  geom_point() +
  geom_errorbar(aes(
    ymin = lower,
    ymax = upper), 
    width = 0.5) + 
  ylim(c(2,6.5)) +
  xlab("Death Cut-Off age") + 
  ylab("Average Marginal Effect") + 
  theme_bw()+
  scale_y_continuous(breaks = seq(2, 7, by = 1))
```

    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.

``` r
ggsave(
  plot = figure3,
  filename = paste0(params$save_path, "figure4.pdf"),
  width = 150,
  height = 100,
  units = "mm"
)

ggsave(
  plot = figure3,
  filename = paste0(params$save_path, "figure3.tiff"),
  width = 150,
  height = 100,
  units = "mm",
  compression = "lzw"
)
```

### Plot Figure 4

``` r
# Model b1 - destination country

death_mar <- margins[c(2,4,6,8,10,12,14,16,18,20,22)]

death_mar_av <- list()

for (k in 1:length(death_mar)) {
  death_mar_av[[k]] <- summary(death_mar[[k]])
}

# unlist and renamefactor into dataframe

death_mar_av <- do.call(rbind.data.frame, death_mar_av)

y <- rep(seq(0,50,5), each = 5)

death_mar_av <- cbind(death_mar_av, y)

death_mar_av$factor <- gsub("death_country", "", death_mar_av$factor)

figure4 <- death_mar_av %>% 
  ggplot(aes(
    x = y,
    y = AME
  )) +
  geom_point() +
  geom_errorbar(aes(
    ymin = lower,
    ymax = upper), 
    width = 1) + 
  xlab("Death Cut-Off age") + 
  ylab("Average Marginal Effect") + 
  theme_bw()+
  facet_wrap(vars(factor))

ggsave(
  plot = figure4,
  filename = paste0(params$save_path, "figure4.pdf"),
  width = 200,
  height = 150,
  units = "mm"
)

ggsave(
  plot = figure4,
  filename = paste0(params$save_path, "figure4.tiff"),
  width = 200,
  height = 150,
  units = "mm",
  compression = "lzw"
)
```
