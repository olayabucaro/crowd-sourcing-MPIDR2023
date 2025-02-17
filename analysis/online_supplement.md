Online Supplement
================

## Analysis - Online Supplement

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
```

``` r
# Make df where everyone has a parent in the data
data_par_complete <- data_par_complete <- data[!is.na(famid)]

# Make df where only people with siblings are in data
data_only_siblings <- data_par_complete %>% 
  filter(sib.ct > 0)
```

## Migrant Model - Only Individuals With Siblings

``` r
# Model A1 - migrant

a1 <- lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
            data = data_par_complete) 
```

### Plot Figure B1

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
birth_ame <- summary(birth_mm)

a2 <- lmer(deathage ~ migrant * birth10 + gender + sib_size_cat + (1|famid),
         data = data_only_siblings) 

birth_sibs_mm <- margins(a2, variables = "migrant", at = list(birth10 = 
                                                            c("1735","1745", 
                                                              "1755", "1765",
                                                              "1775", "1785", 
                                                              "1795", "1805", 
                                                              "1815", "1825", 
                                                              "1835", "1845", 
                                                              "1855", "1865", 
                                                              "1875", "1885")))
birth_sibs_ame <- summary(birth_sibs_mm)

figureb1 <- birth_ame %>% 
  ggplot(aes(
    x = birth10,
    y = AME
  )) + 
  geom_point(color = "grey") +
  geom_errorbar(color = "grey", aes(
    ymin = lower,
    ymax = upper),
    width = 0.15) + 
  xlab("Birth Cohort") + 
  ylab("Average Marginal Effect") + 
  theme_bw() + 
  geom_point(data = birth_sibs_ame)+
  geom_errorbar(data = birth_sibs_ame, aes(
    ymin = lower,
    ymax = upper), 
    width = 0.15) +
  theme(axis.text.x = element_text(angle=35, margin = margin(t = 2, unit = "mm"))) + 
  scale_x_discrete(labels=c("1735", "1745", "1755", "1765", "1775", "1785",
                            "1795", "1805", "1815", "1825", "1835", "1845", 
                            "1855", "1865", "1875", "1885")) +
  scale_y_continuous(breaks = seq(-1, 12, by = 1), limits = c(-1.5,11.5))


ggsave(
  plot = figureb1,
  filename = paste0(params$save_path, "figureb1.pdf"),
  width = 200,
  height = 150,
  units = "mm"
  )

ggsave(
  plot = figureb1,
  filename = paste0(params$save_path, "figureb1.tiff"),
  width = 200,
  height = 150,
  units = "mm",
  compression = "lzw"
)
```

## Death country Model - Only Individuals With Siblings

``` r
# Model b2 - destination country
b2 <- lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
         data = data_only_siblings)
```

    fixed-effect model matrix is rank deficient so dropping 5 columns / coefficients

``` r
b2_mm <- margins(b2, variables = "death_country")
b2_ame <- summary(b2_mm)

b2_ame <- b2_ame %>% 
  mutate(factor = ifelse(factor == "death_countryAustralia", "Australia", factor),
         factor = ifelse(factor == "death_countrySouth Africa", "South Africa", factor),
         factor = ifelse(factor == "death_countryCanada", "Canada", factor),
         factor = ifelse(factor == "death_countryNew Zealand", "New Zealand", factor),
         factor = ifelse(factor == "death_countryUnited States of America", "USA", factor)) 
```

### Plot Figure B2

``` r
birth_sibs_mm_mig <- margins(a2, variables = "migrant")
birth_sibs_ame_mig <- summary(birth_sibs_mm_mig)

a2_mig <- data.frame(factor = 
                 c("  ", "Australia", "Canada", "New Zealand", 
                   "South Africa", "USA", "UU" ), 
                 AME = rep(birth_sibs_ame_mig$AME, 7), gp = rep(1, 7))

figureb2 <- ggplot(a2_mig, aes(x=factor, y=AME, group=gp)) + 
  geom_line(colour="#116656", linetype="dashed") +
  geom_ribbon(aes(ymin = birth_sibs_ame_mig$lower, ymax = birth_sibs_ame_mig$upper),
              fill = "#116656", alpha = 0.3) +
  geom_point(data = b2_ame, aes(x=factor, y=AME, group=factor)) + 
  geom_errorbar(data = b2_ame, aes(
    ymin = lower,
    ymax = upper, group=factor), 
    width = 0.06) +
  coord_cartesian(xlim=c(2, 6)) +
  labs(x = "Destination Country", 
       y = "Average Marginal Effect") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(1, 11, by = 1), limits = c(1.1,11.5))
 
ggsave(
  plot = figureb2,
  filename = paste0(params$save_path, "figureb2.pdf"),
  width = 150,
  height = 150,
  units = "mm"
)

ggsave(
  plot = figureb2,
  filename = paste0(params$save_path, "figureb2.tiff"),
  width = 150,
  height = 150,
  units = "mm",
  compression = "lzw"
)
```

``` r
# Model b1 - destination country
b1 <- lmer(deathage ~ death_country * birth10 + gender + sib_size_cat + (1|famid),
         data = data_par_complete)

cov.labs <- c("Migrant", "Canada", "South Africa", "Australia", 
              "New Zealand", "United States of America")

tblb1 <- stargazer(a1, b1,
                 type = "html",
                 column.labels = c("Migrant", "Country of Death"),
                 covariate.labels = cov.labs,
                 dep.var.caption = "Dependent variable: Age at Death",
                 dep.var.labels = "",
                 keep.stat = c("n"),
                 add.lines=list(c("Sibling random effects", "Yes", "Yes")),
                 out = paste0(params$save_path, "tableb1.html")
)
```

``` r
tblb2 <- stargazer(a2, b2,
                 type = "html",
                 column.labels = c("Migrant", "Country of Death"),
                 covariate.labels = cov.labs,
                 dep.var.caption = "Dependent variable: Age at Death",
                 dep.var.labels = "",
                 keep.stat = c("n"),
                 add.lines=list(c("Sibling random effects", "Yes", "Yes")),
                 out = paste0(params$save_path, "tableb2.html")
)
```
