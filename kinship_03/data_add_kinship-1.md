Add Kinship - 03 - 1
================

## Create nuclear family ties variables

Logic - if parent A is my parent and parent A is the parent of child B,
then child B is my sibling

#### Assumptions – heterosexual parents, minimal remarriage (re-partnering)

variables needed: PID of mother, PID of father - create: \# of siblings,
birth order - variables can be gendered

``` r
library(data.table)
library(dplyr)
library(tidyr)

"%ni%" <- Negate("%in%") # define function

# load parent-child relationship data
links <- fread(file = params$path_links) ## N = 51,807,143

# load genealogical data
profiles <- fread(file = params$path_data, na.strings='', encoding="UTF-8")
```

``` r
# create simplified dataset
profs <- profiles %>% # cut sample size
         select(profileid, gender, age) %>%
         rename(child = profileid)  %>%  # to ease merge
         distinct(child, .keep_all = T)

# Link parent and child ad parent number
joined <- links[profs, on = "child"][, pn := as.integer(row.names(.SD)), by = child]
joined[, pn := as.factor(pn)]
```

``` r
wide <- dcast(joined, child + gender + age ~ pn, value.var = "parent")
setnames(wide, c("child", "gender", "1", "2"), c("ch.id", "ch.gen", "p1.id", "p2.id"))

# make dfs only consisting of ch.id & ch.gen for parent 1 & parent 2
ch.p1 <- wide %>%
  select(ch.id, ch.gen) %>%
  rename(p1.id = ch.id, p1.gen = ch.gen)

ch.p2 <- wide %>%
  select(ch.id, ch.gen) %>%
  rename(p2.id = ch.id, p2.gen = ch.gen)

# join above dfs with wide df to make full df
p1 <- left_join(wide, ch.p1,by=c('p1.id'))
p2 <- left_join(p1, ch.p2, by=c('p2.id'))

# impute missing parent genders assuming heterosexuality
p2.1 <- p2 %>%
  mutate(
    p1.gen = ifelse(is.na(p1.gen)
                    & p2.gen == "male",
                    "female", p1.gen), # fill in female if partner is male
    p1.gen = ifelse(is.na(p1.gen)
                    & p2.gen == "female",
                    "male", p1.gen) # fill in male if partner is female
    ) %>% 
  mutate(
    p2.gen = ifelse(is.na(p2.gen)
                    & p1.gen == "male"
                    & !is.na(p2.id),
                    "female", p2.gen), # fill in female if partner is male
    p2.gen = ifelse(is.na(p2.gen)
                    & p1.gen == "female"
                    & !is.na(p2.id),
                    "male", p2.gen) # fill in male if partner is female
    ) 

# just to speed things up, drop observations with missing gender for child & both parents
p2.2 <- p2.1 %>%
  filter(
    !is.na(ch.gen) | !is.na(p1.gen) | !is.na(p2.gen)
    ) # drops observations

options(dplyr.summarise.inform = F) # to avoid keeping the grouping var 'ch.id'

par1g <- p2.2 %>%
  ungroup() %>%
  select(p1.id, p1.gen) %>%
  rename(ch.id = p1.id, ch.gen = p1.gen) %>%
  filter(!is.na(ch.gen)) %>%
  add_count(across(everything())) %>%
  mutate(dup = n > 1) %>%
  filter(dup == F) %>%
  select(-dup, -n) %>%
  arrange(ch.id)

par2g <- p2.2 %>%
  ungroup() %>%
  select(p2.id, p2.gen) %>%
  rename(ch.id = p2.id, ch.gen = p2.gen) %>%
  filter(!is.na(ch.gen)) %>%
  add_count(across(everything())) %>%
  mutate(dup = n > 1) %>%
  filter(dup == F) %>%
  select(-dup, -n) %>%
  arrange(ch.id)

# fill in child-level gender info using gender of parent
# plan: make parent 1 & parent 2 dfs & merge back to child column
p1g <- merge(p2.2, par1g, by = 'ch.id', all.x = T) %>%
  arrange(ch.gen.x)

p1g <- p1g %>%
  mutate(
    ch.gen.x = ifelse(is.na(ch.gen.x) & !(is.na(ch.gen.y)), ch.gen.y, ch.gen.x)
    ) %>%
  select(-ch.gen.y) %>%
  rename(ch.gen = ch.gen.x)

parents <- merge(p1g, par2g, by = 'ch.id', all.x = T) %>% 
  arrange(ch.gen.x)

parents <- parents %>%
  mutate(
    ch.gen.x = ifelse(is.na(ch.gen.x) & !(is.na(ch.gen.y)), ch.gen.y, ch.gen.x)
    ) %>%
  select(-ch.gen.y) %>%
  rename(ch.gen = ch.gen.x)
```

## Plan of attack:

    (1) gender parent variables
    (2) format data to be list of lists
    (3) create sibling ties based on >= 1 shared parent ties
    (4) create grandparent & first cousin ties
    (5) create great-grandparent ties

## STEP 1 - GENDER PARENT VARIABLES

``` r
parents <- parents %>%
  mutate(mom = ifelse(p1.gen == "female", p1.id, NA)) %>% # mom is either p1 or p2
  mutate(mom = ifelse(p2.gen == "female", p2.id, mom)) %>%
  mutate(pop = ifelse(p1.gen == "male", p1.id, NA )) %>% # pop is either p1 or p2
  mutate(pop = ifelse(p2.gen == "male", p2.id, pop)) %>%
  mutate(mom = ifelse(p1.gen == p2.gen, p1.id, mom)) %>% # same-gen couples
  mutate(pop = ifelse(p1.gen == p2.gen, p2.id, pop)) %>%
  mutate(mom = ifelse(is.na(p1.gen) & is.na(p2.gen), p1.id, mom)) %>% # unpartnered people
  mutate(pop = ifelse(is.na(p1.gen) & is.na(p2.gen), p2.id, pop)) %>%
  mutate(mom = ifelse(is.na(mom) & p1.gen == "female" & is.na(p2.gen), p1.id, mom)) %>% # not sure why these
  mutate(pop = ifelse(is.na(pop) & p1.gen == "male" & is.na(p2.gen), p1.id, pop)) %>% # didn't work originally
  mutate(mom = ifelse(is.na(mom) & p2.gen == "female" & is.na(p1.gen), p2.id, mom)) %>%
  mutate(pop = ifelse(is.na(pop) & p2.gen == "male" & is.na(p1.gen), p2.id, pop))

parents <- parents %>%
  select(ch.id, ch.gen, mom, pop) %>%
  filter(!is.na(mom) | !is.na(pop))
```

## STEP 2 - DATA FORMATTING

``` r
kidsOf <- with(parents, { # think this means, within df parents, 
  c(tapply(ch.id, mom, c), # create a list of each parent's total children's ids
    tapply(ch.id, pop, c))
  })

kidsOf["0"] <- NULL

KidsOf <- list() # create empty list
KidsOf[as.numeric(names(kidsOf))] <- kidsOf # fill in K with k, 
                                            # creates a line for all IDs 
# define function ko
ko <- function(KidsOf = KidsOf, p) {
  lapply(p, function(x) {
    unique(as.vector(unlist(KidsOf[x]))) # if you're the child of both mom & pop you won't be double counted
  })
} # should give all cases where someone is a mom or pop or both

# create direct ties up to great-grandparents
parents$FM <- parents$mom[match(parents$pop, parents$ch.id)] # father's mother (paternal gma)
parents$MM <- parents$mom[match(parents$mom, parents$ch.id)] # mother's mother (maternal gma)
parents$MF <- parents$pop[match(parents$mom, parents$ch.id)] # mother's father (maternal gpa)
parents$FF <- parents$pop[match(parents$pop, parents$ch.id)] # father's father (paternal gpa)
parents$FFM <- parents$FM[match(parents$pop, parents$ch.id)] # father's father's mother
parents$FMM <- parents$MM[match(parents$pop, parents$ch.id)] # ...etc.
parents$FMF <- parents$MF[match(parents$pop, parents$ch.id)]
parents$FFF <- parents$FF[match(parents$pop, parents$ch.id)]
parents$MFM <- parents$FM[match(parents$mom, parents$ch.id)]
parents$MMM <- parents$MM[match(parents$mom, parents$ch.id)]
parents$MMF <- parents$MF[match(parents$mom, parents$ch.id)]
parents$MFF <- parents$FF[match(parents$mom, parents$ch.id)]

res <- list() # create empty list 'res'
ego <- as.list(parents$ch.id)
res$ego <- ego

# own children
res$children <- ko(KidsOf = KidsOf, p = ego)
```

## STEP 3 - SIBLING TIES

``` r
# put parents into list form
res$parents <- lapply(ego, function(ego) {
  as.vector(unlist(parents[parents$ch.id == ego, 
                           c("mom", "pop")]))
  })

res$siblings <- ko(KidsOf = KidsOf, p = res$parents)
res$siblings <- lapply(seq_along(res$siblings), 
                       function(i) res$siblings[[i]][res$siblings[[i]] %ni% ego[[i]]])
```

## STEP 4 - GRANDPARENT AND FIRST COUSIN TIES

``` r
# grandparents first
res$gparents <- lapply(ego, function(ego) { 
  as.vector(unlist(parents[parents$ch.id == ego, 
                           c("MM", "MF", "FM", "FF")]))
  })

# need to get uncles and aunts along the way
g2 <- ko(KidsOf = KidsOf, p = res$gparents)
res$unclesaunts <- lapply(seq_along(g2), 
                          function(i) g2[[i]][g2[[i]] %ni% res$parents[[i]]])
# first cousins
res$firstcousins <- ko(KidsOf = KidsOf, p = res$unclesaunts)
```

## STEP 5 GREAT-GRANDPARENT TIES

``` r
# great-grandparents
res$ggparents <- lapply(ego, function(ego) { 
  as.vector(unlist(parents[parents$ch.id == ego, 
                           c("MMM", "MMF", "MFM", "MFF", "FMM", "FMF", "FFM", "FFF")]))
})
```

### Convert list to data.table

``` r
res.df <- as.data.table(do.call(cbind, res))
res.df <- res.df %>%
  mutate(
    across(c(children, gparents, firstcousins, unclesaunts),
              ~ ifelse(. == "NULL", NA, .))
    ) 
res.df$ego1 <- as.integer(res.df$ego)
```

## Sibling Ties

``` r
# break u p siblings vectors into individual cells
# NOTES: egos with 0 siblings are removed in the unnest, null is not a list

# replace zero values with NA - to keep 0 siblings
res.df$siblings[!(sapply(res.df$siblings, length))] <- 0
res.df$unclesaunts[!(sapply(res.df$unclesaunts, length))] <- 0

siblings <- res.df %>% 
  unnest(siblings) %>%
  group_by(ego1) %>%
  mutate(key = row_number()) %>%
  mutate(key = if_else(siblings == 0 , 0, key)) %>%
  spread(key, siblings)

# pivot wide to long
sib.long <- siblings %>%
  pivot_longer(
    names_to = c("sibling"),
    cols = c("0", "1", "2", "3", "4", "5", "6", "7","8", "9", "10", "11", "12")
    ) %>% 
  rename(sib.num = sibling, sibling = value) %>%
  select(-sib.num) %>%
  filter(!is.na(sibling))

sib.long <- setDT(sib.long)
```

### Number of siblings

``` r
sib.num <- sib.long %>%
  group_by(ego1) %>%
  mutate(sib.ct = n()) %>%
  mutate(sib.ct = if_else(sibling == 0 , 0, sib.ct)) %>%
  select(-sibling) %>%
  ungroup() %>%
  distinct(.keep_all = T)

sib.num$ego1 <- as.integer(sib.num$ego)
sib.num$sib.ct1 <- as.integer(sib.num$sib.ct)

sib.num <- sib.num %>%
  select(ego1, sib.ct1) %>%
  rename(ego = ego1, sib.ct = sib.ct1)
```

## trying to extract firstborns

join ego birth year and sibling birth year to every observation check
that every sibling is younger than the ego (using min command) a way to
do birth order? doing it the proper way 122 born in 1950

``` r
df <- profiles %>% select(profileid, birth_year)

# fixing variable types 
sib.long$ego <- as.numeric(sib.long$ego)
sib.long$sibling <-  as.numeric(sib.long$sibling)

# merging birth date to sibling info 
sib.long2 <- left_join(sib.long, df, by = c('ego' = 'profileid'))
sib.long2 <- sib.long2 %>% rename(ego_birth_year = birth_year)

sib.long2 <- left_join(sib.long2, df, by = c('sibling' = 'profileid'))
sib.long2 <- sib.long2 %>% rename(sibling_birth_year = birth_year)

# generating birth order 
sib.birth.order <- sib.long2 %>%
  group_by(ego) %>% 
  mutate(all_birth_years = list(c(ego_birth_year[1], sibling_birth_year))) %>% 
  unnest(all_birth_years) %>% 
  mutate(birth_order = dense_rank(all_birth_years)) %>% 
  filter(all_birth_years == ego_birth_year[1]) %>% 
  select(ego, sibling, ego_birth_year, sibling_birth_year, birth_order)

# combining with number of siblings 
sib.birth.order <- right_join(sib.birth.order, sib.num, by = 'ego') %>% 
  select(ego, birth_order, sib.ct) %>% 
  distinct(ego, .keep_all = TRUE)
```

## Write files to CSV

For next step - 03 - Data final var

``` r
# EXPORT Parents DATA AS CSV
fwrite(parents, paste0(params$save_path, "ggparents.csv"))

# EXPORT Sibling Ties DATA AS CSV
fwrite(sib.long, paste0(params$save_path, "sibling_ties.csv"))

# WRITE Number of Siblings TO CSV
fwrite(sib.num, paste0(params$save_path, "number_siblings.csv"))

# save Birth Order as csv
fwrite(sib.birth.order, paste0(params$save_path, "birth_order.csv"))
```
