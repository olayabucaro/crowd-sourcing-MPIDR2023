Data UK-IE Birth Extraction
================

## Extract indivuals born in UK or Ireland

Load in packages/data

``` r
library(data.table)
library(dplyr)

data <- fread(params$path_data, na.strings='', encoding="UTF-8")
ties <- fread(params$path_links)
```

Update: took out the year restriction: just want to see what it looks
like as a whole

``` r
subsetData <- data %>% 
  filter(
    !is.na(birth_location_country), !is.na(death_location_country),
    !is.na(death_year), !is.na(birth_year)
    ) 
```

Drop birth/death columns to lowercase

``` r
subsetData$birth_location_country<- tolower(subsetData$birth_location_country)
subsetData$death_location_country <- tolower(subsetData$death_location_country)
```

### 1: Filter for the UK in the birth countries

Various names for the UK and areas in the UK

``` r
uk <- c('united kingdom', 'x-england', 'england', 'x-scotland', 'x-united-kingdom',
        'scotland', '(present uk)', 'x-great-britain', 'x-wales', 'gb',
        'x-northern-ireland', 'uk', 'northern ireland', 'england, uk', 'u.k.',
        'wales', 'england, united kingdom', 'great britain', 'england uk',
        'uk:great britain', 'uk:northern ireland', 'england (present uk)',
        'scotland, united kingdom', 'uk:northern ireland', 'uk.', 'scotland, uk',
        'uk:isle of wight', 'ireland (present northern ireland)', 'north ireland',
        'scotland, uk', 'scotland, united kingdom', 'scotland uk', 'south wales',
        'britain', 'england/ uk', 'n.ireland', 'engand', 'englnd', 'northern ireland, uk',
        'middlesex', 'n. ireland', 'huntingdonshire', 'lancashire', 'london',
        'uk:scotland:shetland islands:mainland', 'u k', 
        'united kingdom of great britain and ireland', 'northern-ireland',
        'nothern ireland, uk', 'ireland (northern)', 'n. ireland', 'ireland or scotland',
        'scotland or ireland', 'enfland', 'storbritannia', 'bonhill, dunbartonshire, scotland',
        'cambridgeshire', 'chatham', 'crickdale, wiltshire, uk', 'eicester, leicestershire, uk',
        'endgland', 'england.', 'englanmd', 'englnad', 'enland', 'essex', 'fifeshire', 'gloucester',
        'great briatin', 'great britai', 'herfordshire', 'lower bebington', 'newcastle upon tyne',
        'north wales', 'reino unido', 'royaume uni', 'scotlans', 'swindon, wiltshire, england',
        'tyrone', 'uk /england', 'uk/wales', 'uk:wales:anglesey', 'umited kingdom',
        'united kinbgdom', 'united kinbgdom', 'winwick', 'woolwich, kent, uk', 'yorkshire',
        'xengland', "yhdistynyt kuningaskunta", "(present u.k.)", "(present day united kingdom)",
        "-england", "(now united kingdom)", "present united kingdom)", "הממלכה המאוחדת",
        "aberdeen city", "englang", "england,uk", "england or pa", "wales or england",
        "verenigd koninkrijk", "unitedkingdom", "united-kingdom", "uk:scotland:barra",
        "uk, england", "u. k.", "heathfield, sussex, england")
```

Clean for the UK in the Birth/Death Columns

``` r
ukEntries <- subsetData %>% 
  mutate(
    birth_location_country = case_when(birth_location_country %in% uk  ~ "United Kingdom",
                                       TRUE ~ birth_location_country)
    ) %>%
  mutate(
    death_location_country = case_when(death_location_country %in% uk ~ "United Kingdom",
                                        TRUE ~ death_location_country)
    ) %>% 
  filter(birth_location_country == "United Kingdom") %>%
  filter(birth_year >= 1600 & death_year >= 1600)
```

Add some more observations with regex

``` r
non_ukEntries <- subsetData %>% 
  mutate(
    birth_location_country = case_when(birth_location_country %in% uk  ~ "United Kingdom",
                                       TRUE ~ birth_location_country)
    ) %>%
  mutate(
    death_location_country = case_when(death_location_country %in% uk ~ "United Kingdom",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(death_location_country = case_when(grepl("^(?!new)england|(united kingdom)|\\b(uk)\\b|(u\\.k)",
                                                  subsetData$death_location_country,
                                                  perl = TRUE) ~ "United Kingdom",
                                            TRUE ~ death_location_country)
    ) %>% 
  filter(birth_location_country != "United Kingdom") %>%
  filter(birth_year >= 1600 & death_year >= 1600)

ukEntries2 <- non_ukEntries[grepl("^(?!new)england|(united kingdom)|\\b(uk)\\b|(u\\.k)",
                                  non_ukEntries$birth_location_country, perl = TRUE)]

ukEntries2 <- ukEntries2 %>% 
  mutate(
    birth_location_country = case_when(grepl("^(?!new)england|(united kingdom)|\\b(uk)\\b|(u\\.k)",
                                             ukEntries2$birth_location_country,
                                             perl = TRUE)  ~ "United Kingdom",
                                       TRUE ~ death_location_country)
    )

ukEntries <- rbind(ukEntries, ukEntries2)
```

### 1.1: Filter for Ireland in the birth countries

``` r
ieEntries <- non_ukEntries[grepl("(ireland)|(éire)|(eire)|\\b(ie)\\b",
                                 non_ukEntries$birth_location_country)]

ieEntries <- ieEntries %>% 
  mutate(
    birth_location_country = case_when(grepl("(ireland)|(éire)|(eire)|\\b(ie)\\b",
                                             ieEntries$birth_location_country)  ~ "Ireland",
                                            TRUE ~ birth_location_country)
    ) %>%
  mutate(
    death_location_country = case_when(grepl("(ireland)|(éire)|(eire)|\\b(ie)\\b",
                                             ieEntries$death_location_country) ~ "Ireland",
                                            TRUE ~ death_location_country)
    )
```

Check most frequent death locations

``` r
ukFreqDeath <- data.table(table(ukEntries$death_location_country))
```

### 2: Clean Death Locations

Starting List: US, South Africa, Australia, Canada, NZ, IE

``` r
us <- c("us", 'usa', 'united states', 'united states of america', 'america', 
        '(present usa)', 'colonial america', 'province of new york', 'new netherland colony',
        'new england colonies', 'new england', 'present united states', 'american colonies', 
        "british america", "new netherlands", 'new york', 
        'american colonies [present united states]', 'british north america', 'u.s.a.',
        'new netherlands (usa)', 'nieuw netherlands', 'british colonies', 'nouvelle france',
        "british colonies of north america", 'american colonies (present usa)', 
        'usa:new york:long island', 'usa:49', 'usa:massachusettes:nantucket island',
        'usa:massachusettes:martha\'s vineyard', 'usa:44', 'the united states of america',
        'massachusetts colony', 'british amercia', 'now usa', 'martin county, indiana, usa',
        'richland county', 'u,s.a.', 'u.sa.', 'united sates', 'unitet states',
        'usa.', 'usa:hawaii:kauai', "ee.uu.", "(currently) united states", 
        "(present  usa)", "(present (usa)", "(present) usa", "amerikas forente stater",
        "cleveland", "estados unidos", "usa:california:santa catalina island",
        "usa (all present day)", "us virgin islands", "kittery, york, maine",
        "in what will be america")

australia <- c("australia", "au", 'australien', 'australia:tasmania',
               'new south wales', 'australia [green slopes hospital]',
               'aust', 'western australia', 'austrailia', 'australis',
               'nsw', 'port melbourne', 'portsea', 'sydney nsw', 'tasmania')

nz <- c('new zealand', 'nz', 'new zealand:north island', 'new zealand:south island',
        'new zealand.', ', new zealand', 'christchurch', 'king street, sydenham,
        christchurch, nz', 'new zeaand', 'new zealand of senile decay', 'new, zealand',
        'tennyson street, sydenham, christchurch, nz', "king street, sydenham, christchurch, nz")

canada <- c('canada', 'ca', 'kanadas', 'canada:27', 'united province of canada',
            'british north america (present canada)', 'canada:cape breton island',
            'province of canada', 'canada:11', 'canada:newfoundland', 'upper canada',
            '(present canada)', "acadie", "canad", "can", "canadá", "canada:15",
            "canada:vancouver island")

sa <- c('za', 'south africa', 'suid afrika', 'cape of good hope', 'cape colony',
        's africa', 'cape colony (south africa)', 'rep south africa', 'south  africa',
        'south afica', 'south africa.', 'union of south africa')

ireland <- c('ireland', 'ie', 'republic of ireland', 'eire', 'bydoney,tyrone ,ireland',
             'ireland, uk', 'uk (ireland)', 'ireland ???', 'ireland (eire)', 'ireland.',
             'or ireland', 'kilkenny', 'tipperary', 'waterford', "ulster", "galway",
             "down", "carlow") 
```

Bonus Wildcards: France, India, Israel

``` r
france <- c('fr', 'france', 'francia', 'frankreich') 

india <- c('india', 'in')

israel <- c('israel', 'il', "ישראל", "ישראל israel")
```

Actual Text Clean Now

``` r
ukEntries <- ukEntries %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% us ~ "United States of America",
                                       TRUE ~ death_location_country)
    )  %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% australia ~ "Australia",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% nz~ "New Zealand",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% canada ~ "Canada",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% sa ~ "South Africa",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% india ~ "India",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% israel ~ "Israel",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% france ~ "France",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% ireland ~ "Ireland",
                                       TRUE ~ death_location_country)
    )

ieEntries <- ieEntries %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% us ~ "United States of America",
                                       TRUE ~ death_location_country)
    )  %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% australia ~ "Australia",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% nz~ "New Zealand",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% canada ~ "Canada",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% sa ~ "South Africa",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% india ~ "India",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% israel ~ "Israel",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% france ~ "France",
                                       TRUE ~ death_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in% ireland ~ "Ireland",
                                       TRUE ~ death_location_country)
    )
```

Check most frequent death locations again

``` r
ukFreqDeath <- data.table(table(ukEntries$death_location_country))
```

crosstabs

``` r
ukCt <- data.table(table(ukEntries$birth_location_country, ukEntries$death_location_country)) %>% 
  filter(V1 != V2 & N > 100)
```

## Merge and do some additonal cleaning

``` r
"%ni%" <- Negate("%in%") # define function

# rbind into one big dt
fullUKIreland <- rbind(ukEntries, ieEntries)

# drop the rows where the birth is either ireland or usa????
ambiguousBirth <- c("us or ireland?", "ireland/usa", "usa or ireland?", "united states or ireland?")
cleanUKIreland <- fullUKIreland %>% filter(birth_location_country %ni% ambiguousBirth)

# Filter age
cleanUKIreland$age <- cleanUKIreland$death_year - cleanUKIreland$birth_year
cleanUKIreland <- cleanUKIreland %>%
  filter((birth_year < death_year) & (age < 110)) %>% 
  filter(
    (death_location_country=="South Africa")|(death_location_country == "United States of America") | 
    (death_location_country == "Australia") |(death_location_country == "New Zealand")|
    (death_location_country=="Canada")|(death_location_country=="Ireland")|(death_location_country=="United Kingdom")
    )

# death freq
ukIrelandFreqDeath <- data.table(table(cleanUKIreland$death_location_country))

ct <- data.table(table(cleanUKIreland$birth_location_country, cleanUKIreland$death_location_country)) %>% 
  filter(V1 != V2 & N > 30)
```

### Write data to CSV

``` r
fwrite(cleanUKIreland, params$save_path)
```
