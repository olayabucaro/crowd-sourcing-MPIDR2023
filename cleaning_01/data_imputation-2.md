Data Imputation 01 - 2
================

- Impute birth/death days
- Impute country of birth/death
- Reduce columns

### Setup

``` r
library(data.table)
library(dplyr)
library(countrycode)

# Load prepared data file (moddata1)
data <- fread(file = params$path_data, na.strings = "", encoding = "UTF-8")
```

## 1: Date Imputations

First: Remove “abnormal” birth years - Some of these were probably BCE
years (were negative) but others were probably recording/scanning errors
since they are very far in the future

``` r
abnormalYears <- data %>% 
  filter(
    (birth_year <= 0| death_year <= 0) | (is.na(birth_year) & (baptism_year <=0 | baptism_year > 2020))| 
    (birth_year >2020| death_year > 2020 ) | (is.na(death_year) & (burial_year <= 0 | burial_year > 2020))
  )

dataReduced <- data %>% anti_join(abnormalYears, by=c('profileid'))
```

Next: Impute birth/death dates from baptism/burial years respectively

``` r
dataReduced <- dataReduced %>% 
  mutate(birth_year = ifelse(is.na(birth_year), baptism_year, birth_year)) %>% 
  mutate(death_year = ifelse(is.na(death_year), burial_year, death_year))
```

## 2: Imputing country of birth/death directly from other columns

First: Impute country of birth/death from the “coordinates column
determined from lat/long

``` r
coordImpute <- dataReduced %>% 
  mutate(
    birth_location_country = ifelse(is.na(birth_location_country),
                                  birth_coord_country, birth_location_country)
    ) %>%                                              
  mutate(death_location_country = ifelse(is.na(death_location_country),
                                       death_coord_country, death_location_country)
    )
```

Next: Impute country code information for countries that are still
missing

``` r
countCodeImpute <- coordImpute %>% 
  mutate(
    birth_location_country = ifelse(is.na(birth_location_country), 
                                    birth_location_country_code, 
                                    birth_location_country)
    ) %>% 
  mutate(
    death_location_country=ifelse(is.na(death_location_country), 
                                       death_location_country_code,
                                       death_location_country)
    )
```

Next: Impute with baptism/burial location (people are baptized/buried in
the same place they’re born/died)

``` r
baptismBurialImpute <- countCodeImpute %>% 
  mutate(
    birth_location_country=ifelse(is.na(birth_location_country), 
                                       baptism_location_country,
                                       birth_location_country)
    ) %>% 
  mutate(
    death_location_country=ifelse(is.na(death_location_country), 
                                       burial_location_country, 
                                       death_location_country)
    )
```

Finally: Impute with baptism/burial country codes

``` r
baptismBurialCCImpute <- baptismBurialImpute %>% 
  mutate(
    birth_location_country=ifelse(is.na(birth_location_country), 
                                  baptism_location_country_code,
                                  birth_location_country)
    ) %>% 
  mutate(
    death_location_country=ifelse(is.na(death_location_country),
                                  burial_location_country_code,
                                  death_location_country)
    )
```

Drop some columns

``` r
data <- baptismBurialCCImpute %>% 
  select(
    "profileid", 'gender', 'current_residence_location_country', 
    'birth_location_country', "death_location_country","birth_year", "death_year",
    ends_with("city"), ends_with("state"), ends_with("latitude"),
    ends_with("longitude"), ends_with("name")
    )
```

## 3: Impute country (USA) from state names in the state column

Use *countrycode* package to detect US states in the state column for
birth/death –\> born/died in the US

``` r
# Set up state matching
cd <- get_dictionary("us_states")
usStates <- cd$state.name #use this later to match US states to mutate to US
```

Use *regex* and the *countrycode* package to detect state names and
abbreviations

``` r
# match by state name
data <- data %>% 
  mutate(
    birth_location_country=ifelse((is.na(birth_location_country) & 
                                   !is.na(birth_location_state)),
                                  countrycode(data$birth_location_state,
                                              "state.regex", "state.name",
                                              custom_dict = cd), 
                                       birth_location_country)
    ) %>% 
   mutate(
     death_location_country=ifelse((is.na(death_location_country) &
                                      !is.na(death_location_state)), 
                                       countrycode(data$death_location_state,
                                                   "state.regex", "state.name",
                                                   custom_dict = cd), 
                                       death_location_country)
     )

# match by state abbreviations 
data <- data %>% 
  mutate(
    birth_location_country=ifelse((is.na(birth_location_country) & 
                                     !is.na(birth_location_state)), 
                                       countrycode(data$birth_location_state,
                                                   "state.abb", "state.name",
                                                   custom_dict = cd), 
                                       birth_location_country)
    ) %>% 
  mutate(
    death_location_country=ifelse((is.na(death_location_country) &
                                     !is.na(death_location_state)), 
                                       countrycode(data$death_location_state,
                                                   "state.abb", "state.name",
                                                   custom_dict = cd), 
                                       death_location_country)
    )
```

## 4: Imputing from the free text column

Extract entries with free text information but no birth/death location
to reduce amount of code to run Regex–match with state names

``` r
freeTextBirth <- data %>%
  filter(!is.na(birth_location_place_name) & is.na(birth_location_country))

freeTextDeath <- data %>%
  filter(!is.na(death_location_place_name) & is.na(death_location_country))

freeTextBirth <- freeTextBirth %>% 
  mutate(
    birth_location_country=countrycode(birth_location_place_name, "state.regex",
                                       "state.name", custom_dict = cd)
    )

freeTextDeath <- freeTextDeath %>% 
  mutate(
    death_location_country=countrycode(death_location_place_name, "state.regex",
                                       "state.name", custom_dict = cd)
    )
```

Finally: merge all state information back into the main table and
replace state names with “United States of America”

``` r
# merge state data back into full dataframe 
data <- rows_patch(data, freeTextBirth, by='profileid')
data <- rows_patch(data, freeTextDeath, by="profileid")

# replace state names with "United States of America"

data <- data %>% 
  mutate(
    birth_location_country = case_when(birth_location_country %in% 
                                       usStates ~ "United States of America",
                                       TRUE ~ birth_location_country)
    ) %>% 
  mutate(
    death_location_country = case_when(death_location_country %in%
                                       usStates ~ "United States of America",
                                       TRUE ~ death_location_country)
    )
```

## 5: Countries from the free text column

First: Isolate entries that still have no country information

``` r
# Birth location
freeTextBirthCountry <- data %>% filter(!is.na(birth_location_place_name) & is.na(birth_location_country))

# Death location
freeTextDeathCountry <- data %>% filter(!is.na(death_location_place_name) & is.na(death_location_country))
```

Detect country names in birth/death free text columns and replace
country columns with detected country names

``` r
freeTextBirthCountry <- freeTextBirthCountry %>% 
  mutate(
    birth_location_country=countrycode(birth_location_place_name,
                                       "country.name.en.regex", "country.name")
    )

freeTextDeathCountry <- freeTextDeathCountry %>% 
  mutate(
    death_location_country=countrycode(death_location_place_name,
                                       "country.name.en.regex", "country.name")
    )

#join back into the main table
data <- rows_patch(data, freeTextBirthCountry, by='profileid')
data <- rows_patch(data, freeTextDeathCountry, by="profileid")
```

Repeat with baptism/burial locations

``` r
freeTextBaptismCountry <- data %>% 
  filter(
    !is.na(data$baptism_location_place_name) & is.na(data$birth_location_country)
    ) %>% 
  mutate(
    birth_location_country=countrycode(baptism_location_place_name,
                                       "country.name.en.regex", "country.name")
    )
```

    Warning: There were 2 warnings in `mutate()`.
    The first warning was:
    i In argument: `birth_location_country = countrycode(...)`.
    Caused by warning:
    ! Some values were not matched unambiguously: 's-Grevelduin, 's Grevelduin-Capelle, --------------, ""Baptisms at Tappan-Cole"", ""Biggarsberg"", ""in utero, out of necessity"", ""na voorafgedane belijdenis"", ""Nancy Blaney"", ""the Hope"" John Bailies Farm, , R.s.a., ( tvilling m/Juliana ), (16. May 1623 a. St.), (2. April 1620 a. St.) (2. Sonntag vor Ostern), (81 Jahre), (af grekisk troslära), (Hirtsholmene), (ogsa° kaldet: Bonders - Bondes), (Opél Ch folio OA p 75), (Opel. Ch.: v.2, p.158, (Opel. Ch.: v.2, p.188, (Opel. Ch.: v.2, p.238, (Opel. Ch.: v.2, p.238), (Opel. Ch.: v.2, p.71), (Opel. Ch.: v.3, p.128), (Pfarrhaus), .GRS 3.03 CD 120 VA Mar.Pittsilvania, VA., ?, ? Vik Kirke, Gaular Parish, Norway, ??, ?? Far:Claus Jensen, \tBinley, Warwickshire, England, \tCrail, Fifeshire, Scotland, \tEccleston, Cheshire, England, \tLittle Baddow, Essex, England, \tMickleham,<U+200B> Surrey,<U+200B> England, \tSaint Margaret, Lee, Kent, England, \tSaint Mary, Hinckley, Leicester, England, \tSpofforth, Yorkshire, England, \tSt. Margaret's, Westminster, Middlesex, England, \tStapleford, Cambridge, England, ____, _____, ______, ’s-Grevelduin-Capelle, €rnomelj, <U+008A>entjur, <U+008A>martno pod <U+008A>marno goro, <U+008A>martno pri Litiji, <U+0412><U+044B><U+043F><U+043E><U+043B><U+0437><U+043E><U+0432><U+043E>, <U+041E><U+0441><U+0442><U+0440><U+043E><U+0432> <U+041B><U+0443><U+043D><U+043A><U+0443><U+043B><U+0430><U+043D><U+0441><U+0430><U+0440><U+0438>, 01/10/1769, 02/10/1803, 02/12/1650, 04 OCT 1767, 04/08/1776, 05/02/1792, 05/11/1771, 06/08/1804, 06/11/1765, 07/06/1857, 08/02/1801, 1.aastane, 1/2 a°r gammel, 10 Feb. 1621/2 Rowley, Yorks,, 10. sønd e trinf, 10/05/1801, 11/15/1741, 11/18/1866, 12 JUN 1825, 12/11/1859, 13.05.1810, 13.07.1817, 13/01/1811, 16 p. Trinit 1752, Fillan, 1683 den 25 dito (April) Judith; Hendrick Everts Smith: en Arriantie Sterrevelt; Jan Vlock en Maria Jansen. NGK Kaapstad., 1689 Den 25 dito (October) Aen Stellenbosch een kindt ghedoopt waer van vader is Lou[i..] Cordier de moeder Fransina Martinjet, als ghetuijge stonden Jacobus de Savoije, ende Maria le Clerq ende ghenaemt Jacobus. Stellenbosch., 1694 12 dito (September) een kind van Gerrit Meijer en Susanna Costers, genaemt Hendrik, getuijgen Jan Coetzë en Hildegonda Boon. NGK Kaapstad., 17/02/1773, 17/12/1820, 1705 8de Febrij Estienne. de Vader Estienne Niel, de Moeder Marie Marais: Getùijgen Charles Marais, en Anne de Rùel. Drakenstein., 1708 18e do [9br] van Johannes Verbeek en Catharina Lubeck, de getuijge Jan Mettrez van 's Heerenrein, en Martha Emanuelsz. Johannes. NGK Kaapstad., 1721 19. 8br. Lacija Ouders: barent buijs, en Alida van den berg Getuigen: Abraham Peltzer, en Elizabeth van den berg. NGK Kaapstad., 1722; Den 3de Jann:- Elisabeth, Doghter van Pieter Jourdaan e Maria Verdaù. Getuijgen Pieter De Villers en Elisabeth Jùbert. Drakenstein., 1724 den 20ste Febr. Gabriel, zoon van Gabriel Russouw en Anna Marais. Getuijgen Daniel Hugot en Anna Druelle. Drakenstein., 1741 Januarij 22 Davidt de Vader, Daniel Malaan de Moeder, Maria Verdoun getuijgen, Davidt Malaan & Leonora Milius. NGK Stellenbosch., 1774 den 10 April Christoffel de Vader Coenraad Joh:s Groenewald Christoffz de Moeder Susanna Elisabeth Wesselsz: de getuijgen Christoffel Groenewald Coenraadz en Elisabeth Minnaar. Stellenbosch, 1776. Jacobus: De Vader is Jacobus Van Zijl De Moeder Jannetje Labescanje Get: zijn .. Den 5 Julij. NGK Cape Town, 18/04/1779, 18/12/1746, 19.aastane, 19.trinitatis, 1st Church, 1st Church, Middletown, Middlesex Co., Connecticut, 1st Presbyterian Church, St. Mary's, OH, 2/4/1877, 2/8/1848, 20st Maart [1707]. Susanne. d' Vader Jacques del Porte, d' Moeder Sara Vitout. Getuijgen Estienne Cronje. Drakenstein, 21 Sep 1798, 21/03/1753, 24 Maij [1699] van Peter Christiaense en Hermina Carels, onder getuijge van Willem Widerholt en Henderijna gent. Carel. NGK Kaapstad., 24/12/1846, 25.7.1762, 25.Desember, 25.sønd. etter Trin., 25/08/1834, 26.05.1805, 28/07/1776, 28/09/1740, 2d church Beverly, 2d church, Beverly, 2d Church, Beverly, 2nd Church of Hartford, 2nd Reformed Church, Berne, 3/20/1642, 3/21/1842, 3/3/1723, 30 Jun 1663, 31.aastane, 3rd Feb 1793, 4/30/1884, 47.aastane (nr 64), 5 apr. 1766, 5/18/1852, 5/26/1854, 6. april i Blenstrup Kirke, 61. aastane, 6th son of Richard and Ann. Father Painter., 7/18/1879, 7/19/1874, 7/19/1974, 8. Grange Fold, Allerton, 9/16/1692 Sarah Mabie and Cornelius Bogert were the witnesses, 96 Colvestone Crescent, Hackney, Å, A-dos-Cunhados, à l'église francaise, A Richard Cawse was baptized in Modbury in 1745?, a twin, A: 1667 May 9 een soontie van Geertruyt Ment weduwe van Wilhem Lodowijck Wiederholt genaemt Wilhem Lodowijck, peeters waren Pieter van Klinckenbergh en Maria Prigon weduwe van Do' Wachtendorp. NGK Kaapstad., A°kerby bruk, O¨sterlo¨vstad (Up), Sverige, A°kerby vretarna, O¨sterbybruk, C, Sverige, A°rnes kirke, Aabenraa, Aaby, Aalbæk, Råbjerg Sogn, Aalsmeer, Aalsø kirke, Aalum, Aalum (opsl. 10), Aalum (opsl. 102+103), Aalum (opsl. 112), Aalum (opsl. 12), Aalum (opsl. 121), Aalum (opsl. 130) ind. 12. august, Aalum (opsl. 15), Aalum (opsl. 153), Aalum (opsl. 173), Aalum (opsl. 196), Aalum (opsl. 220), Aalum (opsl. 26), Aalum (opsl. 263), Aalum (opsl. 28), Aalum (opsl. 31), Aalum (opsl. 40), Aalum (opsl. 44), Aalum (opsl. 45), Aalum (opsl. 51), Aalum (opsl. 53), Aalum (opsl. 55), Aalum (opsl. 56), Aalum (opsl. 57), Aalum (opsl. 60), Aalum (opsl. 61), Aalum (opsl. 65), Aalum (opsl. 68), Aalum (opsl. 69), Aalum (opsl. 74), Aalum (opsl. 76), Aalum (opsl. 80), Aalum (opsl. 85), Aalum (opsl. 88), Aalum (opsl. 93), Aalum hjemmedøbt 8. november, Aare K, Aarre, Aars, Aars (opsl. 13), Aars (opsl. 18), Aars (opsl. 30), aars (opsl. 34), Aars (opsl. 40), Aars (opsl. 43), Aars (opsl. 46), Aars (opsl. 67), Aars (opsl.18), Aars kirke og sogn, Aars, Aalborg, Aarsrudeie, Ringsaker Prestegjeld, Hedmark, Aarstad kirke, Aas kirke, Aas kirke, Østre Toten Kommune, Oppland, Aas kirke, Vestre Toten, Oppland, Aas Kirke, Vestre Toten, Oppland, Aaserud - Strandens i Vinger, Aasted, Aasted kirke, Aasted Kirke, Aastrup, Falster sdr. hrd, Aastrup, Ribe,, Abandames, Abbe´ville Les Conflans (Meurthe et Moselle), Abbington presbyterian Church, Abbington presbyterian Church, , , Pennsylvania, Abbotsbury, ABD Zakroczym, Abenhall, Abingdon, Berks, Abinghall, Gloucestershire, Acadie, Acton Vale, Qc, Adderbury Church, Oxfordshire, England, Addington, Adraku, Adslev, Adum kirke, Ådum kirke, Ådum Kirke, Ådum sogn, adwalton yorkshire, Ærøskøbing, Afferden, Affpuddel, England, age 14 mths - Opel Ch., Age 5 months Opel ch v.1, p.541, Age in 1871: 47, Agerskov Sogn, Aggersborg, Ägläjärvi, Agri Kirke, Agri Kirke, Mols Herred, Agri Sogn, Mols Herred, Agua de Pau, Aidt, Aistrup, Ajstrup Kirke, Aker, Aker Kirke, Aker, Akershus, Norge, Akersloot, Äksi, Alanta, Albæk, Albæk Kirke, ALBÆK s. SÆBY, Alber,,,,,, Albjerg Mark, Albøge, Albøge kirke, Albøge Kirke, Ålborg Budolfi, Albrighton, Salop., Aldbourne, Aldbrough, Aldeneik, Alderminster, Worcestershire, Ålesund, Alexanderwohl Church,Elder Peter Wedel, Alfen, Alida, Geary Co., KS, Alkemade, All Hallows Church, All Hallows Church, Anne Arundel Co, MD, All Saints, all saints aston yorkshire, all saints aston yorkshire., All Saints Church Hindmarsh, S A, All Saints Church Northampton, All Saints Habergham, Lancashire, England, All Saints Ncle IGI, All Saints(1822), All Saints(2010), All Saints(2011), All Saints(611), All Saints(747), All Saints, Cockermouth, Cumberland, England - Same day and place as Peter - late baptism?  Parents listed as John and Deborah, All Saints, Daresbury, All Saints, Newcastle(2111), All Saints, Poplar, All Saints, Poplar, Middlesex, All Saints,,,,,, Allafer, Allegheny Church, Berks, Pennsylvania, Allegheny Reformed Church, Berks, Pennsylvania, Allehelgensdag I  Thorum  Kirke, Allerslev Kirke, Allika talu, Allinge kirke, Allinge Kirke, Almkerk, Als, Als (opsl. 144), Als (opsl. 154), Als (opsl. 155), Als (opsl. 160), Als (opsl. 162), Als (opsl. 163), Als (opsl. 170), Als (opsl. 37), Als (opsl. 44), Als (opsl. 50), Als (opsl. 85), Als (opsl. 95), Als (opsl. 97), Als (opsl.58), Alslev Kirke, Ålsø, Also ""PELLOW"".,
    i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
freeTextBurialCountry <- data %>%
  filter(
    !is.na(data$burial_location_place_name) & is.na(data$death_location_country)
    ) %>% 
  mutate(
    death_location_country=countrycode(burial_location_place_name,
                                       "country.name.en.regex", "country.name")
    )
```

    Warning: There were 2 warnings in `mutate()`.
    The first warning was:
    i In argument: `death_location_country = countrycode(...)`.
    Caused by warning:
    ! Some values were not matched unambiguously: 'Grote Kerk', Catharinakerk, 's Gravenzande, 's Hr Arendskerke, 't Kalf, 't Veld, -, -----------B E W A R E N----------------, -----, -----, --, - ?, - BUCHENWALD, - Nigel, S.A., -Hickman Cem., Stone Co. MS, "", ""<U+05DB><U+05E4><U+05E8> <U+05E1><U+05DE><U+05D9><U+05E8>"", ""Babe"", ""Burying Ground"",(Whitaker), ""Clermont"", Fauquier Co., Va, ""Cloverland"", Pr Wm Co, Va, ""Dipo"", ""Duinhof"" Slingerduinlaan IJmuiden, ""Duken"", ""Eikenhof"" Marquettelaan Heemskerk, ""Ellerslie,"" in Autauga (now Elmore) County, Alabama, ""Erasmuskraal"", ""Fate"", ""Fleetwood,"" Culpeper County, Virginia, ""Gansfontein"", ""Grannys Cemetery"", ""Green Castle"" New Division, Antiqua, West Indies, ""Greenfield"", Rappahannock, Va, ""Greenway"", ""Hamptonsville"", Caroline Co., VA, ""high Church"" Cemetery, ""in linen"" at Rivenhall, ""Kinloch"", Fauquier Co, Va, ""Klippoort"", ""Knuts"", ""linen"" at Rivenhall, ""Lizzie"", ""Meadow Farm"", Pr Wm Co, Va, ""Mt Alta,"" Augusta Co., Va., ""Old Burnt Mill"", ""Old White Church"" on the Santee Wateree River, SC. of Beaufort, SC, ""Portici"", Pr Wm Co, Va, ""Rheeboksfontein"", ""Rose Cottage"" Highway 60,Dillywn, ""Stone Church"", ""Sudley House"", Fairfax Co, Va, ""Taaiboschfontein"", ""The Shelter"", Pr Wm Co, Va, ""Traquare"", ""Wale Weestraat"" (Eerste Weteringdwarsstraat) Nederland, ""Warwickton"", ""Weltevreden"", ""Went into the woods after leatherwood and never returned"", ""woollen"" at Rivenhall, ""woollen"" at Rivenhall in ""Western Vault, #101 Horton Cemetery, Wayne Co., TN, #106 Cromwell Crossroads Cemetery, Wayne Co., TN, #113 Mt. Hope Cemetery, Wayne Co., TN, #171 Horton Cemetery, Wayne Co., TN, #61, #95 Horton Cemetery, Wayne Co., TN, #98 Sims Cemetery, Wayne Co., TN, $1Västra Nyängsstugan,Tunabergs församling ( efter giftermålet) ??$2 Västra Nyängsstugan, Tunaberg (D) (after marriage ??), ( du 3<U+02BB> rang), (? deported 2 Oct 1942), (?)Albany,Berks Co.,PA, (?F), Ljungstorp, (24. Maerz 1615 a. St.), (2nd wife), (3. Januar 1641 a. St.), (44 Jahre), (5)PAUL LEONARD, (53 Jahre), (60a), (67 Jahre), (Angermünde), (Concentration Camp), (DISGEN-ort saknas), (Doubtful) Blenheim Farm, Albemarle Co., VA, (early 1945), (england), (F), Ka¨rda sn, (F), Rydaholms sn, (Haws Mansion) The Highlands, Perth / Angus Border, Scotland, (hillrich Masonic Cemetery) La, Union, Or, (I), (Kant St 105 a, Grab R II), (killed in car accident), (Kirk) Welpington, Northumberland, England, (Leonhardt 17, vgl Kartei; 3, 25, 653), (Lower Tean) Checkley, Stafford, England, (M)Anna Slafter His Dau Irene(M)James, (Maeser Fairview, Uintah, Ut, (MI) Catheral, Lichfield, Hampshire, England, (nach 1682), (nach 1757), (ob 17 Edward 3), (on farm) Rock Crk.,Ashe (Alleghany),NC, (P), Nol?, (pool tuhastatud põrmust), (Prob.) Lee,VA, (Prob.) So Africa, (Prob.) TX, (Reyburn Cemetery), Luzerne, PA, (Same as Husband), (See ""More"" info.), (T), Skagershult, (tuhastatud), (verschwunden),,,,,, (vor  Okt ober 1772), (vor 1650), (vor 1659), ****My 4th Great Grand Uncle*****, *with Father/step-mother+twin sisters, ,,,,Deutschland, ., ...of Sowe, England, /, /Ellecom, : Dido Cemetery Dido Tarrant County Texas, : Pleasant Grove Cemetery #02 Boyd (Wise County) Wise County Texas,, : Stone Cemetery Diamond Jasper County Missouri, ?, ? Amerika, ? Asak Kirke Cemetery, Berg Parish, ? Cem., Burlington, Des Moines Co., Ia., ? Cem., Drumright, Creek Co., Ok., ? Cem., Groton, Brown Co., S. D., ? Cem., Stark Co., Oh., ? Europe, ? Ft. Ancient Cemetery, Warren Co. Ohio, ? Misri Gunjlocation, ? need clarification from John Hodgson of Beaumont, Table of Descents, ? Rakkestad Kirke Cemetery, ? Traill County, ND, ?, Marion, GA, ?, Muscogee, GA, ?, NC, ?, SC, ?, Scotland, ??, ???, ????, ?????, ??ÙjÙGÙjÙjÙGÙjÙLÙjÙjy¨???, [Edgar County, Illinois], [RK] Begraafplaats Sint Adelbertus parochie Egmond-Binnen, NH, [See Notes], \\,,,,,, \n\nCranston\nRI\n\n\n, \n\nDeep River\nWA, \n\nMarlin\nOKLAHOMA\n\n\n, \nBranchport, NY, \nChittenden, VT, \nLuxembourg,, Germany, \nPownal, VT, \t Hanwell, Oxfordshire, England, \t Long Branch Cemetary, Anderson Co, SC, \t Nelson Family Cemetery, Northampton Co., NC, \t Svendstrupgård i Købelev, \tAlta Vista Cemetery, Georgia, USA, \tAmes Chapel Cemetery, Indiana, USA, \tBalquhidder Church Cemetery, Stirling, Scotland, \tBay Springs Cemetery, \tCalvary Cemetery And Mausoleum, \tCulford, Suffolk, England, \tEastside Cemetery, Georgia, USA, \tEden Cemetery, Indiana, USA, \tEden Mennonite Church Cemetery, Schwenksville, Montgomery, Pa, \tFirst Green Plain Friends Burial Ground, \tFirst Presbyterian Churchyard, New Jersey, USA, \tHeilman Lutheran Cemetery, Kittanning Twp., Armstrong Co., Pennsylvania, \tHeilman Lutheran Church Cemetery, Kittanning Twp., Armstrong Co., Pennsylvania, \tHendrix Cemetery, Georgia, USA, \tHillview Cemetery, Georgia, USA, \tKensico, NY, \tLangar, Nottinghamshire, England, \tLiiva kalmistu, Tallinn, \tOld Booker Cemetary across Rd from Lone Star Church 4 miles North of Pine Orchard, Al Zip 36401, \tOsula kalmistu, Sõmerpalu vald, Võrumaa, \tPleasant Grove Cemetery, Indiana, USA, \tPresbyterian Church Cemetery In West,Middlesex, , London, England, \tSaksamaa, Mecklenburg, \tSomers Family Cemetery, New Jersey, USA, \tSt. Anna’s Cemetery, St. Anna, Calument, WI, \tSt. Anna’s Cemetery, St. Anna, Calumet, WI, \tSt. Mary’s Cemetery, Sheboygan Falls, Sheboygan, WI, ____, _____, Maui Co., HI, __________?, ¯ygarden, “Grootkuil”, dist. Koonstad, “Sewefontein” voorheen “Quispberg”, “Vegleegte”, dist. Victoria-Wes, +++, +dsted Kirke, +land Kirke, +mot kapell, +rbµk kirke, +rslev k., +rum, +rum Kirke, +stbirk, +stbirk Kirke, +strup Kirke, <larledge@gnfa.com>, <U+008A>kofja Loka, <U+008A>KOFJA LOKA, <U+008A>martno, <U+008A>martno pod <U+008A>marno goro, <U+0395><U+03BB><U+03BB><U+03AC><U+03C2>, <U+0411><U+0435><U+0436><U+0438><U+0446><U+0430>, <U+0411><U+0435><U+043B><U+0433><U+043E><U+0440><U+043E><U+0434><U+043A><U+0430>, <U+0411><U+0435><U+043B><U+043E><U+0440><U+0443><U+0441><U+0441><U+043A><U+0438><U+0439> <U+043F><U+043E><U+0441><U+0435><U+043B><U+043E><U+043A>, <U+0411><U+0435><U+0440><U+043A><U+043E><U+0432><U+0446><U+044B>, <U+0411><U+043E><U+043B><U+044C><U+0448><U+0435><U+043E><U+0445><U+0442><U+0438><U+043D><U+0441><U+043A><U+043E><U+0435> <U+043A><U+043B><U+0430><U+0434><U+0431>., <U+0412><U+0430><U+0433><U+0430><U+043D><U+044C><U+043A><U+043E><U+0432><U+0441><U+043A><U+043E><U+0435> <U+043A><U+043B><U+0430><U+0434><U+0431>., <U+0412><U+0430><U+0440><U+043D><U+0430>, <U+0412><U+043E><U+043B><U+043A><U+043E><U+0432><U+0441><U+043A><U+043E><U+0435> <U+043A><U+043B><U+0430><U+0434><U+0431>., <U+0412><U+043E><U+043B><U+043A><U+043E><U+0432><U+0441><U+043A><U+043E><U+0435> <U+043B><U+044E><U+0442><U+0435><U+0440><U+0430><U+043D><U+0441><U+043A><U+043E><U+0435> <U+043A><U+043B><U+0430><U+0434><U+0431><U+0438><U+0449><U+0435>, <U+0412><U+043E><U+0441><U+0442><U+0440><U+044F><U+043A><U+043E><U+0432><U+0441><U+043A><U+043E><U+0435> <U+043A><U+043B><U+0430><U+0434><U+0431>., <U+0418><U+0435><U+0440><U+0443><U+0441><U+0430><U+043B><U+0438><U+043C>, <U+0418><U+043D><U+043E><U+0432><U+0435><U+0440><U+0447><U+0435><U+0441><U+043A><U+043E><U+0435> <U+043A><U+043B><U+0430><U+0434><U+0431><U+0438><U+0449><U+0435> <U+043D><U+0430> <U+0412><U+0432><U+0435><U+0434><U+0435><U+043D><U+0441><U+043A><U+0438><U+0445> <U+0433><U+043E><U+0440><U+0430><U+0445>, <U+041A><U+043B><U+044E><U+0447><U+0430><U+0440><U+0435><U+0432><U+0441><U+043A><U+043E><U+0435> <U+043A><U+043B><U+0430><U+0434><U+0431><U+0438><U+0449><U+0435> <U+043A><U+0432><U+0430><U+0440><U+0442><U+0430><U+043B> 10, <U+041A><U+043E><U+043F><U+0447><U+0435><U+043B><U+0438><U+0438><U+0442><U+0435>, <U+041A><U+0440><U+0430><U+0441><U+043D><U+043E><U+0430><U+0440><U+043C><U+0435><U+0439><U+0441><U+043A>, <U+041B><U+0435><U+043D><U+0438><U+043D><U+0433><U+0440><U+0430><U+0434>, <U+041B><U+0435><U+0441><U+043D><U+043E><U+0435> <U+043A><U+043B><U+0430><U+0434><U+0431><U+0438><U+0449><U+0435>, <U+041C><U+0430><U+0440><U+0447><U+0438><U+043D><U+043E>, <U+041C><U+043E><U+0441><U+043A><U+0432><U+0430> <U+0412><U+0430><U+0433><U+0430><U+043D><U
    i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
data <- data %>% 
  rows_patch(freeTextBaptismCountry, by='profileid') %>% 
  rows_patch(freeTextBurialCountry, by="profileid")
```

Drop extra columns

``` r
data <- data %>% 
  select(
    -ends_with("longitude"),-ends_with("latitude"), -ends_with("name"),
    -ends_with("state"), -starts_with("current"), -ends_with("city")
    )
```

## Write to csv

For next step - Filter - Data UK/IE Birth Filter

``` r
fwrite(data, params$save_path)
```
