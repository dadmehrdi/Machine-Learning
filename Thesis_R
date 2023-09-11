# Load required packages using p_load function
pacman::p_load(ggplot2, ipumsr, dplyr, MASS, plotly, ggeasy, stargazer, usethis, tictoc, pryr, smoothie, Gini, dineq, sjlabelled, expss, readxl, broom, GGally, coefplot, dotwhisker, labelled, broom.helpers, ivreg, arrow, fields, labelled, olsrr, dgumbel, gridExtra, ggstance, gumbel, tidyverse, data.table, lubridate, purrr, stringr, ggplot2, tidyr, dplyr, readr, plotly, broom, gridExtra, ggpubr, forcats, knitr, RColorBrewer, rmarkdown, shiny, shinydashboard, devtools, testthat, stringi, scales, magrittr, ggthemes, broom.helpers, patchwork, cowplot, reshape2, ggsci, tibble, dtplyr, ggrepel, pander, ggpmisc, foreach, pheatmap, kableExtra, leaflet, ggmap, rvest, jsonlite, XML, ggforce, glue, hrbrthemes, flexdashboard, lattice, nycflights13, igraph, DoseFinding, effectsize, epiR, graphlayouts, Guerry, lava, maxLik, plotrix, projpred, psychotools, RNetCDF, ROCR, rpact, sn, coronavirus, rmarkdown, tidyverse, ggthemes, plotly, xfun, magick, jsonlite, magrittr, tidycensus, rvest, httr, tidygraph, igraph, ggraph, gapminder, leaflet, gssr, gtrendsR, ggplot2, PWFSLSmoke, AirSensor, AirMonitorPlots, MazamaSpatialUtils, lubridate, VennDiagram, janitor, skimr, DataExplorer, stringr, nycflights13, pandoc)


setwd("/Users/dadmehr/R")
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("usa_00036.xml")
main_data <- read_ipums_micro(ddi)

m_main_data <- subset(main_data, INCTOT !=9999999 & MARST %in% c(1,2) & MARRINYR != 2 & YEAR==2021)

jm_main_data <- subset(main_data, INCTOT !=9999999 & MARRINYR == 2 & YEAR==2021)
co_main_data <- subset(main_data, INCTOT !=9999999 & COUPLETYPE == 3 & MARST == 6 & YEAR==2021)

co_2021 <- subset(main_data, INCTOT !=9999999 & YEAR==2021 & COUPLETYPE == 3 & MARST == 6)


main_data <- main_data %>% mutate(mom = case_when(MOMLOC > 0 ~ 1, !MOMLOC > 0 ~ 0))
main_data <- main_data %>% mutate(dad = case_when(POPLOC > 0 ~ 1, !POPLOC > 0 ~ 0))
main_data <- main_data %>% mutate(parent = case_when(abs(dad+mom) > 0 ~ 1, !abs(dad+mom) > 0 ~ 0))



cut_by <- 0.1

m_income_grid <<- sort(unique(as.integer(c(quantile(m_main_data$INCTOT, probs = seq(0, 1, by = cut_by))))))
m_age_grid <<- sort(unique(as.integer(c(quantile(m_main_data$AGE, probs = seq(0, 1, by = cut_by))))))
m_race_grid <<- sort(unique(as.integer(c(quantile(m_main_data$RACE, probs = seq(0, 1, by = 0.2))))))
m_edu_grid <<- sort(unique(as.integer(c(quantile(m_main_data$EDUCD, probs = seq(0, 1, by = 0.2))))))

m_income_grid <- m_income_grid[-1]
m_age_grid <- m_age_grid[-1]
m_race_grid <- m_race_grid[-1]
m_edu_grid <- m_edu_grid[-1]

jm_income_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$INCTOT, probs = seq(0, 1, by = cut_by))))))
jm_age_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$AGE, probs = seq(0, 1, by = cut_by))))))
jm_race_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$RACE, probs = seq(0, 1, by = 0.2))))))
jm_edu_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$EDUCD, probs = seq(0, 1, by = 0.2))))))

jm_income_grid <- jm_income_grid[-1]
jm_age_grid <- jm_age_grid[-1]
jm_race_grid <- jm_race_grid[-1]
jm_edu_grid <- jm_edu_grid[-1]

co_income_grid <<- sort(unique(as.integer(c(quantile(co_main_data$INCTOT, probs = seq(0, 1, by = cut_by))))))
co_age_grid <<- sort(unique(as.integer(c(quantile(co_main_data$AGE, probs = seq(0, 1, by = cut_by))))))
co_race_grid <<- sort(unique(as.integer(c(quantile(co_main_data$RACE, probs = seq(0, 1, by = 0.2))))))
co_edu_grid <<- sort(unique(as.integer(c(quantile(co_main_data$EDUCD, probs = seq(0, 1, by = 0.2))))))

co_income_grid <- co_income_grid[-1]
co_age_grid <- co_age_grid[-1]
co_race_grid <- co_race_grid[-1]
co_edu_grid <- co_edu_grid[-1]


# next
#income_grid <- c(0,10,20,30,40,50,60,70,80,100,125,180,2000)*1000
#m_income_grid <- jm_income_grid <- co_income_grid <- income_grid

#rm(m_main_data,jm_main_data,co_main_data)

# income_grid <<- sort(unique(c(m_income_grid,jm_income_grid,co_income_grid)))
# age_grid <<- sort(unique(c(m_age_grid,jm_age_grid,co_age_grid)))
# edu_grid <<- sort(unique(c(m_edu_grid,jm_edu_grid,co_edu_grid)))

#m_race_grid <<- jm_race_grid <<- co_race_grid <<- race_grid <<- sort(unique(c(m_race_grid,jm_race_grid,co_race_grid)))
#next
m_race_grid <<- jm_race_grid <<- co_race_grid <<- race_gridrace_grid <<- c(1,2,6,9)
#m_edu_grid <<- jm_edu_grid <<- co_edu_grid <<- edu_grid <<- sort(unique(c(m_edu_grid,jm_edu_grid,co_edu_grid)))
#next
m_edu_grid <<- jm_edu_grid <<- co_edu_grid <<- edu_grid <<- c(63,81,101,116)

# 
# max_age <<- max(jm_main_data$AGE)
# max_income <<- max(jm_main_data$INCTOT[main_data$INCTOT !=9999999])
# 
# 
# 
# income_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$INCTOT, probs = seq(0, 1, by = 0.05))))))
# income_grid <- income_grid[income_grid!=9999999]
# age_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$AGE, probs = seq(0, 1, by = 0.05))))))
# edu_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$EDUCD, probs = seq(0, 1, by = 0.05))))))
# race_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$RACE, probs = seq(0, 1, by = 0.05))))))



# 
#income_grid <<- c(0,10,20,30,40,50,60,70,80,90,100,150,200,2000)*1000
#age_grid <<- c(20,22,24,27,30,33,36,40,45,50,55,60,70,95)
#race_grid <<- c(2,6,9)
#edu_grid <<- c(10,63,64,81,101,114,116)


#m_income_grid <<- jm_income_grid <<- co_income_grid <<- income_grid
#jm_income_grid <<- co_income_grid <<- income_grid
#m_race_grid <<- jm_race_grid <<- co_race_grid <<- race_grid
#m_edu_grid <<- jm_edu_grid <<- co_edu_grid <<- edu_grid


# 
# jm_income_grid <- m_income_grid <- co_income_grid <- income_grid
# jm_age_grid <- m_age_grid <- co_age_grid <- age_grid
# 
# age_grid <- c(15, 20, 22, 25, 27, 29, 31, 34, 36, 38, 41, 45, 49, 54, 61, 95)
# income_grid <- c(-19998,0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000,135000,150000,160000,185000,230000,353000,1265000)
 

# age_distance <- 2
# income_distance <- 10000
# income_grid <- c(-19998,0,15000,30000,45000,60000,75000,90400,106000,126000,150000,165000,191800,238000,331150,503000,1655000)
# #income_grid <<- c(-20,0,20,40,60,80,100,120,160,230,max_income) * 1000
# age_grid <<- c(18,21,24,27,30,33,36,39,42,45,48,51,55,59,62,65,68,73,95)
# edu_grid <<- c(1,14,25,61,63,64,65,71,81,101,114,115,116)
# race_grid <<- c(1,2,6,7,9)


n_age <<- length(jm_age_grid)
n_income <<- length(jm_income_grid)
n_edu <<- length(jm_edu_grid)
n_race <<- length(jm_race_grid)

n_year=2021
if (n_year!=-1){data <- subset(main_data,main_data$YEAR == n_year)}






_____________________________________



precision <<- 0.01
small_epsilon <<- 0.9
age_distance <<- 2
income_distance <<- 10000
income_max <<- 1265000
income_min <<- 0
age_max <<- 95
age_min <<- 18
big_age <<- 19

major_city <<- c(4610,3730,1190,5350,5330,6290,2990,6430,3110)

# 
# 1    0 [Not in identifiable city (or size group)] 2322246
# 2 4610 [New York, NY]                               58688
# 3 3730 [Los Angeles, CA]                            31761
# 4 1190 [Chicago, IL]                                19216
# 5 5350 [Phoenix, AZ]                                10434
# 6 5330 [Philadelphia, PA]                            8794
# 7 6290 [San Francisco, CA]                           6590
# 8 2990 [Indianapolis, IN]                            6030
# 9 6430 [Seattle, WA]                                 5801
# 10 3110 [Jacksonville, FL]                            5680
# 11  810 [Boston, MA]                                  5416
# 12 1710 [Denver, CO]                                  5411
# 13 7230 [Washington, DC]                              5343
# 14 5530 [Portland, OR]                                5289
# 15 4410 [Nashville-Davidson, TN]                      4870
# 16 4050 [Mesa, AZ]                                    4455
# 17  530 [Baltimore, MD]                               4297
# 18 2010 [El Paso, TX]                                 4088
# 19 1750 [Detroit, MI]                                 4003
# 20 4930 [Oakland, CA]                                 3978

funk <- function(n_year){
  
  income_grid <<- jm_income_grid
  age_grid <<- jm_age_grid
  edu_grid <<- jm_edu_grid
  race_grid <<- jm_race_grid
  
  n_age <<- length(age_grid)
  n_income <<- length(income_grid)
  n_edu <<- length(edu_grid)
  n_race <<- length(race_grid)
  
  
  if (n_year!=2021){data <- subset(main_data,main_data$YEAR == n_year)}
  data$income <- data$INCTOT/1000
  
  #
  #
  
  # if (n_year!=2021){data <- subset(data,data$INCTOT < income_max
  #                                & data$INCTOT > income_min
  #                                & data$AGE < age_max
  #                                & data$AGE > age_min
  #                                  )}
  # #
  
  if (n_year == 2021){
    states_codes <- as.integer(sort(unique(data$STATEICP)))
    n_states <- length(states_codes)
    last_state_code <- states_codes[n_states]
    states_gini <- rep(0,last_state_code)
    states_singles_gini <- rep(0,last_state_code)
    for (i in c(1:n_states)){
      state_code <- states_codes [i]
      #print(i)
      state_data <- subset(data, data$STATEICP == state_code )
      states_gini[state_code] <- gini.wtd(state_data$INCTOT, weights = NULL)
      state_single_data <- subset(data, data$STATEICP == state_code &
                                    (data$MARST == 6  | (data$MARST %in% c(1,2) & data$MARRINYR == 2))
      )
      states_singles_gini[state_code] <- gini.wtd(state_single_data$INCTOT, weights = NULL)
    }
    
    #states_gini
    
    data$STATEGINI <- states_gini[data$STATEICP]
    data$SINGLEGINI <- states_singles_gini[data$STATEICP]
    
    
    #with_city_data = subset(data,data$CITYPOP !=0)
    
    city_codes <- as.integer(sort(unique(data$CITY)))
    n_city <- length(city_codes)
    last_city_code <- city_codes[n_city]
    city_gini <- rep(0,last_city_code)
    city_single_gini <- rep(0,last_city_code)
    for (i in c(1:n_city)){
      city_code <- city_codes [i]
      #print(i)
      city_data <- subset(data, data$CITY == city_code )
      city_gini[city_code] <- gini.wtd(city_data$INCTOT, weights = NULL)
      city_single_data <- subset(data, data$CITY == city_code &
                                   (data$MARST == 6  | (data$MARST %in% c(1,2) & data$MARRINYR == 2))
      )
      city_single_gini[city_code] <- gini.wtd(city_single_data$INCTOT, weights = NULL)
    }
    data$CITYGINI <- 0
    data$CITYGINI[data$CITY>0] <- city_gini[data$CITY]
    data$CITYSINGLEGINI <- 0
    data$CITYSINGLEGINI[data$CITY>0] <- city_single_gini[data$CITY]
    
  }
  
  pool_data <- subset(data, (data$MARST == 6 &  data$AGE > 15 )
                      | data$RELATE %in% c(1,2)
                      | (data$MARST %in% c(1,2) 
                         & data$MARRINYR == 2 
                         & data$AGE > 15)
  )
  pool_data <- pool_data %>% mutate(out = case_when(MARRINYR ==2 ~ 1,
                                                    MARRINYR !=2 ~ 0))
  
  
  
  nm_data <- subset(data,data$MARST == 6 &  data$AGE > 15 )
  nmf_data <- subset(data,data$MARST == 6  & data$SEX == 2 & data$AGE > 15 )
  nmm_data <- subset(data,data$MARST == 6  & data$SEX == 1 & data$AGE > 15 )
  jm_data <- subset(data,data$MARST %in% c(1,2) 
                    & data$MARRINYR == 2 # make it == for jm
                    & data$RELATE %in% c(1,2) # new
                    & data$AGE > 15 )
  jmf_data <- subset(data,data$MARST %in% c(1,2) 
                     & data$MARRINYR == 2  # make it == for jm
                     & data$RELATE %in% c(1,2) # new
                     & data$SEX == 2 & data$AGE > 15)
  jmm_data <- subset(data,data$MARST %in% c(1,2) 
                     & data$MARRINYR == 2 
                     & data$RELATE %in% c(1,2) # new
                     & data$SEX == 1 & data$AGE > 15)
  jm_pair_data <- merge(jmf_data,jmm_data,by=c("SERIAL"),all=FALSE)
  
  all_jm_pair_data <- merge(jm_data,jm_data,by=c("SERIAL"),all=FALSE)
  dim(all_jm_pair_data)
  
  all_jm_pair_data <- subset(all_jm_pair_data,all_jm_pair_data$SEX.x == all_jm_pair_data$SEX.y)
  dim(all_jm_pair_data)
  
  all_jm_pair_data <- subset(all_jm_pair_data,all_jm_pair_data$NUMPREC.x != all_jm_pair_data$NUMPREC.y)
  dim(all_jm_pair_data)
  
  
  #big_fam_jm_data <- subset(jm_data,jm_data$SERIAL_n > 2 )
  #big_fam_jm_count <- count(big_fam_jm_data,SERIAL)
  #colnames(big_fam_jm_count) <- c("SERIAL","jm_n")
  
  #big_fam_jm_data <- merge(big_fam_jm_data,big_fam_jm_count,by=c("SERIAL"))
  #print(dim(pair_data)[1]*2/dim(jm_data)[1]*100)
  
  pair_data <- jm_pair_data
  #dim(pair_data) # 12202
  single_data <- nm_data
  
  # age_grid <<- sort(unique(as.integer(c(age_min,quantile(jm_data$AGE, probs = seq(0, 1, by = 0.05)),age_max))))
  # #age_grid <- age_grid[age_grid %% 2 == 0]
  # n_age <<- length(age_grid)
  
  # i=1
  # while (! i == n_age){
  # 
  #   if (abs(age_grid[i+1] - age_grid[i]) < age_distancee){
  #     #print("got one!")
  #     age_grid <<- age_grid[age_grid != age_grid[i+1]]# income_grid[-i-1]; #<- NULL
  #     i = 1
  #     n_age <<- length(age_grid)
  #     #print(income_grid)
  #   }
  #   else {i = i+1}
  # }
  # n_age <<- length(age_grid)
  # 
  # 
  # 
  # income_distance <- 15000
  # income_grid <<- sort(unique(as.integer(c(0,0,quantile(main_data$INCTOT, probs = seq(0, 1, by = 0.005)),max_income))))
  # n_income <<- length(income_grid)
  # 
  # i = 1
  # while (! i == n_income){
  # 
  #   if (abs(income_grid[i+1] - income_grid[i]) < income_distance){
  #     #print("got one!")
  #     income_grid <<- income_grid[income_grid != income_grid[i+1]]# income_grid[-i-1]; #<- NULL
  #     i = 1
  #     n_income <<- length(income_grid)
  #     #print(income_grid)
  #   }
  #   else {i = i+1}
  # 
  # }
  # income_grid <<- income_grid[income_grid !=9999999]
  # n_income <<- length(income_grid)
  # 
  # {
  # # 0                            N/A or no schooling
  # # 1                                            N/A
  # # 2                         No schooling completed
  # # 10                      Nursery school to grade 4
  # # 11                      Nursery school, preschool
  # # 12                                   Kindergarten
  # # 13                            Grade 1, 2, 3, or 4
  # # 14                                        Grade 1
  # # 15                                        Grade 2
  # # 16                                        Grade 3
  # # 17                                        Grade 4
  # # 20                            Grade 5, 6, 7, or 8
  # # 21                                   Grade 5 or 6
  # # 22                                        Grade 5
  # # 23                                        Grade 6
  # # 24                                   Grade 7 or 8
  # # 25                                        Grade 7
  # # 26                                        Grade 8
  # # 30                                        Grade 9
  # # 40                                       Grade 10
  # # 50                                       Grade 11
  # # 60                                       Grade 12
  # # 61                         12th grade, no diploma
  # # 62                    High school graduate or GED
  # # 63                    Regular high school diploma
  # # 64                  GED or alternative credential
  # # 65             Some college, but less than 1 year
  # # 70                              1 year of college
  # # 71   1 or more years of college credit, no degree
  # # 80                             2 years of college
  # # 81         Associate's degree, type not specified
  # # 82       Associate's degree, occupational program
  # # 83           Associate's degree, academic program
  # # 90                             3 years of college
  # # 100                             4 years of college
  # # 101                              Bachelor's degree
  # # 110                            5+ years of college
  # # 111           6 years of college (6+ in 1960-1970)
  # # 112                             7 years of college
  # # 113                            8+ years of college
  # # 114                                Master's degree
  # # 115 Professional degree beyond a bachelor's degree
  # # 116                                Doctoral degree
  # # 999                                        Missing
  # }
  # edu_grid <<- unique(as.integer(0,c(2,quantile(jm_data$EDUCD, probs = seq(0, 1, by = 0.05)),116)))
  # n_edu <<- length(edu_grid)
  # # 
  # {
  # # 1                            White
  # # 2     Black/African American/Negro
  # # 3 American Indian or Alaska Native
  # # 4                          Chinese
  # # 5                         Japanese
  # # 6  Other Asian or Pacific Islander
  # # 7                  Other race, nec
  # # 8                  Two major races
  # # 9        Three or more major races
  # }
  # race_grid <<- unique(as.integer(1,c(1,quantile(jm_data$RACE, probs = seq(0, 1, by = 0.05)),9)))
  # n_race <<- length(race_grid)
  
  jm_data$NEW_INCTOT <- jm_data$INCTOT
  jm_data$NEW_INCTOT <- cut(jm_data$NEW_INCTOT,
                            breaks=c(-Inf,income_grid),
                            labels=seq(1:n_income))
                            #breaks=c(income_grid),
                            #labels=seq(1:n_income-1))
  
  # XXX
  pair_data$NEW_AGE.x <- pair_data$AGE.x
  pair_data$NEW_AGE.x <- cut(pair_data$NEW_AGE.x,
                             breaks=c(-Inf,age_grid),
                             labels=seq(1:n_age))
                             #breaks=c(age_grid),
                             #labels=seq(1:n_age-1))
  
  
  pair_data$NEW_INCTOT.x <- pair_data$INCTOT.x
  pair_data$NEW_INCTOT.x <- cut(pair_data$NEW_INCTOT.x,
                                breaks=c(-Inf,income_grid),
                                labels=seq(1:n_income))
                                #breaks=c(income_grid),
                                #labels=seq(1:n_income-1))
  
  pair_data$NEW_EDUCD.x <- pair_data$EDUCD.x
  pair_data$NEW_EDUCD.x <- cut(pair_data$NEW_EDUCD.x,
                               breaks=c(-Inf, edu_grid),
                               labels=seq(1:n_edu))
  
  pair_data$NEW_RACE.x <- pair_data$RACE.x
  pair_data$NEW_RACE.x <- cut(pair_data$NEW_RACE.x,
                              breaks=c(-Inf, race_grid),
                              labels=seq(1:n_race))
  
  #YYY
  pair_data$NEW_AGE.y <- pair_data$AGE.y
  pair_data$NEW_AGE.y <- cut(pair_data$NEW_AGE.y,
                             breaks=c(-Inf,age_grid),
                             labels=seq(1:n_age))
  
  pair_data$NEW_INCTOT.y <- pair_data$INCTOT.y
  pair_data$NEW_INCTOT.y <- cut(pair_data$NEW_INCTOT.y,
                                breaks=c(-Inf,income_grid),
                                labels=seq(1:n_income))
  
  pair_data$NEW_EDUCD.y <- pair_data$EDUCD.y
  pair_data$NEW_EDUCD.y <- cut(pair_data$NEW_EDUCD.y,
                               breaks=c(-Inf, edu_grid),
                               labels=seq(1:n_edu))
  
  pair_data$NEW_RACE.y <- pair_data$RACE.y
  pair_data$NEW_RACE.y <- cut(pair_data$NEW_RACE.y,
                              breaks=c(-Inf, race_grid),
                              labels=seq(1:n_race))
  
  #pair_data <- na.omit(pair_data, c("NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x", "NEW_AGE.x", "NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y", "NEW_AGE.y")) 
  
  Mu_pair <- count(pair_data,
                   NEW_AGE.x,NEW_INCTOT.x,NEW_EDUCD.x,NEW_RACE.x,
                   NEW_AGE.y,NEW_INCTOT.y,NEW_EDUCD.y,NEW_RACE.y)#,.drop = FALSE)
  
  #Mu_pair=count(pair_data,
  #              NEW_AGE.x,NEW_INCTOT.x,NEW_EDUCD.x,NEW_RACE.x,
  #              NEW_AGE.y,NEW_INCTOT.y,NEW_EDUCD.y,NEW_RACE.y,.drop = FALSE)
  #Mu_pair
  #col <- colnames(pair_data)
  
  #       MALE
  single_male <- subset(single_data,single_data$SEX == 1)
  
  single_male$NEW_AGE <- single_male$AGE
  single_male$NEW_AGE <- as.integer(cut(single_male$NEW_AGE,
                                        breaks=c(-Inf,age_grid),
                                        labels=seq(1:n_age)))
  
  single_male$NEW_INCTOT <- single_male$INCTOT
  single_male$NEW_INCTOT <- as.integer(cut(single_male$NEW_INCTOT,
                                           breaks=c(-Inf,income_grid),
                                           labels=seq(1:n_income)))
  
  single_male$NEW_EDUCD <- single_male$EDUCD
  single_male$NEW_EDUCD <- as.integer(cut(single_male$NEW_EDUCD,
                                          breaks=c(-Inf, edu_grid),
                                          labels=seq(1:n_edu)))
  
  single_male$NEW_RACE <- single_male$RACE
  single_male$NEW_RACE <- as.integer(cut(single_male$NEW_RACE,
                                         breaks=c(-Inf, race_grid),
                                         labels=seq(1:n_race)))
  
  #single_male <- na.omit(single_male, c("NEW_INCTOT","NEW_EDUCD","NEW_RACE", "NEW_AGE")) 
  
  Mu_male <- count(single_male,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE) #,.drop = FALSE)
  #Mu_male=count(single_male,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE,.drop = FALSE)
  
  #Mu_male
  
  colnames(Mu_male) <- c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y","n_male")
  
  #        FEMALE
  single_female <- subset(single_data,single_data$SEX == 2)
  
  single_female$NEW_AGE <- single_female$AGE
  single_female$NEW_AGE <- as.integer(cut(single_female$NEW_AGE,
                                          breaks=c(-Inf,age_grid),
                                          labels=seq(1:n_age)))
  
  single_female$NEW_INCTOT <- single_female$INCTOT
  single_female$NEW_INCTOT <- as.integer(cut(single_female$NEW_INCTOT,
                                             breaks=c(-Inf,income_grid),
                                             labels=seq(1:n_income)))
  
  single_female$NEW_EDUCD <- single_female$EDUCD
  single_female$NEW_EDUCD <- as.integer(cut(single_female$NEW_EDUCD,
                                            breaks=c(-Inf, edu_grid),
                                            labels=seq(1:n_edu)))
  
  single_female$NEW_RACE <- single_female$RACE
  single_female$NEW_RACE <- as.integer(cut(single_female$NEW_RACE,
                                           breaks=c(-Inf, race_grid),
                                           labels=seq(1:n_race)))
  #single_female <- na.omit(single_female, c("NEW_INCTOT","NEW_EDUCD","NEW_RACE", "NEW_AGE")) 
  
  
  Mu_female <- count(single_female,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE) #,.drop = FALSE
  #Mu_female=count(single_female,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE,.drop = FALSE)
  
  #Mu_female
  
  colnames(Mu_female) <- c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x","n_female")
  #Mu_female
  #dim(Mu_pair) # 11810
  #Mu_all <- merge(Mu_pair,Mu_female,by=c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x"),all=FALSE)
  #Mu_all <- merge(Mu_all,Mu_male,by=c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y"),all=FALSE)
  #dim(Mu_all) # 10778
  Mu_all <- merge(Mu_pair,Mu_female,by=c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x"),all=TRUE)
  #dim(Mu_all) # 29350
  Mu_all <- merge(Mu_all,Mu_male,by=c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y"),all=TRUE)
  #dim(Mu_all) # 49157
  Mu_all <- subset(Mu_all,!is.na(Mu_all$n))
  #dim(Mu_all) # 11810
  
  #dim(pair_data) # same ; 12202
  
  
  #Mu_all$n[is.na(Mu_all$n_male)] <- -Mu_all$n
  
  
  Mu_all$n_male[is.na(Mu_all$n_male)] <- small_epsilon
  
  #dim(Mu_all$n_male[is.na(Mu_all$n_male))
  
  Mu_all$n_female[is.na(Mu_all$n_female)] <- small_epsilon
  
  #count(Mu_all,n_male == small_epsilon)
  #count(Mu_all,n_female == small_epsilon)
  
  #Mu_all
  
  
  #count(Mu_all, is.na(Mu_all$n_female) |  is.na(Mu_all$n_female))
  
  #Mu_all$MV <- 0
  #Mu_all$MV[is.na(Mu_all$n_female) | is.na(Mu_all$n_female)] <- 1
  
  Mu_all$MV <- ((Mu_all$n/Mu_all$n_male)*(Mu_all$n/Mu_all$n_female))^0.5 

  #count(Mu_all, is.na(Mu_all$MV))
  #count(Mu_all, Mu_all$MV ==1)
  
  
  
  #Here!!!
  #Mu_all$MV[Mu_all$n_male == small_epsilon | Mu_all$n_female == small_epsilon] <- 1
  
  
  
  #Mu_all$MV[is.na(Mu_all$n_female) | is.na(Mu_all$n_female)] <- 1
  
  #Mu_all$MV <- (Mu_all$n/Mu_all$n_male*Mu_all$n/Mu_all$n_female)^0.5 
  Mu_all$EV <- Mu_all$MV*Mu_all$n 
  
  
  Mu_all$MV_male <- Mu_all$n/Mu_all$n_male 
  #Mu_all$MV_male[Mu_all$n_male == small_epsilon] <- 1
  
  Mu_all$EV_male <- Mu_all$MV_male*Mu_all$n
  
  Mu_all$MV_female <- Mu_all$n/Mu_all$n_female
  #Mu_all$MV_female[Mu_all$fen_male == small_epsilon] <- 1
  Mu_all$EV_female <- Mu_all$MV_female*Mu_all$n 
  
  #log version
  Mu_all$MV_log <- log(Mu_all$MV)
  Mu_all$EV_log <- Mu_all$MV_log*Mu_all$n 
  
  #Expected
  Mu_all$MV_log_expected <- Mu_all$MV_log + G_gumbel(-Mu_all$MV_log)
  Mu_all$EV_log_expected <- Mu_all$MV_log_expected*Mu_all$n 
  
  Mu_all$MV_male_log <- log(Mu_all$MV_male)
  Mu_all$EV_male_log  <- Mu_all$MV_male_log*Mu_all$n
  
  #Expected
  Mu_all$MV_male_log_expected <- Mu_all$MV_male_log + G_gumbel(-Mu_all$MV_male_log)
  Mu_all$EV_male_log_expected <- Mu_all$MV_male_log_expected*Mu_all$n
  
  Mu_all$MV_female_log <- log(Mu_all$MV_female) 
  Mu_all$EV_female_log  <- Mu_all$MV_female_log*Mu_all$n 
  
  #Expected
  Mu_all$MV_female_log_expected <- Mu_all$MV_female_log + G_gumbel(-Mu_all$MV_female_log)
  Mu_all$EV_female_log_expected <- Mu_all$MV_female_log_expected*Mu_all$n
  
  Mu_all$tau <- (Mu_all$MV_male-Mu_all$MV_female)/2
  
  #this one correct?
  Mu_all$tau <- (Mu_all$MV_male_log-Mu_all$MV_female_log)/2
  
  #Expected
  Mu_all$tau_expected <- (Mu_all$MV_male_log_expected - Mu_all$MV_female_log_expected)/2
  
  
  
  theta_min <- as.numeric(quantile(Mu_all$tau,probs=0))
  theta_max <- as.numeric(quantile(Mu_all$tau,probs=1))
  
  Mu_all$theta <- (Mu_all$tau- theta_min)/(theta_max-theta_min)
  Mu_all$theta[Mu_all$theta<0] <- 0
  Mu_all$theta[Mu_all$theta>1] <- 1
  
  
  
  
  
  
  Mu_all$female_net_gain <- Mu_all$MV_female_log+Mu_all$tau
  Mu_all$male_net_gain <- Mu_all$MV_female_log-Mu_all$tau
  
  Mu_all$male_share <- (Mu_all$MV_male_log-Mu_all$tau)/Mu_all$MV_log
  Mu_all$female_share <- (Mu_all$MV_female_log+Mu_all$tau)/Mu_all$MV_log
  
  
  
  #pair_data$tau <- (pair_data$MV_male-pair_data$MV_female)/2
  
  #count(pair_data,is.na(pair_data$MV_log))
  
  col_names <- c("NEW_INCTOT.x", "NEW_INCTOT.y","NEW_AGE.x", "NEW_AGE.y",
                 "NEW_RACE.x", "NEW_RACE.y","NEW_EDUCD.x", "NEW_EDUCD.y")
  
  pair_data <- merge(pair_data, Mu_all, by.x=col_names,
                     by.y=col_names,all.x=TRUE)
  
  pair_data <- pair_data %>%
    set_variable_labels(
      NEW_AGE.x = "Female Age",
      NEW_AGE.y = "Male Age",
      NEW_INCTOT.x = "Female Income",
      NEW_INCTOT.y = "Male Income",
      income.x = "Female Income",
      income.y = "Male Income",
      NEW_EDUCD.x = "Female Education",
      NEW_EDUCD.y = "Male Education",
      NEW_RACE.x = "Female Race",
      NEW_RACE.y = "Male Race"
    )
  
  pair_data$age_dif <- pair_data$AGE.x-pair_data$AGE.y
  pair_data$income_dif <- pair_data$INCTOT.x-pair_data$INCTOT.y
  pair_data$edu_dif <- pair_data$EDUCD.x-pair_data$EDUCD.y
  
  
  pair_data$new_age_dif <- as.integer(pair_data$NEW_AGE.x)-as.integer(pair_data$NEW_AGE.y)
  pair_data$new_income_dif <- as.integer(pair_data$NEW_INCTOT.x)-as.integer(pair_data$NEW_INCTOT.y)
  pair_data$new_edu_dif <- as.integer(pair_data$NEW_EDUCD.x)-as.integer(pair_data$NEW_EDUCD.y)
  
  pair_data$big_age_gap <- 0
  pair_data$big_age_gap[abs(pair_data$age_dif)>big_age] <- 1
  
  
  pair_data$cut_age_dif <- cut(pair_data$age_dif,
                             breaks=c(0,10,20,30,40,50,60,70),
                             labels=c(0,1,2,3,4,5,6))
  
  
  
  
  pair_data$theta_prime <- pair_data$theta/(1-pair_data$theta+0.01)
  
  pair_data$c_m <- 1/(pair_data$dad.y+0.01)
  pair_data$c_f <- 1/(pair_data$dad.x+0.01)
  pair_data$c_t <- pair_data$c_m+pair_data$c_f
  pair_data$LHS <- pair_data$c_m - pair_data$theta_prime*pair_data$c_f
  pair_data$RHS <- - pair_data$theta_prime*pair_data$c_t
  
  
  
  pair_data$c_m_2 <- pair_data$TRANWORK.y
  pair_data$c_f_2 <- pair_data$TRANWORK.x
  pair_data$c_t_2 <- pair_data$c_m_2+pair_data$c_f_2
  pair_data$LHS_2 <- pair_data$c_m_2 - pair_data$theta_prime*pair_data$c_f_2
  pair_data$RHS_2 <- - pair_data$theta_prime*pair_data$c_t_2
  
  
  
  # Nash bargenning -> tau=(alpha-gamma)/2
  
  pair_data$rate <- F_gumbel(-pair_data$MV_log)
  pair_data$rate.x <- F_gumbel(-pair_data$MV_female)
  pair_data$rate.y <- F_gumbel(-pair_data$MV_male)
  
  pair_data <- pair_data %>% mutate(major_city.x = case_when(CITY.x %in% major_city ~ 1,
                                                             !CITY.x %in% major_city ~ 0))
  pair_data <- pair_data %>% mutate(major_city.y = case_when(CITY.y %in% major_city ~ 1,
                                                             !CITY.y %in% major_city ~ 0))
  
  
  pair_data <- pair_data %>% mutate(big_city.x = case_when(CITY.x > 0 ~ 1,
                                                           !CITY.x > 0 ~ 0))
  pair_data <- pair_data %>% mutate(big_city.y = case_when(CITY.y > 0 ~ 1,
                                                           !CITY.y >0  ~ 0))
  
  pair_data$f_income_share <- pair_data$income.x/(pair_data$income.x+pair_data$income.y)
  
  output <- list("output",
                 pair_data = pair_data,
                 Mu_all = Mu_all)
  
  return(output)
}

n_year <<- 2021


draw <- function(n_year){
  
  income_grid <<- jm_income_grid
  age_grid <<- jm_age_grid
  edu_grid <<- jm_edu_grid
  race_grid <<- jm_race_grid
  
  n_age <<- length(age_grid)
  n_income <<- length(income_grid)
  n_edu <<- length(edu_grid)
  n_race <<- length(race_grid)
  
  
  
  store <- funk(n_year)
  Mu_all <- store$Mu_all
  pair_data <- store$pair_data
  
  
  
  ####            AGE 
  
  
  
  N_Age <- matrix(0, n_age, n_age)
  Mu_age <- matrix(0, n_age, n_age)
  Mu_M_age <- matrix(0, n_age, n_age)
  Mu_F_age <- matrix(0, n_age, n_age)
  #log version
  Mu_age_log <- matrix(0, n_age, n_age)
  Mu_M_age_log <- matrix(0, n_age, n_age)
  Mu_F_age_log <- matrix(0, n_age, n_age)
  
  #ave
  Mu_age_ave <- matrix(0, n_age, n_age)
  Mu_M_age_ave <- matrix(0, n_age, n_age)
  Mu_F_age_ave <- matrix(0, n_age, n_age)
  #ave log version
  Mu_age_log_ave <- matrix(0, n_age, n_age)
  Mu_M_age_log_ave <- matrix(0, n_age, n_age)
  Mu_F_age_log_ave <- matrix(0, n_age, n_age)
  
  #median 
  Mu_age_median <- matrix(0, n_age, n_age)
  Mu_M_age_median <- matrix(0, n_age, n_age)
  Mu_F_age_median <- matrix(0, n_age, n_age)
  #median log version
  Mu_age_log_median <- matrix(0, n_age, n_age)
  Mu_M_age_log_median <- matrix(0, n_age, n_age)
  Mu_F_age_log_median <- matrix(0, n_age, n_age)
  
  
  for (i in 1:n_age){
    for (j in 1:n_age){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_AGE.x == i & Mu_all$NEW_AGE.y == j)
      
      total_folks = 1 # sum(current_sub$n)
      
      Mu_age_log[i,j] = sum(current_sub$EV_log_expected) / total_folks
      Mu_F_age_log[i,j] = sum(current_sub$EV_female_log_expected) / total_folks
      Mu_M_age_log[i,j] = sum(current_sub$EV_male_log_expected) / total_folks
      
      Mu_age[i,j] = sum(current_sub$EV) / total_folks
      Mu_F_age[i,j] = sum(current_sub$EV_female) / total_folks
      Mu_M_age[i,j] = sum(current_sub$EV_male) / total_folks
      N_Age[i,j] = dim(current_sub)[1]
      
      
      # median
      
      Mu_age_log_median[i,j] = median(current_sub$EV_log_expected) / total_folks
      Mu_F_age_log_median[i,j] = median(current_sub$EV_female_log_expected) / total_folks
      Mu_M_age_log_median[i,j] = median(current_sub$EV_male_log_expected) / total_folks
      
      Mu_age_median[i,j] = median(current_sub$EV) / total_folks
      Mu_F_age_median[i,j] = median(current_sub$EV_female) / total_folks
      Mu_M_age_median[i,j] = median(current_sub$EV_male) / total_folks

      # ave
      Mu_age_log_ave[i,j] = mean(current_sub$EV_log_expected) / total_folks
      Mu_F_age_log_ave[i,j] = mean(current_sub$EV_female_log_expected) / total_folks
      Mu_M_age_log_ave[i,j] = mean(current_sub$EV_male_log_expected) / total_folks
      
      Mu_age_ave[i,j] = mean(current_sub$EV) / total_folks
      Mu_F_age_ave[i,j] = mean(current_sub$EV_female) / total_folks
      Mu_M_age_ave[i,j] = mean(current_sub$EV_male) / total_folks

    }
  }
  
  
  x_age=c(1:length(age_grid))
  
  # persp(x_age,x_age,MV_Mu_age, theta=120, phi=30, r=35,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Mu Age Both for", n_year))
  # 
  # 
  # persp(x_age,x_age,MV_Mu_F_age, theta=120, phi=30, r=35, 
  #        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #        nticks=5, ticktype="detailed",
  #        col="cyan", xlab="Female age level",
  #        ylab="Male age level", zlab="Estimated value", 
  #        main=paste("Mu Age F for", n_year))
  # 
  # persp(x_age,x_age,MV_Mu_M_age, theta=120, phi=30, r=35, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Mu Age M for", n_year))
  # 
  smooth_N_Age <- kernel2dsmooth(N_Age, kernel.type="disk", r=2)
  
  # self
  smooth_Mu_age <- kernel2dsmooth(Mu_age, kernel.type="disk", r=2)
  smooth_Mu_M_age <- kernel2dsmooth(Mu_M_age, kernel.type="disk", r=2)
  smooth_Mu_F_age <- kernel2dsmooth(Mu_F_age, kernel.type="disk", r=2)
  
  #log
  smooth_Mu_age_log <- kernel2dsmooth(Mu_age_log, kernel.type="disk", r=2)
  smooth_Mu_M_age_log <- kernel2dsmooth(Mu_M_age_log, kernel.type="disk", r=2)
  smooth_Mu_F_age_log <- kernel2dsmooth(Mu_F_age_log, kernel.type="disk", r=2)
  
  # median self
  smooth_Mu_age_median <- kernel2dsmooth(Mu_age_median, kernel.type="disk", r=2)
  smooth_Mu_M_age_median <- kernel2dsmooth(Mu_M_age_median, kernel.type="disk", r=2)
  smooth_Mu_F_age_median <- kernel2dsmooth(Mu_F_age_median, kernel.type="disk", r=2)
  
  # median log
  smooth_Mu_age_log_median <- kernel2dsmooth(Mu_age_log_median, kernel.type="disk", r=2)
  smooth_Mu_M_age_log_median <- kernel2dsmooth(Mu_M_age_log_median, kernel.type="disk", r=2)
  smooth_Mu_F_age_log_median <- kernel2dsmooth(Mu_F_age_log_median, kernel.type="disk", r=2)
  
  #mean self
  smooth_Mu_age_ave <- kernel2dsmooth(Mu_age_ave, kernel.type="disk", r=2)
  smooth_Mu_M_age_ave <- kernel2dsmooth(Mu_M_age_ave, kernel.type="disk", r=2)
  smooth_Mu_F_age_ave <- kernel2dsmooth(Mu_F_age_ave, kernel.type="disk", r=2)
  
  #mean log
  smooth_Mu_age_log_ave <- kernel2dsmooth(Mu_age_log_ave, kernel.type="disk", r=2)
  smooth_Mu_M_age_log_ave <- kernel2dsmooth(Mu_M_age_log_ave, kernel.type="disk", r=2)
  smooth_Mu_F_age_log_ave <- kernel2dsmooth(Mu_F_age_log_ave, kernel.type="disk", r=2)
  
  #F_gumbel(-Mu_age_log)
  # 
  # persp(x_age,x_age,F_gumbel(-Mu_age_log)*100, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("F_gumbel(-Mu_age_log)", n_year))
  # 

  persp(x_age,x_age,smooth_Mu_age, theta=-20, phi=30, r=5,
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        ylab="Male age level", zlab="Estimated mutual gain",
        main=paste("Smooth Mu Age Both for", n_year))


  persp(x_age,x_age,smooth_Mu_M_age, theta=-20, phi=30, r=5, 
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        ylab="Male age level", zlab="Estimated male gain", 
        main=paste("Smooth Mu Age M for", n_year))
  
  persp(x_age,x_age,smooth_Mu_F_age, theta=-20, phi=30, r=5,
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        ylab="Male age level", zlab="Estimated female gain", 
        main=paste("Smooth  Mu Age F for", n_year))
  
  persp(x_age,x_age,smooth_N_Age, theta=-20, phi=30, r=5,
        shade=0.4, axes=TRUE, scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        ylab="Male age level", zlab="Frequency", 
        main=paste("N Age for", n_year))
  
  
  # 
  # persp(x_age,x_age,smooth_Mu_age_ave, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Mu Age Both for", n_year))
  # 
  # 
  # persp(x_age,x_age,smooth_Mu_M_age_ave, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Mu Age M for", n_year))
  # 
  # persp(x_age,x_age,smooth_Mu_F_age_ave, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Mu Age F for", n_year))
  # 
  # 
  # persp(x_age,x_age,smooth_Mu_age_median, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth Median Mu Age Both for", n_year))
  # 
  # 
  # persp(x_age,x_age,smooth_Mu_M_age_median, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth Median Mu Age M for", n_year))
  # 
  # persp(x_age,x_age,smooth_Mu_F_age_median, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth Median Mu Age F for", n_year))
  # 
  # 
  # 
  # 
  # 
  # #LOG
  # 
  # persp(x_age,x_age,smooth_Mu_age_log, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth Log Mu Age Both for", n_year))
  # 
  # 
  # persp(x_age,x_age,smooth_Mu_M_age_log, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth Log Mu Age M for", n_year))
  # 
  # persp(x_age,x_age,smooth_Mu_F_age_log, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth Log  Mu Age F for", n_year))
  # 
  # persp(x_age,x_age,smooth_Mu_age_log_ave, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Log Mu Age Both for", n_year))
  # 
  # 
  # persp(x_age,x_age,smooth_Mu_M_age_log_ave, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Log Mu Age M for", n_year))
  # 
  # persp(x_age,x_age,smooth_Mu_F_age_log_ave, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Log Mu Age F for", n_year))
  # 
  # 
  # persp(x_age,x_age,smooth_Mu_age_log_median, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth Median Log Mu Age Both for", n_year))
  # 
  # 
  # persp(x_age,x_age,smooth_Mu_M_age_log_median, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth Median Log Mu Age M for", n_year))
  # 
  # persp(x_age,x_age,smooth_Mu_F_age_log_median, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female age level",
  #       ylab="Male age level", zlab="Estimated value", 
  #       main=paste("Smooth Median Log Mu Age F for", n_year))
  # 
  # 
  
  ####            INCOME 
  
  N_income <- matrix(0, n_income, n_income)
  Mu_income <- matrix(0, n_income, n_income)
  Mu_M_income <- matrix(0, n_income, n_income)
  Mu_F_income <- matrix(0, n_income, n_income)
  #log version
  Mu_income_log<- matrix(0, n_income, n_income)
  Mu_M_income_log <- matrix(0, n_income, n_income)
  Mu_F_income_log <- matrix(0, n_income, n_income)
  
  for (i in 1:n_income){
    for (j in 1:n_income){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_INCTOT.x == i & Mu_all$NEW_INCTOT.y == j)
      
      total_folks = 1 # sum(current_sub$n)
      
      Mu_income_log[i,j] = sum(current_sub$EV_log) / total_folks
      Mu_F_income_log[i,j] = sum(current_sub$EV_female_log) / total_folks
      Mu_M_income_log[i,j] = sum(current_sub$EV_male_log) / total_folks
      
      
      Mu_income[i,j] = sum(current_sub$EV) / total_folks
      Mu_F_income[i,j] = sum(current_sub$EV_female) / total_folks
      Mu_M_income[i,j] = sum(current_sub$EV_male) / total_folks
      N_income[i,j] = dim(current_sub)[1]
      
    }
  }
  
  x_income=c(1:length(income_grid))
  
  
  smooth_Mu_income <- kernel2dsmooth(Mu_income, kernel.type="disk", r=2)
  smooth_Mu_M_income <- kernel2dsmooth(Mu_M_income, kernel.type="disk", r=2)
  smooth_Mu_F_income <- kernel2dsmooth(Mu_F_income, kernel.type="disk", r=2)
  
  smooth_N_income <- kernel2dsmooth(N_income, kernel.type="disk", r=2)
  
  # persp(x_income/1000,x_income/1000,Mu_income, theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       zlim = c(0,1.01*max(Mu_income[!is.na(Mu_income)])), 
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Mu Both Income", n_year))
  # 
  # persp(x_income/1000,x_income/1000,Mu_M_income, theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       zlim = c(0,1.01*max(Mu_M_income)), 
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Male Income", n_year))
  # 
  # persp(x_income/1000,x_income/1000,Mu_F_income, theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       zlim = c(0,1.01*max(Mu_F_income)), 
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Mu Female Income", n_year))
  # 
  # 
  
  #theta=-20, phi=30, r=35, 
  persp(x_income,x_income,smooth_Mu_income, theta=-40, phi=30, r=5,#theta=-20, phi=30, r=35, #theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated mutual gain", 
        main=paste("Mu Both Income", n_year))
  
  persp(x_income,x_income,smooth_Mu_M_income, theta=-40, phi=30, r=5, #theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated male gain", 
        main=paste("Male Income", n_year))
  
  persp(x_income,x_income,smooth_Mu_F_income, theta=-40, phi=30, r=5, #theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated female gain", 
        main=paste("Mu Female Income", n_year))
  
  
  persp(x_income,x_income,smooth_N_income, theta=-40, phi=30, r=5, #theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Frequency", 
        main=paste("Mu Female Income", n_year))
  
  
  #### ICNOME of AGE
  # 
  # ####            AGE
  # N_income <- matrix(0, n_income, n_income)
  # Mu_income <- matrix(0, n_income, n_income)
  # Mu_M_income <- matrix(0, n_income, n_income)
  # Mu_F_income <- matrix(0, n_income, n_income)
  # #log version
  # Mu_income_log <- matrix(0, n_income, n_income)
  # Mu_M_income_log <- matrix(0, n_income, n_income)
  # Mu_F_income_log <- matrix(0, n_income, n_income)
  # 
  # #ave
  # Mu_income_ave <- matrix(0, n_income, n_income)
  # Mu_M_income_ave <- matrix(0, n_income, n_income)
  # Mu_F_income_ave <- matrix(0, n_income, n_income)
  # #ave log version
  # Mu_income_log_ave <- matrix(0, n_income, n_income)
  # Mu_M_income_log_ave <- matrix(0, n_income, n_income)
  # Mu_F_income_log_ave <- matrix(0, n_income, n_income)
  # 
  # #median 
  # Mu_income_median <- matrix(0, n_income, n_income)
  # Mu_M_income_median <- matrix(0, n_income, n_income)
  # Mu_F_income_median <- matrix(0, n_income, n_income)
  # #median log version
  # Mu_income_log_median <- matrix(0, n_income, n_income)
  # Mu_M_income_log_median <- matrix(0, n_income, n_income)
  # Mu_F_income_log_median <- matrix(0, n_income, n_income)
  # 
  # 
  # for (i in 1:n_income){
  #   for (j in 1:n_income){
  #     #use pi
  #     current_sub <- subset(Mu_all, Mu_all$NEW_INCTOT.x == i & Mu_all$NEW_INCTOT.y == j)
  #     
  #     total_folks = 1 #sum(current_sub$n)
  #     
  #     Mu_income_log[i,j] = sum(current_sub$EV_log) / total_folks
  #     Mu_F_income_log[i,j] = sum(current_sub$EV_female_log) / total_folks
  #     Mu_M_income_log[i,j] = sum(current_sub$EV_male_log) / total_folks
  #     
  #     Mu_income[i,j] = sum(current_sub$EV) / total_folks
  #     Mu_F_income[i,j] = sum(current_sub$EV_female) / total_folks
  #     Mu_M_income[i,j] = sum(current_sub$EV_male) / total_folks
  #     N_income[i,j] = dim(current_sub)[1]
  #     
  #     
  #     # median
  #     
  #     Mu_income_log_median[i,j] = median(current_sub$EV_log) / total_folks
  #     Mu_F_income_log_median[i,j] = median(current_sub$EV_female_log) / total_folks
  #     Mu_M_income_log_median[i,j] = median(current_sub$EV_male_log) / total_folks
  #     
  #     Mu_income_median[i,j] = median(current_sub$EV) / total_folks
  #     Mu_F_income_median[i,j] = median(current_sub$EV_female) / total_folks
  #     Mu_M_income_median[i,j] = median(current_sub$EV_male) / total_folks
  #     
  #     # ave
  #     Mu_income_log_ave[i,j] = mean(current_sub$EV_log) / total_folks
  #     Mu_F_income_log_ave[i,j] = mean(current_sub$EV_female_log) / total_folks
  #     Mu_M_income_log_ave[i,j] = mean(current_sub$EV_male_log) / total_folks
  #     
  #     Mu_income_ave[i,j] = mean(current_sub$EV) / total_folks
  #     Mu_F_income_ave[i,j] = mean(current_sub$EV_female) / total_folks
  #     Mu_M_income_ave[i,j] = mean(current_sub$EV_male) / total_folks
  #     
  #   }
  # }
  # 
  # 
  # x_income=c(1:length(income_grid))
  # 
  # # persp(x_income,x_income,MV_Mu_income, theta=120, phi=30, r=35,
  # #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  # #       nticks=5, ticktype="detailed",
  # #       col="cyan", xlab="Female income level",
  # #       ylab="Male income level", zlab="Estimated value", 
  # #       main=paste("Mu income Both for", n_year))
  # # 
  # # 
  # # persp(x_income,x_income,MV_Mu_F_income, theta=120, phi=30, r=35, 
  # #        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  # #        nticks=5, ticktype="detailed",
  # #        col="cyan", xlab="Female income level",
  # #        ylab="Male income level", zlab="Estimated value", 
  # #        main=paste("Mu income F for", n_year))
  # # 
  # # persp(x_income,x_income,MV_Mu_M_income, theta=120, phi=30, r=35, 
  # #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  # #       nticks=5, ticktype="detailed",
  # #       col="cyan", xlab="Female income level",
  # #       ylab="Male income level", zlab="Estimated value", 
  # #       main=paste("Mu income M for", n_year))
  # # 
  # smooth_N_income <- kernel2dsmooth(N_income, kernel.type="disk", r=2)
  # 
  # # self
  # smooth_Mu_income <- kernel2dsmooth(Mu_income, kernel.type="disk", r=2)
  # smooth_Mu_M_income <- kernel2dsmooth(Mu_M_income, kernel.type="disk", r=2)
  # smooth_Mu_F_income <- kernel2dsmooth(Mu_F_income, kernel.type="disk", r=2)
  # 
  # #log
  # smooth_Mu_income_log <- kernel2dsmooth(Mu_income_log, kernel.type="disk", r=2)
  # smooth_Mu_M_income_log <- kernel2dsmooth(Mu_M_income_log, kernel.type="disk", r=2)
  # smooth_Mu_F_income_log <- kernel2dsmooth(Mu_F_income_log, kernel.type="disk", r=2)
  # 
  # # median self
  # smooth_Mu_income_median <- kernel2dsmooth(Mu_income_median, kernel.type="disk", r=2)
  # smooth_Mu_M_income_median <- kernel2dsmooth(Mu_M_income_median, kernel.type="disk", r=2)
  # smooth_Mu_F_income_median <- kernel2dsmooth(Mu_F_income_median, kernel.type="disk", r=2)
  # 
  # # median log
  # smooth_Mu_income_log_median <- kernel2dsmooth(Mu_income_log_median, kernel.type="disk", r=2)
  # smooth_Mu_M_income_log_median <- kernel2dsmooth(Mu_M_income_log_median, kernel.type="disk", r=2)
  # smooth_Mu_F_income_log_median <- kernel2dsmooth(Mu_F_income_log_median, kernel.type="disk", r=2)
  # 
  # #mean self
  # smooth_Mu_income_ave <- kernel2dsmooth(Mu_income_ave, kernel.type="disk", r=2)
  # smooth_Mu_M_income_ave <- kernel2dsmooth(Mu_M_income_ave, kernel.type="disk", r=2)
  # smooth_Mu_F_income_ave <- kernel2dsmooth(Mu_F_income_ave, kernel.type="disk", r=2)
  # 
  # #mean log
  # smooth_Mu_income_log_ave <- kernel2dsmooth(Mu_income_log_ave, kernel.type="disk", r=2)
  # smooth_Mu_M_income_log_ave <- kernel2dsmooth(Mu_M_income_log_ave, kernel.type="disk", r=2)
  # smooth_Mu_F_income_log_ave <- kernel2dsmooth(Mu_F_income_log_ave, kernel.type="disk", r=2)
  # 
  # persp(x_income,x_income,smooth_Mu_income, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Mu income Both for", n_year))
  # 
  # 
  # persp(x_income,x_income,smooth_Mu_M_income, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Mu income M for", n_year))
  # 
  # persp(x_income,x_income,smooth_Mu_F_income, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth  Mu income F for", n_year))
  # 
  # 
  # 
  # persp(x_income,x_income,smooth_N_income, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("N income for", n_year))
  # 
  # 
  # 
  # persp(x_income,x_income,smooth_Mu_income_ave, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Mu income Both for", n_year))
  # 
  # 
  # persp(x_income,x_income,smooth_Mu_M_income_ave, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Mu income M for", n_year))
  # 
  # persp(x_income,x_income,smooth_Mu_F_income_ave, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Mu income F for", n_year))
  # 
  # 
  # persp(x_income,x_income,smooth_Mu_income_median, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Median Mu income Both for", n_year))
  # 
  # 
  # persp(x_income,x_income,smooth_Mu_M_income_median, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Median Mu income M for", n_year))
  # 
  # persp(x_income,x_income,smooth_Mu_F_income_median, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Median Mu income F for", n_year))
  # 
  # 
  # 
  # 
  # 
  # #LOG
  # 
  # persp(x_income,x_income,smooth_Mu_income_log, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Log Mu income Both for", n_year))
  # 
  # 
  # persp(x_income,x_income,smooth_Mu_M_income_log, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Log Mu income M for", n_year))
  # 
  # persp(x_income,x_income,smooth_Mu_F_income_log, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Log  Mu income F for", n_year))
  # 
  # persp(x_income,x_income,smooth_Mu_income_log_ave, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Log Mu income Both for", n_year))
  # 
  # 
  # persp(x_income,x_income,smooth_Mu_M_income_log_ave, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Log Mu income M for", n_year))
  # 
  # persp(x_income,x_income,smooth_Mu_F_income_log_ave, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth AVE Log Mu income F for", n_year))
  # 
  # 
  # persp(x_income,x_income,smooth_Mu_income_log_median, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Median Log Mu income Both for", n_year))
  # 
  # 
  # persp(x_income,x_income,smooth_Mu_M_income_log_median, theta=-20, phi=30, r=5, 
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Median Log Mu income M for", n_year))
  # 
  # persp(x_income,x_income,smooth_Mu_F_income_log_median, theta=-20, phi=30, r=5,
  #       shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed",
  #       col="cyan", xlab="Female income level",
  #       ylab="Male income level", zlab="Estimated value", 
  #       main=paste("Smooth Median Log Mu income F for", n_year))
  # 
  # 
  # 
  # 
  
  ####            EDUCATION 
  
  N_edu <- matrix(0, n_edu, n_edu)
  Mu_edu <- matrix(0, n_edu, n_edu)
  Mu_M_edu <- matrix(0, n_edu, n_edu)
  Mu_F_edu <- matrix(0, n_edu, n_edu)
  #log version
  Mu_edu_log<- matrix(0, n_edu, n_edu)
  Mu_M_edu_log <- matrix(0, n_edu, n_edu)
  Mu_F_edu_log <- matrix(0, n_edu, n_edu)
  
  for (i in 1:n_edu){
    for (j in 1:n_edu){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_EDUCD.x == i & Mu_all$NEW_EDUCD.y == j)
      
      total_folks = 1 # sum(current_sub$n)
      
      Mu_edu_log[i,j] = sum(current_sub$EV_log) / total_folks
      Mu_F_edu_log[i,j] = sum(current_sub$EV_female_log) / total_folks
      Mu_M_edu_log[i,j] = sum(current_sub$EV_male_log) / total_folks
      
      
      Mu_edu[i,j] = sum(current_sub$EV) / total_folks
      Mu_F_edu[i,j] = sum(current_sub$EV_female) / total_folks
      Mu_M_edu[i,j] = sum(current_sub$EV_male) / total_folks
      N_edu[i,j] = dim(current_sub)[1]
      
    }
  }
  
  x_edu=c(1:length(edu_grid))
  
  
  smooth_Mu_edu <- kernel2dsmooth(Mu_edu, kernel.type="disk", r=2)
  smooth_Mu_M_edu <- kernel2dsmooth(Mu_M_edu, kernel.type="disk", r=2)
  smooth_Mu_F_edu <- kernel2dsmooth(Mu_F_edu, kernel.type="disk", r=2)
  
  # persp(x_edu,x_edu,Mu_edu, theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("Mu Both Education", n_year))
  # 
  # persp(x_edu,x_edu,Mu_M_edu, theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("Male Education", n_year))
  # 
  # persp(x_edu,x_edu,Mu_F_edu, theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("Mu Female Income", n_year))
  # 
  # 
  # 
  # 
  # persp(x_edu,x_edu,smooth_Mu_edu, theta=-20, phi=30, r=5, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("Smooth Mu Both Education", n_year))
  # 
  # persp(x_edu,x_edu,smooth_Mu_M_edu, theta=-20, phi=30, r=5, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("Smooth Male Education", n_year))
  # 
  # persp(x_edu,x_edu,smooth_Mu_F_edu, theta=-20, phi=30, r=5,#theta=-20, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("Smooth Mu Female Education", n_year))
  # 
  # 
  # 
  
  
  
  
  ####            RACE 
  # 
  # N_race <- matrix(0, n_race, n_race)
  # Mu_race <- matrix(0, n_race, n_race)
  # Mu_M_race <- matrix(0, n_race, n_race)
  # Mu_F_race <- matrix(0, n_race, n_race)
  # #log version
  # Mu_race_log<- matrix(0, n_race, n_race)
  # Mu_M_race_log <- matrix(0, n_race, n_race)
  # Mu_F_race_log <- matrix(0, n_race, n_race)
  # 
  # for (i in 1:n_race){
  #   for (j in 1:n_race){
  #     #use pi
  #     current_sub <- subset(Mu_all, Mu_all$NEW_RACE.x == i & Mu_all$NEW_RACE.y == j)
  #     
  #     total_folks = 1 # sum(current_sub$n)
  #     
  #     Mu_race_log[i,j] = sum(current_sub$EV_log) / total_folks
  #     Mu_F_race_log[i,j] = sum(current_sub$EV_female_log) / total_folks
  #     Mu_M_race_log[i,j] = sum(current_sub$EV_male_log) / total_folks
  #     
  #     
  #     Mu_race[i,j] = sum(current_sub$EV) / total_folks
  #     Mu_F_race[i,j] = sum(current_sub$EV_female) / total_folks
  #     Mu_M_race[i,j] = sum(current_sub$EV_male) / total_folks
  #     N_race[i,j] = dim(current_sub)[1]
  #     
  #   }
  # }
  # 
  # x_race=c(1:length(race_grid))
  # 
  # 
  # smooth_Mu_race <- kernel2dsmooth(Mu_race, kernel.type="disk", r=2)
  # smooth_Mu_M_race <- kernel2dsmooth(Mu_M_race, kernel.type="disk", r=2)
  # smooth_Mu_F_race <- kernel2dsmooth(Mu_F_race, kernel.type="disk", r=2)
  # 
  # # persp(x_race,x_race,Mu_race, theta=120, phi=30, r=35, 
  # #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  # #       nticks=5, ticktype="detailed", 
  # #       col="cyan", xlab="Female Race",
  # #       ylab="Male Race", zlab="Estimated value", 
  # #       main=paste("Mu Both Race", n_year))
  # # 
  # # persp(x_race,x_race,Mu_M_race, theta=120, phi=30, r=35, 
  # #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  # #       nticks=5, ticktype="detailed", 
  # #       col="cyan", xlab="Female Race",
  # #       ylab="Male Race", zlab="Estimated value", 
  # #       main=paste("Male Race", n_year))
  # # 
  # # persp(x_race,x_race,Mu_F_race, theta=120, phi=30, r=35, 
  # #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  # #       nticks=5, ticktype="detailed", 
  # #       col="cyan", xlab="Female Race",
  # #       ylab="Male Race", zlab="Estimated value", 
  # #       main=paste("Mu Female Income", n_year))
  # # 
  # # 
  # # 
  # 
  # persp(x_race,x_race,smooth_Mu_race, theta=-20, phi=30, r=35, #theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Race",
  #       ylab="Male Race", zlab="Estimated value", 
  #       main=paste("Smooth Mu Both Race", n_year))
  # 
  # persp(x_race,x_race,smooth_Mu_M_race,theta=-20, phi=30, r=35, # theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Race",
  #       ylab="Male Race", zlab="Estimated value", 
  #       main=paste("Smooth Male Race", n_year))
  # 
  # persp(x_race,x_race,smooth_Mu_F_race, theta=-20, phi=30, r=35, #theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Race",
  #       ylab="Male Race", zlab="Estimated value", 
  #       main=paste("Smooth Mu Female Race", n_year))
  # 
  # 
  # 
  # 
  
  
  #tau
  Tau_age <- matrix(0, n_age, n_age)
  
  for (i in 1:n_age){
    for (j in 1:n_age){
      current_sub <- subset(Mu_all, Mu_all$NEW_AGE.x == i & Mu_all$NEW_AGE.y == j)
      total_folks =  1 # sum(current_sub$n)
      Tau_age[i,j] = sum(current_sub$tau) / total_folks
    }
  }

  x_age=c(1:length(age_grid))
  smooth_Tau_age <- kernel2dsmooth(Tau_age, kernel.type="disk", r=2)
  
  persp(x_age,x_age,smooth_Tau_age, theta=110, phi=30, r=35,
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        #zlim = c(0,2*max(smooth_Mu_age[!is.na(smooth_Mu_age)])), 
        ylab="Male age level", zlab="New marriage estimated value", 
        main=paste("Smooth Tau Age Both for", n_year))
  
  
  
  Tau_income <- matrix(0, n_income, n_income)
  
  for (i in 1:n_income){
    for (j in 1:n_income){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_INCTOT.x == i & Mu_all$NEW_INCTOT.y == j)
      
      total_folks = 1 # sum(current_sub$n)
      
      Tau_income[i,j] = sum(current_sub$tau) / total_folks
    }
  }
  
  x_income=c(1:length(income_grid))
  smooth_Tau_income <- kernel2dsmooth(Tau_income, kernel.type="disk", r=2)
  persp(x_income,x_income,smooth_Tau_income, theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="New marriage estimated value", 
        main=paste("Tau Both Income", n_year))
  
}

reg <- function(n_year){
  
  pair_data <- funk(n_year)$pair_data
  
  major_city=c(4610,3730,1190,5350,3110,5330,6290,2990,810)
  pair_data <- pair_data %>% mutate(major_city.x = case_when(CITY.x %in% major_city ~ 1,
                                                             !CITY.x %in% major_city ~ 0))
  pair_data <- pair_data %>% mutate(major_city.y = case_when(CITY.y %in% major_city ~ 1,
                                                             !CITY.y %in% major_city ~ 0))
  
  
  pair_data <- pair_data %>% mutate(big_city.x = case_when(CITY.x > 0 ~ 1,
                                                           !CITY.x > 0 ~ 0))
  pair_data <- pair_data %>% mutate(big_city.y = case_when(CITY.y > 0 ~ 1,
                                                           !CITY.y >0  ~ 0))
  
  
  pair_data$MV_new <- pair_data$MV*1000
  pair_data$MV_male_new <- pair_data$MV_male*1000
  pair_data$MV_female_new <- pair_data$MV_female*1000
  
  
  pair_data$MV_new_log <- pair_data$MV_log*1000
  pair_data$MV_male_new_log <- pair_data$MV_male_log*1000
  pair_data$MV_female_new_log <- pair_data$MV_female_log*1000
  
  
  pair_data$income_dif <- (pair_data$INCTOT.x-pair_data$INCTOT.y)/1000
  pair_data$income_dif_abs <- abs(pair_data$INCTOT.x-pair_data$INCTOT.y)/1000
  
  
  pair_data$income_cat_dif <- (as.integer(pair_data$NEW_INCTOT.x)-as.integer(pair_data$NEW_INCTOT.y))
  
  pair_data$income_cat_dif_abs <- abs(as.integer(pair_data$NEW_INCTOT.x)-as.integer(pair_data$NEW_INCTOT.y))
  
  
  #pair_data$INCTOT.x.new <- pair_data$INCTOT.x/1000
  #pair_data$INCTOT.y.new <- pair_data$INCTOT.y/1000
  
  #city_pair_data <- subset(pair_data,pair_data$CITYPOP.x != 0 & pair_data$CITYPOP.x != 99999)
  
  
  
  
  
  
  fit <- lm(MV_new_log ~ 
              NEW_AGE.x + NEW_AGE.y
            + NEW_INCTOT.x + NEW_INCTOT.y
            + NEW_EDUCD.x + NEW_EDUCD.y
            + NEW_RACE.x + NEW_RACE.y
            #+ age_dif
            #+ income_dif
            #+ income_dif_abs
            #+ big_city.x
            #+ STATEGINI.x
            , data=pair_data
            , weights = pair_data$HHW)
  
  
  fit_female <- lm(MV_female_new_log ~
                     NEW_AGE.x + NEW_AGE.y
                   + NEW_INCTOT.x + NEW_INCTOT.y
                   + NEW_EDUCD.x + NEW_EDUCD.y
                   + NEW_RACE.x + NEW_RACE.y
                   , data=pair_data
                   , weights = pair_data$HHW)
  fit_male <- lm(MV_male_new_log ~
                   NEW_AGE.x + NEW_AGE.y
                 + NEW_INCTOT.x + NEW_INCTOT.y
                 + NEW_EDUCD.x + NEW_EDUCD.y
                 + NEW_RACE.x + NEW_RACE.y
                 , data=pair_data
                 , weights = pair_data$HHW)
  fit_long <- 0
  
  summary(fit_long)

  
  
  output <- list("output", 
                 fit = fit,
                 fit_female = fit_female,
                 fit_male = fit_male
                 #fit_long = fit_long
                 
                 #,fit_long = fit_long
                 #,fit_female_long = fit_female_long
                 #,fit_male_long = fit_male_long
  )
  
  return(output)
  
}


funk_not_jm <- function(n_year){
  
  income_grid <<- m_income_grid
  age_grid <<- m_age_grid
  edu_grid <<- m_edu_grid
  race_grid <<- m_race_grid
  
  n_age <<- length(age_grid)
  n_income <<- length(income_grid)
  n_edu <<- length(edu_grid)
  n_race <<- length(race_grid)
  
  
  
  if (n_year!=2021){data <- subset(main_data,main_data$YEAR == n_year)}
  data$income <- data$INCTOT/1000
  
  
  
  if (n_year == 2021){
    states_codes <- as.integer(sort(unique(data$STATEICP)))
    n_states <- length(states_codes)
    last_state_code <- states_codes[n_states]
    states_gini <- rep(0,last_state_code)
    states_singles_gini <- rep(0,last_state_code)
    for (i in c(1:n_states)){
      state_code <- states_codes [i]
      #print(i)
      state_data <- subset(data, data$STATEICP == state_code )
      states_gini[state_code] <- gini.wtd(state_data$INCTOT, weights = NULL)
      state_single_data <- subset(data, data$STATEICP == state_code &
                                    (data$MARST == 6  | (data$MARST %in% c(1,2) & data$MARRINYR == 2))
      )
      states_singles_gini[state_code] <- gini.wtd(state_single_data$INCTOT, weights = NULL)
    }
    
    #states_gini
    
    data$STATEGINI <- states_gini[data$STATEICP]
    data$SINGLEGINI <- states_singles_gini[data$STATEICP]
    
    
    #with_city_data = subset(data,data$CITYPOP !=0)
    
    city_codes <- as.integer(sort(unique(data$CITY)))
    n_city <- length(city_codes)
    last_city_code <- city_codes[n_city]
    city_gini <- rep(0,last_city_code)
    city_single_gini <- rep(0,last_city_code)
    for (i in c(1:n_city)){
      city_code <- city_codes [i]
      #print(i)
      city_data <- subset(data, data$CITY == city_code )
      city_gini[city_code] <- gini.wtd(city_data$INCTOT, weights = NULL)
      city_single_data <- subset(data, data$CITY == city_code &
                                   (data$MARST == 6  | (data$MARST %in% c(1,2) & data$MARRINYR == 2))
      )
      city_single_gini[city_code] <- gini.wtd(city_single_data$INCTOT, weights = NULL)
    }
    data$CITYGINI <- 0
    data$CITYGINI[data$CITY>0] <- city_gini[data$CITY]
    data$CITYSINGLEGINI <- 0
    data$CITYSINGLEGINI[data$CITY>0] <- city_single_gini[data$CITY]
    
  }
  
  pool_data <- subset(data, (data$MARST == 6 &  data$AGE > 15 )
                      | data$RELATE %in% c(1,2)
                      | (data$MARST %in% c(1,2) 
                         & data$MARRINYR == 2 
                         & data$AGE > 15)
  )
  pool_data <- pool_data %>% mutate(out = case_when(MARRINYR ==2 ~ 1,
                                                    MARRINYR !=2 ~ 0))
  
  
  
  nm_data <- subset(data,data$MARST == 6 &  data$AGE > 15 )
  nmf_data <- subset(data,data$MARST == 6  & data$SEX == 2 & data$AGE > 15 )
  nmm_data <- subset(data,data$MARST == 6  & data$SEX == 1 & data$AGE > 15 )
  jm_data <- subset(data,data$MARST %in% c(1,2) 
                    #& data$MARRINYR != 2 # make it == for jm
                    & data$RELATE %in% c(1,2) # new
                    & data$AGE > 15 )
  jmf_data <- subset(data,data$MARST %in% c(1,2) 
                     #& data$MARRINYR != 2  # make it == for jm
                     & data$RELATE %in% c(1,2) # new
                     & data$SEX == 2 & data$AGE > 15)
  jmm_data <- subset(data,data$MARST %in% c(1,2) 
                     #& data$MARRINYR != 2 
                     & data$RELATE %in% c(1,2) # new
                     & data$SEX == 1 & data$AGE > 15)
  jm_pair_data <- merge(jmf_data,jmm_data,by=c("SERIAL"),all=FALSE)
  
  all_jm_pair_data <- merge(jm_data,jm_data,by=c("SERIAL"),all=FALSE)
  dim(all_jm_pair_data)
  
  all_jm_pair_data <- subset(all_jm_pair_data,all_jm_pair_data$SEX.x == all_jm_pair_data$SEX.y)
  dim(all_jm_pair_data)
  
  all_jm_pair_data <- subset(all_jm_pair_data,all_jm_pair_data$NUMPREC.x != all_jm_pair_data$NUMPREC.y)
  dim(all_jm_pair_data)
  
  
  #big_fam_jm_data <- subset(jm_data,jm_data$SERIAL_n > 2 )
  #big_fam_jm_count <- count(big_fam_jm_data,SERIAL)
  #colnames(big_fam_jm_count) <- c("SERIAL","jm_n")
  
  #big_fam_jm_data <- merge(big_fam_jm_data,big_fam_jm_count,by=c("SERIAL"))
  #print(dim(pair_data)[1]*2/dim(jm_data)[1]*100)
  
  pair_data <- jm_pair_data
  #dim(pair_data) # 12202
  single_data <- nm_data
  
  
  
  jm_data$NEW_INCTOT <- jm_data$INCTOT
  jm_data$NEW_INCTOT <- cut(jm_data$NEW_INCTOT,
                            breaks=c(-Inf,income_grid),
                            labels=seq(1:n_income))
  
  # XXX
  pair_data$NEW_AGE.x <- pair_data$AGE.x
  pair_data$NEW_AGE.x <- cut(pair_data$NEW_AGE.x,
                             breaks=c(-Inf,age_grid),
                             labels=seq(1:n_age))
  
  pair_data$NEW_INCTOT.x <- pair_data$INCTOT.x
  pair_data$NEW_INCTOT.x <- cut(pair_data$NEW_INCTOT.x,
                                breaks=c(-Inf,income_grid),
                                labels=seq(1:n_income))
  
  pair_data$NEW_EDUCD.x <- pair_data$EDUCD.x
  pair_data$NEW_EDUCD.x <- cut(pair_data$NEW_EDUCD.x,
                               breaks=c(-Inf, edu_grid),
                               labels=seq(1:n_edu))
  
  pair_data$NEW_RACE.x <- pair_data$RACE.x
  pair_data$NEW_RACE.x <- cut(pair_data$NEW_RACE.x,
                              breaks=c(-Inf, race_grid),
                              labels=seq(1:n_race))
  
  #YYY
  pair_data$NEW_AGE.y <- pair_data$AGE.y
  pair_data$NEW_AGE.y <- cut(pair_data$NEW_AGE.y,
                             breaks=c(-Inf,age_grid),
                             labels=seq(1:n_age))
  
  pair_data$NEW_INCTOT.y <- pair_data$INCTOT.y
  pair_data$NEW_INCTOT.y <- cut(pair_data$NEW_INCTOT.y,
                                breaks=c(-Inf,income_grid),
                                labels=seq(1:n_income))
  
  pair_data$NEW_EDUCD.y <- pair_data$EDUCD.y
  pair_data$NEW_EDUCD.y <- cut(pair_data$NEW_EDUCD.y,
                               breaks=c(-Inf, edu_grid),
                               labels=seq(1:n_edu))
  
  pair_data$NEW_RACE.y <- pair_data$RACE.y
  pair_data$NEW_RACE.y <- cut(pair_data$NEW_RACE.y,
                              breaks=c(-Inf, race_grid),
                              labels=seq(1:n_race))
  
  #pair_data <- na.omit(pair_data, c("NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x", "NEW_AGE.x", "NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y", "NEW_AGE.y")) 
  
  Mu_pair <- count(pair_data,
                   NEW_AGE.x,NEW_INCTOT.x,NEW_EDUCD.x,NEW_RACE.x,
                   NEW_AGE.y,NEW_INCTOT.y,NEW_EDUCD.y,NEW_RACE.y)#,.drop = FALSE)
  
  #Mu_pair=count(pair_data,
  #              NEW_AGE.x,NEW_INCTOT.x,NEW_EDUCD.x,NEW_RACE.x,
  #              NEW_AGE.y,NEW_INCTOT.y,NEW_EDUCD.y,NEW_RACE.y,.drop = FALSE)
  #Mu_pair
  #col <- colnames(pair_data)
  
  #       MALE
  single_male <- subset(single_data,single_data$SEX == 1)
  
  single_male$NEW_AGE <- single_male$AGE
  single_male$NEW_AGE <- as.integer(cut(single_male$NEW_AGE,
                                        breaks=c(-Inf,age_grid),
                                        labels=seq(1:n_age)))
  
  single_male$NEW_INCTOT <- single_male$INCTOT
  single_male$NEW_INCTOT <- as.integer(cut(single_male$NEW_INCTOT,
                                           breaks=c(-Inf,income_grid),
                                           labels=seq(1:n_income)))
  
  single_male$NEW_EDUCD <- single_male$EDUCD
  single_male$NEW_EDUCD <- as.integer(cut(single_male$NEW_EDUCD,
                                          breaks=c(-Inf, edu_grid),
                                          labels=seq(1:n_edu)))
  
  single_male$NEW_RACE <- single_male$RACE
  single_male$NEW_RACE <- as.integer(cut(single_male$NEW_RACE,
                                         breaks=c(-Inf, race_grid),
                                         labels=seq(1:n_race)))
  
  #single_male <- na.omit(single_male, c("NEW_INCTOT","NEW_EDUCD","NEW_RACE", "NEW_AGE")) 
  
  Mu_male <- count(single_male,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE) #,.drop = FALSE)
  #Mu_male=count(single_male,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE,.drop = FALSE)
  
  #Mu_male
  
  colnames(Mu_male) <- c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y","n_male")
  
  #        FEMALE
  single_female <- subset(single_data,single_data$SEX == 2)
  
  single_female$NEW_AGE <- single_female$AGE
  single_female$NEW_AGE <- as.integer(cut(single_female$NEW_AGE,
                                          breaks=c(-Inf,age_grid),
                                          labels=seq(1:n_age)))
  
  single_female$NEW_INCTOT <- single_female$INCTOT
  single_female$NEW_INCTOT <- as.integer(cut(single_female$NEW_INCTOT,
                                             breaks=c(-Inf,income_grid),
                                             labels=seq(1:n_income)))
  
  single_female$NEW_EDUCD <- single_female$EDUCD
  single_female$NEW_EDUCD <- as.integer(cut(single_female$NEW_EDUCD,
                                            breaks=c(-Inf, edu_grid),
                                            labels=seq(1:n_edu)))
  
  single_female$NEW_RACE <- single_female$RACE
  single_female$NEW_RACE <- as.integer(cut(single_female$NEW_RACE,
                                           breaks=c(-Inf, race_grid),
                                           labels=seq(1:n_race)))
  #single_female <- na.omit(single_female, c("NEW_INCTOT","NEW_EDUCD","NEW_RACE", "NEW_AGE")) 
  
  
  Mu_female <- count(single_female,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE) #,.drop = FALSE
  #Mu_female=count(single_female,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE,.drop = FALSE)
  
  #Mu_female
  
  colnames(Mu_female) <- c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x","n_female")
  #Mu_female
  #dim(Mu_pair) # 11810
  #Mu_all <- merge(Mu_pair,Mu_female,by=c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x"),all=FALSE)
  #Mu_all <- merge(Mu_all,Mu_male,by=c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y"),all=FALSE)
  #dim(Mu_all) # 10778
  Mu_all <- merge(Mu_pair,Mu_female,by=c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x"),all=TRUE)
  #dim(Mu_all) # 29350
  Mu_all <- merge(Mu_all,Mu_male,by=c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y"),all=TRUE)
  #dim(Mu_all) # 49157
  Mu_all <- subset(Mu_all,!is.na(Mu_all$n))
  #dim(Mu_all) # 11810
  
  #dim(pair_data) # same ; 12202
  
  
  #Mu_all$n[is.na(Mu_all$n_male)] <- -Mu_all$n
  
  
  Mu_all$n_male[is.na(Mu_all$n_male)] <- small_epsilon
  
  #dim(Mu_all$n_male[is.na(Mu_all$n_male))
  
  Mu_all$n_female[is.na(Mu_all$n_female)] <- small_epsilon
  
  #count(Mu_all,n_male == small_epsilon)
  #count(Mu_all,n_female == small_epsilon)
  
  #Mu_all
  
  
  #count(Mu_all, is.na(Mu_all$n_female) |  is.na(Mu_all$n_female))
  
  #Mu_all$MV <- 0
  #Mu_all$MV[is.na(Mu_all$n_female) | is.na(Mu_all$n_female)] <- 1
  
  Mu_all$MV <- ((Mu_all$n/Mu_all$n_male)*(Mu_all$n/Mu_all$n_female))^0.5 
  
  
  
  Mu_all$EV <- Mu_all$MV*Mu_all$n 
  
  
  Mu_all$MV_male <- Mu_all$n/Mu_all$n_male 
  #Mu_all$MV_male[Mu_all$n_male == small_epsilon] <- 1
  
  Mu_all$EV_male <- Mu_all$MV_male*Mu_all$n
  
  Mu_all$MV_female <- Mu_all$n/Mu_all$n_female
  #Mu_all$MV_female[Mu_all$fen_male == small_epsilon] <- 1
  Mu_all$EV_female <- Mu_all$MV_female*Mu_all$n 
  
  #log version
  Mu_all$MV_log <- log(Mu_all$MV)
  Mu_all$EV_log <- Mu_all$MV_log*Mu_all$n 
  
  Mu_all$MV_male_log <- log(Mu_all$MV_male)
  Mu_all$EV_male_log  <- Mu_all$MV_male_log*Mu_all$n
  
  Mu_all$MV_female_log <- log(Mu_all$MV_female) 
  Mu_all$EV_female_log  <- Mu_all$MV_female_log*Mu_all$n 
  
  Mu_all$tau <- (Mu_all$MV_male-Mu_all$MV_female)/2
  Mu_all$female_net_gain <- Mu_all$MV_female_log+Mu_all$tau
  Mu_all$male_net_gain <- Mu_all$MV_female_log-Mu_all$tau
  
  
  
  
  
  Mu_all$theta <- (Mu_all$tau-min(Mu_all$tau))/(max(Mu_all$tau)-min(Mu_all$tau))

  #pair_data$tau <- (pair_data$MV_male-pair_data$MV_female)/2
  
  #count(pair_data,is.na(pair_data$MV_log))
  
  col_names <- c("NEW_INCTOT.x", "NEW_INCTOT.y","NEW_AGE.x", "NEW_AGE.y",
                 "NEW_RACE.x", "NEW_RACE.y","NEW_EDUCD.x", "NEW_EDUCD.y")
  
  pair_data <- merge(pair_data, Mu_all, by.x=col_names,
                     by.y=col_names,all.x=TRUE)
  
  pair_data <- pair_data %>%
    set_variable_labels(
      NEW_AGE.x = "Female Age",
      NEW_AGE.y = "Male Age",
      NEW_INCTOT.x = "Female Income",
      NEW_INCTOT.y = "Male Income",
      NEW_EDUCD.x = "Female Education",
      NEW_EDUCD.y = "Male Education",
      NEW_RACE.x = "Female Race",
      NEW_RACE.y = "Male Race"
    )
  
  pair_data$age_dif <- pair_data$AGE.x-pair_data$AGE.y
  pair_data$income_dif <- pair_data$INCTOT.x-pair_data$INCTOT.y
  pair_data$edu_dif <- pair_data$EDUCD.x-pair_data$EDUCD.y
  
  pair_data$new_age_dif <- as.integer(pair_data$NEW_AGE.x)-as.integer(pair_data$NEW_AGE.y)
  pair_data$new_income_dif <- as.integer(pair_data$NEW_INCTOT.x)-as.integer(pair_data$NEW_INCTOT.y)
  pair_data$new_edu_dif <- as.integer(pair_data$NEW_EDUCD.x)-as.integer(pair_data$NEW_EDUCD.y)
  
  pair_data$big_age_gap <- 0
  pair_data$big_age_gap[abs(pair_data$age_dif)>big_age] <- 1
  
  pair_data$cut_age_dif <- cut(pair_data$age_dif,
                               breaks=c(0,10,20,30,40,50,60,70),
                               labels=c(0,1,2,3,4,5,6))
  # Nash bargenning -> tau=(alpha-gamma)/2
  
  
  pair_data$theta_prime <- pair_data$theta/(1-pair_data$theta+0.01)
  pair_data$c_m <- 1/(pair_data$dad.y+0.01)
  pair_data$c_f <- 1/(pair_data$dad.x+0.01)
  pair_data$c_t <- pair_data$c_m+pair_data$c_f
  pair_data$LHS <- pair_data$c_m - pair_data$theta_prime*pair_data$c_f
  pair_data$RHS <- - pair_data$theta_prime*pair_data$c_t
  
  
  
  pair_data$rate <- F_gumbel(-pair_data$MV_log)
  pair_data$rate.x <- F_gumbel(-pair_data$MV_female)
  pair_data$rate.y <- F_gumbel(-pair_data$MV_male)
  
  pair_data <- pair_data %>% mutate(major_city.x = case_when(CITY.x %in% major_city ~ 1,
                                                             !CITY.x %in% major_city ~ 0))
  pair_data <- pair_data %>% mutate(major_city.y = case_when(CITY.y %in% major_city ~ 1,
                                                             !CITY.y %in% major_city ~ 0))
  
  
  pair_data <- pair_data %>% mutate(big_city.x = case_when(CITY.x > 0 ~ 1,
                                                           !CITY.x > 0 ~ 0))
  pair_data <- pair_data %>% mutate(big_city.y = case_when(CITY.y > 0 ~ 1,
                                                           !CITY.y >0  ~ 0))
  
  pair_data$f_income_share <- pair_data$income.x/(pair_data$income.x+pair_data$income.y)
  
  output <- list("output",
                 pair_data = pair_data,
                 Mu_all = Mu_all)
  
  return(output)
}



#calculate
co_funk <- function(n_year){
  
  income_grid <<- co_income_grid
  age_grid <<- co_age_grid
  edu_grid <<- co_edu_grid
  race_grid <<- co_race_grid
  
  n_age <<- length(age_grid)
  n_income <<- length(income_grid)
  n_edu <<- length(edu_grid)
  n_race <<- length(race_grid)
  
  
  
  
  if (n_year!=-1){data <- subset(main_data,main_data$YEAR == n_year)}
  data$income <- data$INCTOT/1000
  #
  #
  
  if (n_year==2021){
    states_codes <- as.integer(sort(unique(data$STATEICP)))
    n_states <- length(states_codes)
    last_state_code <- states_codes[n_states]
    states_gini <- rep(0,last_state_code)
    states_singles_gini <- rep(0,last_state_code)
    for (i in c(1:n_states)){
      state_code <- states_codes [i]
      #print(i)
      state_data <- subset(data, data$STATEICP == state_code )
      states_gini[state_code] <- gini.wtd(state_data$INCTOT, weights = NULL)
      state_single_data <- subset(data, data$STATEICP == state_code &
                                    (data$MARST == 6  | (data$MARST %in% c(1,2) & data$MARRINYR == 2))
      )
      states_singles_gini[state_code] <- gini.wtd(state_single_data$INCTOT, weights = NULL)
    }
    
    #states_gini
    
    data$STATEGINI <- states_gini[data$STATEICP]
    data$SINGLEGINI <- states_singles_gini[data$STATEICP]
    
    
    #with_city_data = subset(data,data$CITYPOP !=0)
    
    city_codes <- as.integer(sort(unique(data$CITY)))
    n_city <- length(city_codes)
    last_city_code <- city_codes[n_city]
    city_gini <- rep(0,last_city_code)
    city_single_gini <- rep(0,last_city_code)
    for (i in c(1:n_city)){
      city_code <- city_codes [i]
      #print(i)
      city_data <- subset(data, data$CITY == city_code )
      city_gini[city_code] <- gini.wtd(city_data$INCTOT, weights = NULL)
      city_single_data <- subset(data, data$CITY == city_code &
                                   (data$MARST == 6  | (data$MARST %in% c(1,2) & data$MARRINYR == 2))
      )
      city_single_gini[city_code] <- gini.wtd(city_single_data$INCTOT, weights = NULL)
    }
    data$CITYGINI <- 0
    data$CITYGINI[data$CITY>0] <- city_gini[data$CITY]
    data$CITYSINGLEGINI <- 0
    data$CITYSINGLEGINI[data$CITY>0] <- city_single_gini[data$CITY]
    
  }
  
  jm_data <- co_data <- subset(data,data$COUPLETYPE == 3 & data$MARST == 6 & data$AGE > 15 )
  co_f_data <- subset(co_data,co_data$SEX == 2)
  co_m_data <- subset(co_data,co_data$SEX == 1)
  
  nm_data <- not_co_data <- subset(data,(
    (data$MARST %in% c(1,2) & data$COUPLETYPE %in% c(1) & data$MARRINYR == 2)
    | (data$MARST ==6 & data$COUPLETYPE == 0 )
  )
  & data$AGE > 15 )
  
  not_co_f_data <- subset(not_co_data,not_co_data$SEX == 2)
  not_co_m_data <- subset(not_co_data,not_co_data$SEX == 1)
  
  co_pair_data <- merge(co_f_data,co_m_data,by=c("SERIAL"),all=FALSE)
  
  pair_data <- co_pair_data
  #dim(pair_data) # 12202
  single_data <- not_co_data
  
  
  jm_data$NEW_INCTOT <- jm_data$INCTOT
  jm_data$NEW_INCTOT <- cut(jm_data$NEW_INCTOT,
                            breaks=c(-Inf,income_grid),
                            labels=seq(1:n_income))
  
  # XXX
  pair_data$NEW_AGE.x <- pair_data$AGE.x
  pair_data$NEW_AGE.x <- cut(pair_data$NEW_AGE.x,
                             breaks=c(-Inf,age_grid),
                             labels=seq(1:n_age))
  
  pair_data$NEW_INCTOT.x <- pair_data$INCTOT.x
  pair_data$NEW_INCTOT.x <- cut(pair_data$NEW_INCTOT.x,
                                breaks=c(-Inf,income_grid),
                                labels=seq(1:n_income))
  
  pair_data$NEW_EDUCD.x <- pair_data$EDUCD.x
  pair_data$NEW_EDUCD.x <- cut(pair_data$NEW_EDUCD.x,
                               breaks=c(-Inf, edu_grid),
                               labels=seq(1:n_edu))
  
  pair_data$NEW_RACE.x <- pair_data$RACE.x
  pair_data$NEW_RACE.x <- cut(pair_data$NEW_RACE.x,
                              breaks=c(-Inf, race_grid),
                              labels=seq(1:n_race))
  
  #YYY
  pair_data$NEW_AGE.y <- pair_data$AGE.y
  pair_data$NEW_AGE.y <- cut(pair_data$NEW_AGE.y,
                             breaks=c(-Inf,age_grid),
                             labels=seq(1:n_age))
  
  pair_data$NEW_INCTOT.y <- pair_data$INCTOT.y
  pair_data$NEW_INCTOT.y <- cut(pair_data$NEW_INCTOT.y,
                                breaks=c(-Inf,income_grid),
                                labels=seq(1:n_income))
  
  pair_data$NEW_EDUCD.y <- pair_data$EDUCD.y
  pair_data$NEW_EDUCD.y <- cut(pair_data$NEW_EDUCD.y,
                               breaks=c(-Inf, edu_grid),
                               labels=seq(1:n_edu))
  
  pair_data$NEW_RACE.y <- pair_data$RACE.y
  pair_data$NEW_RACE.y <- cut(pair_data$NEW_RACE.y,
                              breaks=c(-Inf, race_grid),
                              labels=seq(1:n_race))
  
  #pair_data <- na.omit(pair_data, c("NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x", "NEW_AGE.x", "NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y", "NEW_AGE.y")) 
  
  Mu_pair <- count(pair_data,
                   NEW_AGE.x,NEW_INCTOT.x,NEW_EDUCD.x,NEW_RACE.x,
                   NEW_AGE.y,NEW_INCTOT.y,NEW_EDUCD.y,NEW_RACE.y)#,.drop = FALSE)
  
  
  
  
  #       MALE
  single_male <- subset(single_data,single_data$SEX == 1)
  
  single_male$NEW_AGE <- single_male$AGE
  single_male$NEW_AGE <- cut(single_male$NEW_AGE,
                             breaks=c(-Inf,age_grid),
                             labels=seq(1:n_age))
  
  single_male$NEW_INCTOT <- single_male$INCTOT
  single_male$NEW_INCTOT <- cut(single_male$NEW_INCTOT,
                                breaks=c(-Inf,income_grid),
                                labels=seq(1:n_income))
  
  single_male$NEW_EDUCD <- single_male$EDUCD
  single_male$NEW_EDUCD <- cut(single_male$NEW_EDUCD,
                               breaks=c(-Inf, edu_grid),
                               labels=seq(1:n_edu))
  
  single_male$NEW_RACE <- single_male$RACE
  single_male$NEW_RACE <- cut(single_male$NEW_RACE,
                              breaks=c(-Inf, race_grid),
                              labels=seq(1:n_race))
  
  #single_male <- na.omit(single_male, c("NEW_INCTOT","NEW_EDUCD","NEW_RACE", "NEW_AGE")) 
  
  Mu_male <- count(single_male,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE) #,.drop = FALSE)
  #Mu_male=count(single_male,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE,.drop = FALSE)
  
  #Mu_male
  
  colnames(Mu_male) <- c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y","n_male")
  
  #        FEMALE
  single_female <- subset(single_data,single_data$SEX == 2)
  
  single_female$NEW_AGE <- single_female$AGE
  single_female$NEW_AGE <- cut(single_female$NEW_AGE,
                               breaks=c(-Inf,age_grid),
                               labels=seq(1:n_age))
  
  single_female$NEW_INCTOT <- single_female$INCTOT
  single_female$NEW_INCTOT <- cut(single_female$NEW_INCTOT,
                                  breaks=c(-Inf,income_grid),
                                  labels=seq(1:n_income))
  
  single_female$NEW_EDUCD <- single_female$EDUCD
  single_female$NEW_EDUCD <- cut(single_female$NEW_EDUCD,
                                 breaks=c(-Inf, edu_grid),
                                 labels=seq(1:n_edu))
  
  single_female$NEW_RACE <- single_female$RACE
  single_female$NEW_RACE <- cut(single_female$NEW_RACE,
                                breaks=c(-Inf, race_grid),
                                labels=seq(1:n_race))
  #single_female <- na.omit(single_female, c("NEW_INCTOT","NEW_EDUCD","NEW_RACE", "NEW_AGE")) 
  
  
  Mu_female <- count(single_female,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE) #,.drop = FALSE
  #Mu_female=count(single_female,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE,.drop = FALSE)
  
  #Mu_female
  
  colnames(Mu_female) <- c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x","n_female")
  #Mu_female
  #dim(Mu_pair) # 11810
  #Mu_all <- merge(Mu_pair,Mu_female,by=c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x"),all=FALSE)
  #Mu_all <- merge(Mu_all,Mu_male,by=c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y"),all=FALSE)
  #dim(Mu_all) # 10778
  Mu_all <- merge(Mu_pair,Mu_female,by=c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x"),all=TRUE)
  #dim(Mu_all) # 29350
  Mu_all <- merge(Mu_all,Mu_male,by=c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y"),all=TRUE)
  #dim(Mu_all) # 49157
  Mu_all <- subset(Mu_all,!is.na(Mu_all$n))
  #dim(Mu_all) # 11810
  
  #dim(pair_data) # same ; 12202
  
  
  #Mu_all$n[is.na(Mu_all$n_male)] <- -Mu_all$n
  
  
  Mu_all$n_male[is.na(Mu_all$n_male)] <- small_epsilon
  
  #dim(Mu_all$n_male[is.na(Mu_all$n_male))
  
  Mu_all$n_female[is.na(Mu_all$n_female)] <- small_epsilon
  
  #count(Mu_all,n_male == small_epsilon)
  #count(Mu_all,n_female == small_epsilon)
  
  #Mu_all
  
  
  #count(Mu_all, is.na(Mu_all$n_female) |  is.na(Mu_all$n_female))
  
  #Mu_all$MV <- 0
  #Mu_all$MV[is.na(Mu_all$n_female) | is.na(Mu_all$n_female)] <- 1
  
  Mu_all$MV <- ((Mu_all$n/Mu_all$n_male)*(Mu_all$n/Mu_all$n_female))^0.5 
  
  #count(Mu_all, is.na(Mu_all$MV))
  #count(Mu_all, Mu_all$MV ==1)
  
  #Mu_all$MV[Mu_all$n_male == small_epsilon | Mu_all$n_female == small_epsilon] <- 1
  #Mu_all$MV[is.na(Mu_all$n_female) | is.na(Mu_all$n_female)] <- 1
  
  #Mu_all$MV <- (Mu_all$n/Mu_all$n_male*Mu_all$n/Mu_all$n_female)^0.5 
  Mu_all$EV <- Mu_all$MV*Mu_all$n 
  
  
  Mu_all$MV_male <- Mu_all$n/Mu_all$n_male 
  #Mu_all$MV_male[Mu_all$n_male == small_epsilon] <- 1
  
  Mu_all$EV_male <- Mu_all$MV_male*Mu_all$n
  
  Mu_all$MV_female <- Mu_all$n/Mu_all$n_female
  #Mu_all$MV_female[Mu_all$fen_male == small_epsilon] <- 1
  Mu_all$EV_female <- Mu_all$MV_female*Mu_all$n 
  
  #log version
  Mu_all$MV_log <- log(Mu_all$MV)
  Mu_all$EV_log <- Mu_all$MV_log*Mu_all$n 
  
  Mu_all$MV_male_log <- log(Mu_all$MV_male)
  Mu_all$EV_male_log  <- Mu_all$MV_male_log*Mu_all$n
  
  Mu_all$MV_female_log <- log(Mu_all$MV_female) 
  Mu_all$EV_female_log  <- Mu_all$MV_female_log*Mu_all$n 
  
  Mu_all$tau <- (Mu_all$MV_male-Mu_all$MV_female)/2
  Mu_all$female_net_gain <- Mu_all$MV_female_log+Mu_all$tau
  Mu_all$male_net_gain <- Mu_all$MV_female_log-Mu_all$tau
  
  
  Mu_all$theta <- (Mu_all$tau-min(Mu_all$tau))/(max(Mu_all$tau)-min(Mu_all$tau))
  
  #count(pair_data,is.na(pair_data$MV_log))
  
  col_names <- c("NEW_INCTOT.x", "NEW_INCTOT.y","NEW_AGE.x", "NEW_AGE.y",
                 "NEW_RACE.x", "NEW_RACE.y","NEW_EDUCD.x", "NEW_EDUCD.y")
  
  pair_data <- merge(pair_data, Mu_all, by.x=col_names,
                     by.y=col_names,all.x=TRUE)
  
  pair_data <- pair_data %>%
    set_variable_labels(
      NEW_AGE.x = "Female Age",
      NEW_AGE.y = "Male Age",
      NEW_INCTOT.x = "Female Income",
      NEW_INCTOT.y = "Male Income",
      NEW_EDUCD.x = "Female Education",
      NEW_EDUCD.y = "Male Education",
      NEW_RACE.x = "Female Race",
      NEW_RACE.y = "Male Race"
    )
  
  pair_data$age_dif <- pair_data$AGE.x-pair_data$AGE.y
  pair_data$income_dif <- pair_data$INCTOT.x-pair_data$INCTOT.y
  pair_data$edu_dif <- pair_data$EDUCD.x-pair_data$EDUCD.y
  
  pair_data$new_age_dif <- as.integer(pair_data$NEW_AGE.x)-as.integer(pair_data$NEW_AGE.y)
  pair_data$new_income_dif <- as.integer(pair_data$NEW_INCTOT.x)-as.integer(pair_data$NEW_INCTOT.y)
  pair_data$new_edu_dif <- as.integer(pair_data$NEW_EDUCD.x)-as.integer(pair_data$NEW_EDUCD.y)
  
  pair_data$big_age_gap <- 0
  pair_data$big_age_gap[abs(pair_data$age_dif)>big_age] <- 1
  
  pair_data$cut_age_dif <- cut(pair_data$age_dif,
                               breaks=c(0,10,20,30,40,50,60,70),
                               labels=c(0,1,2,3,4,5,6))
  
  pair_data$rate <- F_gumbel(-pair_data$MV_log)
  pair_data$rate.x <- F_gumbel(-pair_data$MV_female)
  pair_data$rate.y <- F_gumbel(-pair_data$MV_male)
  
  
  pair_data$theta_prime <- pair_data$theta/(1-pair_data$theta+0.01)
  pair_data$c_m <- 1/(pair_data$dad.y+0.01)
  pair_data$c_f <- 1/(pair_data$dad.x+0.01)
  pair_data$c_t <- pair_data$c_m+pair_data$c_f
  pair_data$LHS <- pair_data$c_m - pair_data$theta_prime*pair_data$c_f
  pair_data$RHS <- - pair_data$theta_prime*pair_data$c_t
  
  
  pair_data <- pair_data %>% mutate(major_city.x = case_when(CITY.x %in% major_city ~ 1,
                                                             !CITY.x %in% major_city ~ 0))
  pair_data <- pair_data %>% mutate(major_city.y = case_when(CITY.y %in% major_city ~ 1,
                                                             !CITY.y %in% major_city ~ 0))
  
  
  pair_data <- pair_data %>% mutate(big_city.x = case_when(CITY.x > 0 ~ 1,
                                                           !CITY.x > 0 ~ 0))
  pair_data <- pair_data %>% mutate(big_city.y = case_when(CITY.y > 0 ~ 1,
                                                           !CITY.y >0  ~ 0))
  
  pair_data$f_income_share <- pair_data$income.x/(pair_data$income.x+pair_data$income.y)
  
  output <- list("output",
                 pair_data = pair_data,
                 Mu_all = Mu_all)
  
  return(output)
}
#draw 1st

co_draw <- function(n_year){
  
  income_grid <<- co_income_grid
  age_grid <<- co_age_grid
  edu_grid <<- co_edu_grid
  race_grid <<- co_race_grid
  
  n_age <<- length(age_grid)
  n_income <<- length(income_grid)
  n_edu <<- length(edu_grid)
  n_race <<- length(race_grid)
  
  
  store <- co_funk(n_year)
  Mu_all <- store$Mu_all
  pair_data <- store$pair_data
  
  
  
  ####            AGE 
  
  
  
  N_Age <- matrix(0, n_age, n_age)
  Mu_age <- matrix(0, n_age, n_age)
  Mu_M_age <- matrix(0, n_age, n_age)
  Mu_F_age <- matrix(0, n_age, n_age)
  #log version
  Mu_age_log<- matrix(0, n_age, n_age)
  Mu_M_age_log <- matrix(0, n_age, n_age)
  Mu_F_age_log <- matrix(0, n_age, n_age)
  
  for (i in 1:n_age){
    for (j in 1:n_age){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_AGE.x == i & Mu_all$NEW_AGE.y == j)
      
      total_folks = 1 #sum(current_sub$n)
      
      Mu_age_log[i,j] = sum(current_sub$EV_log) / total_folks
      Mu_F_age_log[i,j] = sum(current_sub$EV_female_log) / total_folks
      Mu_M_age_log[i,j] = sum(current_sub$EV_male_log) / total_folks
      
      Mu_age[i,j] = sum(current_sub$EV) / total_folks
      Mu_F_age[i,j] = sum(current_sub$EV_female) / total_folks
      Mu_M_age[i,j] = sum(current_sub$EV_male) / total_folks
      N_Age[i,j] = dim(current_sub)[1]
      
    }
  }
  
  
  x_age=c(1:length(age_grid))
  
  
  
  
  
  smooth_Mu_age <- kernel2dsmooth(Mu_age, kernel.type="disk", r=2)
  smooth_Mu_M_age <- kernel2dsmooth(Mu_M_age, kernel.type="disk", r=2)
  smooth_Mu_F_age <- kernel2dsmooth(Mu_F_age, kernel.type="disk", r=2)
  smooth_N_Age <- kernel2dsmooth(N_Age, kernel.type="disk", r=2)
  
  persp(x_age,x_age,smooth_Mu_age, theta=-30, phi=30, r=5,#theta=130, phi=30, r=5,#theta=-40, phi=30, r=35, #theta=110, phi=30, r=35,
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        #zlim = c(0,2*max(smooth_Mu_age[!is.na(smooth_Mu_age)])), 
        ylab="Male age level", zlab="Estimated mutual gain", 
        main=paste("Cohab; Smooth Mu Age Both for", n_year))
  
  
  persp(x_age,x_age,smooth_Mu_M_age, theta=-30, phi=30, r=5,#theta=130, phi=30, r=5, #theta=110, phi=30, r=35, 
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        ylab="Male age level", zlab="Estimated male gain", 
        main=paste("Cohab; Smooth Mu Age M for", n_year))
  
  persp(x_age,x_age,smooth_Mu_F_age, theta=-30, phi=30, r=5,#theta=130, phi=30, r=5,#theta=-20, phi=30, r=35, #theta=110, phi=30, r=35, 
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        ylab="Male age level", zlab="Estimated female gain", 
        main=paste("Cohab; Smooth  Mu Age F for", n_year))
  
  persp(x_age,x_age,smooth_N_Age, theta=-30, phi=30, r=5,#theta=130, phi=30, r=5,#theta=-20, phi=30, r=35, #theta=110, phi=30, r=35, 
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        ylab="Male age level", zlab="Frequency", 
        main=paste("Cohab; Smooth  Mu Age F for", n_year))
  
  
  
  
  ####            INCOME 
  
  N_income <- matrix(0, n_income, n_income)
  Mu_income <- matrix(0, n_income, n_income)
  Mu_M_income <- matrix(0, n_income, n_income)
  Mu_F_income <- matrix(0, n_income, n_income)
  #log version
  Mu_income_log<- matrix(0, n_income, n_income)
  Mu_M_income_log <- matrix(0, n_income, n_income)
  Mu_F_income_log <- matrix(0, n_income, n_income)
  
  for (i in 1:n_income){
    for (j in 1:n_income){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_INCTOT.x == i & Mu_all$NEW_INCTOT.y == j)
      
      total_folks = 1 # sum(current_sub$n)
      
      Mu_income_log[i,j] = sum(current_sub$EV_log) / total_folks
      Mu_F_income_log[i,j] = sum(current_sub$EV_female_log) / total_folks
      Mu_M_income_log[i,j] = sum(current_sub$EV_male_log) / total_folks
      
      
      Mu_income[i,j] = sum(current_sub$EV) / total_folks
      Mu_F_income[i,j] = sum(current_sub$EV_female) / total_folks
      Mu_M_income[i,j] = sum(current_sub$EV_male) / total_folks
      N_income[i,j] = dim(current_sub)[1]
      
    }
  }
  
  x_income=c(1:length(income_grid))
  
  
  smooth_Mu_income <- kernel2dsmooth(Mu_income, kernel.type="disk", r=2)
  smooth_Mu_M_income <- kernel2dsmooth(Mu_M_income, kernel.type="disk", r=2)
  smooth_Mu_F_income <- kernel2dsmooth(Mu_F_income, kernel.type="disk", r=2)
  smooth_N_income <- kernel2dsmooth(N_income, kernel.type="disk", r=2)
  
  
  
  
  
  persp(x_income,x_income,smooth_Mu_income, theta=-40, phi=30, r=5,#theta=-20, phi=30, r=35, # theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated mutual gain", 
        main=paste("Cohab; Mu Both Income", n_year))
  
  persp(x_income,x_income,smooth_Mu_M_income,theta=-40, phi=30, r=5,# theta=-20, phi=30, r=35, #theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated male gain", 
        main=paste("Cohab; Male Income", n_year))
  
  persp(x_income,x_income,smooth_Mu_F_income, theta=-40, phi=30, r=5,#theta=-20, phi=30, r=35, #theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated female gain", 
        main=paste("Cohab; Mu Female Income", n_year))
  
  persp(x_income,x_income,smooth_N_income, theta=-40, phi=30, r=5,#theta=-20, phi=30, r=35, #theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Frequency", 
        main=paste("Cohab; Mu Female Income", n_year))
  
  
  
  ####            EDUCATION 
  # 
  # N_edu <- matrix(0, n_edu, n_edu)
  # Mu_edu <- matrix(0, n_edu, n_edu)
  # Mu_M_edu <- matrix(0, n_edu, n_edu)
  # Mu_F_edu <- matrix(0, n_edu, n_edu)
  # #log version
  # Mu_edu_log<- matrix(0, n_edu, n_edu)
  # Mu_M_edu_log <- matrix(0, n_edu, n_edu)
  # Mu_F_edu_log <- matrix(0, n_edu, n_edu)
  # 
  # for (i in 1:n_edu){
  #   for (j in 1:n_edu){
  #     #use pi
  #     current_sub <- subset(Mu_all, Mu_all$NEW_EDUCD.x == i & Mu_all$NEW_EDUCD.y == j)
  #     
  #     total_folks = 1 # sum(current_sub$n)
  #     
  #     Mu_edu_log[i,j] = sum(current_sub$EV_log) / total_folks
  #     Mu_F_edu_log[i,j] = sum(current_sub$EV_female_log) / total_folks
  #     Mu_M_edu_log[i,j] = sum(current_sub$EV_male_log) / total_folks
  #     
  #     
  #     Mu_edu[i,j] = sum(current_sub$EV) / total_folks
  #     Mu_F_edu[i,j] = sum(current_sub$EV_female) / total_folks
  #     Mu_M_edu[i,j] = sum(current_sub$EV_male) / total_folks
  #     N_edu[i,j] = dim(current_sub)[1]
  #     
  #   }
  # }
  # 
  # x_edu=c(1:length(edu_grid))
  # 
  # 
  # smooth_Mu_edu <- kernel2dsmooth(Mu_edu, kernel.type="disk", r=2)
  # smooth_Mu_M_edu <- kernel2dsmooth(Mu_M_edu, kernel.type="disk", r=2)
  # smooth_Mu_F_edu <- kernel2dsmooth(Mu_F_edu, kernel.type="disk", r=2)
  # 
  # 
  # 
  # 
  # persp(x_edu,x_edu,smooth_Mu_edu, theta=130, phi=30, r=5,#theta=-20, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("Cohab; Smooth Mu Both Education", n_year))
  # 
  # persp(x_edu,x_edu,smooth_Mu_M_edu, theta=130, phi=30, r=5,#theta=-20, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("Cohab; Smooth Male Education", n_year))
  # 
  # persp(x_edu,x_edu,smooth_Mu_F_edu, theta=130, phi=30, r=5,#theta=-20, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("Cohab; Smooth Mu Female Education", n_year))
  # 
  # ####            RACE 
  # 
  # N_race <- matrix(0, n_race, n_race)
  # Mu_race <- matrix(0, n_race, n_race)
  # Mu_M_race <- matrix(0, n_race, n_race)
  # Mu_F_race <- matrix(0, n_race, n_race)
  # #log version
  # Mu_race_log<- matrix(0, n_race, n_race)
  # Mu_M_race_log <- matrix(0, n_race, n_race)
  # Mu_F_race_log <- matrix(0, n_race, n_race)
  # 
  # for (i in 1:n_race){
  #   for (j in 1:n_race){
  #     #use pi
  #     current_sub <- subset(Mu_all, Mu_all$NEW_RACE.x == i & Mu_all$NEW_RACE.y == j)
  #     
  #     total_folks = 1 # sum(current_sub$n)
  #     
  #     Mu_race_log[i,j] = sum(current_sub$EV_log) / total_folks
  #     Mu_F_race_log[i,j] = sum(current_sub$EV_female_log) / total_folks
  #     Mu_M_race_log[i,j] = sum(current_sub$EV_male_log) / total_folks
  #     
  #     
  #     Mu_race[i,j] = sum(current_sub$EV) / total_folks
  #     Mu_F_race[i,j] = sum(current_sub$EV_female) / total_folks
  #     Mu_M_race[i,j] = sum(current_sub$EV_male) / total_folks
  #     N_race[i,j] = dim(current_sub)[1]
  #     
  #   }
  # }
  # 
  # x_race=c(1:length(race_grid))
  # 
  # 
  # smooth_Mu_race <- kernel2dsmooth(Mu_race, kernel.type="disk", r=2)
  # smooth_Mu_M_race <- kernel2dsmooth(Mu_M_race, kernel.type="disk", r=2)
  # smooth_Mu_F_race <- kernel2dsmooth(Mu_F_race, kernel.type="disk", r=2)
  # 
  # 
  # 
  # persp(x_race,x_race,smooth_Mu_race, theta=130, phi=30, r=5,#theta=-20, phi=30, r=35, #theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Race",
  #       ylab="Male Race", zlab="Estimated value", 
  #       main=paste("Smooth Mu Both Race", n_year))
  # 
  # persp(x_race,x_race,smooth_Mu_M_race, theta=130, phi=30, r=5,#theta=-20, phi=30, r=35, #theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Race",
  #       ylab="Male Race", zlab="Estimated value", 
  #       main=paste("Smooth Male Race", n_year))
  # 
  # persp(x_race,x_race,smooth_Mu_F_race, theta=130, phi=30, r=5,#theta=-20, phi=30, r=35, #theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Race",
  #       ylab="Male Race", zlab="Estimated value", 
  #       main=paste("Smooth Mu Female Race", n_year))
  # 
  
  #tau
  Tau_age <- matrix(0, n_age, n_age)
  
  for (i in 1:n_age){
    for (j in 1:n_age){
      current_sub <- subset(Mu_all, Mu_all$NEW_AGE.x == i & Mu_all$NEW_AGE.y == j)
      total_folks = 1 # sum(current_sub$n)
      Tau_age[i,j] = sum(current_sub$tau) / total_folks
    }
  }
  
  x_age=c(1:length(age_grid))
  smooth_Tau_age <- kernel2dsmooth(Tau_age, kernel.type="disk", r=2)
  
  persp(x_age,x_age,smooth_Tau_age, theta=110, phi=30, r=35,
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        #zlim = c(0,2*max(smooth_Mu_age[!is.na(smooth_Mu_age)])), 
        ylab="Male age level", zlab="Cohabitation estimated value", 
        main=paste("CO Tau Age for", n_year))
  
  
  
  Tau_income <- matrix(0, n_income, n_income)
  
  for (i in 1:n_income){
    for (j in 1:n_income){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_INCTOT.x == i & Mu_all$NEW_INCTOT.y == j)
      
      total_folks = 1# sum(current_sub$n)
      
      Tau_income[i,j] = sum(current_sub$tau) / total_folks
    }
  }
  
  x_income=c(1:length(income_grid))
  smooth_Tau_income <- kernel2dsmooth(Tau_income, kernel.type="disk", r=2)
  persp(x_income,x_income,smooth_Tau_income, theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Cohabitation estimated value", 
        main=paste("CO Tau Income", n_year))
  
}

#reg 1st
co_reg <- function(n_year){
  
  pair_data <- co_funk(n_year)$pair_data
  
  major_city=c(4610,3730,1190,5350,3110,5330,6290,2990,810)
  pair_data <- pair_data %>% mutate(major_city.x = case_when(CITY.x %in% major_city ~ 1,
                                                             !CITY.x %in% major_city ~ 0))
  pair_data <- pair_data %>% mutate(major_city.y = case_when(CITY.y %in% major_city ~ 1,
                                                             !CITY.y %in% major_city ~ 0))
  
  
  pair_data <- pair_data %>% mutate(big_city.x = case_when(CITY.x > 0 ~ 1,
                                                           !CITY.x > 0 ~ 0))
  pair_data <- pair_data %>% mutate(big_city.y = case_when(CITY.y > 0 ~ 1,
                                                           !CITY.y >0  ~ 0))
  
  
  pair_data$MV_new <- pair_data$MV*1000
  pair_data$MV_male_new <- pair_data$MV_male*1000
  pair_data$MV_female_new <- pair_data$MV_female*1000
  
  
  pair_data$MV_new_log <- pair_data$MV_log*1000
  pair_data$MV_male_new_log <- pair_data$MV_male_log*1000
  pair_data$MV_female_new_log <- pair_data$MV_female_log*1000
  
  
  pair_data$INCTOT.x.new <- pair_data$INCTOT.x/1000
  pair_data$INCTOT.y.new <- pair_data$INCTOT.y/1000
  
  #city_pair_data <- subset(pair_data,pair_data$CITYPOP.x != 0 & pair_data$CITYPOP.x != 99999)
  
  fit <- lm(MV_new_log ~ 
              NEW_AGE.x + NEW_AGE.y
            + NEW_INCTOT.x + NEW_INCTOT.y
            + NEW_EDUCD.x + NEW_EDUCD.y
            + NEW_RACE.x + NEW_RACE.y
            
            #+ big_city.x
            #+ STATEGINI.x
            
            
            
            
            , data=pair_data
            , weights = pair_data$HHW)
  
  
  
  print(n_year)
  summary(fit)
  
  #hundred_city = unique(pair_data$CITY.x[pair_data$CITY.x>0])
  
  
  
  fit_female <- lm(MV_female_new_log ~
                     NEW_AGE.x + NEW_AGE.y
                   + NEW_INCTOT.x + NEW_INCTOT.y
                   + NEW_EDUCD.x + NEW_EDUCD.y
                   + NEW_RACE.x + NEW_RACE.y
                   
                   #+ big_city.x
                   #+ STATEGINI.x
                   
                   , data=pair_data
                   , weights = pair_data$HHW)
  
  #summary(fit_female)
  
  fit_male <- lm(MV_male_new_log ~
                   NEW_AGE.x + NEW_AGE.y
                 + NEW_INCTOT.x + NEW_INCTOT.y
                 + NEW_EDUCD.x + NEW_EDUCD.y
                 + NEW_RACE.x + NEW_RACE.y
                 
                 #+ big_city.x
                 #+ STATEGINI.x
                 
                 , data=pair_data
                 , weights = pair_data$HHW)
  
  
  
  
  output <- list("output", 
                 fit = fit,
                 fit_female = fit_female,
                 fit_male = fit_male
                 
                 #,fit_long = fit_long
                 #,fit_female_long = fit_female_long
                 #,fit_male_long = fit_male_long
  )
  
  return(output)
  
}

draw_not_jm <- function(n_year){
  
  income_grid <<- m_income_grid
  age_grid <<- m_age_grid
  edu_grid <<- m_edu_grid
  race_grid <<- m_race_grid
  
  n_age <<- length(age_grid)
  n_income <<- length(income_grid)
  n_edu <<- length(edu_grid)
  n_race <<- length(race_grid)
  
  
  
  
  
  store <- funk_not_jm(n_year)
  Mu_all <- store$Mu_all
  pair_data <- store$pair_data
  
  
  
  ####            AGE 
  
  
  
  N_Age <- matrix(0, n_age, n_age)
  Mu_age <- matrix(0, n_age, n_age)
  Mu_M_age <- matrix(0, n_age, n_age)
  Mu_F_age <- matrix(0, n_age, n_age)
  #log version
  Mu_age_log<- matrix(0, n_age, n_age)
  Mu_M_age_log <- matrix(0, n_age, n_age)
  Mu_F_age_log <- matrix(0, n_age, n_age)
  
  for (i in 1:n_age){
    for (j in 1:n_age){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_AGE.x == i & Mu_all$NEW_AGE.y == j)
      
      total_folks = 1 #sum(current_sub$n)
      
      Mu_age_log[i,j] = sum(current_sub$EV_log) / total_folks
      Mu_F_age_log[i,j] = sum(current_sub$EV_female_log) / total_folks
      Mu_M_age_log[i,j] = sum(current_sub$EV_male_log) / total_folks
      
      Mu_age[i,j] = sum(current_sub$EV) / total_folks
      Mu_F_age[i,j] = sum(current_sub$EV_female) / total_folks
      Mu_M_age[i,j] = sum(current_sub$EV_male) / total_folks
      N_Age[i,j] = dim(current_sub)[1]
      
    }
  }
  
  
  x_age=c(1:length(age_grid))
  
  smooth_Mu_age <- kernel2dsmooth(Mu_age, kernel.type="disk", r=2)
  smooth_Mu_M_age <- kernel2dsmooth(Mu_M_age, kernel.type="disk", r=2)
  smooth_Mu_F_age <- kernel2dsmooth(Mu_F_age, kernel.type="disk", r=2)
  
  
  persp(x_age,x_age,smooth_Mu_age, theta=110, phi=30, r=35,
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="NJM Female age level",
        #zlim = c(0,2*max(smooth_Mu_age[!is.na(smooth_Mu_age)])), 
        ylab="Male age level", zlab="Estimated value", 
        main=paste("NJM Smooth Mu Age Both for", n_year))
  
  
  persp(x_age,x_age,smooth_Mu_M_age, theta=110, phi=30, r=35, 
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        ylab="Male age level", zlab="Estimated value", 
        main=paste("NJM Smooth Mu Age M for", n_year))
  
  persp(x_age,x_age,smooth_Mu_F_age, theta=110, phi=30, r=35, 
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        ylab="Male age level", zlab="Estimated value", 
        main=paste("NJM Smooth  Mu Age F for", n_year))
  
  
  
  
  ####            INCOME 
  
  N_income <- matrix(0, n_income, n_income)
  Mu_income <- matrix(0, n_income, n_income)
  Mu_M_income <- matrix(0, n_income, n_income)
  Mu_F_income <- matrix(0, n_income, n_income)
  #log version
  Mu_income_log<- matrix(0, n_income, n_income)
  Mu_M_income_log <- matrix(0, n_income, n_income)
  Mu_F_income_log <- matrix(0, n_income, n_income)
  
  for (i in 1:n_income){
    for (j in 1:n_income){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_INCTOT.x == i & Mu_all$NEW_INCTOT.y == j)
      
      total_folks = 1 #sum(current_sub$n)
      
      Mu_income_log[i,j] = sum(current_sub$EV_log) / total_folks
      Mu_F_income_log[i,j] = sum(current_sub$EV_female_log) / total_folks
      Mu_M_income_log[i,j] = sum(current_sub$EV_male_log) / total_folks
      
      
      Mu_income[i,j] = sum(current_sub$EV) / total_folks
      Mu_F_income[i,j] = sum(current_sub$EV_female) / total_folks
      Mu_M_income[i,j] = sum(current_sub$EV_male) / total_folks
      N_income[i,j] = dim(current_sub)[1]
      
    }
  }
  
  x_income=c(1:length(income_grid))
  
  
  smooth_Mu_income <- kernel2dsmooth(Mu_income, kernel.type="disk", r=2)
  smooth_Mu_M_income <- kernel2dsmooth(Mu_M_income, kernel.type="disk", r=2)
  smooth_Mu_F_income <- kernel2dsmooth(Mu_F_income, kernel.type="disk", r=2)
  
  
  
  persp(x_income,x_income,smooth_Mu_income, theta=120, phi=30, r=5, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated value", 
        main=paste("NJM Mu Both Income", n_year))
  
  persp(x_income,x_income,smooth_Mu_M_income, theta=120, phi=30, r=5, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated value", 
        main=paste("NJM Male Income", n_year))
  
  persp(x_income,x_income,smooth_Mu_F_income, theta=120, phi=30, r=5, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated value", 
        main=paste("NJM Mu Female Income", n_year))
  
  
  
  # 
  # 
  # ####            EDUCATION 
  # 
  # N_edu <- matrix(0, n_edu, n_edu)
  # Mu_edu <- matrix(0, n_edu, n_edu)
  # Mu_M_edu <- matrix(0, n_edu, n_edu)
  # Mu_F_edu <- matrix(0, n_edu, n_edu)
  # #log version
  # Mu_edu_log<- matrix(0, n_edu, n_edu)
  # Mu_M_edu_log <- matrix(0, n_edu, n_edu)
  # Mu_F_edu_log <- matrix(0, n_edu, n_edu)
  # 
  # for (i in 1:n_edu){
  #   for (j in 1:n_edu){
  #     #use pi
  #     current_sub <- subset(Mu_all, Mu_all$NEW_EDUCD.x == i & Mu_all$NEW_EDUCD.y == j)
  #     
  #     total_folks = 1 #sum(current_sub$n)
  #     
  #     Mu_edu_log[i,j] = sum(current_sub$EV_log) / total_folks
  #     Mu_F_edu_log[i,j] = sum(current_sub$EV_female_log) / total_folks
  #     Mu_M_edu_log[i,j] = sum(current_sub$EV_male_log) / total_folks
  #     
  #     
  #     Mu_edu[i,j] = sum(current_sub$EV) / total_folks
  #     Mu_F_edu[i,j] = sum(current_sub$EV_female) / total_folks
  #     Mu_M_edu[i,j] = sum(current_sub$EV_male) / total_folks
  #     N_edu[i,j] = dim(current_sub)[1]
  #     
  #   }
  # }
  # 
  # x_edu=c(1:length(edu_grid))
  # 
  # 
  # smooth_Mu_edu <- kernel2dsmooth(Mu_edu, kernel.type="disk", r=2)
  # smooth_Mu_M_edu <- kernel2dsmooth(Mu_M_edu, kernel.type="disk", r=2)
  # smooth_Mu_F_edu <- kernel2dsmooth(Mu_F_edu, kernel.type="disk", r=2)
  # 
  # 
  # 
  # persp(x_edu,x_edu,smooth_Mu_edu, theta=-20, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("NJM Smooth Mu Both Education", n_year))
  # 
  # persp(x_edu,x_edu,smooth_Mu_M_edu, theta=-20, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("NJM Smooth Male Education", n_year))
  # 
  # persp(x_edu,x_edu,smooth_Mu_F_edu, theta=-20, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Education",
  #       ylab="Male Education", zlab="Estimated value", 
  #       main=paste("NJM Smooth Mu Female Education", n_year))
  # 
  # ####            RACE 
  # 
  # N_race <- matrix(0, n_race, n_race)
  # Mu_race <- matrix(0, n_race, n_race)
  # Mu_M_race <- matrix(0, n_race, n_race)
  # Mu_F_race <- matrix(0, n_race, n_race)
  # #log version
  # Mu_race_log<- matrix(0, n_race, n_race)
  # Mu_M_race_log <- matrix(0, n_race, n_race)
  # Mu_F_race_log <- matrix(0, n_race, n_race)
  # 
  # for (i in 1:n_race){
  #   for (j in 1:n_race){
  #     #use pi
  #     current_sub <- subset(Mu_all, Mu_all$NEW_RACE.x == i & Mu_all$NEW_RACE.y == j)
  #     
  #     total_folks = 1 #sum(current_sub$n)
  #     
  #     Mu_race_log[i,j] = sum(current_sub$EV_log) / total_folks
  #     Mu_F_race_log[i,j] = sum(current_sub$EV_female_log) / total_folks
  #     Mu_M_race_log[i,j] = sum(current_sub$EV_male_log) / total_folks
  #     
  #     
  #     Mu_race[i,j] = sum(current_sub$EV) / total_folks
  #     Mu_F_race[i,j] = sum(current_sub$EV_female) / total_folks
  #     Mu_M_race[i,j] = sum(current_sub$EV_male) / total_folks
  #     N_race[i,j] = dim(current_sub)[1]
  #     
  #   }
  # }
  # 
  # x_race=c(1:length(race_grid))
  # 
  # 
  # smooth_Mu_race <- kernel2dsmooth(Mu_race, kernel.type="disk", r=2)
  # smooth_Mu_M_race <- kernel2dsmooth(Mu_M_race, kernel.type="disk", r=2)
  # smooth_Mu_F_race <- kernel2dsmooth(Mu_F_race, kernel.type="disk", r=2)
  # 
  #  
  # 
  # persp(x_race,x_race,smooth_Mu_race, theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Race",
  #       ylab="Male Race", zlab="Estimated value", 
  #       main=paste("NJM Smooth Mu Both Race", n_year))
  # 
  # persp(x_race,x_race,smooth_Mu_M_race, theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Race",
  #       ylab="Male Race", zlab="Estimated value", 
  #       main=paste("NJM Smooth Male Race", n_year))
  # 
  # persp(x_race,x_race,smooth_Mu_F_race, theta=120, phi=30, r=35, 
  #       shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
  #       nticks=5, ticktype="detailed", 
  #       col="cyan", xlab="Female Race",
  #       ylab="Male Race", zlab="Estimated value", 
  #       main=paste("NJM Smooth Mu Female Race", n_year))
  # 
  # 
  
  
  #tau
  Tau_age <- matrix(0, n_age, n_age)
  
  for (i in 1:n_age){
    for (j in 1:n_age){
      current_sub <- subset(Mu_all, Mu_all$NEW_AGE.x == i & Mu_all$NEW_AGE.y == j)
      total_folks = 1 #sum(current_sub$n)
      Tau_age[i,j] = sum(current_sub$tau) / total_folks
    }
  }
  
  x_age=c(1:length(age_grid))
  smooth_Tau_age <- kernel2dsmooth(Tau_age, kernel.type="disk", r=2)
  
  persp(x_age,x_age,smooth_Tau_age, theta=110, phi=30, r=35,
        shade=0.4, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed",
        col="cyan", xlab="Female age level",
        #zlim = c(0,2*max(smooth_Mu_age[!is.na(smooth_Mu_age)])), 
        ylab="Male age level", zlab="Estimated value", 
        main=paste("njm Tau Age for", n_year))
  
  
  
  Tau_income <- matrix(0, n_income, n_income)
  
  for (i in 1:n_income){
    for (j in 1:n_income){
      #use pi
      current_sub <- subset(Mu_all, Mu_all$NEW_INCTOT.x == i & Mu_all$NEW_INCTOT.y == j)
      
      total_folks = 1 #sum(current_sub$n)
      
      Tau_income[i,j] = sum(current_sub$tau) / total_folks
    }
  }
  
  x_income=c(1:length(income_grid))
  smooth_Tau_income <- kernel2dsmooth(Tau_income, kernel.type="disk", r=2)
  persp(x_income,x_income,smooth_Tau_income, theta=120, phi=30, r=35, 
        shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
        nticks=5, ticktype="detailed", 
        col="cyan", xlab="Female income level",
        ylab="Male income level", zlab="Estimated value", 
        main=paste("njm Tau Income", n_year))
  
}


reg_not_jm <- function(n_year){
  
  pair_data <- funk_not_jm(n_year)$pair_data
  
  major_city=c(4610,3730,1190,5350,3110,5330,6290,2990,810)
  pair_data <- pair_data %>% mutate(major_city.x = case_when(CITY.x %in% major_city ~ 1,
                                                             !CITY.x %in% major_city ~ 0))
  pair_data <- pair_data %>% mutate(major_city.y = case_when(CITY.y %in% major_city ~ 1,
                                                             !CITY.y %in% major_city ~ 0))
  
  
  pair_data <- pair_data %>% mutate(big_city.x = case_when(CITY.x > 0 ~ 1,
                                                           !CITY.x > 0 ~ 0))
  pair_data <- pair_data %>% mutate(big_city.y = case_when(CITY.y > 0 ~ 1,
                                                           !CITY.y >0  ~ 0))
  
  
  pair_data$MV_new <- pair_data$MV*1000
  pair_data$MV_male_new <- pair_data$MV_male*1000
  pair_data$MV_female_new <- pair_data$MV_female*1000
  
  
  pair_data$MV_new_log <- pair_data$MV_log*1000
  pair_data$MV_male_new_log <- pair_data$MV_male_log*1000
  pair_data$MV_female_new_log <- pair_data$MV_female_log*1000
  
  
  pair_data$income_dif <- (pair_data$INCTOT.x-pair_data$INCTOT.y)/1000
  pair_data$income_dif_abs <- abs(pair_data$INCTOT.x-pair_data$INCTOT.y)/1000
  
  
  pair_data$income_cat_dif <- (as.integer(pair_data$NEW_INCTOT.x)-as.integer(pair_data$NEW_INCTOT.y))
  
  pair_data$income_cat_dif_abs <- abs(as.integer(pair_data$NEW_INCTOT.x)-as.integer(pair_data$NEW_INCTOT.y))
  
  
  fit <- lm(MV_new_log ~ 
              NEW_AGE.x + NEW_AGE.y
            + NEW_INCTOT.x + NEW_INCTOT.y
            + NEW_EDUCD.x + NEW_EDUCD.y
            + NEW_RACE.x + NEW_RACE.y
            #+ age_dif
            #+ income_dif
            #+ income_dif_abs
            #+ big_city.x
            #+ STATEGINI.x
            , data=pair_data
            , weights = pair_data$HHW)
  
  
  
  fit_female <- lm(MV_female_new_log ~
                     NEW_AGE.x + NEW_AGE.y
                   + NEW_INCTOT.x + NEW_INCTOT.y
                   + NEW_EDUCD.x + NEW_EDUCD.y
                   + NEW_RACE.x + NEW_RACE.y
                   , data=pair_data
                   , weights = pair_data$HHW)
  fit_male <- lm(MV_male_new_log ~
                   NEW_AGE.x + NEW_AGE.y
                 + NEW_INCTOT.x + NEW_INCTOT.y
                 + NEW_EDUCD.x + NEW_EDUCD.y
                 + NEW_RACE.x + NEW_RACE.y
                 , data=pair_data
                 , weights = pair_data$HHW)
  fit_long <- 0
  
  
  
  summary(fit_long)
  
  
  
  output <- list("output", 
                 fit = fit,
                 fit_female = fit_female,
                 fit_male = fit_male
                 #fit_long = fit_long
                 
                 #,fit_long = fit_long
                 #,fit_female_long = fit_female_long
                 #,fit_male_long = fit_male_long
  )
  
  return(output)
  
}


F_gumbel <- function(n){
  return(exp(-exp(-n)))
}

gumbel <- function(n){
  return(exp(-exp(-n)))
}

G_gumbel <- function(n){
  a <- gumbel(n)+(1-gumbel(n))/2
  return(-(log(log(1/a))))
}

# 
# 
# 
# 
# 
# jm_data$NEW_INCTOT <- jm_data$INCTOT
# jm_data$NEW_INCTOT <- cut(jm_data$NEW_INCTOT,
#                           #breaks=c(-Inf,income_grid),
#                           #labels=seq(1:n_income))
#                           breaks=c(income_grid),
#                           labels=seq(1:n_income-1),
#                           right = FALSE)
# 
# # XXX
# pair_data$NEW_AGE.x <- pair_data$AGE.x
# pair_data$NEW_AGE.x <- cut(pair_data$NEW_AGE.x,
#                            #breaks=c(-Inf,age_grid),
#                            #labels=seq(1:n_age))
#                            breaks=c(age_grid),
#                            labels=seq(1:n_age-1),
#                            right = FALSE)
# 
# 
# pair_data$NEW_INCTOT.x <- pair_data$INCTOT.x
# pair_data$NEW_INCTOT.x <- cut(pair_data$NEW_INCTOT.x,
#                               #breaks=c(-Inf,income_grid),
#                               #labels=seq(1:n_income))
#                               breaks=c(income_grid),
#                               labels=seq(1:n_income-1),
#                               right = FALSE)
# 
# pair_data$NEW_EDUCD.x <- pair_data$EDUCD.x
# pair_data$NEW_EDUCD.x <- cut(pair_data$NEW_EDUCD.x,
#                              #breaks=c(-Inf, edu_grid),
#                              #labels=seq(1:n_edu),
#                              breaks=c(edu_grid),
#                              labels=seq(1:n_edu-1),
#                              right = FALSE)
# 
# pair_data$NEW_RACE.x <- pair_data$RACE.x
# pair_data$NEW_RACE.x <- cut(pair_data$NEW_RACE.x,
#                             #breaks=c(-Inf, race_grid),
#                             #labels=seq(1:n_race),
#                             breaks=c(race_grid),
#                             labels=seq(1:n_race-1),
#                             right = FALSE)
# 
# #YYY
# pair_data$NEW_AGE.y <- pair_data$AGE.y
# pair_data$NEW_AGE.y <- cut(pair_data$NEW_AGE.y,
#                            #breaks=c(-Inf,age_grid),
#                            #labels=seq(1:n_age),
#                            breaks=c(age_grid),
#                            labels=seq(1:n_age-1),
#                            right = FALSE)
# 
# pair_data$NEW_INCTOT.y <- pair_data$INCTOT.y
# pair_data$NEW_INCTOT.y <- cut(pair_data$NEW_INCTOT.y,
#                               #breaks=c(-Inf,income_grid),
#                               #labels=seq(1:n_income),
#                               breaks=c(income_grid),
#                               labels=seq(1:n_income-1),
#                               right = FALSE)
# 
# pair_data$NEW_EDUCD.y <- pair_data$EDUCD.y
# pair_data$NEW_EDUCD.y <- cut(pair_data$NEW_EDUCD.y,
#                              #breaks=c(-Inf, edu_grid),
#                              #labels=seq(1:n_edu),
#                              breaks=c(edu_grid),
#                              labels=seq(1:n_edu-1),
#                              right = FALSE)
# 
# pair_data$NEW_RACE.y <- pair_data$RACE.y
# pair_data$NEW_RACE.y <- cut(pair_data$NEW_RACE.y,
#                             #breaks=c(-Inf, race_grid),
#                             #labels=seq(1:n_race),
#                             breaks=c(race_grid),
#                             labels=seq(1:n_race-1),
#                             right = FALSE)

race_labels = c("1" = "White","2" = "Black",
                "3" = "Indian/Native","4" = "Chinese",
                "5" = "Japanese","6" = "Other Asian",
                "7" = "Other","8" = "Two races","9" = "Three or more")

edu_labels = c('0' = 'No schooling',
               '1' = 'N/A',
               '2' = 'No schooling',
               '10' = 'Nursery to grade 4',
               '11' = 'Nursery',
               '12' = 'Kindergarten',
               '13' = 'Grade 1-4',
               '14' = 'Grade 1',
               '15' = 'Grade 2',
               '16' = 'Grade 3',
               '17' = 'Grade 4',
               '20' = 'Grade 5-8',
               '21' = 'Grade 5-6',
               '22' = 'Grade 5',
               '23' = 'Grade 6',
               '24' = 'Grade 7-8',
               '25' = 'Grade 7',
               '26' = 'Grade 8',
               '30' = 'Grade 9',
               '40' = 'Grade 10',
               '50' = 'Grade 11',
               '60' = 'Grade 12',
               '61' = '12th grade, no diploma',
               '62' = 'High school graduate or GED',
               '63' = 'High school diploma',
               '64' = 'GED',
               '65' = 'Some college',
               '70' = '1 year of college',
               '71' = 'College credit',
               '80' = '2 years of college',
               '81' = 'Associate',
               '82' = 'Associate, occupational',
               '83' = 'Associate, academic',
               '90' = '3 years of college',
               '100' = '4 years of college',
               '101' = 'Bachelor',
               '110' = '5+ years of college',
               '111' = '6 years of college',
               '112' = '7 years of college',
               '113' = '8+ years of college',
               '114' = 'Master',
               '115' = 'Professional',
               '116' = 'Doctoral')



------------------




draw(2021)
co_draw(2021)
draw_not_jm(2021)
