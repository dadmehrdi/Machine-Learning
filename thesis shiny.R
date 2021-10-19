# let us go!
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
#library(extrafont)
#library(fontquiver)
#library(bslib)
library(rsconnect)
library(ipumsr)
#library(MASS)
#library(plotly)
#library(moderndive)
#library(sjPlot)
#library(sjmisc)
#library(sjlabelled)
library (readr)
library(RCurl)




urlfile="https://github.com/dadmehrdi/dadmehr/blob/master/Mu_all_08.RData"
mydata <-read_csv(url(urlfile))

x <- getURL("https://github.com/dadmehrdi/dadmehr/blob/master/Mu_all_08.RData")
y <- read.csv(text = x)

library("rio")

import(urlfile)



#urlfile<-'https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv'
dsin<-read.csv(urlfile)




ddi <- read_ipums_ddi("usa_00007.xml")
data_main <- read_ipums_micro(ddi)
format(object.size(data_main), units = "auto")


data <- data_main 
count(data_main,YEAR)
n_year=2019

n_state=-1
n_city=-1    #Seattle = 6430 | NY = 4610
n_edu=-1
n_field=-1
step_age <- 3
step_income <- 30000

if (n_state!=-1){data <- subset(data_main,data_main$STATEICP==n_state)}
if (n_year!=-1){data <- subset(data,data$YEAR == n_year)}
if (n_city!=-1){data <- subset(data,data$CITY==n_city)}
if (n_edu!=-1){data <- subset(data,data$EDUCD==n_edu)}
if (n_field!=-1){data <- subset(data,data$DEGFIELD2D==n_field)}

nm_data <- subset(data,data$MARST == 6)
jmf_data <- subset(data,data$MARST == 1 & data$MARRINYR == 2 & data$SEX == 2)
jmm_data <- subset(data,data$MARST == 1 & data$MARRINYR == 2 & data$SEX == 1)
jm_pair_data <- merge(jmf_data,jmm_data,by=c("SERIAL"),all=FALSE)

pair_data <- jm_pair_data
single_data <- nm_data


max_income <- max(c(pair_data$INCTOT.x,pair_data$INCTOT.y))
max_income <- 1000000
min_income <- min(c(pair_data$INCTOT.x,pair_data$INCTOT.y))
min_income <- 0
income_grid <- seq(min_income,max_income,step_income)
n_income <- length(income_grid)

max_age <- max(c(pair_data$AGE.x,pair_data$AGE.y))
max_age <-100
min_age <- min(c(pair_data$AGE.x,pair_data$AGE.y))
min_age <- 17
age_grid <- seq(min_age,max_age,step_age)
n_age <- length(age_grid)

#     0                            N/A or no schooling
#     2                         No schooling completed
#    62                    High school graduate or GED
#    63                    Regular high school diploma
#    71   1 or more years of college credit, no degree
#   101                              Bachelor's degree
#   114                                Master's degree
#   116                                Doctoral degree

edu_grid <- c(0,63,65,71,101,114,116)
n_edu <- length(edu_grid)

#     1                            White
#     2     Black/African American/Negro
#     9        Three or more major races

race_grid <- c(1,2,9)
n_race <- length(race_grid)

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

pair_data <- na.omit(pair_data, c("NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x", "NEW_AGE.x",
                                  "NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y", "NEW_AGE.y")) 

Mu_pair=count(pair_data,
              NEW_AGE.x,NEW_INCTOT.x,NEW_EDUCD.x,NEW_RACE.x,
              NEW_AGE.y,NEW_INCTOT.y,NEW_EDUCD.y,NEW_RACE.y)#,.drop = FALSE)
#Mu_pair
col <- colnames(pair_data)

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

single_male <- na.omit(single_male, c("NEW_INCTOT","NEW_EDUCD","NEW_RACE", "NEW_AGE")) 

Mu_male=count(single_male,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE) #,.drop = FALSE)
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

single_female <- na.omit(single_female, c("NEW_INCTOT","NEW_EDUCD","NEW_RACE", "NEW_AGE")) 


Mu_female=count(single_male,NEW_AGE,NEW_INCTOT,NEW_EDUCD,NEW_RACE) #,.drop = FALSE
#Mu_female

colnames(Mu_female) <- c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x","n_female")
#Mu_female

Mu_all <- merge(Mu_pair,Mu_female,by=c("NEW_AGE.x","NEW_INCTOT.x","NEW_EDUCD.x","NEW_RACE.x"),all=FALSE)
Mu_all <- merge(Mu_all,Mu_male,by=c("NEW_AGE.y","NEW_INCTOT.y","NEW_EDUCD.y","NEW_RACE.y"),all=FALSE)
#Mu_all

Mu_all$MV <- (Mu_all$n/Mu_all$n_male*Mu_all$n/Mu_all$n_male)^0.5
Mu_all$EV <- Mu_all$MV*Mu_all$n

col_names <- c("NEW_INCTOT.x", "NEW_INCTOT.y","NEW_AGE.x", "NEW_AGE.y",
               "NEW_RACE.x", "NEW_RACE.y","NEW_EDUCD.x", "NEW_EDUCD.y")

pair_data <- merge(pair_data, Mu_all, by.x=col_names,
                   by.y=col_names,all.x=TRUE)


setwd("/Users/dadmehr/Shiny")
load("Mu_all_19.RData")
load("Mu_all_18.RData")
load("Mu_all_17.RData")
load("Mu_all_16.RData")
load("Mu_all_15.RData")
load("Mu_all_14.RData")
load("Mu_all_13.RData")
load("Mu_all_12.RData")
load("Mu_all_11.RData")
load("Mu_all_10.RData")
load("Mu_all_09.RData")
load("Mu_all_08.RData")

mu_all_years  <<- list()
mu_all_years$"19" <<- Mu_all_19
mu_all_years$"18" <<- Mu_all_18
mu_all_years$"17" <<- Mu_all_17
mu_all_years$"16" <<- Mu_all_16
mu_all_years$"15" <<- Mu_all_19
mu_all_years$"14" <<- Mu_all_18
mu_all_years$"13" <<- Mu_all_17
mu_all_years$"12" <<- Mu_all_16
mu_all_years$"11" <<- Mu_all_15
mu_all_years$"10" <<- Mu_all_19
mu_all_years$"09" <<- Mu_all_18
mu_all_years$"08" <<- Mu_all_15


ui <- fluidPage(
  titlePanel("Dadmehr Thesis 1.0"),
  theme = bs_theme(version = 4, bootswatch = "lumen"),
  #'arg' should be one of “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, “lux”, “materia”, “minty”, “pulse”, “sandstone”, “simplex”, “sketchy”, “slate”, “solar”, “spacelab”, “superhero”, “united”, “yeti”
  #shinythemes::themeSelector(),
  #::themeSelector()
  #setBackgroundColor("khaki"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(inputId = "year", label = "Year", min = 2008, max = 2019, value = 2019),
      sliderInput(inputId = "age", label = "Age", min = 20, max = 80, value = 30),
      sliderInput(inputId = "income", label = "Income", min = 0, max = 400, value = 100),
      sliderInput(inputId = "race", label = "Race", min = 1, max = 10, value = 2),
      sliderInput(inputId = "edu", label = "Education", min = 0, max = 116, value = 80),
      
      
    ),
    
    
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      #tableOutput("values"),
      tabsetPanel(type = "tabs",
                  tabPanel("Data",
                           plotOutput(outputId = "Mu_age"),
                           #plotOutput(outputId = "Mu_income"),
                           
                           ),
                  
                  
                  tabPanel("Yours",
                           plotOutput(outputId = "Plot_CGM_SMBG_cost")
                           ),
                  
                  tabPanel("Reg",
                           plotOutput(outputId = "Plot_ROI")
                           
                           
                           )
      )
    )
  )
)


server <- function(input, output) {
  output$Mu_age <- renderPlot({
    #plot somthing 
    load("Mu_all_19.RData")
    load("Mu_all_18.RData")
    load("Mu_all_17.RData")
    load("Mu_all_16.RData")
    load("Mu_all_15.RData")
    load("Mu_all_14.RData")
    load("Mu_all_13.RData")
    load("Mu_all_12.RData")
    load("Mu_all_11.RData")
    load("Mu_all_10.RData")
    load("Mu_all_09.RData")
    load("Mu_all_08.RData")
    
    mu_all_years  <- list()
    mu_all_years$"19" <- Mu_all_19
    mu_all_years$"18" <- Mu_all_18
    mu_all_years$"17" <- Mu_all_17
    mu_all_years$"16" <- Mu_all_16
    mu_all_years$"15" <- Mu_all_15
    mu_all_years$"14" <- Mu_all_14
    mu_all_years$"13" <- Mu_all_13
    mu_all_years$"12" <- Mu_all_12
    mu_all_years$"11" <- Mu_all_11
    mu_all_years$"10" <- Mu_all_10
    mu_all_years$"09" <- Mu_all_09
    mu_all_years$"08" <- Mu_all_08
    
    
    
    i=input$year-2000
    
    varname <- paste(as.character(i))
    Mu_all <- mu_all_years[[varname]]
    
    #Mu_all <- Mu_all_18
    N_Age <- matrix(0, n_age, n_age)
    #C_Age=count(pair_data,Mu_all$NEW_AGE.x,Mu_all$NEW_AGE.y,.drop = FALSE)
    Mu_age <- matrix(0, n_age, n_age)
    for (i in 1:n_age){
      for (j in 1:n_age){
        Mu_age[i,j]=sum(Mu_all$EV[Mu_all$NEW_AGE.x == i & Mu_all$NEW_AGE.y == j])
        #N_Age[i,j]=C_Age$n[C_Age$NEW_AGE.x ==i & C_Age$NEW_AGE.y ==j][1]
      }}
    
    persp(age_grid,age_grid,Mu_age, theta=-70, phi=30, r=3, shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
          nticks=5, ticktype="detailed", zlim = c(0,1.4*max(Mu_age)), col="cyan", xlab="Age of Females",
          ylab="Age of Males", zlab=" ", main="b")
    
  })
  
  output$Mu_income <- renderPlot({
    #plot somthing 
    load("Mu_all_19.RData")
    load("Mu_all_18.RData")
    load("Mu_all_17.RData")
    load("Mu_all_16.RData")
    load("Mu_all_15.RData")
    load("Mu_all_14.RData")
    load("Mu_all_13.RData")
    load("Mu_all_12.RData")
    load("Mu_all_11.RData")
    load("Mu_all_10.RData")
    load("Mu_all_09.RData")
    load("Mu_all_08.RData")
    
    mu_all_years  <<- list()
    mu_all_years$"19" <<- Mu_all_19
    mu_all_years$"18" <<- Mu_all_18
    mu_all_years$"17" <<- Mu_all_17
    mu_all_years$"16" <<- Mu_all_16
    mu_all_years$"15" <<- Mu_all_15
    mu_all_years$"14" <<- Mu_all_14
    mu_all_years$"13" <<- Mu_all_13
    mu_all_years$"12" <<- Mu_all_12
    mu_all_years$"11" <<- Mu_all_11
    mu_all_years$"10" <<- Mu_all_10
    mu_all_years$"09" <<- Mu_all_09
    mu_all_years$"08" <<- Mu_all_08
    
    i=input$year-2000
    
    varname <- paste(as.character(i))
    Mu_all <- mu_all_years[[varname]]
    
    N_Income <- matrix(0, n_income, n_income)
    #C_Income=count(pair_data,Mu_all$NEW_INCTOT.x,Mu_all$NEW_INCTOT.y,.drop = FALSE)
    Mu_income <- matrix(0, n_income, n_income)
    for (i in 1:n_income){
      for (j in 1:n_income){
        Mu_income[i,j]=sum(Mu_all$EV[Mu_all$NEW_INCTOT.x == i & Mu_all$NEW_INCTOT.y == j])
        N_Income[i,j]=C_Income$n[C_Income$NEW_INCTOT.x ==i & C_Income$NEW_INCTOT.y ==j][1]
      }}
    
    persp(income_grid/1000,income_grid/1000,Mu_income, theta=30, phi=30, r=3, shade=0.6, axes=TRUE,scale=TRUE, box=TRUE,
          nticks=5, ticktype="detailed", zlim = c(0,1.4*max(Mu_income)), col="cyan", xlab="Female income in thousands",
          ylab="Male income in thousands", zlab=" ", main="b")
  })
  
}

shinyApp(ui = ui, server = server)


