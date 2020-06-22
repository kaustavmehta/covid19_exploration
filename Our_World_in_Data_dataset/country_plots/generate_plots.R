#### ORGANISED OWD R SCRIPT BASED ON COUNTRY-WISE CODE BLOCKING

#set working directory - You will have to change this to where you have downloaded the folder

setwd("~/Desktop/COVID Research/covid19_exploration/Our_World_in_Data_dataset/country_plots")

#import libraries

library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(reshape2)
library(latticeExtra)

library(hrbrthemes)
library(kableExtra)
library(viridis)
library(DT)
#library(plotly)

#import datasets
## Source: https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv
global_covid_data <- read_csv("~/Desktop/COVID Research/covid19_exploration/Our_World_in_Data_dataset/covid-19-data/public/data/owid-covid-data.csv")
global_covid_data$date <- as.Date(global_covid_data$date, "%d%m%y") #convert dates to a date-readable format

global_testing_data <- read_csv("~/Desktop/COVID Research/covid19_exploration/Our_World_in_Data_dataset/covid-19-data/public/data/testing/covid-testing-all-observations.csv")
global_testing_data$Date <- as.Date(global_testing_data$Date, "%d%m%y")

## Countries in consideration (based on 15/6/2020 Discussion): USA, Brazil, Russia, India, UK, 
## Spain, Italy, Peru, Germany, Iran, South Korea, China
## Countries in consideration (as of 17/6/2020): South Africa, Vietnam, France,
## New Zealand, Australia, Bangladesh, Pakistan, Israel, Saudi Arabia, Sweden,
## Thailand, Ghana, Libya, Iraq, Turkey, Syria, Somalia, Afghanistan, Yemen

## Sources: https://en.wikipedia.org/wiki/List_of_ongoing_armed_conflicts

## Functions
{
  ## Global Declaration of Vectors containing relevant, irrelevant and static data
  
  relevant_data <- c("total_cases", "total_deaths", "new_cases", "new_deaths", "total_tests", "new_tests")
  irrelevant_data <- c(1, 2, 3, 9, 10, 11, 12, 15, 16, 17, 18, 19, 20)
  static_data <- c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34)
  
  ## Disable Scientific Notation in the plots
  
  options(scipen=999)
  
  ## Filters the Global COVID dataset and creates a subset of the country filtered by its Alpha-3 ISO code
  extract_country_data <- function(ISO_code, dataset=global_covid_data){
    
    country <- global_covid_data[grep(ISO_code, global_covid_data$iso_code),]
    country <- country[-(1:1),]
    return(country)
    
  }
  
  ## Extracts the static data from the dataset and returns it to a pre-defined variable (assumed) 'country_static'
  extract_static <- function(country){
    
    country_static <- select(country, -all_of(relevant_data), -all_of(irrelevant_data))
    return(country_static)
    
  }
  
  ## Extracts the irrelevant data from the country dataset if one requires such a thing
  extract_irrelevant <- function(country){
    
    country_irrelevant <- select(country, -all_of(relevant_data), -all_of(static_data))
    return(country_irrelevant)
    
  }
  
  ## Extracts the relevant dataset for our consideration
  extract_relevant <- function(country){
    
    country_relevant <- select(country, -all_of(irrelevant_data), -all_of(static_data))
    return(country_relevant)
    
  }
  
  ## Converts country dataset by aggregating data across a 7 day period and normalizes it in terms of relative no. of weeks
  convert_to_normalised_weekly <- function(country){
    
    country_weekly <- country %>%
      group_by(week = week(country$date)) %>%
      summarize_if(is.numeric, sum)
    country_weekly$week <- seq(1:nrow(country_weekly))
    country_weekly <- country_weekly[-(nrow(country_weekly):nrow(country_weekly)),]
    return(country_weekly)
    
  }
  
  ## Converts country dataset by aggregating data across a 7 day period and normalizes it in terms of 
  ## absolute no. of weeks sine the first case of COVID-19
  convert_to_absolute_weekly <- function(country){
    
    country_weekly <- country %>%
      group_by(week = week(country$date)) %>%
      summarize_if(is.numeric, sum)
    #country_weekly$week <- seq(1:nrow(country_weekly))
    country_weekly <- country_weekly[-(nrow(country_weekly):nrow(country_weekly)),]
    return(country_weekly)
    
  }
  
  ## Generates linear plot of total no. of cases
  
  generatePlot_linearTC <- function(country_weekly, title_name, subtitle_name = "Total No. of COVID-19 Cases vs. Weeks",
                                    caption_name = "Data Source: Ourworldindata.org/coronavirus"){
    
    country_plot <- ggplot(country_weekly, aes(x=week, y=total_cases)) +
      scale_y_continuous("Total Cases (Linear)",
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() +
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
   return(country_plot)
     
  }
  
  ## Generates log10 plot of total no. of cases
  
  generatePlot_log10TC <- function(country_weekly, title_name, 
                                   subtitle_name = expression("log"*""[10]*" (Total No. of COVID-19 Cases) vs. Weeks"), 
                                   caption_name = "Data Source: Ourworldindata.org/coronavirus"){
    
    country_plot <- ggplot(country_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("log10 of Total Cases", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
  }
  
  ## Generates linear plot of total no. of cases vs. total no. of deaths
  
  generatePlot_linearTC_TD <- function(country_weekly, title_name, 
                                       subtitle_name = "Total No. of COVID-19 Cases and Deaths vs. Weeks",
                                       caption_name = "Data Source: Ourworldindata.org/coronavirus"){
    
    removeVector <- c(3,5,6,7)
    country_weekly <- select(country_weekly, -all_of(removeVector))
    
    
    country_long <- melt(country_weekly, id="week")
    country_plot <- ggplot(country_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Total Cases and Total Deaths",
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
    
  }
  
  ## Generates log10 plot of total no. of cases vs. total no. of deaths
  
  generatePlot_log10TC_TD <- function(country_weekly, title_name, 
                                      subtitle_name = "Total No. of COVID-19 Cases vs. Weeks (log10)",
                                       caption_name = "Data Source: Ourworldindata.org/coronavirus"){
    
    removeVector <- c(3,5,6,7)
    country_weekly <- select(country_weekly, -all_of(removeVector))
    
    
    country_long <- melt(country_weekly, id="week")
    country_plot <- ggplot(country_long, 
                           aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Total Cases and Total Deaths",
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
    
  }
  
  ## Generates linear plot of total cases, total deaths, total tests
  
  generatePlot_linearTC_TD_TT <- function(country_weekly, title_name, 
                                          subtitle_name = "Total No. of COVID-19 Cases, Deaths and Tests vs. Weeks",
                                       caption_name = "Data Source: Ourworldindata.org/coronavirus
                                       If total_tests has no data points, the data doesn't exist in the dataset"){
    
    removeVector <- c(3,5,7)
    country_weekly <- select(country_weekly, -all_of(removeVector))
    
    
    country_long <- melt(country_weekly, id="week")
    country_plot <- ggplot(country_long, 
                           aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Total Cases, Total Deaths and Total Tests (Linear)",
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
    
  }
  
  ## Generates log10 plot of total cases, total deaths and total tests
  
  generatePlot_log10TC_TD_TT <- function(country_weekly, title_name, 
                                          subtitle_name = "log10 of Total No. of COVID-19 Cases, Deaths and Tests vs. Weeks",
                                          caption_name = "Data Source: Ourworldindata.org/coronavirus
                                       If total_tests has no data points, the data doesn't exist in the dataset"){
    
    removeVector <- c(3,5,7)
    country_weekly <- select(country_weekly, -all_of(removeVector))
    
    
    country_long <- melt(country_weekly, id="week")
    country_plot <- ggplot(country_long, 
                           aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("log10 Total Cases, Total Deaths and Total Tests (Linear)",
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
    
  }
  
  ## Generates linear plot of no. of new cases
  
  generatePlot_linearNC <- function(country_weekly, title_name, subtitle_name = "New COVID-19 Cases vs. Weeks",
                                    caption_name = "Data Source: Ourworldindata.org/coronavirus"){
    
    country_plot <- ggplot(country_weekly, aes(x=week, y=new_cases)) +
      scale_y_continuous("New Cases (Linear)",
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() +
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
    
  }
  
  ## Generates log10 plot of no. of new cases
  
  generatePlot_log10NC <- function(country_weekly, title_name, 
                                   subtitle_name = expression("log"*""[10]*"New COVID-19 Cases vs. Weeks"), 
                                   caption_name = "Data Source: Ourworldindata.org/coronavirus"){
    
    country_plot <- ggplot(country_weekly, aes(x=week, y=new_cases)) + 
      scale_y_continuous("log10 of New Cases", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
  }
  
  ## Generates linear plot of no. of new cases vs. new deaths
  
  generatePlot_linearNC_ND <- function(country_weekly, title_name, 
                                      subtitle_name = "New COVID-19 Cases and Deaths vs. Weeks (Linear)",
                                       caption_name = "Data Source: Ourworldindata.org/coronavirus"){
    
    removeVector <- c(2,4,6,7)
    country_weekly <- select(country_weekly, -all_of(removeVector))
    
    
    country_long <- melt(country_weekly, id="week")
    country_plot <- ggplot(country_long, 
                           aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("New Cases and New Deaths)",
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
    
  }
  
  ## Generates log10 plot of no. of new cases vs. new deaths
  
  generatePlot_log10NC_ND <- function(country_weekly, title_name, 
                                         subtitle_name = "log10 of New COVID-19 Cases and Deaths vs. Weeks",
                                         caption_name = "Data Source: Ourworldindata.org/coronavirus"){
    
    removeVector <- c(2,4,6,7)
    country_weekly <- select(country_weekly, -all_of(removeVector))
    
    
    country_long <- melt(country_weekly, id="week")
    country_plot <- ggplot(country_long, 
                           aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("log10 of New Cases and New Deaths)",
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
    
  }
  
  
  ## Generates linear plot of new cases, new deaths and new tests
  
  generatePlot_linearNC_ND_NT <- function(country_weekly, title_name, 
                                          subtitle_name = "New COVID-19 Cases, Deaths and Tests vs. Weeks",
                                          caption_name = "Data Source: Ourworldindata.org/coronavirus
                                       If total_tests has no data points, the data doesn't exist in the dataset"){
    
    removeVector <- c(2,4,6)
    country_weekly <- select(country_weekly, -all_of(removeVector))
    
    
    country_long <- melt(country_weekly, id="week")
    country_plot <- ggplot(country_long, 
                           aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("New Cases, New Deaths and New Tests)",
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
    
  }
  
  ## Generates log10 plot of new cases, new deaths and new tests
  
  generatePlot_log10NC_ND_NT <- function(country_weekly, title_name, 
                                          subtitle_name = "log10 of New COVID-19 Cases, Deaths and Tests vs. Weeks",
                                          caption_name = "Data Source: Ourworldindata.org/coronavirus
                                       If total_tests has no data points, the data doesn't exist in the dataset"){
    
    removeVector <- c(2,4,6)
    country_weekly <- select(country_weekly, -all_of(removeVector))
    
    
    country_long <- melt(country_weekly, id="week")
    country_plot <- ggplot(country_long, 
                           aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("log10 New Cases, New Deaths and New Tests)",
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = title_name,
           subtitle = subtitle_name,
           caption = caption_name)
    
    return(country_plot)
    
  }

  generateMultiPlot_twoCountries_NC <- function(country1weekly, country2weekly, country1string, country2string,
                                              xlabel="Weeks since 1st January 2020",
                                              ylabel = "New Cases/Week"){
  
  #country1weekly <- country1weekly[-(nrow(country1weekly):nrow(country1weekly)),]
  #country2weekly <- country2weekly[-(nrow(country2weekly):nrow(country2weekly)),]
  
  multiplot <- xyplot(country1weekly$new_cases + country2weekly$new_cases ~ country1weekly$week,
                      xlab = xlabel,
                      ylab = ylabel,
                      auto.key = list(text = c(country1string, country2string)),
                      scales=list(y = list(tick.number = 10, alternating=3), x = list(tick.number = 10)),
                      type = "l", lwd=2)
  
  return(multiplot)
}

  ## Bell curve countries NC are defined as countries whose plots follow a bell shape in terms of new COVID-19 cases... 
  ## ...with respect to time
  ## As of 18th June 2020, these countries are:
  ## Australia, China, France, Germany, Iran, Israel, Italy, New Zealand, Russia, Spain, Thailand, Turkey, UK, USA, Vietnam
  ## Note: because USA is so dank and tops everything in the world, you might want to generate a plot with and without it...
  ## ...This is because it's incredible contribution to the world will overshadow every other country, as always.

  generate_MultiPlot_bellcurveCountries_NC <- function(titleDate){
    
    multiplot <- xyplot(Australia_weekly$new_cases + China_weekly$new_cases + France_weekly$new_cases + Germany_weekly$new_cases + 
                          Iran_weekly$new_cases + Israel_weekly$new_cases + Italy_weekly$new_cases + New_Zealand_weekly$new_cases +
                          Russia_weekly$new_cases + Spain_weekly$new_cases + Thailand_weekly$new_cases + Turkey_weekly$new_cases +
                          UK_weekly$new_cases + 
                          #USA_weekly$new_cases + 
                          Vietnam_weekly$new_cases ~ USA_weekly$week,
                        
                        lattice.options = list(title = titleDate),
                        xlab = "Weeks since 1st January 2020",
                        ylab = "New Cases/Week",
                        auto.key = list(text = c("Australia", "China", "France", "Germany", "Iran",
                                                "Israel", "Italy", "New_Zealand", "Russia", "Spain",
                                                "Thailand", "Turkey", "UK", "USA", "Vietnam")),
                        scales = list(y = list(tick.number = 10, alternating=3), x = list(tick.number = 10)),
                        type = "l", lwd=2)
    
    return(multiplot)
  }


  
}

## Country-wise plots with relative week numbering
{
  ## USA Plots
  {
    USA <- extract_country_data("USA")
    USA <- extract_relevant(USA)
    USA_weekly <- convert_to_normalised_weekly(USA)
    {
      ggsave(
      filename = "NC/USA_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(USA_weekly, "USA"))
    )
    ggsave(
      filename = "NC_ND/USA_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(USA_weekly, "USA"))
    )
    ggsave(
      filename = "NC_ND_NT/USA_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(USA_weekly, "USA"))
    )
    ggsave(
      filename = "TC/USA_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(USA_weekly, "USA"))
    )
    ggsave(
      filename = "TC_TD/USA_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(USA_weekly, "USA"))
    )
    ggsave(
      filename = "TC_TD_TT/USA_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(USA_weekly, "USA"))
    )
    ggsave(
      filename = "log10NC/USA_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(USA_weekly, "USA"))
    )
    ggsave(
      filename = "log10NC_ND/USA_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(USA_weekly, "USA"))
    )
    ggsave(
      filename = "log10NC_ND_NT/USA_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(USA_weekly, "USA"))
    )
    ggsave(
      filename = "log10TC/USA_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(USA_weekly, "USA"))
    )
    ggsave(
      filename = "log10TC_TD/USA_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(USA_weekly, "USA"))
    )
    ggsave(
      filename = "log10TC_TD_TT/USA_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(USA_weekly, "USA"))
    )}
  }
  
  ## Brazil Plots
  {
    Brazil <- extract_country_data("BRA")
    Brazil <- extract_relevant(Brazil)
    Brazil_weekly <- convert_to_normalised_weekly(Brazil)
    {
      ggsave(
      filename = "NC/Brazil_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "NC_ND/Brazil_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "NC_ND_NT/Brazil_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "TC/Brazil_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "TC_TD/Brazil_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "TC_TD_TT/Brazil_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "log10NC/Brazil_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "log10NC_ND/Brazil_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Brazil_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "log10TC/Brazil_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "log10TC_TD/Brazil_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Brazil_weekly, "Brazil"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Brazil_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Brazil_weekly, "Brazil"))
    )}
  }
  
  ## Russia Plots
  {
    Russia <- extract_country_data("RUS")
    Russia <- extract_relevant(Russia)
    Russia_weekly <- convert_to_normalised_weekly(Russia)
    {
      ggsave(
      filename = "NC/Russia_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "NC_ND/Russia_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "NC_ND_NT/Russia_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "TC/Russia_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "TC_TD/Russia_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "TC_TD_TT/Russia_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "log10NC/Russia_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "log10NC_ND/Russia_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Russia_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "log10TC/Russia_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "log10TC_TD/Russia_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Russia_weekly, "Russia"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Russia_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Russia_weekly, "Russia"))
    )}
  }
  
  ## India Plots
  {
    India <- extract_country_data("IND")
    India <- extract_relevant(India)
    India_weekly <- convert_to_normalised_weekly(India)
    {
      ggsave(
      filename = "NC/India_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(India_weekly, "India"))
    )
    ggsave(
      filename = "NC_ND/India_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(India_weekly, "India"))
    )
    ggsave(
      filename = "NC_ND_NT/India_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(India_weekly, "India"))
    )
    ggsave(
      filename = "TC/India_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(India_weekly, "India"))
    )
    ggsave(
      filename = "TC_TD/India_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(India_weekly, "India"))
    )
    ggsave(
      filename = "TC_TD_TT/India_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(India_weekly, "India"))
    )
    ggsave(
      filename = "log10NC/India_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(India_weekly, "India"))
    )
    ggsave(
      filename = "log10NC_ND/India_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(India_weekly, "India"))
    )
    ggsave(
      filename = "log10NC_ND_NT/India_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(India_weekly, "India"))
    )
    ggsave(
      filename = "log10TC/India_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(India_weekly, "India"))
    )
    ggsave(
      filename = "log10TC_TD/India_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(India_weekly, "India"))
    )
    ggsave(
      filename = "log10TC_TD_TT/India_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(India_weekly, "India"))
    )}
  }
  
  ## UK Plots
  {
    UK <- extract_country_data("GBR")
    UK <- extract_relevant(UK)
    UK_weekly <- convert_to_normalised_weekly(UK)
    {ggsave(
      filename = "NC/UK_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(UK_weekly, "UK"))
    )
    ggsave(
      filename = "NC_ND/UK_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(UK_weekly, "UK"))
    )
    ggsave(
      filename = "NC_ND_NT/UK_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(UK_weekly, "UK"))
    )
    ggsave(
      filename = "TC/UK_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(UK_weekly, "UK"))
    )
    ggsave(
      filename = "TC_TD/UK_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(UK_weekly, "UK"))
    )
    ggsave(
      filename = "TC_TD_TT/UK_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(UK_weekly, "UK"))
    )
    ggsave(
      filename = "log10NC/UK_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(UK_weekly, "UK"))
    )
    ggsave(
      filename = "log10NC_ND/UK_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(UK_weekly, "UK"))
    )
    ggsave(
      filename = "log10NC_ND_NT/UK_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(UK_weekly, "UK"))
    )
    ggsave(
      filename = "log10TC/UK_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(UK_weekly, "UK"))
    )
    ggsave(
      filename = "log10TC_TD/UK_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(UK_weekly, "UK"))
    )
    ggsave(
      filename = "log10TC_TD_TT/UK_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(UK_weekly, "UK"))
    )}
  }
  
  ## Spain Plots
  {
    Spain <- extract_country_data("ESP")
    Spain <- extract_relevant(Spain)
    Spain_weekly <- convert_to_normalised_weekly(Spain)
    {ggsave(
      filename = "NC/Spain_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "NC_ND/Spain_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "NC_ND_NT/Spain_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "TC/Spain_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "TC_TD/Spain_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "TC_TD_TT/Spain_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "log10NC/Spain_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "log10NC_ND/Spain_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Spain_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "log10TC/Spain_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "log10TC_TD/Spain_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Spain_weekly, "Spain"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Spain_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Spain_weekly, "Spain"))
    )}
  }
  
  ## Italy Plots
  {
    Italy <- extract_country_data("ITA")
    Italy <- extract_relevant(Italy)
    Italy_weekly <- convert_to_normalised_weekly(Italy)
    {ggsave(
      filename = "NC/Italy_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "NC_ND/Italy_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "NC_ND_NT/Italy_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "TC/Italy_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "TC_TD/Italy_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "TC_TD_TT/Italy_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "log10NC/Italy_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "log10NC_ND/Italy_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Italy_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "log10TC/Italy_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "log10TC_TD/Italy_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Italy_weekly, "Italy"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Italy_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Italy_weekly, "Italy"))
    )}
  }
  
  ## Peru Plots
  {
    Peru <- extract_country_data("PER")
    Peru <- extract_relevant(Peru)
    Peru_weekly <- convert_to_normalised_weekly(Peru)
    {ggsave(
      filename = "NC/Peru_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "NC_ND/Peru_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "NC_ND_NT/Peru_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "TC/Peru_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "TC_TD/Peru_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "TC_TD_TT/Peru_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "log10NC/Peru_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "log10NC_ND/Peru_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Peru_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "log10TC/Peru_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "log10TC_TD/Peru_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Peru_weekly, "Peru"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Peru_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Peru_weekly, "Peru"))
    )}
  }
  
  ## Germany Plots
  {
    Germany <- extract_country_data("DEU")
    Germany <- extract_relevant(Germany)
    Germany_weekly <- convert_to_normalised_weekly(Germany)
    
    {ggsave(
      filename = "NC/Germany_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "NC_ND/Germany_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "NC_ND_NT/Germany_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "TC/Germany_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "TC_TD/Germany_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "TC_TD_TT/Germany_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "log10NC/Germany_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "log10NC_ND/Germany_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Germany_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "log10TC/Germany_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "log10TC_TD/Germany_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Germany_weekly, "Germany"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Germany_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Germany_weekly, "Germany"))
    )}
  }
  
  ## Iran Plots
  {
    Iran <- extract_country_data("IRN")
    Iran <- extract_relevant(Iran)
    Iran_weekly <- convert_to_normalised_weekly(Iran)
    {ggsave(
      filename = "NC/Iran_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "NC_ND/Iran_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "NC_ND_NT/Iran_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "TC/Iran_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "TC_TD/Iran_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "TC_TD_TT/Iran_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "log10NC/Iran_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "log10NC_ND/Iran_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Iran_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "log10TC/Iran_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "log10TC_TD/Iran_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Iran_weekly, "Iran"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Iran_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Iran_weekly, "Iran"))
    )}
  }
  
  ## South Korea Plots
  {
    South_Korea <- extract_country_data("KOR")
    South_Korea <- extract_relevant(South_Korea)
    South_Korea_weekly <- convert_to_normalised_weekly(South_Korea)
    
    {ggsave(
      filename = "NC/South_Korea_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(South_Korea_weekly, "South_Korea"))
    )
    ggsave(
      filename = "NC_ND/South_Korea_NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND(South_Korea_weekly, "South_Korea")
      )
    )
    ggsave(
      filename = "NC_ND_NT/South_Korea_NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND_NT(South_Korea_weekly, "South_Korea")
      )
    )
    ggsave(
      filename = "TC/South_Korea_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(South_Korea_weekly, "South_Korea"))
    )
    ggsave(
      filename = "TC_TD/South_Korea_TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD(South_Korea_weekly, "South_Korea")
      )
    )
    ggsave(
      filename = "TC_TD_TT/South_Korea_TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD_TT(South_Korea_weekly, "South_Korea")
      )
    )
    ggsave(
      filename = "log10NC/South_Korea_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(South_Korea_weekly, "South_Korea"))
    )
    ggsave(
      filename = "log10NC_ND/South_Korea_log10NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND(South_Korea_weekly, "South_Korea")
      )
    )
    ggsave(
      filename = "log10NC_ND_NT/South_Korea_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND_NT(South_Korea_weekly, "South_Korea")
      )
    )
    ggsave(
      filename = "log10TC/South_Korea_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(South_Korea_weekly, "South_Korea"))
    )
    ggsave(
      filename = "log10TC_TD/South_Korea_log10TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD(South_Korea_weekly, "South_Korea")
      )
    )
    ggsave(
      filename = "log10TC_TD_TT/South_Korea_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD_TT(South_Korea_weekly, "South_Korea")
      )
    )}
  }
  
  ## China Plots
  {
    China <- extract_country_data("CHN")
    China <- extract_relevant(China)
    China_weekly <- convert_to_normalised_weekly(China)
    
    {ggsave(
      filename = "NC/China_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(China_weekly, "China"))
    )
    ggsave(
      filename = "NC_ND/China_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(China_weekly, "China"))
    )
    ggsave(
      filename = "NC_ND_NT/China_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(China_weekly, "China"))
    )
    ggsave(
      filename = "TC/China_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(China_weekly, "China"))
    )
    ggsave(
      filename = "TC_TD/China_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(China_weekly, "China"))
    )
    ggsave(
      filename = "TC_TD_TT/China_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(China_weekly, "China"))
    )
    ggsave(
      filename = "log10NC/China_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(China_weekly, "China"))
    )
    ggsave(
      filename = "log10NC_ND/China_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(China_weekly, "China"))
    )
    ggsave(
      filename = "log10NC_ND_NT/China_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(China_weekly, "China"))
    )
    ggsave(
      filename = "log10TC/China_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(China_weekly, "China"))
    )
    ggsave(
      filename = "log10TC_TD/China_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(China_weekly, "China"))
    )
    ggsave(
      filename = "log10TC_TD_TT/China_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(China_weekly, "China"))
    )}
  }
  
  ## South Africa Plots
  {
    South_Africa <- extract_country_data("ZAF")
    South_Africa <- extract_relevant(South_Africa)
    South_Africa_weekly <- convert_to_normalised_weekly(South_Africa)
    
    {ggsave(
      filename = "NC/South_Africa_NC.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC(South_Africa_weekly, "South_Africa")
      )
    )
    ggsave(
      filename = "NC_ND/South_Africa_NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND(South_Africa_weekly, "South_Africa")
      )
    )
    ggsave(
      filename = "NC_ND_NT/South_Africa_NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND_NT(South_Africa_weekly, "South_Africa")
      )
    )
    ggsave(
      filename = "TC/South_Africa_TC.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC(South_Africa_weekly, "South_Africa")
      )
    )
    ggsave(
      filename = "TC_TD/South_Africa_TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD(South_Africa_weekly, "South_Africa")
      )
    )
    ggsave(
      filename = "TC_TD_TT/South_Africa_TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD_TT(South_Africa_weekly, "South_Africa")
      )
    )
    ggsave(
      filename = "log10NC/South_Africa_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(South_Africa_weekly, "South_Africa"))
    )
    ggsave(
      filename = "log10NC_ND/South_Africa_log10NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND(South_Africa_weekly, "South_Africa")
      )
    )
    ggsave(
      filename = "log10NC_ND_NT/South_Africa_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND_NT(South_Africa_weekly, "South_Africa")
      )
    )
    ggsave(
      filename = "log10TC/South_Africa_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(South_Africa_weekly, "South_Africa"))
    )
    ggsave(
      filename = "log10TC_TD/South_Africa_log10TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD(South_Africa_weekly, "South_Africa")
      )
    )
    ggsave(
      filename = "log10TC_TD_TT/South_Africa_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD_TT(South_Africa_weekly, "South_Africa")
      )
    )}
  }
  
  ## Vietnam Plots
  {
    Vietnam <- extract_country_data("VNM")
    Vietnam <- extract_relevant(Vietnam)
    Vietnam_weekly <- convert_to_normalised_weekly(Vietnam)
    
    {ggsave(
      filename = "NC/Vietnam_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "NC_ND/Vietnam_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "NC_ND_NT/Vietnam_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "TC/Vietnam_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "TC_TD/Vietnam_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "TC_TD_TT/Vietnam_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "log10NC/Vietnam_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "log10NC_ND/Vietnam_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Vietnam_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "log10TC/Vietnam_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "log10TC_TD/Vietnam_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Vietnam_weekly, "Vietnam"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Vietnam_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Vietnam_weekly, "Vietnam"))
    )}
  }
  
  ## France Plots
  {
    France <- extract_country_data("FRA")
    France <- extract_relevant(France)
    France_weekly <- convert_to_normalised_weekly(France)
    
    {ggsave(
      filename = "NC/France_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(France_weekly, "France"))
    )
    ggsave(
      filename = "NC_ND/France_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(France_weekly, "France"))
    )
    ggsave(
      filename = "NC_ND_NT/France_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(France_weekly, "France"))
    )
    ggsave(
      filename = "TC/France_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(France_weekly, "France"))
    )
    ggsave(
      filename = "TC_TD/France_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(France_weekly, "France"))
    )
    ggsave(
      filename = "TC_TD_TT/France_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(France_weekly, "France"))
    )
    ggsave(
      filename = "log10NC/France_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(France_weekly, "France"))
    )
    ggsave(
      filename = "log10NC_ND/France_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(France_weekly, "France"))
    )
    ggsave(
      filename = "log10NC_ND_NT/France_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(France_weekly, "France"))
    )
    ggsave(
      filename = "log10TC/France_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(France_weekly, "France"))
    )
    ggsave(
      filename = "log10TC_TD/France_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(France_weekly, "France"))
    )
    ggsave(
      filename = "log10TC_TD_TT/France_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(France_weekly, "France"))
    )}
  }
  
  ## New Zealand Plots
  {
    New_Zealand <- extract_country_data("NZL")
    New_Zealand <- extract_relevant(New_Zealand)
    New_Zealand_weekly <- convert_to_normalised_weekly(New_Zealand)
    {ggsave(
      filename = "NC/New_Zealand_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(New_Zealand_weekly, "New_Zealand"))
    )
    ggsave(
      filename = "NC_ND/New_Zealand_NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND(New_Zealand_weekly, "New_Zealand")
      )
    )
    ggsave(
      filename = "NC_ND_NT/New_Zealand_NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND_NT(New_Zealand_weekly, "New_Zealand")
      )
    )
    ggsave(
      filename = "TC/New_Zealand_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(New_Zealand_weekly, "New_Zealand"))
    )
    ggsave(
      filename = "TC_TD/New_Zealand_TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD(New_Zealand_weekly, "New_Zealand")
      )
    )
    ggsave(
      filename = "TC_TD_TT/New_Zealand_TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD_TT(New_Zealand_weekly, "New_Zealand")
      )
    )
    ggsave(
      filename = "log10NC/New_Zealand_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(New_Zealand_weekly, "New_Zealand"))
    )
    ggsave(
      filename = "log10NC_ND/New_Zealand_log10NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND(New_Zealand_weekly, "New_Zealand")
      )
    )
    ggsave(
      filename = "log10NC_ND_NT/New_Zealand_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND_NT(New_Zealand_weekly, "New_Zealand")
      )
    )
    ggsave(
      filename = "log10TC/New_Zealand_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(New_Zealand_weekly, "New_Zealand"))
    )
    ggsave(
      filename = "log10TC_TD/New_Zealand_log10TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD(New_Zealand_weekly, "New_Zealand")
      )
    )
    ggsave(
      filename = "log10TC_TD_TT/New_Zealand_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD_TT(New_Zealand_weekly, "New_Zealand")
      )
    )}
  }
  
  ## Australia Plots
  {
    Australia <- extract_country_data("AUS")
    Australia <- extract_relevant(Australia)
    Australia_weekly <- convert_to_normalised_weekly(Australia)
    {ggsave(
      filename = "NC/Australia_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Australia_weekly, "Australia"))
    )
    ggsave(
      filename = "NC_ND/Australia_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Australia_weekly, "Australia"))
    )
    ggsave(
      filename = "NC_ND_NT/Australia_NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND_NT(Australia_weekly, "Australia")
      )
    )
    ggsave(
      filename = "TC/Australia_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Australia_weekly, "Australia"))
    )
    ggsave(
      filename = "TC_TD/Australia_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Australia_weekly, "Australia"))
    )
    ggsave(
      filename = "TC_TD_TT/Australia_TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD_TT(Australia_weekly, "Australia")
      )
    )
    ggsave(
      filename = "log10NC/Australia_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Australia_weekly, "Australia"))
    )
    ggsave(
      filename = "log10NC_ND/Australia_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Australia_weekly, "Australia"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Australia_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Australia_weekly, "Australia"))
    )
    ggsave(
      filename = "log10TC/Australia_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Australia_weekly, "Australia"))
    )
    ggsave(
      filename = "log10TC_TD/Australia_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Australia_weekly, "Australia"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Australia_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Australia_weekly, "Australia"))
    )}
  }
  
  ## Bangladesh Plots
  {
    Bangladesh <- extract_country_data("BGD")
    Bangladesh <- extract_relevant(Bangladesh)
    Bangladesh_weekly <- convert_to_normalised_weekly(Bangladesh)
    {ggsave(
      filename = "NC/Bangladesh_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Bangladesh_weekly, "Bangladesh"))
    )
    ggsave(
      filename = "NC_ND/Bangladesh_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Bangladesh_weekly, "Bangladesh"))
    )
    ggsave(
      filename = "NC_ND_NT/Bangladesh_NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND_NT(Bangladesh_weekly, "Bangladesh")
      )
    )
    ggsave(
      filename = "TC/Bangladesh_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Bangladesh_weekly, "Bangladesh"))
    )
    ggsave(
      filename = "TC_TD/Bangladesh_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Bangladesh_weekly, "Bangladesh"))
    )
    ggsave(
      filename = "TC_TD_TT/Bangladesh_TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD_TT(Bangladesh_weekly, "Bangladesh")
      )
    )
    ggsave(
      filename = "log10NC/Bangladesh_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Bangladesh_weekly, "Bangladesh"))
    )
    ggsave(
      filename = "log10NC_ND/Bangladesh_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Bangladesh_weekly, "Bangladesh"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Bangladesh_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND_NT(Bangladesh_weekly, "Bangladesh")
      )
    )
    ggsave(
      filename = "log10TC/Bangladesh_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Bangladesh_weekly, "Bangladesh"))
    )
    ggsave(
      filename = "log10TC_TD/Bangladesh_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Bangladesh_weekly, "Bangladesh"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Bangladesh_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD_TT(Bangladesh_weekly, "Bangladesh")
      )
    )}
  }
  
  ## Pakistan Plots
  {
    Pakistan <- extract_country_data("PAK")
    Pakistan <- extract_relevant(Pakistan)
    Pakistan_weekly <- convert_to_normalised_weekly(Pakistan)
    {ggsave(
      filename = "NC/Pakistan_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "NC_ND/Pakistan_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "NC_ND_NT/Pakistan_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "TC/Pakistan_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "TC_TD/Pakistan_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "TC_TD_TT/Pakistan_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "log10NC/Pakistan_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "log10NC_ND/Pakistan_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Pakistan_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "log10TC/Pakistan_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "log10TC_TD/Pakistan_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Pakistan_weekly, "Pakistan"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Pakistan_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Pakistan_weekly, "Pakistan"))
    )}
  }
  
  ## Israel Plots
  {
    Israel <- extract_country_data("ISR")
    Israel <- extract_relevant(Israel)
    Israel_weekly <- convert_to_normalised_weekly(Israel)
    {ggsave(
      filename = "NC/Israel_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "NC_ND/Israel_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "NC_ND_NT/Israel_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "TC/Israel_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "TC_TD/Israel_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "TC_TD_TT/Israel_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "log10NC/Israel_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "log10NC_ND/Israel_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Israel_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "log10TC/Israel_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "log10TC_TD/Israel_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Israel_weekly, "Israel"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Israel_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Israel_weekly, "Israel"))
    )}
  }
  
  ## Saudi Arabia Plots
  {
    Saudi_Arabia <- extract_country_data("SAU")
    Saudi_Arabia <- extract_relevant(Saudi_Arabia)
    Saudi_Arabia_weekly <- convert_to_normalised_weekly(Saudi_Arabia)
    {ggsave(
      filename = "NC/Saudi_Arabia_NC.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )
    ggsave(
      filename = "NC_ND/Saudi_Arabia_NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )
    ggsave(
      filename = "NC_ND_NT/Saudi_Arabia_NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND_NT(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )
    ggsave(
      filename = "TC/Saudi_Arabia_TC.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )
    ggsave(
      filename = "TC_TD/Saudi_Arabia_TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )
    ggsave(
      filename = "TC_TD_TT/Saudi_Arabia_TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD_TT(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )
    ggsave(
      filename = "log10NC/Saudi_Arabia_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Saudi_Arabia_weekly, "Saudi_Arabia"))
    )
    ggsave(
      filename = "log10NC_ND/Saudi_Arabia_log10NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )
    ggsave(
      filename = "log10NC_ND_NT/Saudi_Arabia_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND_NT(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )
    ggsave(
      filename = "log10TC/Saudi_Arabia_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Saudi_Arabia_weekly, "Saudi_Arabia"))
    )
    ggsave(
      filename = "log10TC_TD/Saudi_Arabia_log10TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )
    ggsave(
      filename = "log10TC_TD_TT/Saudi_Arabia_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD_TT(Saudi_Arabia_weekly, "Saudi_Arabia")
      )
    )}
  }
  
  ## Sweden Plots
  {
    Sweden <- extract_country_data("SWE")
    Sweden <- extract_relevant(Sweden)
    Sweden_weekly <- convert_to_normalised_weekly(Sweden)
    {ggsave(
      filename = "NC/Sweden_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "NC_ND/Sweden_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "NC_ND_NT/Sweden_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "TC/Sweden_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "TC_TD/Sweden_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "TC_TD_TT/Sweden_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "log10NC/Sweden_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "log10NC_ND/Sweden_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Sweden_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "log10TC/Sweden_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "log10TC_TD/Sweden_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Sweden_weekly, "Sweden"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Sweden_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Sweden_weekly, "Sweden"))
    )}
  }
  
  ## Thailand Plots
  {
    Thailand <- extract_country_data("THA")
    Thailand <- extract_relevant(Thailand)
    Thailand_weekly <- convert_to_normalised_weekly(Thailand)
    {ggsave(
      filename = "NC/Thailand_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "NC_ND/Thailand_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "NC_ND_NT/Thailand_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "TC/Thailand_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "TC_TD/Thailand_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "TC_TD_TT/Thailand_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "log10NC/Thailand_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "log10NC_ND/Thailand_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Thailand_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "log10TC/Thailand_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "log10TC_TD/Thailand_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Thailand_weekly, "Thailand"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Thailand_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Thailand_weekly, "Thailand"))
    )}
  }
  
  ## Ghana Plots
  {
    Ghana <- extract_country_data("GHA")
    Ghana <- extract_relevant(Ghana)
    Ghana_weekly <- convert_to_normalised_weekly(Ghana)
    {ggsave(
      filename = "NC/Ghana_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "NC_ND/Ghana_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "NC_ND_NT/Ghana_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "TC/Ghana_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "TC_TD/Ghana_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "TC_TD_TT/Ghana_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "log10NC/Ghana_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "log10NC_ND/Ghana_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Ghana_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "log10TC/Ghana_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "log10TC_TD/Ghana_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Ghana_weekly, "Ghana"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Ghana_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Ghana_weekly, "Ghana"))
    )}
  }
  
  ## Libya Plots
  {
    Libya <- extract_country_data("LBY")
    Libya <- extract_relevant(Libya)
    Libya_weekly <- convert_to_normalised_weekly(Libya)
    {ggsave(
      filename = "NC/Libya_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "NC_ND/Libya_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "NC_ND_NT/Libya_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "TC/Libya_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "TC_TD/Libya_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "TC_TD_TT/Libya_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "log10NC/Libya_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "log10NC_ND/Libya_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Libya_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "log10TC/Libya_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "log10TC_TD/Libya_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Libya_weekly, "Libya"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Libya_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Libya_weekly, "Libya"))
    )}
  }
  
  ## Iraq Plots
  {
    Iraq <- extract_country_data("IRQ")
    Iraq <- extract_relevant(Iraq)
    Iraq_weekly <- convert_to_normalised_weekly(Iraq)
    {ggsave(
      filename = "NC/Iraq_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "NC_ND/Iraq_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "NC_ND_NT/Iraq_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "TC/Iraq_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "TC_TD/Iraq_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "TC_TD_TT/Iraq_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "log10NC/Iraq_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "log10NC_ND/Iraq_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Iraq_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "log10TC/Iraq_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "log10TC_TD/Iraq_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Iraq_weekly, "Iraq"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Iraq_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Iraq_weekly, "Iraq"))
    )}
  }
  
  ## Turkey Plots
  {
    Turkey <- extract_country_data("TUR")
    Turkey <- extract_relevant(Turkey)
    Turkey_weekly <- convert_to_normalised_weekly(Turkey)
    {ggsave(
      filename = "NC/Turkey_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "NC_ND/Turkey_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "NC_ND_NT/Turkey_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "TC/Turkey_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "TC_TD/Turkey_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "TC_TD_TT/Turkey_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "log10NC/Turkey_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "log10NC_ND/Turkey_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Turkey_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "log10TC/Turkey_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "log10TC_TD/Turkey_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Turkey_weekly, "Turkey"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Turkey_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Turkey_weekly, "Turkey"))
    )}
  }
  
  ## Syria Plots
  {
    Syria <- extract_country_data("SYR")
    Syria <- extract_relevant(Syria)
    Syria_weekly <- convert_to_normalised_weekly(Syria)
    {ggsave(
      filename = "NC/Syria_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "NC_ND/Syria_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "NC_ND_NT/Syria_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "TC/Syria_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "TC_TD/Syria_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "TC_TD_TT/Syria_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "log10NC/Syria_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "log10NC_ND/Syria_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Syria_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "log10TC/Syria_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "log10TC_TD/Syria_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Syria_weekly, "Syria"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Syria_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Syria_weekly, "Syria"))
    )}
  }
  
  ## Somalia Plots
  {
    Somalia <- extract_country_data("SOM")
    Somalia <- extract_relevant(Somalia)
    Somalia_weekly <- convert_to_normalised_weekly(Somalia)
    {ggsave(
      filename = "NC/Somalia_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "NC_ND/Somalia_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "NC_ND_NT/Somalia_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "TC/Somalia_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "TC_TD/Somalia_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "TC_TD_TT/Somalia_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "log10NC/Somalia_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "log10NC_ND/Somalia_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Somalia_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "log10TC/Somalia_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "log10TC_TD/Somalia_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Somalia_weekly, "Somalia"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Somalia_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Somalia_weekly, "Somalia"))
    )}
  }
  
  ## Afghanistan Plots
  {
    Afghanistan <- extract_country_data("AFG")
    Afghanistan <- extract_relevant(Afghanistan)
    Afghanistan_weekly <- convert_to_normalised_weekly(Afghanistan)
    {ggsave(
      filename = "NC/Afghanistan_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Afghanistan_weekly, "Afghanistan"))
    )
    ggsave(
      filename = "NC_ND/Afghanistan_NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND(Afghanistan_weekly, "Afghanistan")
      )
    )
    ggsave(
      filename = "NC_ND_NT/Afghanistan_NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearNC_ND_NT(Afghanistan_weekly, "Afghanistan")
      )
    )
    ggsave(
      filename = "TC/Afghanistan_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Afghanistan_weekly, "Afghanistan"))
    )
    ggsave(
      filename = "TC_TD/Afghanistan_TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD(Afghanistan_weekly, "Afghanistan")
      )
    )
    ggsave(
      filename = "TC_TD_TT/Afghanistan_TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_linearTC_TD_TT(Afghanistan_weekly, "Afghanistan")
      )
    )
    ggsave(
      filename = "log10NC/Afghanistan_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Afghanistan_weekly, "Afghanistan"))
    )
    ggsave(
      filename = "log10NC_ND/Afghanistan_log10NC_ND.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND(Afghanistan_weekly, "Afghanistan")
      )
    )
    ggsave(
      filename = "log10NC_ND_NT/Afghanistan_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10NC_ND_NT(Afghanistan_weekly, "Afghanistan")
      )
    )
    ggsave(
      filename = "log10TC/Afghanistan_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Afghanistan_weekly, "Afghanistan"))
    )
    ggsave(
      filename = "log10TC_TD/Afghanistan_log10TC_TD.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD(Afghanistan_weekly, "Afghanistan")
      )
    )
    ggsave(
      filename = "log10TC_TD_TT/Afghanistan_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(
        generatePlot_log10TC_TD_TT(Afghanistan_weekly, "Afghanistan")
      )
    )}
  }
  
  ## Yemen Plots
  {
    Yemen <- extract_country_data("YEM")
    Yemen <- extract_relevant(Yemen)
    Yemen_weekly <- convert_to_normalised_weekly(Yemen)
    {ggsave(
      filename = "NC/Yemen_NC.png",
      scale = 1,
      plot = plot(generatePlot_linearNC(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "NC_ND/Yemen_NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "NC_ND_NT/Yemen_NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_linearNC_ND_NT(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "TC/Yemen_TC.png",
      scale = 1,
      plot = plot(generatePlot_linearTC(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "TC_TD/Yemen_TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "TC_TD_TT/Yemen_TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_linearTC_TD_TT(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "log10NC/Yemen_log10NC.png",
      scale = 1,
      plot = plot(generatePlot_log10NC(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "log10NC_ND/Yemen_log10NC_ND.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "log10NC_ND_NT/Yemen_log10NC_ND_NT.png",
      scale = 1,
      plot = plot(generatePlot_log10NC_ND_NT(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "log10TC/Yemen_log10TC.png",
      scale = 1,
      plot = plot(generatePlot_log10TC(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "log10TC_TD/Yemen_log10TC_TD.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD(Yemen_weekly, "Yemen"))
    )
    ggsave(
      filename = "log10TC_TD_TT/Yemen_log10TC_TD_TT.png",
      scale = 1,
      plot = plot(generatePlot_log10TC_TD_TT(Yemen_weekly, "Yemen"))
    )}
  }
}

## Multi-plots

## Load data to absolute week standard for comparisons between countries
{
  ## USA Plots
  {
    USA <- extract_country_data("USA")
    USA <- extract_relevant(USA)
    USA_weekly <- convert_to_absolute_weekly(USA)
  }
  
  ## Brazil Plots
  {
    Brazil <- extract_country_data("BRA")
    Brazil <- extract_relevant(Brazil)
    Brazil_weekly <- convert_to_absolute_weekly(Brazil)
  }
  
  ## Russia Plots
  {
    Russia <- extract_country_data("RUS")
    Russia <- extract_relevant(Russia)
    Russia_weekly <- convert_to_absolute_weekly(Russia)
  }
  
  ## India Plots
  {
    India <- extract_country_data("IND")
    India <- extract_relevant(India)
    India_weekly <- convert_to_absolute_weekly(India)
  }
  
  ## UK Plots
  {
    UK <- extract_country_data("GBR")
    UK <- extract_relevant(UK)
    UK_weekly <- convert_to_absolute_weekly(UK)
  }
  
  ## Spain Plots
  {
    Spain <- extract_country_data("ESP")
    Spain <- extract_relevant(Spain)
    Spain_weekly <- convert_to_absolute_weekly(Spain)
  }
  
  ## Italy Plots
  {
    Italy <- extract_country_data("ITA")
    Italy <- extract_relevant(Italy)
    Italy_weekly <- convert_to_absolute_weekly(Italy)
  }
  
  ## Peru Plots
  {
    Peru <- extract_country_data("PER")
    Peru <- extract_relevant(Peru)
    Peru_weekly <- convert_to_absolute_weekly(Peru)
  }
  
  ## Germany Plots
  {
    Germany <- extract_country_data("DEU")
    Germany <- extract_relevant(Germany)
    Germany_weekly <- convert_to_absolute_weekly(Germany)
  }
  
  ## Iran Plots
  {
    Iran <- extract_country_data("IRN")
    Iran <- extract_relevant(Iran)
    Iran_weekly <- convert_to_absolute_weekly(Iran)
  }
  
  ## South Korea Plots
  {
    South_Korea <- extract_country_data("KOR")
    South_Korea <- extract_relevant(South_Korea)
    South_Korea_weekly <- convert_to_absolute_weekly(South_Korea)
  }
  
  ## China Plots
  {
    China <- extract_country_data("CHN")
    China <- extract_relevant(China)
    China_weekly <- convert_to_absolute_weekly(China)
  }
  
  ## South Africa Plots
  {
    South_Africa <- extract_country_data("ZAF")
    South_Africa <- extract_relevant(South_Africa)
    South_Africa_weekly <- convert_to_absolute_weekly(South_Africa)
  }
  
  ## Vietnam Plots
  {
    Vietnam <- extract_country_data("VNM")
    Vietnam <- extract_relevant(Vietnam)
    Vietnam_weekly <- convert_to_absolute_weekly(Vietnam)
  }
  
  ## France Plots
  {
    France <- extract_country_data("FRA")
    France <- extract_relevant(France)
    France_weekly <- convert_to_absolute_weekly(France)
  }
  
  ## New Zealand Plots
  {
    New_Zealand <- extract_country_data("NZL")
    New_Zealand <- extract_relevant(New_Zealand)
    New_Zealand_weekly <- convert_to_absolute_weekly(New_Zealand)
  }
  
  ## Australia Plots
  {
    Australia <- extract_country_data("AUS")
    Australia <- extract_relevant(Australia)
    Australia_weekly <- convert_to_absolute_weekly(Australia)
  }
  
  ## Bangladesh Plots
  {
    Bangladesh <- extract_country_data("BGD")
    Bangladesh <- extract_relevant(Bangladesh)
    Bangladesh_weekly <- convert_to_absolute_weekly(Bangladesh)
  }
  
  ## Pakistan Plots
  {
    Pakistan <- extract_country_data("PAK")
    Pakistan <- extract_relevant(Pakistan)
    Pakistan_weekly <- convert_to_absolute_weekly(Pakistan)
  }
  
  ## Israel Plots
  {
    Israel <- extract_country_data("ISR")
    Israel <- extract_relevant(Israel)
    Israel_weekly <- convert_to_absolute_weekly(Israel)
  }
  
  ## Saudi Arabia Plots
  {
    Saudi_Arabia <- extract_country_data("SAU")
    Saudi_Arabia <- extract_relevant(Saudi_Arabia)
    Saudi_Arabia_weekly <- convert_to_absolute_weekly(Saudi_Arabia)
  }
  
  ## Sweden Plots
  {
    Sweden <- extract_country_data("SWE")
    Sweden <- extract_relevant(Sweden)
    Sweden_weekly <- convert_to_absolute_weekly(Sweden)
  }
  
  ## Thailand Plots
  {
    Thailand <- extract_country_data("THA")
    Thailand <- extract_relevant(Thailand)
    Thailand_weekly <- convert_to_absolute_weekly(Thailand)
  }
  
  ## Ghana Plots
  {
    Ghana <- extract_country_data("GHA")
    Ghana <- extract_relevant(Ghana)
    Ghana_weekly <- convert_to_absolute_weekly(Ghana)
  }
  
  ## Libya Plots
  {
    Libya <- extract_country_data("LBY")
    Libya <- extract_relevant(Libya)
    Libya_weekly <- convert_to_absolute_weekly(Libya)
  }
  
  ## Iraq Plots
  {
    Iraq <- extract_country_data("IRQ")
    Iraq <- extract_relevant(Iraq)
    Iraq_weekly <- convert_to_absolute_weekly(Iraq)
  }
  
  ## Turkey Plots
  {
    Turkey <- extract_country_data("TUR")
    Turkey <- extract_relevant(Turkey)
    Turkey_weekly <- convert_to_absolute_weekly(Turkey)
  }
  
  ## Syria Plots
  {
    Syria <- extract_country_data("SYR")
    Syria <- extract_relevant(Syria)
    Syria_weekly <- convert_to_absolute_weekly(Syria)
  }
  
  ## Somalia Plots
  {
    Somalia <- extract_country_data("SOM")
    Somalia <- extract_relevant(Somalia)
    Somalia_weekly <- convert_to_absolute_weekly(Somalia)
  }
  
  ## Afghanistan Plots
  {
    Afghanistan <- extract_country_data("AFG")
    Afghanistan <- extract_relevant(Afghanistan)
    Afghanistan_weekly <- convert_to_absolute_weekly(Afghanistan)
  }
  
  ## Yemen Plots
  {
    Yemen <- extract_country_data("YEM")
    Yemen <- extract_relevant(Yemen)
    Yemen_weekly <- convert_to_absolute_weekly(Yemen)
  }
}

{
  ## Make Spain (23) and Turkey (14) equivalent in terms of rows to add to a tibble without issues (as of June 17 2020)
  Spain_weekly[nrow(Spain_weekly)+1,] <- NA
  for(i in seq(1,10)){Turkey_weekly[nrow(Turkey_weekly)+1,] <- NA}

bellCurveCountries_NCdata_withUSA <- tibble(Week = USA_weekly$week, Australia = Australia_weekly$new_cases, 
                                    China = China_weekly$new_cases,
                                    France = France_weekly$new_cases, Germany = Germany_weekly$new_cases, Iran = Iran_weekly$new_cases,
                                    Israel = Israel_weekly$new_cases, Italy = Italy_weekly$new_cases, 
                                    New_Zealand = New_Zealand_weekly$new_cases, Russia = Russia_weekly$new_cases,
                                    Spain = Spain_weekly$new_cases, Thailand = Thailand_weekly$new_cases, 
                                    Turkey = Turkey_weekly$new_cases, UK = UK_weekly$new_cases,
                                    USA = USA_weekly$new_cases,)

bellCurveCountries_NCdata_withoutUSA <- tibble(Week = USA_weekly$week, Australia = Australia_weekly$new_cases, 
                                            China = China_weekly$new_cases,
                                            France = France_weekly$new_cases, Germany = Germany_weekly$new_cases, 
                                            Iran = Iran_weekly$new_cases,
                                            Israel = Israel_weekly$new_cases, Italy = Italy_weekly$new_cases, 
                                            New_Zealand = New_Zealand_weekly$new_cases, Russia = Russia_weekly$new_cases,
                                            Spain = Spain_weekly$new_cases, Thailand = Thailand_weekly$new_cases, 
                                            Turkey = Turkey_weekly$new_cases, UK = UK_weekly$new_cases)
  

bellCurveCountries_NCdata_withUSA_long <- melt(bellCurveCountries_NCdata_withUSA, id="Week")
bellCurveCountries_NCdata_withoutUSA_long <- melt(bellCurveCountries_NCdata_withoutUSA, id="Week")

a <- bellCurveCountries_NCdata_withUSA_long %>%
  ggplot(aes(x=Week, y=value, group=variable, fill=variable))+
  facet_wrap(~ bellCurveCountries_NCdata_withUSA_long$variable, scales = "free") +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none") +
  ggtitle("New COVID-19 Cases/week starting from 1 January 2020") +
  theme_ipsum()
  theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), 
        plot.title = element_text(size = 14))
  

  tmp <- bellCurveCountries_NCdata_withUSA_long %>%
    mutate(variable2=variable)
  
  b <- tmp %>%
    ggplot( aes(x=Week, y=value)) +
    geom_line( data=tmp %>% dplyr::select(-variable), aes(group=variable2), color="#003554", size=0.5, alpha=0.2) +
    geom_point(size = 0.5, alpha=0.5, color = "0988AB") +
    geom_line( aes(color=variable), color="red", size=0.75, alpha = 0.8 )+
    scale_color_viridis(discrete = TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14),
      panel.grid = element_blank()
    ) +
    ggtitle("New COVID-19 Cases/week starting from 1 January 2020") +
    facet_wrap(~variable, scales = "free")
  
    
  
  }

##### UNORGANISED EXPORT CODE FOR QUICK PLOTTING #####

ggsave("bellCurveNC_Countries.png", scale=2, plot = a)
ggsave("bellCurveNC_Countries_patterns.png", scale = 2, plot=b)

{
all_countries <- tibble(tibble(Week = USA_weekly$week, Afghanistan = Afghanistan_weekly$new_cases, 
                               Australia = Australia_weekly$new_cases, Bangladesh = Bangladesh_weekly$new_cases,
                               Brazil = Brazil_weekly$new_cases, 
                               China = China_weekly$new_cases,
                               France = France_weekly$new_cases, Germany = Germany_weekly$new_cases, 
                               Ghana = Ghana_weekly$new_cases, India = India_weekly$new_cases,
                               Iran = Iran_weekly$new_cases, Iraq = Iraq_weekly$new_cases, 
                               Israel = Israel_weekly$new_cases, Italy = Italy_weekly$new_cases,
                               Libya = Libya_weekly$new_cases, 
                               New_Zealand = New_Zealand_weekly$new_cases, Pakistan = Pakistan_weekly$new_cases,
                               Peru = Peru_weekly$new_cases, 
                               Russia = Russia_weekly$new_cases, Saudi_Arabia = Saudi_Arabia_weekly$new_cases,
                               Somalia = Somalia_weekly$new_cases, South_Africa = South_Africa_weekly$new_cases,
                               South_Korea = South_Korea_weekly$new_cases, Sweden = Sweden_weekly$new_cases,
                               Spain = Spain_weekly$new_cases, Thailand = Thailand_weekly$new_cases, 
                               Turkey = Turkey_weekly$new_cases, UK = UK_weekly$new_cases,
                               USA = USA_weekly$new_cases, Vietnam = Vietnam_weekly$new_cases, 
                               Yemen=Yemen_weekly$new_cases),)

## Syria 13, Bangladesh 15, Ghana 14, Libya 12, Peru 15, Saudi 15, Somalia 13, South Africa 19, Syria 13, Yemen 10

for(i in seq(1,11)){Syria_weekly[nrow(Syria_weekly)+1,] <- NA}
for(i in seq(1,9)){Bangladesh_weekly[nrow(Bangladesh_weekly)+1,] <- NA}
for(i in seq(1,10)){Ghana_weekly[nrow(Ghana_weekly)+1,] <- NA}
for(i in seq(1,12)){Libya_weekly[nrow(Libya_weekly)+1,] <- NA}
for(i in seq(1,9)){Peru_weekly[nrow(Peru_weekly)+1,] <- NA}
for(i in seq(1,9)){Saudi_Arabia_weekly[nrow(Saudi_Arabia_weekly)+1,] <- NA}
for(i in seq(1,11)){Somalia_weekly[nrow(Somalia_weekly)+1,] <- NA}
for(i in seq(1,5)){South_Africa_weekly[nrow(South_Africa_weekly)+1,] <- NA}
for(i in seq(1,11)){Syria_weekly[nrow(Syria_weekly)+1,] <- NA}
for(i in seq(1,14)){Yemen_weekly[nrow(Yemen_weekly)+1,] <- NA}

all_countries_NCdata_long <- melt(all_countries, id="Week")

c <- all_countries_NCdata_long %>%
  ggplot(aes(x=Week, y=value, group=variable, fill=variable))+
  facet_wrap(~all_countries_NCdata_long$variable, scales = "free") +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none") +
  ggtitle("New COVID-19 Cases/week starting from 1 January 2020") +
  theme_ipsum()
theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8), 
      plot.title = element_text(size = 14))


tmp <- all_countries_NCdata_long %>%
  mutate(variable2=variable)

d <- tmp %>%
  ggplot( aes(x=Week, y=value)) +
  geom_line( data=tmp %>% dplyr::select(-variable), aes(group=variable2), color="#003554", size=0.5, alpha=0.2) +
  geom_point(size = 0.5, alpha=0.5, color = "0988AB") +
  geom_line( aes(color=variable), color="red", size=0.75, alpha = 0.8 )+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("New COVID-19 Cases/week starting from 1 January 2020") +
  facet_wrap(~variable, scales = "free")

ggsave("all_countriesNC.png", scale=2, plot=c)
ggsave("all_countriesNC_pattern.png", scale=2, plot=d)

}

USA <- extract_country_data("USA")
USA_static <- extract_static(USA)
USA_static <- USA_static[-(2:nrow(USA_static)),]
USA_static <- melt(USA_static)
USA_static <- cbind(id = seq(1:nrow(USA_static)), USA_static)
{
  
  generate_circleBarPlot <- function(ISO_code){
    country <- extract_country_data(ISO_code)
    data <- extract_static(country)
    data$date <- NULL
    data$population <- NULL
    data <- data[-(2:nrow(data)),]
    data <- melt(data)
    data <- cbind(id = seq(1:nrow(data)), data)
    
    label_data <- data
  
    # calculate the ANGLE of the labels
    number_of_bar <- nrow(label_data)
    angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    
    # calculate the alignment of labels: right or left
    # If I am on the left part of the plot, my labels have currently an angle < -90
    label_data$hjust<-ifelse( angle < -90, 1, 0)
    
    # flip angle BY to make them readable
    label_data$angle<-ifelse(angle < -90, angle+180, angle)
    # ----- ------------------------------------------- ---- #
    
    
    # Start the plot
    p <- ggplot(data, aes(x=as.numeric(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      # This add the bars with a blue color
      geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
      
      # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
      ylim(-10,150) +
      
      # Custom the theme: no axis title and no cartesian grid
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
      ) +
      
      # This makes the coordinate polar instead of cartesian.
      coord_polar(start = 0) +
      
      # Add the labels, using the label_data dataframe that we have created before
      geom_text(data=label_data, aes(x=id, y=value+15, label=value, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
      geom_text(data=label_data, aes(x=id, y=value+40, label=variable, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )

  }
    
  
  data <- (generate_circleBarPlot("USA"))
  
  plot(generate_circleBarPlot("IND"))
  
  
  {
  ggsave("USA_static.png", scale = 2, plot=plot(generate_circleBarPlot("USA")))
  ggsave("Brazil_static.png", scale = 2, plot=plot(generate_circleBarPlot("BRA")))
  ggsave("Russia_static.png", scale = 2, plot=plot(generate_circleBarPlot("RUS")))
  ggsave("India_static.png", scale = 2, plot=plot(generate_circleBarPlot("IND")))
  ggsave("UK_static.png", scale = 2, plot=plot(generate_circleBarPlot("GBR")))
  ggsave("Spain_static.png", scale = 2, plot=plot(generate_circleBarPlot("ESP")))
  ggsave("Italy_static.png", scale = 2, plot=plot(generate_circleBarPlot("ITA")))
  ggsave("Peru_static.png", scale = 2, plot=plot(generate_circleBarPlot("PER")))
  ggsave("Germany_static.png", scale = 2, plot=plot(generate_circleBarPlot("DEU")))
  ggsave("Iran_static.png", scale = 2, plot=plot(generate_circleBarPlot("IRN")))
  ggsave("South_Korea_static.png", scale = 2, plot=plot(generate_circleBarPlot("KOR")))
  ggsave("China_static.png", scale = 2, plot=plot(generate_circleBarPlot("CHN")))
  ggsave("South_Africa_static.png", scale = 2, plot=plot(generate_circleBarPlot("ZAF")))
  ggsave("Vietnam_static.png", scale = 2, plot=plot(generate_circleBarPlot("VNM")))
  ggsave("France_static.png", scale = 2, plot=plot(generate_circleBarPlot("FRA")))
  ggsave("New_Zealand_static.png", scale = 2, plot=plot(generate_circleBarPlot("NZL")))
  ggsave("Australia_static.png", scale = 2, plot=plot(generate_circleBarPlot("AUS")))
  ggsave("Bangladesh_static.png", scale = 2, plot=plot(generate_circleBarPlot("BGD")))
  ggsave("Pakistan_static.png", scale = 2, plot=plot(generate_circleBarPlot("PAK")))
  ggsave("Israel_static.png", scale = 2, plot=plot(generate_circleBarPlot("ISR")))
  ggsave("Saudi_Arabia_static.png", scale = 2, plot=plot(generate_circleBarPlot("SAU")))
  ggsave("Sweden_static.png", scale = 2, plot=plot(generate_circleBarPlot("SWE")))
  ggsave("Thailand_static.png", scale = 2, plot=plot(generate_circleBarPlot("THA")))
  ggsave("Ghana_static.png", scale = 2, plot=plot(generate_circleBarPlot("GHA")))
  ggsave("Libya_static.png", scale = 2, plot=plot(generate_circleBarPlot("LBY")))
  ggsave("Iraq_static.png", scale = 2, plot=plot(generate_circleBarPlot("IRQ")))
  ggsave("Turkey_static.png", scale = 2, plot=plot(generate_circleBarPlot("TUR")))
  ggsave("Syria_static.png", scale = 2, plot=plot(generate_circleBarPlot("SYR")))
  ggsave("Somalia_static.png", scale = 2, plot=plot(generate_circleBarPlot("SOM")))
  ggsave("Afghanistan_static.png", scale = 2, plot=plot(generate_circleBarPlot("AFG")))
  ggsave("Yemen_static.png", scale = 2, plot=plot(generate_circleBarPlot("YEM")))

  }
  
  
  
  
  
  
  
  
  
  
  
  
}


