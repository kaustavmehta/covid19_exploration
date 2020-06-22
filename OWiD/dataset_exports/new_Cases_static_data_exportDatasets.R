#execute this code block to run the whole program
{
  setwd("~/Desktop/COVID Research/Global COVID19 Dataset - Our World in Data/dataset_exports")
{
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(reshape2)
}
#Source: https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv

global_covid_data <- read_csv("~/Desktop/COVID Research/Global COVID19 Dataset - Our World in Data/covid-19-data/public/data/owid-covid-data.csv")

#View(global_covid_data)

global_covid_data$date <- as.Date(global_covid_data$date, "%d%m%y")
#global_covid_data$date <- format(global_covid_data$date, "%Y-%V")

#Top 10 countries in terms of COVID-19 cases (as of 15th June 2020): USA, Brazil, Russia, India, UK, Spain, Italy, Peru, ...
#..., Germany, Iran ; + South Korea and China to observe rising and falling phases 
{
USA <- global_covid_data[grep("USA", global_covid_data$iso_code),]
brazil <- global_covid_data[grep("BRA", global_covid_data$iso_code),]
russia <- global_covid_data[grep("RUS", global_covid_data$iso_code),]
india <- global_covid_data[grep("IND", global_covid_data$iso_code),]
UK <- global_covid_data[grep("GBR", global_covid_data$iso_code),]
spain <- global_covid_data[grep("ESP", global_covid_data$iso_code),]
italy <- global_covid_data[grep("ITA", global_covid_data$iso_code),]
peru <- global_covid_data[grep("PER", global_covid_data$iso_code),]
germany <- global_covid_data[grep("DEU", global_covid_data$iso_code),]
iran <- global_covid_data[grep("IRN", global_covid_data$iso_code),]
south_korea <- global_covid_data[grep("KOR", global_covid_data$iso_code),]
china <- global_covid_data[grep("CHN", global_covid_data$iso_code),]
####
{
south_africa <- global_covid_data[grep("ZAF", global_covid_data$iso_code),]
vietnam <- global_covid_data[grep("VNM", global_covid_data$iso_code),]
france <- global_covid_data[grep("FRA", global_covid_data$iso_code),]
new_zealand <- global_covid_data[grep("NZL", global_covid_data$iso_code),]
australia <- global_covid_data[grep("AUS", global_covid_data$iso_code),]
bangladesh <- global_covid_data[grep("BGD", global_covid_data$iso_code),]
pakistan <- global_covid_data[grep("PAK", global_covid_data$iso_code),]
israel <- global_covid_data[grep("ISR", global_covid_data$iso_code),]
saudi_arabia <- global_covid_data[grep("SAU", global_covid_data$iso_code),]
sweden <- global_covid_data[grep("SWE", global_covid_data$iso_code),]
thailand <- global_covid_data[grep("THA", global_covid_data$iso_code),]
#### excepting for Ghana, adding war-torn and conflict ridden countries from
#### source: https://en.wikipedia.org/wiki/List_of_ongoing_armed_conflicts
ghana <- global_covid_data[grep("GHA", global_covid_data$iso_code),]
libya <- global_covid_data[grep("LBY", global_covid_data$iso_code),]
iraq <- global_covid_data[grep("IRQ", global_covid_data$iso_code),]
turkey <- global_covid_data[grep("TUR", global_covid_data$iso_code),]
syria <- global_covid_data[grep("SYR", global_covid_data$iso_code),]
somalia <- global_covid_data[grep("SOM", global_covid_data$iso_code),]
afghanistan <- global_covid_data[grep("AFG", global_covid_data$iso_code),]
yemen <- global_covid_data[grep("YEM", global_covid_data$iso_code),]

}

}
#datasets_list <- c(USA, brazil, russia, india, UK, spain, italy, peru, germany, iran, south_korea, china)
#datasets_list <- c("USA", "brazil", "russia", "india", "UK", "spain", "italy", "peru", "germany", "iran", "south_korea", "china")

#Demarcating relevant, irrelevant and unchanging data in the datasets
#irrelevant_data <- c(1, 2, 15, 16, 17, 18, 19, 20)
#relevant_data <- c("total_cases", "new_cases", "total_deaths", "new_deaths", "total_cases_per_million", "new_cases_per_million", "total_deaths_per_million", "new_deaths_per_million")


{
relevant_data <- c("total_cases", "total_deaths", "new_cases", "new_deaths", "total_tests", "new_tests")
irrelevant_data <- c(1, 2, 3, 9, 10, 11, 12, 15, 16, 17, 18, 19, 20)
static_data <- c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34)
}

### Exporting Relevant and Static Datasets

USA_static <- select(USA, -all_of(relevant_data), -all_of(irrelevant_data))
USA <- select(USA, -all_of(irrelevant_data), -all_of(static_data))
  USA <- USA[-(1:1),]

#Reducing dataset to relevant time-series numeric data + remove 2019 data because it is insignificant
#Source: https://www.marsja.se/how-to-remove-a-column-in-r-using-dplyr-by-name-and-index/
# {
# USA <- select(USA, -all_of(irrelevant_data), -all_of(static_data))
#   USA <- USA[-(1:1),]
# brazil <- select(brazil, -all_of(irrelevant_data), -all_of(static_data))
#   brazil <- brazil[-(1:1),]
# russia <- select(russia, -all_of(irrelevant_data), -all_of(static_data))
#   russia <- russia[-(1:1),]
# india <- select(india, -all_of(irrelevant_data), -all_of(static_data))
#   india <- india[-(1:1),]
# UK <- select(UK, -all_of(irrelevant_data), -all_of(static_data))
#   UK <- UK[-(1:1),]
# spain <- select(spain, -all_of(irrelevant_data), -all_of(static_data))
#   spain <- spain[-(1:1),]
# italy <- select(italy, -all_of(irrelevant_data), -all_of(static_data))
#   italy <- italy[-(1:1),]
# peru <- select(peru, -all_of(irrelevant_data), -all_of(static_data))
#   peru <- peru[-(1:1),]
# germany <- select(germany, -all_of(irrelevant_data), -all_of(static_data))
#   germany <- germany[-(1:1),]
# iran <- select(iran, -all_of(irrelevant_data), -all_of(static_data))
#   iran <- iran[-(1:1),]
# south_korea <- select(south_korea, -all_of(irrelevant_data), -all_of(static_data))
#   south_korea <- south_korea[-(1:1),]
# china <- select(china, -all_of(irrelevant_data), -all_of(static_data))
#   china <- china[-(1:1),]
# #########
#   {
#   south_africa <- select(south_africa, -all_of(irrelevant_data), -all_of(static_data))
#   south_africa <- south_africa[-(1:1),]
#   france <- select(france, -all_of(irrelevant_data), -all_of(static_data))
#   france <- france[-(1:1),]
#   vietnam <- select(vietnam, -all_of(irrelevant_data), -all_of(static_data))
#   vietnam <- vietnam[-(1:1),]
#   new_zealand <- select(new_zealand, -all_of(irrelevant_data), -all_of(static_data))
#   new_zealand <- new_zealand[-(1:1),]
#   australia <- select(australia, -all_of(irrelevant_data), -all_of(static_data))
#   australia <- australia[-(1:1),]
#   bangladesh <- select(bangladesh, -all_of(irrelevant_data), -all_of(static_data))
#   bangladesh <- bangladesh[-(1:1),]
#   pakistan <- select(pakistan, -all_of(irrelevant_data), -all_of(static_data))
#   pakistan <- pakistan[-(1:1),]
#   israel <- select(israel, -all_of(irrelevant_data), -all_of(static_data))
#   israel <- israel[-(1:1),]
#   saudi_arabia <- select(saudi_arabia, -all_of(irrelevant_data), -all_of(static_data))
#   saudi_arabia <- saudi_arabia[-(1:1),]
#   sweden <- select(sweden, -all_of(irrelevant_data), -all_of(static_data))
#   sweden <- sweden[-(1:1),]
#   thailand <- select(thailand, -all_of(irrelevant_data), -all_of(static_data))
#   thailand <- thailand[-(1:1),]
#   }
#   }

##### TRYING TO USE LOOPS TO CREATE WEEKLY DATASETS OF RELEVANT NUMERIC TIME SERIES DATA #####


# countries <- c("south_africa", "france", "vietnam", "new_zealand", "australia", "bangladesh",
#                "pakistan", "israel", "saudi_arabia", "sweden", "thailand")
#Create weekly dataset of numeric time-series of countries
#Source: https://stackoverflow.com/questions/49756987/r-aggregate-by-week
{

USA_weekly <- USA %>%
       group_by(week = week(USA$date)) %>%
       summarize_if(is.numeric, sum)
  
brazil_weekly <- brazil %>%
  group_by(week = week(brazil$date)) %>%
  summarize_if(is.numeric, sum)

russia_weekly <- russia %>%
  group_by(week = week(russia$date)) %>%
  summarize_if(is.numeric, sum)

india_weekly <- india %>%
  group_by(week = week(india$date)) %>%
  summarize_if(is.numeric, sum)

UK_weekly <- UK %>%
  group_by(week = week(UK$date)) %>%
  summarize_if(is.numeric, sum)

spain_weekly <- spain %>%
  group_by(week = week(spain$date)) %>%
  summarize_if(is.numeric, sum)

italy_weekly <- italy %>%
  group_by(week = week(italy$date)) %>%
  summarize_if(is.numeric, sum)

peru_weekly <- peru %>%
  group_by(week = week(peru$date)) %>%
  summarize_if(is.numeric, sum)

germany_weekly <- germany %>%
  group_by(week = week(germany$date)) %>%
  summarize_if(is.numeric, sum)

iran_weekly <- iran %>%
  group_by(week = week(iran$date)) %>%
  summarize_if(is.numeric, sum)

south_korea_weekly <- south_korea %>%
  group_by(week = week(south_korea$date)) %>%
  summarize_if(is.numeric, sum)

china_weekly <- china %>%
  group_by(week = week(china$date)) %>%
  summarize_if(is.numeric, sum)
#########
{
south_africa_weekly <- south_africa %>%
  group_by(week = week(south_africa$date)) %>%
  summarize_if(is.numeric, sum)

france_weekly <- france %>%
  group_by(week = week(france$date)) %>%
  summarize_if(is.numeric, sum)

vietnam_weekly <- vietnam %>%
  group_by(week = week(vietnam$date)) %>%
  summarize_if(is.numeric, sum)

new_zealand_weekly <- new_zealand %>%
  group_by(week = week(new_zealand$date)) %>%
  summarize_if(is.numeric, sum)

australia_weekly <- australia %>%
  group_by(week = week(australia$date)) %>%
  summarize_if(is.numeric, sum)

bangladesh_weekly <- bangladesh %>%
  group_by(week = week(bangladesh$date)) %>%
  summarize_if(is.numeric, sum)

pakistan_weekly <- pakistan %>%
  group_by(week = week(pakistan$date)) %>%
  summarize_if(is.numeric, sum)

israel_weekly <- israel %>%
  group_by(week = week(israel$date)) %>%
  summarize_if(is.numeric, sum)

saudi_arabia_weekly <- saudi_arabia %>%
  group_by(week = week(saudi_arabia$date)) %>%
  summarize_if(is.numeric, sum)

sweden_weekly <- sweden %>%
  group_by(week = week(sweden$date)) %>%
  summarize_if(is.numeric, sum)

thailand_weekly <- thailand %>%
  group_by(week = week(thailand$date)) %>%
  summarize_if(is.numeric, sum)

}
}


### GRAPHING ###
#Source: https://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html#4.%20Changing%20the%20title%20and%20axis%20labels

options(scipen=999) #disables scientific notation in the graphs
{
# Graph for United States
  {
  #Graph of Total Cases for USA in log10
  {
  USA_TC_log10 <- ggplot(USA_weekly, aes(x=week, y=total_cases)) + 
    scale_y_continuous("Total Cases (log-10 scale)", 
                     trans = 'log10',
                     labels = function(x){paste0(x/1000, 'K')}) +
  geom_point() + 
  geom_smooth(method="gam") +
  labs(title = "United States COVID-19 Data (updated as of 16 June 2020) ",
       subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
       caption = "Source: Ourworldindata.org/coronavirus")
  }

# Graph of Total Cases for USA

  {
  USA_TC <- ggplot(USA_weekly, aes(x=week, y=total_cases)) + 
    scale_y_continuous("Total Cases", 
                       breaks=seq(0, 15000000, 2500000),
                       labels = function(x){paste0(x/1000, 'K')}) +
    geom_point() + 
    geom_smooth(method="gam") +
    labs(title = "United States COVID-19 Data (updated as of 16 June 2020) ",
         subtitle = "No. of COVID-19 Cases vs. Weeks",
         caption = "Source: Ourworldindata.org/coronavirus")
  
  }

# Graph of Total Cases and Total Deaths for USA

  {
  USA_weekly_long <- melt(USA_weekly, id="week")
  USA_TC_TD <- ggplot(USA_weekly_long, 
         aes(x=week, y = value, colour = variable)) +
    scale_y_continuous("Incidents (Cases/Deaths)", 
                       breaks=seq(0, 15000000, 2500000),
                       labels = function(x){paste0(x/1000, 'K')}) +
    geom_point() + 
    geom_smooth(method = "gam") +
    labs(title = "United States COVID-19 Data (updated as of 16 June 2020) ",
         subtitle = "No. of Cases and No. of Deaths vs. Weeks",
         caption = "Source: Ourworldindata.org/coronavirus")
  
  }
}

# Graph for Brazil
{
  #Graph of Total Cases for Brazil in log10
  {
    brazil_TC_log10 <- ggplot(brazil_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Brazil COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for Brazil
  
  {
    brazil_TC <- ggplot(brazil_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Brazil COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for brazil
  
  {
    brazil_weekly_long <- melt(brazil_weekly, id="week")
    brazil_TC_TD <- ggplot(brazil_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "Brazil COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
}

# Graph for China
{
  #Graph of Total Cases for China in log10
  {
    china_TC_log10 <- ggplot(china_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "China COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for China
  
  {
    china_TC <- ggplot(china_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "China COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for china
  
  {
    china_weekly_long <- melt(china_weekly, id="week")
    china_TC_TD <- ggplot(china_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "China COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
}

# Graph for Germany
{
  #Graph of Total Cases for germany in log10
  {
    germany_TC_log10 <- ggplot(germany_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Germany COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for germany
  
  {
    germany_TC <- ggplot(germany_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Germany COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for germany
  
  {
    germany_weekly_long <- melt(germany_weekly, id="week")
    germany_TC_TD <- ggplot(germany_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "Germany COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
}

# Graph for India
{
  #Graph of Total Cases for india in log10
  {
    india_TC_log10 <- ggplot(india_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "India COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for india
  
  {
    india_TC <- ggplot(india_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "India COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for india
  
  {
    india_weekly_long <- melt(india_weekly, id="week")
    india_TC_TD <- ggplot(india_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "India COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
}

# Graph for Iran
{
  #Graph of Total Cases for iran in log10
  {
    iran_TC_log10 <- ggplot(iran_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Iran COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for iran
  
  {
    iran_TC <- ggplot(iran_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Iran COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for iran
  
  {
    iran_weekly_long <- melt(iran_weekly, id="week")
    iran_TC_TD <- ggplot(iran_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "Iran COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
}

# Graph for Italy
{
  #Graph of Total Cases for italy in log10
  {
    italy_TC_log10 <- ggplot(italy_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Italy COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for italy
  
  {
    italy_TC <- ggplot(italy_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Italy COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for italy
  
  {
    italy_weekly_long <- melt(italy_weekly, id="week")
    italy_TC_TD <- ggplot(italy_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "Italy COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
}

# Graph for Peru
{
  #Graph of Total Cases for peru in log10
  {
    peru_TC_log10 <- ggplot(peru_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Peru COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for peru
  
  {
    peru_TC <- ggplot(peru_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Peru COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for peru
  
  {
    peru_weekly_long <- melt(peru_weekly, id="week")
    peru_TC_TD <- ggplot(peru_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "Peru COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
}

# Graph for Russia
{
  #Graph of Total Cases for russia in log10
  {
    russia_TC_log10 <- ggplot(russia_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Russia COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for russia
  
  {
    russia_TC <- ggplot(russia_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Russia COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for russia
  
  {
    russia_weekly_long <- melt(russia_weekly, id="week")
    russia_TC_TD <- ggplot(russia_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "Russia COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
}

# Graph for South Korea
{
  #Graph of Total Cases for south_korea in log10
  {
    south_korea_TC_log10 <- ggplot(south_korea_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "South Korea COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for south_korea
  
  {
    south_korea_TC <- ggplot(south_korea_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "South Korea COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for south_korea
  
  {
    south_korea_weekly_long <- melt(south_korea_weekly, id="week")
    south_korea_TC_TD <- ggplot(south_korea_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "South Korea COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
}

# Graph for Spain
{
  #Graph of Total Cases for spain in log10
  {
    spain_TC_log10 <- ggplot(spain_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases (log-10 scale)", 
                         trans = 'log10',
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Spain COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
           caption = "Source: Ourworldindata.org/coronavirus")
  }
  
  # Graph of Total Cases for spain
  
  {
    spain_TC <- ggplot(spain_weekly, aes(x=week, y=total_cases)) + 
      scale_y_continuous("Total Cases", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method="gam") +
      labs(title = "Spain COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of COVID-19 Cases vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }
  
  # Graph of Total Cases and Total Deaths for spain
  
  {
    spain_weekly_long <- melt(spain_weekly, id="week")
    spain_TC_TD <- ggplot(spain_weekly_long, 
                        aes(x=week, y = value, colour = variable)) +
      scale_y_continuous("Incidents (Cases/Deaths)", 
                         #
                         labels = function(x){paste0(x/1000, 'K')}) +
      geom_point() + 
      geom_smooth(method = "gam") +
      labs(title = "Spain COVID-19 Data (updated as of 16 June 2020) ",
           subtitle = "No. of Cases and No. of Deaths vs. Weeks",
           caption = "Source: Ourworldindata.org/coronavirus")
    
  }

  # Graph for United Kingdom
  {
    #Graph of Total Cases for UK in log10
    {
      UK_TC_log10 <- ggplot(UK_weekly, aes(x=week, y=total_cases)) + 
        scale_y_continuous("Total Cases (log-10 scale)", 
                           trans = 'log10',
                           labels = function(x){paste0(x/1000, 'K')}) +
        geom_point() + 
        geom_smooth(method="gam") +
        labs(title = "UK COVID-19 Data (updated as of 16 June 2020) ",
             subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
             caption = "Source: Ourworldindata.org/coronavirus")
    }
    
    # Graph of Total Cases for UK
    
    {
      UK_TC <- ggplot(UK_weekly, aes(x=week, y=total_cases)) + 
        scale_y_continuous("Total Cases", 
                           #
                           labels = function(x){paste0(x/1000, 'K')}) +
        geom_point() + 
        geom_smooth(method="gam") +
        labs(title = "UK COVID-19 Data (updated as of 16 June 2020) ",
             subtitle = "No. of COVID-19 Cases vs. Weeks",
             caption = "Source: Ourworldindata.org/coronavirus")
      
    }

    # Graph of Total Cases and Total Deaths for UK
    
    {
      UK_weekly_long <- melt(UK_weekly, id="week")
      UK_TC_TD <- ggplot(UK_weekly_long, 
                            aes(x=week, y = value, colour = variable)) +
        scale_y_continuous("Incidents (Cases/Deaths)", 
                           #
                           labels = function(x){paste0(x/1000, 'K')}) +
        geom_point() + 
        geom_smooth(method = "gam") +
        labs(title = "UK COVID-19 Data (updated as of 16 June 2020) ",
             subtitle = "No. of Cases and No. of Deaths vs. Weeks",
             caption = "Source: Ourworldindata.org/coronavirus")
      
    }
    
  }

  {
    # Graph for South Africa
    {
      #Graph of Total Cases for south_africa in log10
      {
        south_africa_TC_log10 <- ggplot(south_africa_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "South Africa COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for South Africa
      
      {
        south_africa_TC <- ggplot(south_africa_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "South Africa COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for South Africa
      
      {
        south_africa_weekly_long <- melt(south_africa_weekly, id="week")
        south_africa_TC_TD <- ggplot(south_africa_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "South Africa COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for France
    {
      #Graph of Total Cases for france in log10
      {
        france_TC_log10 <- ggplot(france_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "France COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for France
      
      {
        france_TC <- ggplot(france_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "France COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for France
      
      {
        france_weekly_long <- melt(france_weekly, id="week")
        france_TC_TD <- ggplot(france_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "France COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for Vietnam
    {
      #Graph of Total Cases for Vietnam in log10
      {
        vietnam_TC_log10 <- ggplot(vietnam_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Vietnam COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for Vietnam
      
      {
        vietnam_TC <- ggplot(vietnam_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Vietnam COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for Vietnam
      
      {
        vietnam_weekly_long <- melt(vietnam_weekly, id="week")
        vietnam_TC_TD <- ggplot(vietnam_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "vietnam COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for New Zealand
    {
      #Graph of Total Cases for new_zealand in log10
      {
        new_zealand_TC_log10 <- ggplot(new_zealand_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "new_zealand COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for new_zealand
      
      {
        new_zealand_TC <- ggplot(new_zealand_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "new_zealand COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for new_zealand
      
      {
        new_zealand_weekly_long <- melt(new_zealand_weekly, id="week")
        new_zealand_TC_TD <- ggplot(new_zealand_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "new_zealand COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for Australia
    {
      #Graph of Total Cases for australia in log10
      {
        australia_TC_log10 <- ggplot(australia_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "australia COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for australia
      
      {
        australia_TC <- ggplot(australia_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "australia COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for australia
      
      {
        australia_weekly_long <- melt(australia_weekly, id="week")
        australia_TC_TD <- ggplot(australia_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "australia COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for Bangladesh
    {
      #Graph of Total Cases for UK in log10
      {
        bangladesh_TC_log10 <- ggplot(bangladesh_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Bangladesh COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for Bangladesh
      
      {
        bangladesh_TC <- ggplot(bangladesh_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Bangladesh COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for Bangladesh
      
      {
        bangladesh_weekly_long <- melt(bangladesh_weekly, id="week")
        bangladesh_TC_TD <- ggplot(bangladesh_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "Bangladesh COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for Pakistan
    {
      #Graph of Total Cases for Pakistan in log10
      {
        pakistan_TC_log10 <- ggplot(pakistan_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Pakistan COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for Pakistan
      
      {
        pakistan_TC <- ggplot(pakistan_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Pakistan COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for pakistan
      
      {
        pakistan_weekly_long <- melt(pakistan_weekly, id="week")
        pakistan_TC_TD <- ggplot(pakistan_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "Pakistan COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for Israel
    {
      #Graph of Total Cases for Israel in log10
      {
        israel_TC_log10 <- ggplot(israel_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Israel COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for Israel
      
      {
        israel_TC <- ggplot(israel_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Israel COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for Israel
      
      {
        israel_weekly_long <- melt(israel_weekly, id="week")
        israel_TC_TD <- ggplot(israel_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "Israel COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for Saudi Arabia
    {
      #Graph of Total Cases for saudi_arabia in log10
      {
        saudi_arabia_TC_log10 <- ggplot(saudi_arabia_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Saudi Arabia COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for Saudi Arabia
      
      {
        saudi_arabia_TC <- ggplot(saudi_arabia_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Saudi Arabia COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for Saudi Arabia
      
      {
        saudi_arabia_weekly_long <- melt(saudi_arabia_weekly, id="week")
        saudi_arabia_TC_TD <- ggplot(saudi_arabia_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "Saudi Arabia COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for Sweden
    {
      #Graph of Total Cases for Sweden in log10
      {
        sweden_TC_log10 <- ggplot(sweden_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Sweden COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for Sweden
      
      {
        sweden_TC <- ggplot(sweden_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Sweden COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for Sweden
      
      {
        sweden_weekly_long <- melt(sweden_weekly, id="week")
        sweden_TC_TD <- ggplot(sweden_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "Sweden COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    # Graph for Thailand
    {
      #Graph of Total Cases for Thailand in log10
      {
        thailand_TC_log10 <- ggplot(thailand_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases (log-10 scale)", 
                             trans = 'log10',
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Thailand COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = expression("log"*""[10]*" (No. of COVID-19 Cases) vs. Weeks"),
               caption = "Source: Ourworldindata.org/coronavirus")
      }
      
      # Graph of Total Cases for Thailand
      
      {
        thailand_TC <- ggplot(thailand_weekly, aes(x=week, y=total_cases)) + 
          scale_y_continuous("Total Cases", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method="gam") +
          labs(title = "Thailand COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of COVID-19 Cases vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
      # Graph of Total Cases and Total Deaths for Thailand
      
      {
        thailand_weekly_long <- melt(thailand_weekly, id="week")
        thailand_TC_TD <- ggplot(thailand_weekly_long, 
                           aes(x=week, y = value, colour = variable)) +
          scale_y_continuous("Incidents (Cases/Deaths)", 
                             #
                             labels = function(x){paste0(x/1000, 'K')}) +
          geom_point() + 
          geom_smooth(method = "gam") +
          labs(title = "Thailand COVID-19 Data (updated as of 16 June 2020) ",
               subtitle = "No. of Cases and No. of Deaths vs. Weeks",
               caption = "Source: Ourworldindata.org/coronavirus")
        
      }
      
    }
    
  }
  
  }

} #execute this code block to generate and store all the plots

{
  #PNG and PDF Plots
  {
    #United States
    ggsave(filename = "USA_TC.png", plot = USA_TC, scale = 2)
    ggsave(filename = "USA_TC_log10.png", plot = USA_TC_log10, scale = 2)
    ggsave(filename = "USA_TC_TD.png", plot = USA_TC_TD, scale = 2)
    ggsave(filename = "USA_TC.pdf", plot = USA_TC, scale = 2)
    ggsave(filename = "USA_TC_log10.pdf", plot = USA_TC_log10, scale = 2)
    ggsave(filename = "USA_TC_TD.pdf", plot = USA_TC_TD, scale = 2)
  }
  {
    #Brazil
    ggsave(filename = "brazil_TC.png", plot = brazil_TC, scale = 2)
    ggsave(filename = "brazil_TC_log10.png", plot = brazil_TC_log10, scale = 2)
    ggsave(filename = "brazil_TC_TD.png", plot = brazil_TC_TD, scale = 2)
    ggsave(filename = "brazil_TC.pdf", plot = brazil_TC, scale = 2)
    ggsave(filename = "brazil_TC_log10.pdf", plot = brazil_TC_log10, scale = 2)
    ggsave(filename = "brazil_TC_TD.pdf", plot = brazil_TC_TD, scale = 2)
  }
  {
    #China
    ggsave(filename = "china_TC.png", plot = china_TC, scale = 2)
    ggsave(filename = "china_TC_log10.png", plot = china_TC_log10, scale = 2)
    ggsave(filename = "china_TC_TD.png", plot = china_TC_TD, scale = 2)
    ggsave(filename = "china_TC.pdf", plot = china_TC, scale = 2)
    ggsave(filename = "china_TC_log10.pdf", plot = china_TC_log10, scale = 2)
    ggsave(filename = "china_TC_TD.pdf", plot = china_TC_TD, scale = 2)
  }
  {
    #Germany
    ggsave(filename = "germany_TC.png", plot = germany_TC, scale = 2)
    ggsave(filename = "germany_TC_log10.png", plot = germany_TC_log10, scale = 2)
    ggsave(filename = "germany_TC_TD.png", plot = germany_TC_TD, scale = 2)
    ggsave(filename = "germany_TC.pdf", plot = germany_TC, scale = 2)
    ggsave(filename = "germany_TC_log10.pdf", plot = germany_TC_log10, scale = 2)
    ggsave(filename = "germany_TC_TD.pdf", plot = germany_TC_TD, scale = 2)
  }
  {
    #India
    ggsave(filename = "india_TC.png", plot = india_TC, scale = 2)
    ggsave(filename = "india_TC_log10.png", plot = india_TC_log10, scale = 2)
    ggsave(filename = "india_TC_TD.png", plot = india_TC_TD, scale = 2)
    ggsave(filename = "india_TC.pdf", plot = india_TC, scale = 2)
    ggsave(filename = "india_TC_log10.pdf", plot = india_TC_log10, scale = 2)
    ggsave(filename = "india_TC_TD.pdf", plot = india_TC_TD, scale = 2)
  }
  {
    #Iran
    ggsave(filename = "iran_TC.png", plot = iran_TC, scale = 2)
    ggsave(filename = "iran_TC_log10.png", plot = iran_TC_log10, scale = 2)
    ggsave(filename = "iran_TC_TD.png", plot = iran_TC_TD, scale = 2)
    ggsave(filename = "iran_TC.pdf", plot = iran_TC, scale = 2)
    ggsave(filename = "iran_TC_log10.pdf", plot = iran_TC_log10, scale = 2)
    ggsave(filename = "iran_TC_TD.pdf", plot = iran_TC_TD, scale = 2)
  }
  {
    #Italy
    ggsave(filename = "italy_TC.png", plot = italy_TC, scale = 2)
    ggsave(filename = "italy_TC_log10.png", plot = italy_TC_log10, scale = 2)
    ggsave(filename = "italy_TC_TD.png", plot = italy_TC_TD, scale = 2)
    ggsave(filename = "italy_TC.pdf", plot = italy_TC, scale = 2)
    ggsave(filename = "italy_TC_log10.pdf", plot = italy_TC_log10, scale = 2)
    ggsave(filename = "italy_TC_TD.pdf", plot = italy_TC_TD, scale = 2)
  }
  {
    #Peru
    ggsave(filename = "peru_TC.png", plot = peru_TC, scale = 2)
    ggsave(filename = "peru_TC_log10.png", plot = peru_TC_log10, scale = 2)
    ggsave(filename = "peru_TC_TD.png", plot = peru_TC_TD, scale = 2)
    ggsave(filename = "peru_TC.pdf", plot = peru_TC, scale = 2)
    ggsave(filename = "peru_TC_log10.pdf", plot = peru_TC_log10, scale = 2)
    ggsave(filename = "peru_TC_TD.pdf", plot = peru_TC_TD, scale = 2)
  }
  {
    #Russia
    ggsave(filename = "russia_TC.png", plot = russia_TC, scale = 2)
    ggsave(filename = "russia_TC_log10.png", plot = russia_TC_log10, scale = 2)
    ggsave(filename = "russia_TC_TD.png", plot = russia_TC_TD, scale = 2)
    ggsave(filename = "russia_TC.pdf", plot = russia_TC, scale = 2)
    ggsave(filename = "russia_TC_log10.pdf", plot = russia_TC_log10, scale = 2)
    ggsave(filename = "russia_TC_TD.pdf", plot = russia_TC_TD, scale = 2)
  }
  {
    #South Korea
    ggsave(filename = "south_korea_TC.png", plot = south_korea_TC, scale = 2)
    ggsave(filename = "south_korea_TC_log10.png", plot = south_korea_TC_log10, scale = 2)
    ggsave(filename = "south_korea_TC_TD.png", plot = south_korea_TC_TD, scale = 2)
    ggsave(filename = "south_korea_TC.pdf", plot = south_korea_TC, scale = 2)
    ggsave(filename = "south_korea_TC_log10.pdf", plot = south_korea_TC_log10, scale = 2)
    ggsave(filename = "south_korea_TC_TD.pdf", plot = south_korea_TC_TD, scale = 2)
  }
  {
    #Spain
    ggsave(filename = "spain_TC.png", plot = spain_TC, scale = 2)
    ggsave(filename = "spain_TC_log10.png", plot = spain_TC_log10, scale = 2)
    ggsave(filename = "spain_TC_TD.png", plot = spain_TC_TD, scale = 2)
    ggsave(filename = "spain_TC.pdf", plot = spain_TC, scale = 2)
    ggsave(filename = "spain_TC_log10.pdf", plot = spain_TC_log10, scale = 2)
    ggsave(filename = "spain_TC_TD.pdf", plot = spain_TC_TD, scale = 2)
  }
  {
    #UK
    ggsave(filename = "UK_TC.png", plot = UK_TC, scale = 2)
    ggsave(filename = "UK_TC_log10.png", plot = UK_TC_log10, scale = 2)
    ggsave(filename = "UK_TC_TD.png", plot = UK_TC_TD, scale = 2)
    ggsave(filename = "UK_TC.pdf", plot = UK_TC, scale = 2)
    ggsave(filename = "UK_TC_log10.pdf", plot = UK_TC_log10, scale = 2)
    ggsave(filename = "UK_TC_TD.pdf", plot = UK_TC_TD, scale = 2)
  }
  {
    #south_africa
    ggsave(filename = "south_africa_TC.png", plot = south_africa_TC, scale = 2)
    ggsave(filename = "south_africa_TC_log10.png", plot = south_africa_TC_log10, scale = 2)
    ggsave(filename = "south_africa_TC_TD.png", plot = south_africa_TC_TD, scale = 2)
    ggsave(filename = "south_africa_TC.pdf", plot = south_africa_TC, scale = 2)
    ggsave(filename = "south_africa_TC_log10.pdf", plot = south_africa_TC_log10, scale = 2)
    ggsave(filename = "south_africa_TC_TD.pdf", plot = south_africa_TC_TD, scale = 2)
  }
  {
    #france
    ggsave(filename = "france_TC.png", plot = france_TC, scale = 2)
    ggsave(filename = "france_TC_log10.png", plot = france_TC_log10, scale = 2)
    ggsave(filename = "france_TC_TD.png", plot = france_TC_TD, scale = 2)
    ggsave(filename = "france_TC.pdf", plot = france_TC, scale = 2)
    ggsave(filename = "france_TC_log10.pdf", plot = france_TC_log10, scale = 2)
    ggsave(filename = "france_TC_TD.pdf", plot = france_TC_TD, scale = 2)
  }
  {
    #vietnam
    ggsave(filename = "vietnam_TC.png", plot = vietnam_TC, scale = 2)
    ggsave(filename = "vietnam_TC_log10.png", plot = vietnam_TC_log10, scale = 2)
    ggsave(filename = "vietnam_TC_TD.png", plot = vietnam_TC_TD, scale = 2)
    ggsave(filename = "vietnam_TC.pdf", plot = vietnam_TC, scale = 2)
    ggsave(filename = "vietnam_TC_log10.pdf", plot = vietnam_TC_log10, scale = 2)
    ggsave(filename = "vietnam_TC_TD.pdf", plot = vietnam_TC_TD, scale = 2)
  }
  {
    #new_zealand
    ggsave(filename = "new_zealand_TC.png", plot = new_zealand_TC, scale = 2)
    ggsave(filename = "new_zealand_TC_log10.png", plot = new_zealand_TC_log10, scale = 2)
    ggsave(filename = "new_zealand_TC_TD.png", plot = new_zealand_TC_TD, scale = 2)
    ggsave(filename = "new_zealand_TC.pdf", plot = new_zealand_TC, scale = 2)
    ggsave(filename = "new_zealand_TC_log10.pdf", plot = new_zealand_TC_log10, scale = 2)
    ggsave(filename = "new_zealand_TC_TD.pdf", plot = new_zealand_TC_TD, scale = 2)
  }
  {
    #australia
    ggsave(filename = "australia_TC.png", plot = australia_TC, scale = 2)
    ggsave(filename = "australia_TC_log10.png", plot = australia_TC_log10, scale = 2)
    ggsave(filename = "australia_TC_TD.png", plot = australia_TC_TD, scale = 2)
    ggsave(filename = "australia_TC.pdf", plot = australia_TC, scale = 2)
    ggsave(filename = "australia_TC_log10.pdf", plot = australia_TC_log10, scale = 2)
    ggsave(filename = "australia_TC_TD.pdf", plot = australia_TC_TD, scale = 2)
  }
  {
    #pakistan
    ggsave(filename = "pakistan_TC.png", plot = pakistan_TC, scale = 2)
    ggsave(filename = "pakistan_TC_log10.png", plot = pakistan_TC_log10, scale = 2)
    ggsave(filename = "pakistan_TC_TD.png", plot = pakistan_TC_TD, scale = 2)
    ggsave(filename = "pakistan_TC.pdf", plot = pakistan_TC, scale = 2)
    ggsave(filename = "pakistan_TC_log10.pdf", plot = pakistan_TC_log10, scale = 2)
    ggsave(filename = "pakistan_TC_TD.pdf", plot = pakistan_TC_TD, scale = 2)
  }
  {
    #bangladesh
    ggsave(filename = "bangladesh_TC.png", plot = bangladesh_TC, scale = 2)
    ggsave(filename = "bangladesh_TC_log10.png", plot = bangladesh_TC_log10, scale = 2)
    ggsave(filename = "bangladesh_TC_TD.png", plot = bangladesh_TC_TD, scale = 2)
    ggsave(filename = "bangladesh_TC.pdf", plot = bangladesh_TC, scale = 2)
    ggsave(filename = "bangladesh_TC_log10.pdf", plot = bangladesh_TC_log10, scale = 2)
    ggsave(filename = "bangladesh_TC_TD.pdf", plot = bangladesh_TC_TD, scale = 2)
  }
  {
    #israel
    ggsave(filename = "israel_TC.png", plot = israel_TC, scale = 2)
    ggsave(filename = "israel_TC_log10.png", plot = israel_TC_log10, scale = 2)
    ggsave(filename = "israel_TC_TD.png", plot = israel_TC_TD, scale = 2)
    ggsave(filename = "israel_TC.pdf", plot = israel_TC, scale = 2)
    ggsave(filename = "israel_TC_log10.pdf", plot = israel_TC_log10, scale = 2)
    ggsave(filename = "israel_TC_TD.pdf", plot = israel_TC_TD, scale = 2)
  }
  {
    #saudi_arabia
    ggsave(filename = "saudi_arabia_TC.png", plot = saudi_arabia_TC, scale = 2)
    ggsave(filename = "saudi_arabia_TC_log10.png", plot = saudi_arabia_TC_log10, scale = 2)
    ggsave(filename = "saudi_arabia_TC_TD.png", plot = saudi_arabia_TC_TD, scale = 2)
    ggsave(filename = "saudi_arabia_TC.pdf", plot = saudi_arabia_TC, scale = 2)
    ggsave(filename = "saudi_arabia_TC_log10.pdf", plot = saudi_arabia_TC_log10, scale = 2)
    ggsave(filename = "saudi_arabia_TC_TD.pdf", plot = saudi_arabia_TC_TD, scale = 2)
  }
  {
    #sweden
    ggsave(filename = "sweden_TC.png", plot = sweden_TC, scale = 2)
    ggsave(filename = "sweden_TC_log10.png", plot = sweden_TC_log10, scale = 2)
    ggsave(filename = "sweden_TC_TD.png", plot = sweden_TC_TD, scale = 2)
    ggsave(filename = "sweden_TC.pdf", plot = sweden_TC, scale = 2)
    ggsave(filename = "sweden_TC_log10.pdf", plot = sweden_TC_log10, scale = 2)
    ggsave(filename = "sweden_TC_TD.pdf", plot = sweden_TC_TD, scale = 2)
  }
  {
    #thailand
    ggsave(filename = "thailand_TC.png", plot = thailand_TC, scale = 2)
    ggsave(filename = "thailand_TC_log10.png", plot = thailand_TC_log10, scale = 2)
    ggsave(filename = "thailand_TC_TD.png", plot = thailand_TC_TD, scale = 2)
    ggsave(filename = "thailand_TC.pdf", plot = thailand_TC, scale = 2)
    ggsave(filename = "thailand_TC_log10.pdf", plot = thailand_TC_log10, scale = 2)
    ggsave(filename = "thailand_TC_TD.pdf", plot = thailand_TC_TD, scale = 2)
  }
}
}
