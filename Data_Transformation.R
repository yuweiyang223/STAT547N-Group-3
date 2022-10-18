library(fs)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(dplyr)


data_env <- Covariates_data
rm(Covariates_data)

head(data_env)
data_env <- data_env[-c(1:4),]


data_env <- rename(data_env, Salinity = "SALINITY..PSU.")
data_env <- rename(data_env, SST = "DirectValue")
data_env <- rename(data_env, Total_Rain = "Total Rain (mm)")
data_env <- rename(data_env, Max_Temp = "Max Temp (??C)")
data_env <- rename(data_env, Min_Temp = "Min Temp (??C)")
data_env <- rename(data_env, Mean_Temp = "Mean Temp (??C)")
data_env <- rename(data_env, Total_Precip = "Total Precip (mm)")

#Create a year variable (used later)
data_env$year <- format(as.Date(data_env$Date, format="%Y/%m/%d"),"%Y")

# Extract the week number for each year 
data_env$epiweek <- epiweek(data_env$Date)



# Following methods are incorrect but keeping for reference as they have a different definition of the start day of the week

# data_env <- data_env %>% lubridate::week(ymd(Date)) 
# data_env2 <- data_env %>% mutate(date=as.Date(Date,'%Y/%m/%d'),
                                # lubridate::isoweek(ymd(Date))) %>% select(-date) 
# data_env2 <- rename(data_env2, week = "lubridate::isoweek(ymd(Date))")


data_env2 <- data_env %>% 
  group_by(year, epiweek) %>%
  summarise(WDIR = round(median(WDIR), 2), 
            WSPD = round(mean(WSPD), 2), 
            GSPD = round(max(GSPD), 2), 
            Salinity = round(mean(Salinity), 2), 
            SST = round(mean(SST), 2), 
            Total_Rain = round(mean(Total_Rain), 2), 
            Max_Temp = round(max(Max_Temp), 2), 
            Min_Temp = round(min(Min_Temp), 2),
            Mean_Temp = round(mean(Mean_Temp), 2), 
            Total_Precip = round(mean(Total_Precip), 2))

getwd()
write.csv(data_env, "data_env.csv")


getwd()
data_noro <- read_csv("Norovirus_data.csv")


###########Complete data
data_complete <- subset(data_env2, select = c(WDIR, WSPD, Salinity, SST, Mean_Temp, Total_Rain, year, epiweek))

data_complete <- data_complete %>% 
  left_join(data_noro, by = c("year", "epiweek"))


data_complete$WDIR <- as.factor(ifelse(data_complete$WDIR < 45, "N",
                                 ifelse(data_complete$WDIR < 135, "E",
                                        ifelse(data_complete$WDIR < 225, "S",
                                               ifelse(data_complete$WDIR < 315, "W", "N")))))

data_complete <- na.omit(data_complete)

write.csv(data_complete, "Data_Complete.csv")  












######## Time Series Imputed Data
Data_TS$year <- format(as.Date(Data_TS$data.Date, format="%Y/%m/%d"),"%Y")

Data_TS$epiweek <- epiweek(Data_TS$data.Date)

Data_TS <- rename(Data_TS, Date = "data.Date")

Data_TS <- subset(Data_TS, select = c(WDIR, WSPD, Salinity, SST, Mean_Temp, Total_Rain, year, epiweek))

Data_TS <- Data_TS %>% 
  group_by(year, epiweek) %>%
  summarise(WDIR = round(median(WDIR), 2), 
            WSPD = round(mean(WSPD), 2), 
            Salinity = round(mean(Salinity), 2), 
            SST = round(mean(SST), 2), 
            Total_Rain = round(mean(Total_Rain), 2), 
            Mean_Temp = round(mean(Mean_Temp), 2))

data_noro <- rename(data_noro, year = "Epiyear")
data_noro <- rename(data_noro, epiweek = "Epiweek")

data_noro$year <- as.character(data_noro$year)

Data_TS <- Data_TS %>% 
  left_join(data_noro, by = c("year", "epiweek"))

write.csv(Data_TS, "Data_TS")  

Data_TS$WDIR <- as.factor(ifelse(Data_TS$WDIR < 45, "N",
                                              ifelse(Data_TS$WDIR < 135, "E",
                                              ifelse(Data_TS$WDIR < 225, "S",
                                              ifelse(Data_TS$WDIR < 315, "W", "N")))))


write.csv(Data_TS, "Data_TS.csv")  



#### MI Data sets

Data_TS <- rename(Data_TS, Date = "data.Date")

Data_TS <- subset(Data_TS, select = c(WDIR, WSPD, Salinity, SST, Mean_Temp, Total_Rain, year, epiweek))

Data_TS <- Data_TS %>% 
  group_by(year, epiweek) %>%
  summarise(WDIR = round(median(WDIR), 2), 
            WSPD = round(mean(WSPD), 2), 
            Salinity = round(mean(Salinity), 2), 
            SST = round(mean(SST), 2), 
            Total_Rain = round(mean(Total_Rain), 2), 
            Mean_Temp = round(mean(Mean_Temp), 2))

data_noro <- rename(data_noro, year = "Epiyear")
data_noro <- rename(data_noro, epiweek = "Epiweek")

data_noro$year <- as.character(data_noro$year)

Data_TS <- Data_TS %>% 
  left_join(data_noro, by = c("year", "epiweek"))

write.csv(Data_TS, "Data_TS")  

Data_TS$WDIR <- as.factor(ifelse(Data_TS$WDIR < 45, "N",
                                 ifelse(Data_TS$WDIR < 135, "E",
                                        ifelse(Data_TS$WDIR < 225, "S",
                                               ifelse(Data_TS$WDIR < 315, "W", "N")))))


write.csv(Data_TS, "Data_TS.csv")  





