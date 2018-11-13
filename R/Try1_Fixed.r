# Clean workspace
rm(list = ls())

# Libraries
# ======================================================================================
# Visualization
library("ggplot2")
library("corrplot")

# Data Manipulation
library("data.table")
library("TTR")

# Factor and String Manipulation
library("stringr")

# Date and Time
library("lubridate")
library("chron")

# Modeling
library("dummies")
library("xgboost")
library("caret")

# Load data
# ======================================================================================
setwd("C:/Users/mrrodriguezn/Projects/rpc-times-analysis/")
load("2017_2018_Normal_PLOP.RData")
DB <- data.table(DB)


# Feature Engineering
# ======================================================================================
# Calculate puntuallity in minutes
DB[, planllegada := as.POSIXct(paste(Deliv.Date, Time),
                               format = "%d.%m.%Y %H:%M:%S")]
DB[, puntual := as.numeric(difftime(INSI, planllegada, units = "mins"))]

# Remove outliers (impunctuality less/more 1 day - 1440 minutes)
DB[puntual > 1440 | puntual < -1440, puntual := NA]

# Percent of data with punctuality info
sum(!is.na(DB$puntual)) / nrow(DB)

# Calculate puntuallity T/F 30 mins
DB[LoadNumbe == "L001", puntual30 := puntual <= 30]

# Calculate puntuallity T/F 15 mins
DB[, puntual15 := puntual <= 15]

# Delete rows with NAs in plan arrival on site
DB <- DB[!is.na(planllegada)]

# National punctuality
sum(DB$puntual30, na.rm = T) / sum(!is.na(DB$puntual30))
sum(DB$puntual15, na.rm = T) / sum(!is.na(DB$puntual15))

# Only useful variables
dt <- DB[, .(Plnt, Deliv.Date, ZoneText, ClusterTe, OrderSale, Veh.Plate1,
             Driver, GRqty, TdC = as.numeric(TdC), planllegada, INSI, puntual,
             puntual30, puntual15)]

# Group Hour by 3 hrs. from 9pm to 6am is night shift
dt[, hour := hour(planllegada)]
dt$hourGroup <- as.factor(ifelse(dt$hour < 6 | dt$hour > 18, "night", "day"))


# Group by Plant/Day
daily <- dt[, .(
  ZoneText = .SD[1, ZoneText], ClusterTe = .SD[1, ClusterTe],
  nDeliveries = .N,
  nOrders = length(unique(OrderSale)),
  nTrucks = length(unique(Veh.Plate1)),
  nDrivers = length(unique(Driver)),
  volume = sum(GRqty),
  loadSize = mean(GRqty),
  cycleTime = mean(TdC, na.rm = TRUE),
  punctuality15 = sum(puntual15, na.rm = TRUE) / sum(!is.na(puntual15)),
  punctuality30 = sum(puntual30, na.rm = TRUE) / sum(!is.na(puntual30))
), by = .(Plnt, Deliv.Date, hourGroup)]

# Only big cities
cities <- daily[, .(
  volume = sum(volume),
  puntual = mean(punctuality30, na.rm = T)),
  by = ClusterTe][order(-volume)]

cities$percent <- cities$volume / sum(cities$volume)
bigCities <- as.character(cities[volume > 250000]$ClusterTe)
daily <- daily[ClusterTe %in% bigCities]

# Feature engineering
daily$Deliv.Date <- as.Date(daily$Deliv.Date, format = "%d.%m.%Y")
daily[, quarter := paste("Q", quarter(Deliv.Date), sep = "")]
daily$quarter <- factor(daily$quarter)
daily[, weekday := wday(Deliv.Date, label = TRUE)]
daily[, month := months(Deliv.Date)]
daily$month <- factor(daily$month)

# Season of Year
daily[, season := month]
daily[month %in% c("August", "September", "October", "November"),
      season := "2sem"]
daily[month %in% c("February", "March", "April", "May"),
      season := "1sem"]
daily[month %in% c("June", "July"),
      season := "summerbr"]
daily[month %in% c("December", "January"),
      season := "winterbr"]
daily$season <- factor(daily$season)

# Working Day

# Plant Capacity
capDT <- teoricosPlantas[, .(LoadCapacity = .SD[1, VALUE]), by = WERKS]
daily <- merge(daily, capDT, by.x = "Plnt", by.y = "WERKS", all.x = TRUE)

# Rain

# Model to predict 15 minutes punctuality

# Historic punctuality (moving average 21 days)
punt15 <- daily[!is.na(punctuality15)][order(Deliv.Date)]
punt15[,
       historic := shift_left(runMean(punctuality15, min(21, .N)), 1),
       by = Plnt]

# Fill NAs
punt15[is.na(historic), historic := mean(punctuality15), by = Plnt]
punt15[is.na(LoadCapacity), LoadCapacity := 6]
punt15[is.na(cycleTime), cycleTime := 125.723]

# Hot encoding
dummyCols <- c("hourGroup", "quarter", "weekday", "month", "season")
punt15 <- data.table(dummy.data.frame(punt15, names = dummyCols, sep = "_"))


# Split 80/20
train_idx <- createDataPartition(punt15$punctuality15, p = 0.8, list = F)
dt_train <- punt15[train_idx]
dt_test <- punt15[-train_idx]

# RMSE Moving Average 0.1838
RMSE(dt_test$punctuality15, dt_test$historic)

# Plot
ggplot(dt_test, aes(x = nDeliveries / nDrivers,
                    y = punctuality30,
                    color = ClusterTe,
                    size = nDeliveries)) +
  geom_point(alpha = 0.3) + geom_smooth()

# Linear Model
lmodel <- lm(punctuality15 ~ hourGroup_day + hourGroup_night + nDeliveries +
               nOrders + nTrucks + nDrivers + volume + loadSize + cycleTime +
               weekday_Sun + weekday_Mon + weekday_Tue + weekday_Wed +
               weekday_Thu + weekday_Fri + weekday_Sat + month_April +
               month_August + month_February + month_January + month_July +
               month_June + month_March + month_May + month_September +
               LoadCapacity + historic,
               data = dt_train)

summary(lmodel)

predictions <- predict(lmodel, dt_test)

RMSE(dt_test$punctuality15, predictions)


#
