# Librarys needed




# Download, store and read de file

url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
dataDir = "./data"
dataFile = "./data/StormData.csv.bz2"
DataFN ="StormData.csv.bz2"

if (!file.exists(dataDir) || (!file.exists(dataFile))) {
  dir.create(dataDir)
  download.file(url, destfile = "./data/StormData.csv.bz2")
  #unzip(zipfile = DataFileName, exdir = dataDir)
}


stormData <- read.csv("./data/StormData.csv.bz2")

sapply(stormData, function(x) sum(is.na(x)))


library(dplyr)
library(ggplot2)


head(stormData)

# sum(is.na(stormData$COUNTYENDN) == FALSE)
# sum(is.na(stormData))









#stormData1 <-stormData[!complete.cases(stormData),]
# Subsetting dataset with useful columns
dataDMN <- subset(stormData, FATALITIES + INJURIES >0, select = c(STATE,BGN_DATE,COUNTYNAME,STATE,EVTYPE,FATALITIES,INJURIES,PROPDMG))



#dataDMN$BGN_DATE <- format(as.POSIXct(dataDMN$BGN_DATE,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')

sapply(dataDMN, function(x) sum(is.na(x)))
nrow(dataDMN)
# We've got 27 NA values from 21929 in field BGN_DATE, representing a 0,12% of missing values from most harmful events respect to population health

dataDMN <- na.omit(dataDMN)




dataDMG <- subset(stormData, PROPDMG >0, select = c(STATE,BGN_DATE,COUNTYNAME,STATE,EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP))
nrow(dataDMG)
sapply(dataDMG, function(x) sum(is.na(x)))
dataDMG <- na.omit(dataDMG)
## We've got 252 NA values of 239174 in field BGN_DATE, representing a 0,10% of missing values from greatest economy consequences events.



#dataDMN$BGN_DATE<- format(as.POSIXct(dataDMN$BGN_DATE,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')


str(dataDMN)
head(dataDMN)
tail(dataDMN)





human <- dataDMN %>%
  group_by(EVTYPE) %>%
  summarise(DAMN = sum(INJURIES+FATALITIES)) %>%
  top_n(5)

miles <- dataDMG %>%
  filter(PROPDMGEXP == "K") %>%
  group_by(EVTYPE) %>%
  summarise(DMG_m = sum(PROPDMG)/1000)

millones <- dataDMG %>%
  filter(PROPDMGEXP == "M") %>%
  group_by(EVTYPE) %>%
  summarise(DMG_m = sum(PROPDMG))

econ <- bind_rows(miles, millones) %>%
  group_by(EVTYPE) %>%
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  top_n(5) %>%
  arrange(desc(DMG))



ggplot(human, aes(EVTYPE, DAMN)) + 
  ggtitle("Most harmful weather events in US") +
  geom_bar(stat="identity", fill = "#FF6666") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))


ggplot(econ, aes(EVTYPE, DMG)) + 
  ggtitle("Weather events with major economic consequences in US") +
  geom_bar(stat="identity", fill = "#FF6666") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))

