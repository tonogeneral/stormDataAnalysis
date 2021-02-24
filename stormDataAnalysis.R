# Librarys needed




# Download, store and read de file

url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
dataDir = "./data"
DataFN ="StormData.csv.bz2"

if (!file.exists(dataDir)) {
  dir.create(dataDir)
  download.file(url, destfile = "./data/StormData.csv.bz2")
  stormData <- read.csv("./data/StormData.csv.bz2")
  #unzip(zipfile = DataFileName, exdir = dataDir)
}


library(dplyr)
library(ggplot2)


head(stormData)

# sum(is.na(stormData$COUNTYENDN) == FALSE)
# sum(is.na(stormData))


stormData$BGN_DATE<- format(as.POSIXct(stormData$BGN_DATE,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')

#stormData1 <-stormData[!complete.cases(stormData),]
# Subsetting dataset with useful columns
dataDMN <- subset(stormData, FATALITIES + INJURIES >0, select = c(STATE,BGN_DATE,COUNTYNAME,STATE,EVTYPE,FATALITIES,INJURIES,PROPDMG))
dataDMG <- subset(stormData, PROPDMG >0, select = c(STATE,BGN_DATE,COUNTYNAME,STATE,EVTYPE,FATALITIES,INJURIES,PROPDMG))

#dataDMN$BGN_DATE<- format(as.POSIXct(dataDMN$BGN_DATE,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')


str(dataDMN)
head(dataDMN)
tail(dataDMN)





danger <- dataDMN %>%
  group_by(EVTYPE) %>%
  summarise(DAMN = sum(INJURIES+FATALITIES)) %>%
  top_n(5)

econ <- dataDMG %>%
  group_by(EVTYPE) %>%
  summarise(DMG = sum(PROPDMG)) %>%
  top_n(5)


ggplot(danger, aes(EVTYPE, DAMN)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))


ggplot(econ, aes(EVTYPE, DMG)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))

