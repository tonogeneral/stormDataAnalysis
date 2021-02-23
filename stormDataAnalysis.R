
url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
dataDir = "./data"
DataFN ="StormData.csv.bz2"

if (!file.exists(dataDir)) {
  dir.create(dataDir)
  download.file(url, destfile = "./data/StormData.csv.bz2")
  stormData <- read.csv("./data/StormData.csv.bz2")
  #unzip(zipfile = DataFileName, exdir = dataDir)
}


head(stormData)
