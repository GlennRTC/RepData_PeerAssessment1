# LIBRARIES
library(ggplot2)
# DONLOAD FILE
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "StormData.csv.bz2")
# CREATE TABLE
StormData <- read.csv("StormData.csv.bz2")
# AGGREGATING FATALITIES & INJURIES
StormDataAgg <- aggregate(FATALITIES~EVTYPE + INJURIES, StormData, sum, na.rm = TRUE)
# SUM FATALITIES AND INJURIES
StormDataAgg$TOTALHEALTHDMG <- StormDataAgg$FATALITIES + StormDataAgg$INJURIES
# ORDERING VALUES
OrderedStormDat <- StormDataAgg[order(-StormDataAgg$TOTALHEALTHDMG),]
StormMaxValue <- head(OrderedStormDat,3)
# PLOTTING
ggplot(StormMaxValue, aes(EVTYPE,TOTALHEALTHDMG, fill = EVTYPE)) + 
    geom_bar(position = "dodge",stat="identity") +
    geom_text(aes(label=TOTALHEALTHDMG), vjust=-0.3, size=3.5) +
    labs(x = "Event Type", y = "People Affected") +
    labs(title = "Top 3 Most Harmful Event on Population Health") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

# *************************************************************************************************************
# AGGREGATING PROPERTY & CROP
StormECAgg <- aggregate(PROPDMG~EVTYPE + CROPDMG, StormData, sum, na.rm = TRUE)
# SUM PROPERTY & CROP DMG
StormECAgg$TOTALPROPDMG <- StormECAgg$PROPDMG + StormECAgg$CROPDMG
# ORDERING VALUES
OrderedStormECDat <- StormECAgg[order(-StormECAgg$TOTALPROPDMG),]
StormECMaxValue <- head(OrderedStormECDat,6)
# PLOTTING
ggplot(StormECMaxValue, aes(EVTYPE,TOTALPROPDMG, fill = EVTYPE)) + 
    geom_bar(position = "dodge",stat="identity") +
    geom_text(aes(label=TOTALPROPDMG), vjust=-0.3, size=3.5) +
    labs(x = "Event Type", y = "People Affected") +
    labs(title = "Top 6 Most Harmful Economic Events") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))