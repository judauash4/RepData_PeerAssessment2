inputFileName <- "repdata_data_StormData.csv.bz2";

myData <- read.table(file=bzfile(inputFileName),
                     quote='"',
                     na.strings="",
                     header=TRUE, 
                     sep=",",
                     fill=TRUE, 
                     stringsAsFactors=FALSE);

# 48 types of event
evtType <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood",
             "Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought",
             "Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill",
             "Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud",
             "Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind",
             "Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow",
             "Lightning","Marine Hail","Marine High Wind","Marine Strong Wind",
             "Marine Thunderstorm/Tstm Winds","Rip Current","Seiche","Sleet","Storm Tide",
             "Strong Wind","Thunderstorm/Tstm Wind","Tornado","Tropical Depression",
             "Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire",
             "Winter Storm","Winter Weather");

# initialize an empty data frame to store data later
myDmgData <- data.frame();

# looping each weather event and summarize damage done by the event
for(i in 1:length(evtType)) {
  # type <- "Thunderstorm/Tstm Wind";
  type <- evtType[i];
  
  # for event names having '/' symbol, translate it into '|', just like 'OR'
  type <- gsub("/","|",type); 
  type <- paste("(.*)+",type,"(.*)+",sep="");
  
  # extracts data of one event
  subData <-subset(myData, grepl(type, myData[,8], ignore.case = T));
  
  # summarize damage done by the event
  fatalities <- sum(as.numeric(subData[,23]),na.rm=TRUE);
  injuries <- sum(as.numeric(subData[,24]),na.rm=TRUE);
  propDmg <- sum(as.numeric(subData[,25]),na.rm=TRUE);
  cropDmg <- sum(as.numeric(subData[,27]),na.rm=TRUE);
  
  newData <- data.frame(EvtType=evtType[i],
                           Fatalities=fatalities,
                           Injuries=injuries,
                           PropertyDmg=propDmg,
                           CropDmg=cropDmg)  
  
  # merge summarized data into big basket
  myDmgData <- rbind(myDmgData, newData);
}

library(ggplot2);
library(reshape);

# sorting the data and separate population damage from economic damage
myDmgData.popDmg <- myDmgData[order(myDmgData$Fatalities,myDmgData$Injuries,decreasing=TRUE),
                              c("EvtType","Fatalities","Injuries")];
myDmgData.ecoDmg <- myDmgData[order(myDmgData$PropertyDmg,myDmgData$CropDmg,decreasing=TRUE),
                              c("EvtType","PropertyDmg","CropDmg")];


# select top 5 weather event by population damage
ggData <- melt(data=myDmgData.popDmg[1:5,],id.vars=c("EvtType"))

# draw the picture of top 5 population damaging
g <- ggplot(data=ggData,aes(x=ggData$EvtType,
                            y=ggData$value,
                            fill=ggData$variable))
g + geom_bar(stat="identity") + geom_text(aes(label=ggData$value)) + 
  xlab("Weather Event") + ylab("Damage done by event")


# select top 5 weather event by economy damage
ggData <- melt(data=myDmgData.ecoDmg[1:5,],id.vars=c("EvtType"))

# draw the picture of top 5 economy damaging event
g <- ggplot(data=ggData,aes(x=ggData$EvtType,
                            y=ggData$value,
                            fill=ggData$variable))
g + geom_bar(stat="identity") + geom_text(aes(label=ggData$value)) + 
  xlab("Weather Event") + ylab("Damage done by event")

