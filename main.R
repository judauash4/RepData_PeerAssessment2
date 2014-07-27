inputFile <- "repdata_data_StormData.csv";

con  <- file(inputFile, open = "r");

myData <- data.frame();

currentLine = 1;
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  myVector <- unlist(strsplit(oneLine, ","));
  
  if(currentLine == 1) {
    # do nothing;
  } else if(length(myVector) == 37) {
    myData <- c(myData,myVector);
  } else if(currentLine == 100) {
    break;
  }
  
  currentLine <- currentLine + 1;
} 

close(con);

# 48 types of event
evtType <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood",
             "Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought",
             "Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill",
             "Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud",
             "Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind",
             "Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow",
             "Lightning","Marine Hail","Marine High Wind","Marine Strong Wind",
             "Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Tide",
             "Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression",
             "Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire",
             "Winter Storm","Winter Weather");

myDmgData <- data.frame();

# looping each event and summarize its damage
for(i in length(evtType)) {
  type <- evtType[i];
  # type <- "TORNADO";
  
  subData <-subset(myData, grepl(type, myData[8], ignore.case = T));
  
  fatalities <- sum(as.numeric(subData[23]));
  injuries <- sum(as.numeric(subData[24]));
  propDmg <- sum(as.numeric(subData[25]));
  cropDmg <- sum(as.numeric(subData[27]));
  
  newData <- data.frame(EvtType=type,
                           Fatalities=fatalities,
                           Injuries=injuries,
                           PropertyDmg=propDmg,
                           CropDmg=cropDmg)
  
  myDmgData <- rbind(myDmgData, newData);
}

