inputFileName <- "repdata_data_StormData.csv.bz2";

myData <- read.table(file=bzfile(inputFileName),
                     quote='"',
                     na.strings="",
                     header=TRUE, 
                     sep=",",
                     fill=TRUE, 
                     stringsAsFactors=FALSE);

# con  <- file(inputFile, open="r");
# # con <- bzfile(inputFile, open="r");
# 
# myList <- list();
# 
# # reading data line by line and validate the data in 37 column format is time consuming
# currentLine <- 1;
# while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
#   oneLine <- strsplit(oneLine, ",")[[1]];
#   
#   if(currentLine != 1 && length(oneLine) == 37) {
#     myList <- rbind(myList,oneLine);
#   }
#   
#   print(paste("@line:", currentLine, ",isValidFormat?", length(oneLine) == 37));
#   
#   currentLine <- currentLine + 1;
# } 
# 
# close(con);

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

myDmgData <- data.frame();

# looping each event and summarize its damage
for(i in 1:length(evtType)) {
  # type <- "Thunderstorm/Tstm Wind";
  type <- evtType[i];
  type <- gsub("/","|",type);
  type <- paste("(.*)+",type,"(.*)+",sep="");
  
  subData <-subset(myData, grepl(type, myData[,8], ignore.case = T));
  
  fatalities <- sum(as.numeric(subData[,23]),na.rm=TRUE);
  injuries <- sum(as.numeric(subData[,24]),na.rm=TRUE);
  propDmg <- sum(as.numeric(subData[,25]),na.rm=TRUE);
  cropDmg <- sum(as.numeric(subData[,27]),na.rm=TRUE);
  
  newData <- data.frame(EvtType=evtType[i],
                           Fatalities=fatalities,
                           Injuries=injuries,
                           PropertyDmg=propDmg,
                           CropDmg=cropDmg)  
  
  myDmgData <- rbind(myDmgData, newData);
}

