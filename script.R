filename <- "repdata_data_StormData.csv.bz2"
#all.data <- subset(read.csv("repdata_data_StormData.csv.bz2", header=TRUE), !is.na(EVTYPE))
raw.data <- all.data[1:50000,]

# STEP 1: Extract data by event type by health and economic harm.
injuries <- c()
fatalities <- c()
totalhealth <- c()
propdam <- c()
cropdam <- c()
totalcost <- c()
events <- unique(raw.data$EVTYPE)

print(as.character(events))
#for(i in 1:length(events)) {
#	curr <- subset(raw.data, as.character(EVTYPE) == as.character(events[i]))
#	injuries[i] <- sum(as.numeric(curr$INJURIES), na.rm = TRUE)
#	fatalities[i] <- sum(as.numeric(curr$FATALITIES), na.rm = TRUE)
#	totalhealth[i] <- injuries[i] + fatalities[i]
#	propdam[i] <- sum(as.numeric(curr$PROPDMG), na.rm = TRUE)
#	cropdam[i] <- sum(as.numeric(curr$CROPDMG), na.rm = TRUE)
#	totalcost[i] <- propdam[i] + cropdam[i]
#}

i <- 1
for(evt in events) {
	curr <- subset(raw.data, as.character(EVTYPE) == as.character(events[i]))
	injuries[i] <- sum(as.numeric(curr$INJURIES), na.rm = TRUE)
	fatalities[i] <- sum(as.numeric(curr$FATALITIES), na.rm = TRUE)
	totalhealth[i] <- injuries[i] + fatalities[i]
	propdam[i] <- sum(as.numeric(curr$PROPDMG), na.rm = TRUE)
	cropdam[i] <- sum(as.numeric(curr$CROPDMG), na.rm = TRUE)
	totalcost[i] <- propdam[i] + cropdam[i]
}

# STEP 2: Prepare appropriate data frames
n <- 10
health <- data.frame(events=as.character(events), injuries, fatalities, totalhealth)
health <- health[with(health, order(-totalhealth)), ][1:min(n, nrow(health)), ]

# Step 3a: Prepare Health Chart
#plot(1:length(health$events), health$totalhealth, type="l", col="blue")
library(lattice)
barchart(Species~Reason,data=Reasonstats,groups=Catergory, 
         scales=list(x=list(rot=90,cex=0.8)))

#plot(factor(health[,1]), as.numeric(health[,4]), type="h", col="blue")
#health$total
#health <- health[with(health, order(-totalhealth)), ]

#print(health)

#economic <- data.frame(cbind(evt=events, propdam, cropdam, cost = totalcost))
#economic <- economic[order(-totalcost)][1:n, ]

# Step 3a: Prepare Health Chart
#plot(factor(health[,1]), as.numeric(health[,4]), type="h", col="blue")


# -------------------------------------------
#health <- cbind(et = as.character(events), inj=injuries, fat=fatalities, totalhealth)
# Step 3a: Prepare Health Chart
#plot(factor(health[,1]), as.numeric(health[,4]), type="h", col="blue")
