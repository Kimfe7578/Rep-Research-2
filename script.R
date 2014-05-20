filename <- "repdata_data_StormData.csv.bz2"
#all.data <- subset(read.csv("repdata_data_StormData.csv.bz2", header=TRUE), !is.na(EVTYPE))
#all.data <- subset(read.table("repdata_data_StormData.csv.bz2", quote="\"s", sep=",", header=TRUE), !is.na(EVTYPE))
raw.data <- all.data[1:15000,]

# STEP 1: Extract data by event type by health and economic harm.
injuries <- c()
fatalities <- c()
totalhealth <- c()
propdam <- c()
cropdam <- c()
totalcost <- c()
events <- unique(raw.data$EVTYPE)

for(i in 1:length(events)) {
	curr <- subset(raw.data, as.character(EVTYPE) == as.character(events[i]))
	injuries[i] <- sum(curr$INJURIES, na.rm = TRUE)
	fatalities[i] <- sum(curr$FATALITIES, na.rm = TRUE)
	totalhealth[i] <- injuries[i] + fatalities[i]
	propdam <- sum(curr$PROPDMG, na.rm = TRUE)
	cropdam <- sum(curr$CROPDMG, na.rm = TRUE)
	totalcost <- propdam[i] + cropdam[i]
}

# STEP 2: Prepare appropriate data frames
n <- 10
#print(events[1:n])
print(as.character(events))
health <- data.frame(cbind(et = as.character(events), inj=injuries, fat=fatalities, tot=as.numeric(totalhealth)))
print(head(health))
health <- health[with(health, order(-tot)), ]
print(health)
#economic <- data.frame(cbind(evt=events, propdam, cropdam, cost = totalcost))
#economic <- economic[order(-totalcost)][1:n, ]

# Step 3a: Prepare Health Chart
with(health, plot(et, tot, col="green"))

