filename <- "repdata_data_StormData.csv.bz2"
#all.data <- subset(read.csv("repdata_data_StormData.csv.bz2", header=TRUE), !is.na(EVTYPE))
raw.data <- all.data#[1:50000,]

freqify <- function(categories, counts) {
	dat <- c()
	for(i in 1:length(counts)) {
		dat <- append(dat, rep(categories[i], counts[i])) 
	}
	dat
}

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
	injuries[i] <- sum(as.numeric(curr$INJURIES), na.rm = TRUE)
	fatalities[i] <- sum(as.numeric(curr$FATALITIES), na.rm = TRUE)
	totalhealth[i] <- injuries[i] + fatalities[i]
	propdam[i] <- sum(as.numeric(curr$PROPDMG), na.rm = TRUE)
	cropdam[i] <- sum(as.numeric(curr$CROPDMG), na.rm = TRUE)
	totalcost[i] <- propdam[i] + cropdam[i]
}

# Set number of events to get.
n <- 10
par(mfrow = c(2, 1))

# STEP 2: Prepare and plot health data
health <- data.frame(evt=as.character(events), inj=injuries, fat=fatalities, th=totalhealth)
health <- health[with(health, order(-th)), ][1:min(n, nrow(health)), ]
title1.str <- paste("Overall Health Incidents by Event Type (Top", min(n, nrow(health)), "Highest Total)")
with(health,{
	plot(1:length(evt), th, type="b", pch=17, lwd=3, col="blue", 
	     axes = FALSE, xlab = "Event Type", ylab = "Total Incidents on Record", 
		 main=title1.str)
	lines(1:length(evt), fat, type="b", pch=17, lwd=3, col="red")
	lines(1:length(evt), inj, type="b", pch=17, lwd=3, col="green")
	axis(1, at=1:length(evt), labels=evt)
	axis(2)
	legend("topright", pch = 17, col = c("red", "green", "blue"), legend = c("Fatalities", "Injuries", "Total"))
})

# Step 3: Prepare and plot economic data
economic <- data.frame(evt=as.character(events), prop=propdam, crop=cropdam, tc=totalcost)
economic <- economic[with(economic, order(-tc)), ][1:min(n, nrow(economic)), ]
title2.str <- paste("Overall Economic Impact by Event Type (Top", min(n, nrow(health)), "Highest Total Losses)")
with(economic,{
	plot(1:length(evt), tc, type="b", pch=17, lwd=3, col="blue", 
	     axes = FALSE, xlab = "Event Type", ylab = "Total Losses in USD($)", 
		 main=title2.str)
	lines(1:length(evt), prop, type="b", pch=17, lwd=3, col="red")
	lines(1:length(evt), crop, type="b", pch=17, lwd=3, col="green")
	axis(1, at=1:length(evt), labels=evt)
	axis(2)
	legend("topright", pch = 17, col = c("red", "green", "blue"), legend = c("Property Damage", "Crop Damage", "Total"))
})

# ----------------------------------------------------------------------------------------------
#plot(1:length(health$evt), health$th, type="b", pch=17, col="blue", axes = FALSE)
#axis(1, at=1:length(health$evt), labels=health$evt)
#axis(2)

# Step 3a: Prepare Health Chart
#plot(1:length(health$events), health$totalhealth, type="l", col="blue")
#library(lattice)
#barchart(Species~Reason,data=Reasonstats,groups=Catergory, 
#         scales=list(x=list(rot=90,cex=0.8)))

#plot(factor(health[,1]), as.numeric(health[,4]), type="h", col="blue")
#health$total
#health <- health[with(health, order(-totalhealth)), ]

#print(health)

#economic <- data.frame(cbind(evt=events, propdam, cropdam, cost = totalcost))
#economic <- economic[order(-totalcost)][1:n, ]

# Step 3a: Prepare Health Chart
#plot(factor(health[,1]), as.numeric(health[,4]), type="h", col="blue")


# ----------------------------------------------------------------------------------------------
#health <- cbind(et = as.character(events), inj=injuries, fat=fatalities, totalhealth)
# Step 3a: Prepare Health Chart
#plot(factor(health[,1]), as.numeric(health[,4]), type="h", col="blue")
