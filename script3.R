filename <- "repdata_data_StormData.csv.bz2"
#filename <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#all.data <- subset(read.csv(filename, header=TRUE), !is.na(EVTYPE))
raw.data <- all.data#[1:5000,]

# This function takes a magnitude (PROPDMGEXP or CROPDMGEXP column value) with a numeric value
# and performs the conversion. Recoginized formats are k, K, m, M, b, and B. Others are ignored.
at.mag <- function(num, mag) {
	if(is.na(num)) { num <- 0 }
	if(is.element(as.factor(mag), as.factor(c("k", "K")))) { num <- num * 1000 }
	else if(is.element(as.factor(mag), as.factor(c("m", "M")))) { num <- num * 1000000 }
	else if(is.element(as.factor(mag), as.factor(c("b", "B")))) { num <- num * 1000000000 }
	num
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
	propdam[i] <- sum(sapply(1:nrow(curr), function(j) at.mag(curr$PROPDMG[j], curr$PROPDMGEXP[j])))
	#propdam[i] <- sum(as.numeric(curr$PROPDMG), na.rm = TRUE)
	cropdam[i] <- sum(sapply(1:nrow(curr), function(j) at.mag(curr$CROPDMG[j], curr$CROPDMGEXP[j])))
	#cropdam[i] <- sum(as.numeric(curr$CROPDMG), na.rm = TRUE)
	totalcost[i] <- propdam[i] + cropdam[i]
}

# Set number of events to get.
n <- 5

# Set number of chart rows.
par(mfrow = c(2, 1))

# STEP 2: Prepare and plot health data
health <- data.frame(evt=as.character(events), inj=injuries, fat=fatalities, th=totalhealth)
health <- health[with(health, order(-th)), ][1:min(n, nrow(health)), ]
title1.str <- paste("Fig. 1: Overall Health Incidents (Top", min(n, nrow(health)), "Highest Totals)")
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
title2.str <- paste("Fig 2: Overall Economic Impact (Largest", min(n, nrow(health)), "Total Losses)")
with(economic,{
	plot(1:length(evt), tc, type="b", pch=17, lwd=3, col="blue", 
	     axes = FALSE, xlab = "Event Type", ylab = "Losses in USD($)", 
		 main=title2.str)
	lines(1:length(evt), prop, type="b", pch=17, lwd=3, col="red")
	lines(1:length(evt), crop, type="b", pch=17, lwd=3, col="green")
	axis(1, at=1:length(evt), labels=evt)
	axis(2)
	legend("topright", pch = 17, col = c("red", "green", "blue"), legend = c("Property Damage", "Crop Damage", "Total"))
})
