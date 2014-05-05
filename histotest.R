histotest <- function(userdata, bins = 10) {
        
        ## Construct upper and lower bounds for bins
        binSize <- (max(userdata) - min(userdata))/bins
        binLower <- vector(mode = "numeric", length = 0)
        binUpper <- vector(mode = "numeric", length = 0)
        
        for (i in 1:bins) {
                
                binLower <- c(binLower, min(userdata) + (i - 1)*binSize)
                binUpper <- c(binUpper, min(userdata) + i*binSize)
        }
        
        
        
        ## Count instances in which individual data values fit in a given bin
        
        histoValues <- rep(0, bins)
        lowerCheck <- rep(0, bins)
        upperCheck <- rep(0, bins)
        for (i in 1:length(userdata)) {
                lowerCheck <- rep(0, bins)
                upperCheck <- rep(0, bins)
                lowerCheck[which(binLower <= userdata[i])] <- 1
                upperCheck[which(binUpper > userdata[i])] <- 1
                histoValues <- histoValues + (lowerCheck + upperCheck - 1)
        }
        ## crude method to correct count in last bin.  Bin is off by one due
        ## to exclusive inequality in upperCheck
        correction <- c(rep(0, bins - 1), 1)
        histoValues <- histoValues + correction
        
        ## Output histogram of user input data and bins
        names(histoValues) <- binUpper
        barplot(histoValues, names.arg = signif(as.numeric(names(histoValues)), 3),
                xlab = "bins (upper bounds)", ylab = "frequency")
        
}
