################################################################################
##Create fake data##############################################################
################################################################################
Year = NULL
Acousticness = NULL
for (i in 1:50) {
  ## Create the year
  YearTemp = rep(1950 + i, 100)
  Year = c(Year, YearTemp)
  ## Create the acousticness
  AcousticnessTemp = rbeta(100, i, 10)
  Acousticness = c(Acousticness, AcousticnessTemp)
}

################################################################################
##Write data to disc############################################################
################################################################################
## Create dataframe
acoDat = data.frame(Year, Acousticness)
## Write to disk
write.csv(acoDat, 
          "/Users/danielwoodie/Documents/d3book/spotify_rock/acousticness.csv",
          row.names = FALSE)



