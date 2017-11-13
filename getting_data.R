################################################################################
### R Script for Determining How Much the 60s Rocked                         ###
################################################################################
### It is broken out into a few parts numbered below.                        ###
### 1.) Getting the Billboard data                                           ###
### 2.) Getting the Spotify data                                             ###
### 3.) Building ROC Curves & Bootstrapping AUCs                             ###
################################################################################
### First, let's bring in the necessary packages
library(rvest)
library(dplyr)
library(httr)
library(jsonlite)
library(stringr)
library(pROC)


################################################################################
### 1.) Getting the Billboard data                                           ###
################################################################################
hits <- NULL
### 1951 and after
years <- c(1951:2016)
urlBase <- "https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_"
for (i in 1:length(years)) {
  
  url <- paste0(urlBase, years[i])
  
  hitsTemp <- url %>% 
    read_html() %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/table') %>% 
    html_table()
  
  ## Write a test if the data is structured differently
  ## to prevent against appending incorrect data to your
  ## data frame
  if (length(hitsTemp) > 1) {
    
    for (j in 1:length(hitsTemp)) {
    rowNamesToTest <- data.frame(names = c("№", "Title", "Artist(s)"))
    rowNames <- attributes(hitsTemp[[j]])$names
    rowNames <- filter(rowNamesToTest, names %in% rowNames)
    
      if (dim(rowNames)[1] == 3) {
        hitsTemp <- hitsTemp[[j]]
      }
    }
  }
  
  ## Clean up the structure of the data before binding
  ## with the others
  hitsTemp <- data.frame(hitsTemp)
  hitsTemp <- data.frame(
                Year = rep(years[i], nrow(hitsTemp)),
                No = hitsTemp[,1],
                Title = hitsTemp[,2],
                Artist = hitsTemp[,3],
                stringsAsFactors = FALSE)
  
  hits <- rbind(hits, data.frame(hitsTemp))
  if(i%%10 == 0) {print(i)}
}

################################################################################
### 2.) Getting the Spotify data                                             ###
################################################################################
spot <- NULL
## Get the TrackID
for (i in 1:nrow(hits)) {
  url <- paste0("https://api.spotify.com/v1/search?q=",
                gsub(" ", "+", str_replace_all(hits$Title[i], "[[:punct:]]", "")), "+",
                gsub(" ", "+", str_replace_all(hits$Artist[i], "[[:punct:]]", "")), 
                "'&type=track&market=US")
  
  resp <- GET(url)
  ### Check status of response
  ### If it's not 200 then go to the next entry
  if (resp$status_code != 200) {
    spotTemp <- data.frame(
                           trackid = "",
                           spotifyartist = "",
                           Title = hits$Title[i],
                           Artist = hits$Artist[i],
                           stringsAsFactors = FALSE)
  } else {
    resp <- fromJSON(content(resp, "text"))
    trackid <- resp$tracks$items$id[[1]]
    spotifyartist <- resp$tracks$items$album$artists[[1]]$name
    
    if (is.null(trackid) == TRUE) {
      spotTemp <- data.frame(
                             trackid = "",
                             spotifyartist = "",
                             Title = hits$Title[i],
                             Artist = hits$Artist[i],
                             stringsAsFactors = FALSE)
    } else {
    spotTemp <- data.frame(trackid, 
                           spotifyartist,
                           Title = hits$Title[i],
                           Artist = hits$Artist[i],
                           stringsAsFactors = FALSE)
    }
  }
  spot <- rbind(spot, spotTemp)
  if(i%%50 == 0) {print(i)}
}

## Get the acousticness
## Requites OAuth
## First get your OAuth token
clientID = '6d5ad90082724978a14ae7b932ace839'
secret = '863ea8a98d5e4ef7b3d5489667fb2006'
response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

acousticness <- NULL
for (i in 1:nrow(spot)) {
  if (spot$trackid[i] == "") {
    acousticnessTemp <- data.frame(
      Acousticness = "",
      trackid = spot$trackid[i],
      stringsAsFactors = FALSE)
  } else {
    ## Now use that token to get the acousticness
    mytoken <- content(response)$access_token
    HeaderValue <- paste0('Bearer ', mytoken)
    URI <- paste0('https://api.spotify.com/v1/audio-features/', spot$trackid[i])
    resp2 <- GET(url = URI, add_headers(Authorization = HeaderValue))
    resp2 <- fromJSON(content(resp2, "text"))
    acousticnessTemp <- data.frame(
      Acousticness = resp2$acousticness,
      trackid = spot$trackid[i],
      stringsAsFactors = FALSE)
  }
  
  acousticness <- rbind(acousticness, acousticnessTemp)
  if(i%%50 == 0) {
    print(i)
    response = POST(
      'https://accounts.spotify.com/api/token',
      accept_json(),
      authenticate(clientID, secret),
      body = list(grant_type = 'client_credentials'),
      encode = 'form',
      verbose()
    )
    }
}

# Remove duplicates
acousticnessFinal <- acousticness[!duplicated(acousticness$trackid),]
# Make sure acousticness is a number
acousticnessFinal$acousticness <- as.numeric(acousticnessFinal$acousticness)

### Now clean up the tables and join them
spotFinal <- spot[!duplicated(spot$trackid),]
# Join hits & spotFinal by Artist & Title
hitsSpot <- left_join(hits, spotFinal)
# Last join the acousticness data by trackid
hitsSpot <- left_join(hitsSpot, acousticnessFinal)
# Write to disk
write.csv(hitsSpot, "acousticness2.csv", row.names=FALSE)


hitsSpot <- data.frame(read.csv("hitsSpot.csv"), stringsAsFactors = FALSE)
hitsSpot$No <- as.character(hitsSpot$No)
hitsSpot$Title <- as.character(hitsSpot$Title)
hitsSpot$Artist <- as.character(hitsSpot$Artist)
hitsSpot$trackid <- as.character(hitsSpot$trackid)
hitsSpot$spotifyartist <- as.character(hitsSpot$spotifyartist)
hitsSpot$Year <- as.character(hitsSpot$Year)
## Remove NAs for acousticness
hitsSpot <- hitsSpot %>% filter(Acousticness != "")

################################################################################
###3.) Building ROC Curves & Bootstrapping AUCs                              ###
################################################################################
years <- c(1951, 1960, 1970, 1980, 1990, 2000, 2010)
decade <- c("50s", "60s", "70s", "80s", "90s", "00s")
bootAuc <- NULL
for (i in 1:(length(years)-1)) {
  hitsSpotTemp <- hitsSpot %>% 
    filter(Year %in% c(years[i], years[i+1])) %>% 
    filter(Acousticness != "") %>% 
    select(Year, Acousticness)
  
  year1 <- hitsSpotTemp %>% filter(Year == years[i])
  year2 <- hitsSpotTemp %>% filter(Year == years[i+1])
  myAuc <- NULL
  for (j in 1:1000) {
    
    hitsSpotTempBoot <- rbind(
      year1Rand <- year1[sample(1:nrow(year1), nrow(year1), TRUE),],
      year2Rand <- year2[sample(1:nrow(year2), nrow(year2), TRUE),]
    )
    
    hitsSpotTempBoot$label <- ifelse(hitsSpotTemp$Year == years[i], 0, 1)
    hitsSpotTempBoot$Pred <- rank(hitsSpotTempBoot$Acousticness)/nrow(hitsSpotTempBoot)
    myAuc[j] <- as.numeric(auc(hitsSpotTempBoot$label, hitsSpotTempBoot$Pred))
  }
  bootAucTemp <- data.frame(
    Decade = as.character(rep(decade[i], j)),
    AUC = myAuc,
    stringsAsFactors = FALSE
  )
  
  bootAuc <- rbind(bootAuc, bootAucTemp)
  print(i)
} 

### Transform to wide format data
temp = data.frame(
  "50s" = filter(bootAuc, Decade == "50s") %>%  .$AUC,
  "60s" = filter(bootAuc, Decade == "60s") %>%  .$AUC,
  "70s" = filter(bootAuc, Decade == "70s") %>%  .$AUC,
  "80s" = filter(bootAuc, Decade == "80s") %>%  .$AUC,
  "90s" = filter(bootAuc, Decade == "90s") %>%  .$AUC,
  "00s" = filter(bootAuc, Decade == "00s") %>%  .$AUC
)

write.csv(temp, "data2.csv", row.names = FALSE)

ggplot(bootAuc, aes(x=AUC,fill=Decade)) + 
  geom_density(alpha=.5) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



bootAucSummary <- bootAuc %>% 
  group_by(Year) %>% 
  summarise(meanAuc = mean(AUC))
bootAucSummary


