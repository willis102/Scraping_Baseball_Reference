# Scrape list of all playerids from Baseball Ref Minor league player pages 
# within certain time frame.
#
# I broke this into 4 timeframes: 
# Before 1900, 1900 to 1939, 19040 to 1969 and 1970 to 2015
#
# Currently for: 
# for players born before 1900


# Load Packages
library(stringr)
library(rvest)

# Define range of years to take (includes these years, i.e. <= and >=)
minYear <- 1970
maxYear <- 2018

# Make vector for letters with apostrophe first
#apletters <- c("'",letters)
apletters <- letters

# Progress Bar
pb <- txtProgressBar(min = 1, max = length(apletters), style = 3)

# For all letters
for(i in 2:length(letters)){#
  
  # For apostrophe and all letters
  for(j in 1:length(apletters)){#
    
    # Update progress bar
    Sys.sleep(0.1)
    setTxtProgressBar(pb, j)
    
    # Define URL for that i and j combo
    url <- paste("http://www.baseball-reference.com/register/player.cgi?initial=",letters[i],apletters[j],sep="")
    # Find desired HTML nodes (the one that lists all players, with birth and death info)
    htmlpage <- read_html(url)
    # div we care about is `#div_players_aa`
    xpath <- paste("#div_players_",letters[i],apletters[j],sep="")
    playersHtml <- html_nodes(htmlpage, xpath)
    # grab links
    playerLinks <- html_nodes(playersHtml, 'a')
    playerLinks <- html_attr(playerLinks, 'href')
    if(length(playerLinks) > 0) {
      # cast to string, so we can do string manipulation since the html doesn't give us much to work with structurally.
      playersText <- as.character(html_text(playersHtml, TRUE))
      # split on newlines
      playerTexts <- strsplit(playersText, "\n")[[1]]
      playerBorns <- 1:length(playerTexts)
      playerIds <- 1:length(playerTexts)
      for(p in 1:length(playerTexts)){
        # extract birth year
        playerBorns[p] = str_match(playerTexts[p], "b\\. (\\d{4})")[1,2]
        playerIds[p] = str_match(playerLinks[p], "/register/player\\.cgi\\?id=([a-zA-Z0-9-]{12})")[1,2]
      }
      playerBorns <- as.integer(playerBorns)
      playid <- as.data.frame(cbind(playerIds, playerBorns))
      # drop rows with missing birth year (or less likely, brefid)
      playid <- playid[complete.cases(playid), ]
      # set column names
      names(playid) <- c("bref_id", "BirthYear")
      # cast BirthYear
      playid <- transform(playid, BirthYear = as.numeric(as.character(BirthYear)))
      
      # Subset to players with only this birth year
      playid <- subset(playid, BirthYear >=minYear & BirthYear <=maxYear)
      
      # if for any reason we have no data (filtering, empty intials, etc)
      # create empty data frame
      if(nrow(playid)==0 ){
        playid <- as.data.frame(cbind(NA,NA))
        names(playid) <- c("bref_id","BirthYear")
      }
    } else {
      # no players found on this page.
      playid <- as.data.frame(cbind(NA,NA))
      names(playid) <- c("bref_id","BirthYear")
    }
    
    if(j==1){
      # If first page use that to start new Compiled output
      brefids <- playid
    }else if(j < length(apletters)){
      # Otherwise, add this page info to past compiled output
      brefids <- rbind(brefids, playid)
    # If at the end of j, then save to file
    }else{
      # Define sheet name
      sheet <- paste(letters[i],"'s",sep="")
      # Remove NA player ids
      brefids <- brefids[!is.na(brefids$bref_id),]
      
      # Save to file
      file <- paste("data/BrefIDs(New)/", sheet,".rds", sep="" )
      saveRDS(brefids, file)
    }
  }
  # Show the letter while looping
  print(i)
}
# Close progress bar
close(pb)





