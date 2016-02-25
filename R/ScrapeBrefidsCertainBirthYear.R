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
minYear <- 1700
maxYear <- 1899

# Progress Bar
pb <- txtProgressBar(min = 1, max = length(apletters), style = 3)

# Make vector for letters with apostrophe first
apletters <- c("'",letters)

# For all letters
for(i in 1:length(letters)){#
  
  # For apostrophe and all letters
  for(j in 1:length(apletters)){#
    
    # Update progress bar
    Sys.sleep(0.1)
    setTxtProgressBar(pb, j)
    
    # Define URL for that i and j combo
    url <- paste("http://www.baseball-reference.com/register/player.cgi?initial=",letters[i],apletters[j],sep="")
    # Find desired HTML nodes (the one that lists all players, with birth and death info)
    htmlpage <- read_html(url)
    playidhtml <- html_nodes(htmlpage, "#page_content span , #page_content span a")
    # Change into character string data frame
    playidhtml <- as.character(playidhtml)
    playidhtml <- as.data.frame(playidhtml)
    
    # Check if no rows
    # Lots of string cleaning coming up
    if(nrow(playidhtml)!=0){
      # Each player has 3 rows of data
      # Just want to keep player id info and birth year
      # Remove the 2nd row from each player
      playidhtml$playidhtml <- as.character(playidhtml$playidhtml)
      nrow <-nrow(playidhtml)
      nplay <- nrow/3
      torem <- 0
      # Define rows to remove based on number of players
      for(k in 1:nplay) {
        torem <- c(torem, (3*k)-1)
      }
      # Remove rows
      playidhtml <- playidhtml[-torem,]
      # Split into player id and birth info columns (right and left)
      right <- 0
      left <- 0
      for(k in 1:nplay){
        right <- c(right, 2*k)
        left <- c(left, (2*k)-1)
      }
      tempr <- playidhtml[right]
      templ<- playidhtml[left]
      # Combine these into one data frame
      playid <- as.data.frame(cbind(templ,tempr),stringAsFactors=F)
      # Make character
      playid$tempr <- as.character(playid$tempr)
      playid$templ <- as.character(playid$templ)
      
      # Remove (Ceb.)
      if(length(grep("(Ceb.)",playid$tempr))!=0) playid <- playid[-grep("(Ceb.)",playid$tempr),]
      
      # Keep only player id's with known birth year info
      playid <- playid[grep("b\\.",playid$tempr),]
      # Check if 0 rows
      if(nrow(playid)!=0){
        # Split the left and right strings at the id and birth info
        playid$templ <- strsplit(playid$templ, "id=")
        playid$tempr <- strsplit(playid$tempr, "b.")
        # Take only useful info which is at the end of the split strings
        for(k in 1:nrow(playid)){
          playid$templ[k] <- playid$templ[[k]][length(playid$templ[[k]])]
          playid$tempr[k] <- playid$tempr[[k]][length(playid$tempr[[k]])]
        }
        
        # Different cleaning of HTML strings to get the year info...
        if(substr(playid$tempr[1],1,4)=="</i>"){
          playid$tempr <- as.numeric(substr(playid$tempr, 6,9))
        }else if(substr(playid$tempr[1],1,5)=="</em>"){
          playid$tempr <- as.numeric(substr(playid$tempr, 7,10))
        }
        
        # All minor league BRef Id's are 12 characters long
        playid$templ <- substr(playid$templ, 1,12)
        names(playid) <- c("bref_id","BirthYear")
        
        # Subset to players with only this birth year
        playid <- subset(playid, BirthYear >=minYear & BirthYear <=maxYear)
        
        # If no players from that page, add some NA
      }else if(nrow(playid)==0 ){
        playid <- as.data.frame(cbind(NA,NA))
        names(playid) <- c("bref_id","BirthYear")
      }
      # Other possibility for no players, again add NA
    }else if(nrow(playidhtml)==0 ){
      playid <- as.data.frame(cbind(NA,NA))
      names(playid) <- c("bref_id","BirthYear")
      
    }
    
    # If first page use that to start new Compiled output
    if( j==1){
      brefids <- playid
      # Otherwise, add this page info to past compiled output
    }else
      brefids <- rbind(brefids, playid)
    # If at the end of j, then save to file
    if(j== length(apletters)){
      # Define sheet name
      sheet <- paste(letters[i],"'s",sep="")
      # Remove NA player ids
      brefids <- brefids[!is.na(brefids$bref_id),]
      
      # Save to file
      file <- paste("data/BrefIDs(Bef1900)/", sheet,".rds", sep="" )
      saveRDS(brefids, file)
      
    }
    
  }
  # Show the letter while looping
  print(i)
}
# Close progress bar
close(pb)





