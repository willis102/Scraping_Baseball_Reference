# Scrape position info for given folder of Bref playerids 
#
#
#

# Load Packages
library(stringr)
library(rvest)

# For all letters
# to do: 1 and up
for(i in 26:26){#length(letters)
  # Load list of BRef IDs
  file <- paste("data/BrefIDs(Bef1900)/",letters[i],"'s.rds",sep="")
  brefids.temp <- readRDS(file)
  
  # Open progress bar
  pb <- txtProgressBar(min = 0, max = nrow(brefids.temp), style = 3)
  # Set Pos to NA
  brefids.temp$Pos <- NA
  
  # For each player look up position info
  for(j in 1:nrow(brefids.temp)){#
    # Update progress bar
    Sys.sleep(0.1)
    setTxtProgressBar(pb, j)
    
    # Read HTML from desired player's minor league BRef URL
    htmlpage <- read_html(paste("http://www.baseball-reference.com/register/player.cgi?id=",
                                brefids.temp$bref_id[j],sep=""))
    # String cleaning to single out the position info
    html <- as.character(htmlpage)
    html <- strsplit(html, "Position")
    html <- as.data.frame(html, stringsAsFactors=F)[2,]
    html <- substr(html, 28,200)
    html <- strsplit(html, ">")
    html <- as.data.frame(html, stringsAsFactors=F)[2,]
    html <- strsplit(html, "<")
    html <- as.data.frame(html, stringsAsFactors=F)[1,]
    # Store final pos info
    pos <- html
    brefids.temp$Pos[j] <- pos[1]
    
    # Compile output
    if(j==1){
      brefids.p <- brefids.temp[j,]
    }else {
      brefids.p <- rbind(brefids.p, brefids.temp[j,])
    }
    
    # If last player save to file
    if(j== nrow(brefids.temp)){
      file <- paste("data/BrefIDsPos(Bef1900)/",letters[i],"'s.rds",sep="")
      saveRDS(brefids.p, file)
    }
  }
  # Show letter progress while looping
  print(paste(i, letters[i], sep="   "))
  
}
# Close Progress Bar
close(pb)

