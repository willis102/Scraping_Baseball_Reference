# Scrape Standard Batting stats and batting value given MiLB Bref ID
# Also goes to player's MLB page if he has one and takes the MLB data as well
#
#

# Load Packages
library(rvest)
library(stringr)

# For all letters
# to do: 1 and up
for(i in 1:length(letters)){#length(letters)
  
  # Load list of BRef IDs with Position info
  file <- paste("data/BrefIDsPos(Bef1900)/",letters[i],"'s.rds",sep="")
  brefids.p <- readRDS(file)
  
  # Make sure Babe Ruth gets treated as hitter (only listed as pitcher in minors)
  for(j in 1:nrow(brefids.p)){
    if(brefids.p$bref_id[i]=="ruth--002geo"){
      brefids.p$Pos[i] <- "Outfielder and Pitcher"
    }
  }
  
  # Subset to just hitters
  hitids <- subset(brefids.p, Pos!="Pitcher" & Pos!="")
  
  # Open progress bar
  pb <- txtProgressBar(min = 0, max = nrow(hitids), style = 3)
  
  # For each player go to his MiLB Bref page
  # if MLB page exists, it goes there too
  # Combines all standard hitting and hitting value data into one data frame
  for(j in 1:nrow(hitids)){#
    # Update progress bar
    Sys.sleep(0.1)
    setTxtProgressBar(pb, j)
    
    # Go to players minors page and see if theres a link to a majors page
    url <- paste("http://www.baseball-reference.com/register/player.cgi?id=",hitids$bref_id[j],sep="")
    htmlpage <- read_html(url)
    mlbhtml <- html_nodes(htmlpage, "#page_content .bold_text a")
    mlb <- as.character(mlbhtml)
    
    # If there is a majors page lets go to that
    if(length(mlb)!=0){
      
      # Cleans URL to give MLB Bref ID
      mlb <- strsplit(mlb, "\\.shtml")
      mlb <- as.data.frame(mlb, stringsAsFactors=F)[1,]
      mlb <- strsplit(mlb, "/")
      mlb <- as.data.frame(mlb, stringsAsFactors=F)
      mlb <- mlb[nrow(mlb),]
      
      # Make URL from the acquired MLB BRef ID
      urlmlb <- paste("http://www.baseball-reference.com/players/", substr(mlb,1,1),sep="")
      urlmlb <- paste(urlmlb, "/",sep="")
      urlmlb <- paste(urlmlb, mlb,sep="")
      urlmlb <- paste(urlmlb, "-bat.shtml", sep="")
      
      # Read all tables
      tables.mlb <- readHTMLTable(urlmlb, stringsAsFactors=F)
      
      # If there are tables
      if(length(tables.mlb)!=0){
        # Take standard batting table
        table1.mlb <- tables.mlb$batting_standard
        # Check if null
        if(!is.null(table1.mlb)){
          # Add Bref ID and clean Projected data and data w/o year
          table1.mlb$bref_id <- hitids$bref_id[j]
          table1.mlb <- subset(table1.mlb, Year!="" & Tm!="Proj.")
          # This removes a stupid star that indicates an all star season
          table1.mlb$Year <- substr(table1.mlb$Year,1,4)
          
          # Take batting value data and perform same cleaning
          table2.mlb <- tables.mlb$batting_value
          table2.mlb$bref_id <- hitids$bref_id[j]
          table2.mlb <- subset(table2.mlb, Year!="" & Tm!="Proj.")
          table2.mlb$Year <- substr(table2.mlb$Year,1,4)
          
          # Merge the two data sets together
          table.mlb <- merge(table1.mlb, table2.mlb[, c(1,3,4,7:22)],by=c("Year","Tm","Lg"),all.x=T)
          # Add in MLB BrefID
          table.mlb$bref_id_mlb <- mlb
          table.mlb$DBsource <- "MLB"
          
          # Go back to this players minors page and take his minors data as well
          tables.milb <- readHTMLTable(url, stringsAsFactors=F)
          # Take standard batting table and add in ID
          table.milb <- tables.milb$standard_batting
          table.milb$DBsource <- "MiLB"
          table.milb$bref_id <- hitids$bref_id[j]
          
          # If things don't have the right columns fix it
          milb.toadd <- names(table.mlb)[!(names(table.mlb) %in% names(table.milb))]
          table.milb[ , milb.toadd] <- NA
          
          # Change some Lg/Lev stuff
          names(table.mlb)[ names(table.mlb)=="Lg"] <- "Lev"
          
          # Fix mixing columns again
          mlb.toadd <- names(table.milb)[!(names(table.milb) %in% names(table.mlb))]
          table.mlb[ , mlb.toadd] <- NA
          
          # Combine into one table
          table <- rbind(table.milb, table.mlb)
        }
        
        # This is for players that are only ever in the minors (i.e. 95% of them)
        # but have MLB pages ( I think this guy was a manager? Stupid funky error)
      }else {
        
        # Go to minors page URL and take table
        tables.milb <- readHTMLTable(url, stringsAsFactors=F)
        table.milb <- tables.milb$standard_batting
        table.milb$DBsource <- "MiLB"
        table.milb$bref_id <- hitids$bref_id[j]
        
        table <- table.milb
      }
      
      
      # Also just minors players
    }else {
      # Go to URL take data
      tables.milb <- readHTMLTable(url, stringsAsFactors=F)
      table.milb <- tables.milb$standard_batting
      # Check if null
      if(!is.null(table.milb)){
        table.milb$DBsource <- "MiLB"
        table.milb$bref_id <- hitids$bref_id[j]
        
        table <- table.milb
      }else {
        table <- table.milb
      }
      
      
    }
    
    # Check if null
    if(!is.null(table)){
      # IF the first start the output
      if(j==1){
        BRefBat <- table
        # Check to make sure columns are ok to merge
      }else if(ncol(table)!=ncol(BRefBat)){
        bref.toadd <- names(table)[!(names(table) %in% names(BRefBat))]
        BRefBat[ , bref.toadd] <- NA
        
        table.toadd <- names(BRefBat)[!(names(BRefBat) %in% names(table))]
        table[ , table.toadd] <- NA
        # Compile data
        BRefBat <- rbind(BRefBat, table)
      }else {
        # Compile data
        BRefBat <- rbind(BRefBat, table)
      }
    }
  
    
      # IF the last hitter save to file
    if(j== nrow(hitids)){
      file <- paste("data/Hitters(Bef1900)/",letters[i],"'s.rds",sep="")
      saveRDS(BRefBat, file)
      
    } 
  }
  # Show letter progress in loop
  print(paste(i, letters[i], sep="   "))
}
# Close progress bar
close(pb)