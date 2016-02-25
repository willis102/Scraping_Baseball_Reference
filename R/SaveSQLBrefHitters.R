# Loads all Minors and MLB Batting data and saves to a SQL database
# Saves as a Minors Table and a MLB Table
#

# Load Packages
library(RMySQL)

#
# 1970 to 2015 Load Data
#

# For all letters
for(i in 1:length(letters)){#
  if(i!=24){ # no X's 
    # Define file and load
    file <- paste("data/Hitters(70to15)/",
                  letters[i],"\'s.rds",sep="")
    BRefBat <- readRDS(file)
    
    # Take only affliated Minors data
    milb <- c("Rk","A","A+","AA","AAA","FRk","A-")
    table.aff <- subset(BRefBat,  Aff!="")
    
    # MLB data without TOT (for combined AL/NL data)
    table.MLB <- subset(BRefBat, DBsource=="MLB" & Lev %in% c("AL","NL"))
    
    # Compile output
    if(i==1){
      BRefBat.aff <- table.aff
      BRefBat.MLB <- table.MLB
    }else {
      BRefBat.aff <- rbind(BRefBat.aff, table.aff)
      BRefBat.MLB <- rbind(BRefBat.MLB, table.MLB)
    }
  }
} 
# Remove rows with additional header info
BRefBat.aff <- BRefBat.aff[!(BRefBat.aff$Year=="Year"),]



#
# 1940 to 1969
#

# For all letters
for(i in 1:length(letters)){#
  # Define file and load
  file <- paste("data/Hitters(40to69)/",
                letters[i],"\'s.rds",sep="")
  BRefBat <- readRDS(file)
  
  # Take only affiliated minors data
    milb <- c("Rk","A","A+","AA","AAA","FRk","A-")
    table.aff <- subset(BRefBat,  Aff!="")
    # Remove extra headers
    table.aff <- subset(table.aff, Year!="Year")
    
    # MLB Data
    table.MLB <- subset(BRefBat, DBsource=="MLB" & Lev %in% c("AL","NL"))
    
    # Compile output
    if(i==1){
      BRefBat.aff2 <- table.aff
      BRefBat.MLB2 <- table.MLB
      # Make sure column names match
    }else {
      if(ncol(table.aff)!=ncol(BRefBat.aff2)){
        toadd <- names(BRefBat.aff2)[!(names(BRefBat.aff2) %in% names(table.aff))]
        table.aff[,toadd] <-NA
      }
      if(ncol(table.MLB)!=ncol(BRefBat.MLB2)){
        toadd <- names(BRefBat.MLB2)[!(names(BRefBat.MLB2) %in% names(table.MLB))]
        names(table.MLB) <- paste(names(table.MLB), toadd)
      }
      BRefBat.aff2 <- rbind(BRefBat.aff2, table.aff)
      BRefBat.MLB2 <- rbind(BRefBat.MLB2, table.MLB)
    }
  
} 

#
# 1900 to 1939
#

# For all letters
for(i in 1:length(letters)){#
  # Define file and load
  file <- paste("data/Hitters(1900to39)/",
                letters[i],"\'s.rds",sep="")
  BRefBat <- readRDS(file)
  
  # Take only affiliated Minors data and remove extra headers
  milb <- c("Rk","A","A+","AA","AAA","FRk","A-")
  table.aff <- subset(BRefBat,  Aff!="")
  table.aff <- subset(table.aff, Year!="Year")
  # MLB data
  table.MLB <- subset(BRefBat, DBsource=="MLB" & Lev %in% c("AL","NL"))
  
  # Compile output
  if(i==1){
    BRefBat.aff3 <- table.aff
    BRefBat.MLB3 <- table.MLB
    # Check column names
  }else {
    if(ncol(table.aff)!=ncol(BRefBat.aff3)){
      toadd <- names(BRefBat.aff3)[!(names(BRefBat.aff3) %in% names(table.aff))]
      table.aff[,toadd] <-NA
    }
    if(ncol(table.MLB)!=ncol(BRefBat.MLB3)){
      toadd <- names(BRefBat.MLB3)[!(names(BRefBat.MLB3) %in% names(table.MLB))]
      names(table.MLB) <- paste(names(table.MLB), toadd)
    }
    BRefBat.aff3 <- rbind(BRefBat.aff3, table.aff)
    BRefBat.MLB3 <- rbind(BRefBat.MLB3, table.MLB)
  }
  
} 

#
# Before 1900
#

# For all letters
for(i in 1:length(letters)){#
  if(i!=24){
    # Define file and load
    file <- paste("data/Hitters(Bef1900)/",
                  letters[i],"\'s.rds",sep="")
    BRefBat <- readRDS(file)
    
    # Take only affiliate minors data and remove extra headers
  milb <- c("Rk","A","A+","AA","AAA","FRk","A-")
  table.aff <- subset(BRefBat,  Aff!="")
  table.aff <- subset(table.aff, Year!="Year")
  
  # Take MLB data
  table.MLB <- subset(BRefBat, DBsource=="MLB" & Lev %in% c("AL","NL"))
  
  # Compile output
  if(i==1){
    BRefBat.aff4 <- table.aff
    BRefBat.MLB4 <- table.MLB
    # Check column names
  }else {
    if(ncol(table.aff)!=ncol(BRefBat.aff4)){
      toadd <- names(BRefBat.aff4)[!(names(BRefBat.aff4) %in% names(table.aff))]
      table.aff[,toadd] <-NA
    }
    if(ncol(table.MLB)!=ncol(BRefBat.MLB4)){
      toadd <- names(BRefBat.MLB4)[!(names(BRefBat.MLB4) %in% names(table.MLB))]
      names(table.MLB) <- paste(names(table.MLB), toadd)
    }
    BRefBat.aff4 <- rbind(BRefBat.aff4, table.aff)
    BRefBat.MLB4 <- rbind(BRefBat.MLB4, table.MLB)
  }
}
} 


#
# Write to SQL
#

# Make connection
conn <- dbConnect(MySQL(), user="root", dbname="BRef-Hitters", 
                  unix.sock="/Applications/XAMPP/xamppfiles/var/mysql/mysql.sock")

# Write various dataframes to database
dbWriteTable(conn, value = BRefBat.aff, name = "Minors",overwrite=T) 
dbWriteTable(conn, value = BRefBat.MLB, name = "MLB",overwrite=T) 
dbWriteTable(conn, value = BRefBat.aff2, name = "Minors",append=T) 
dbWriteTable(conn, value = BRefBat.MLB2, name = "MLB",append=T) 
dbWriteTable(conn, value = BRefBat.aff3, name = "Minors",append=T) 
dbWriteTable(conn, value = BRefBat.MLB3, name = "MLB",append=T) 
dbWriteTable(conn, value = BRefBat.aff4, name = "Minors",append=T) 
dbWriteTable(conn, value = BRefBat.MLB4, name = "MLB",append=T) 

# Disconnect
dbDisconnect(conn=conn)