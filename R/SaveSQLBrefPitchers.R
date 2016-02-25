# Loads all Minors and MLB Pitching data and saves to a SQL database
# Saves as a Minors Table and a MLB Table
#

# Load Packages
library(RMySQL)

#
# 1970 to 2015 Load Data
#

# For all letters
for(i in 1:length(letters)){#
  # Define file and load
  file <- paste("data/Pitchers(70to15)/",
                letters[i],"\'s.rds",sep="")
  BRefPitch <- readRDS(file)
  
  # Take only affliated Minors data
    milb <- c("Rk","A","A+","AA","AAA","FRk","A-")
    table.aff <- subset(BRefPitch,  Aff!="")
    # MLB Data
    table.MLB <- subset(BRefPitch, DBsource=="MLB" & Lev %in% c("AL","NL"))
    
    # Compile output
    if(i==1){
      BRefPitch.aff <- table.aff
      BRefPitch.MLB <- table.MLB
    }else {
      BRefPitch.aff <- rbind(BRefPitch.aff, table.aff)
      BRefPitch.MLB <- rbind(BRefPitch.MLB, table.MLB)
    }
  
} 

#
# 1940 to 69
#

# For all letters
for(i in 1:length(letters)){#
  if(i!=24){ # no X's ?
    # Define file and load
    file <- paste("data/Pitchers(40to69)/",
                  letters[i],"\'s.rds",sep="")
    BRefPitch <- readRDS(file)
    
    # Take only affiliated minors data
   milb <- c("Rk","A","A+","AA","AAA","FRk","A-")
    table.aff <- subset(BRefPitch,  Aff!="")
    table.aff <- subset(table.aff, Year!="Year")
    # MLB Data
    table.MLB <- subset(BRefPitch, DBsource=="MLB" & Lev %in% c("AL","NL"))
    
    # Compile output
    if(i==1){
      BRefPitch.aff2 <- table.aff
      BRefPitch.MLB2 <- table.MLB
    }else {
      BRefPitch.aff2 <- rbind(BRefPitch.aff2, table.aff)
      BRefPitch.MLB2 <- rbind(BRefPitch.MLB2, table.MLB)
    }
  }
} 

#
# 1900 to 39
#

# For all letters
for(i in 1:length(letters)){#
  if(i!=24){ # no X's ?
    # Define file and load
    file <- paste("data/Pitchers(1900to39)/",
                  letters[i],"\'s.rds",sep="")
    BRefPitch <- readRDS(file)
    
    # Take only affiliated data
    milb <- c("Rk","A","A+","AA","AAA","FRk","A-")
    table.aff <- subset(BRefPitch,  Aff!="")
    table.aff <- subset(table.aff, Year!="Year")
    # MLB data
    table.MLB <- subset(BRefPitch, DBsource=="MLB" & Lev %in% c("AL","NL"))
    
    # Compile output
    if(i==1){
      BRefPitch.aff3 <- table.aff
      BRefPitch.MLB3 <- table.MLB
    }else {
      BRefPitch.aff3 <- rbind(BRefPitch.aff3, table.aff)
      BRefPitch.MLB3 <- rbind(BRefPitch.MLB3, table.MLB)
    }
  }
} 

#
# Bef1900
#

# For all letters
for(i in 1:length(letters)){#
  if(i!=24){ # no X's ?
    # Define file and load
    file <- paste("data/Pitchers(Bef1900)/",
                  letters[i],"\'s.rds",sep="")
    BRefPitch <- readRDS(file)
    
    # Take only affiliated data
    milb <- c("Rk","A","A+","AA","AAA","FRk","A-")
    table.aff <- subset(BRefPitch,  Aff!="")
    table.aff <- subset(table.aff, Year!="Year")
    # MLB data
    table.MLB <- subset(BRefPitch, DBsource=="MLB" & Lev %in% c("AL","NL"))
    
    # Compile output
    if(i==1){
      BRefPitch.aff4 <- table.aff
      BRefPitch.MLB4 <- table.MLB
    }else {
      BRefPitch.aff4 <- rbind(BRefPitch.aff4, table.aff)
      BRefPitch.MLB4 <- rbind(BRefPitch.MLB4, table.MLB)
    }
  }
} 


#
# Write to SQL
#

# Make connection
conn <- dbConnect(MySQL(), user="root", dbname="BRef-Pitchers", 
                  unix.sock="/Applications/XAMPP/xamppfiles/var/mysql/mysql.sock")

# Write various dataframes to SQL database
dbWriteTable(conn, value = BRefPitch.aff, name = "Minors",overwrite=T) 
dbWriteTable(conn, value = BRefPitch.MLB, name = "MLB",overwrite=T) 
dbWriteTable(conn, value = BRefPitch.aff2, name = "Minors",append=T) 
dbWriteTable(conn, value = BRefPitch.MLB2, name = "MLB",append=T) 
dbWriteTable(conn, value = BRefPitch.aff3, name = "Minors",append=T) 
dbWriteTable(conn, value = BRefPitch.MLB3, name = "MLB",append=T) 
dbWriteTable(conn, value = BRefPitch.aff4, name = "Minors",append=T) 
dbWriteTable(conn, value = BRefPitch.MLB4, name = "MLB",append=T) 

# Disconnect
dbDisconnect(conn=conn)