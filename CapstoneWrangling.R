# Introduction ----
#Created by:
#Stephanie Wong 
#U3209816
#For the University of Canberra Capstone Project Semester 1, 2023.
#This code contains the wrangling of the malicious and benign websites
#dataset!

# Set up ----
#Load libraries
library(dplyr)
library(BBmisc)

#Set working directory
setwd(getwd())

#Load dataset
df <- read.csv("dataset.csv")

#Remove URL feature as this gives away the Type
df$URL <- NULL

# Wrangling ----
#View structure of dataframe
str(df)

#Shuffle dataset
df <- df[sample(nrow(df)),]

#Capitalize values
df <- data.frame(lapply(df, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

#Change "GB" and similar abbreviations to "UK" 
#as Great Britain doesn't exist
df$WHOIS_COUNTRY[df$WHOIS_COUNTRY == "[U'GB'; U'UK']"] <- "UK"
df$WHOIS_COUNTRY[df$WHOIS_COUNTRY == "GB"] <- "UK"
df$WHOIS_COUNTRY[df$WHOIS_COUNTRY == "UNITED KINGDOM"] <- "UK"

#Change "CYPRUS" to "CY" for consistency
df$WHOIS_COUNTRY[df$WHOIS_COUNTRY == "CYPRUS"] <- "CY"

#Create new variable - date differences between update and register
#using a placeholder dataset
dfYear <- df

#correct format of update date
dfYear[['WHOIS_UPDATED_DATE']] <- as.POSIXct(
  dfYear[['WHOIS_UPDATED_DATE']],
  format = "%d/%m/%Y"
)

#correct format of reg date
dfYear[['WHOIS_REGDATE']] <- as.POSIXct(
  dfYear[['WHOIS_REGDATE']],
  format = "%d/%m/%Y"
)

#replace NAs with newest date
dfYear[is.na(dfYear)] <- "2017-06-01 00:00:00"

#convert to reg year
dfYear$WHOIS_REGDATE <- as.integer(
    format(
      dfYear$WHOIS_REGDATE, 
      format = "%Y"
    )
  )

#convert to updated year
dfYear$WHOIS_UPDATED_DATE <- as.integer(
  format(
    dfYear$WHOIS_UPDATED_DATE, 
    format = "%Y"
  )
)

#create date diff variable
dfYear$DATE_DIFF <- dfYear$WHOIS_UPDATED_DATE - dfYear$WHOIS_REGDATE

#add to real dataframe
df <- data.frame(dfYear$DATE_DIFF, df)

#rename variable
df <- rename(df, DATE_DIFF = dfYear.DATE_DIFF)

#Remove NA values with a value not used in the column
df[is.na(df)] <- -1

#Transform the DIST_REMOTE_TCP_PORT variable to a higher value feature
#using a log transformation - discovered during EDA that it might need one
df$DIST_REMOTE_TCP_PORT <- df$DIST_REMOTE_TCP_PORT + 1
df$DIST_REMOTE_TCP_PORT <- log(df$DIST_REMOTE_TCP_PORT)

#Save dataset for EDA before normalizing and encoding it
write.csv(
  df, 
  file = "websitesEDA.csv", 
  row.names = F
  )

#Encode variables to positive integers
df <- df %>% 
  mutate(across(where(
    ~is.character(.) | 
      is.factor(.)), 
    ~match(., unique(.)) - 1)
    )

#Normalize data
dfScaled = as.data.frame(
  normalize(
    df[,1:21], 
    method = "range",
    range = c(0, 1)
  )
)

#View structure of dataframe
str(dfScaled)

#Save dataset for algorithms
write.csv(
  dfScaled, 
  file = "websitesCleaned.csv", 
  row.names = F
  )