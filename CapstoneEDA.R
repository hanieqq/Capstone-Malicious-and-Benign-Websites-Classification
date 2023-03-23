# Introduction ----
#Created by:
#Stephanie Wong 
#U3209816
#For the University of Canberra Capstone Project Semester 1, 2023.
#This code contains the exploratory data analysis for the major variables
#in the malicious and benign websites dataset, with some additional 
#wrangling to ensure the plots worked correctly.

# Set up ----
#Load libraries
library(tidyverse)
library(corrplot)
library(tidyquant)
library(ggsci)

#Set working directory
setwd(getwd())

# Clean website dataset for EDA ----
#Load base dataset
websites <- read.csv("websitesEDA.csv")

#view dataset
summary(websites)

#Enhance readability of Type
websites$Type[websites$Type == 0] <- "Benign"
websites$Type[websites$Type == 1] <- "Malicious"

#correct Type format
websites$Type <- as.factor(websites$Type)

#treat updated date format
websites[['WHOIS_UPDATED_DATE']] <- as.POSIXct(
  websites[['WHOIS_UPDATED_DATE']],
  format = "%d/%m/%Y"
)

#convert to year for readability and ease of use
websites$WHOIS_UPDATED_DATE <- as.integer(
  format(
    websites$WHOIS_UPDATED_DATE, 
    format = "%Y"
  )
)

#treat reg date format
websites[['WHOIS_REGDATE']] <- as.POSIXct(
  websites[['WHOIS_REGDATE']],
  format = "%d/%m/%Y"
)

#convert to year for readability and ease of use
websites$WHOIS_REGDATE <- as.integer(
  format(
    websites$WHOIS_REGDATE, 
    format = "%Y"
  )
)


#impute NA values with '2018' for visibility
websites[is.na(websites)] <- "2018"

#Subset the dataset into our main variables
webFeatures <- websites[c(
  "URL_LENGTH",
  "NUMBER_SPECIAL_CHARACTERS",
  "WHOIS_UPDATED_DATE",
  "WHOIS_REGDATE",
  "DATE_DIFF",
  "DIST_REMOTE_TCP_PORT",
  "Type"
)]

#view dataset
str(webFeatures)

#all variables are numerical


##Clean dataset for correlation matrix ----
#Load the standardized dataset for correlation
webCorr <- read.csv("websitesCleaned.csv")

#need to use the standardized dataset
webCorr2 <- webCorr[c(
  "URL_LENGTH",
  "NUMBER_SPECIAL_CHARACTERS",
  "WHOIS_UPDATED_DATE",
  "WHOIS_REGDATE",
  "DATE_DIFF",
  "DIST_REMOTE_TCP_PORT",
  "Type"
)]



# Exploratory Data Analysis ----
## Correlation ----

#correlation plot
corrplot(
  cor(webCorr2), 
  method = 'square', 
  col.lim = c(-1, 1),
  col = colorRampPalette(c(
    "#CC6666", 
    "#FFFFFF", 
    "#9999CC"
    ))(100),
  tl.col = "black", 
  type = "lower",  
  diag = TRUE, 
  tl.pos = "ld",
  mar = c(
    0, 
    0, 
    2, 
    0
    ), 
  title = "Correlation Matrix of Top 6 Features"
)
#interesting - correlations between URL_LENGTH, NUMBER_SPECIAL_CHARACTERS
#not going to delve deeper into that at this stage


## Overall ----
###Type bar plot ----
ggplot(
  data = webFeatures, 
  mapping = aes(
    x = Type, 
    fill = Type
    )) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "Malicious" = "#CC6666",
      "Benign" = "#9999CC"
      )) +
  labs(
    title = "Benign and Malicious Observations",
    x = "Class", 
    y = "Observations"
    ) +
  theme(
    legend.position = "none",
    text = element_text(size = 20)
    )
#a clearly unbalanced dataset, with a much higher benign count
#than malicious


##Features ----
#EDA for all independent variables selected by correlation matrix
#and select k best.

###histogram plot URL_LENGTH ----
#view variable stats
summary(webFeatures$URL_LENGTH)
str(webFeatures$URL_LENGTH)

#plot variable
ggplot(
  data = webFeatures, 
  mapping = aes(
    x = URL_LENGTH, 
    fill = Type
    )) +
  geom_histogram(
    binwidth = 20, 
    color = "black"
    ) + 
  scale_fill_manual(values = c(
    "Malicious" = "#CC6666",
    "Benign" = "#9999CC"
    )) +
  labs(
    title = "URL Lengths Coloured by Class",
    y = "Observations",
    x = "Lengths of URLs",
    fill = "Class"
    ) +
  theme(
    text = element_text(size = 20)
    )

###histogram plot NUMBER_SPECIAL_CHARACTERS ----
#view variable stats
summary(webFeatures$NUMBER_SPECIAL_CHARACTERS)
str(webFeatures$NUMBER_SPECIAL_CHARACTERS)

#plot variable
ggplot(
  data = webFeatures, 
  mapping = aes(
    x = NUMBER_SPECIAL_CHARACTERS, 
    fill = Type
    )) +
  geom_histogram(
    binwidth = 4, 
    color = "black"
    ) + 
  scale_fill_manual(values = c(
    "Malicious" = "#CC6666",
    "Benign" = "#9999CC"
    )) +
  labs(
    title = "Number of Special Characters Coloured by Class",
    y = "Observations",
    x = "Number of Special Characters",
    fill = "Class"
    ) +
  theme(
    text = element_text(size = 20)
    )

###histogram plot DIST_REMOTE_TCP_PORT + data transformation ----
#view variable stats
summary(webFeatures$DIST_REMOTE_TCP_PORT)
str(webFeatures$DIST_REMOTE_TCP_PORT)

#plot variable
ggplot(
  data = webFeatures, 
  mapping = aes(
    x = DIST_REMOTE_TCP_PORT, 
    fill = Type)
  ) +
  geom_histogram(
    binwidth = 0.75, 
    color = "black"
    ) + 
  scale_fill_manual(values = c(
    "Malicious" = "#CC6666",
    "Benign" = "#9999CC"
    )) +
  labs(
    title = "Log of Distance of Remote TCP Ports Coloured by Class",
    y = "Observations",
    x = "Log of Distance of Remote TCP Ports",
    fill = "Class"
    ) +
  theme(
    text = element_text(size = 20)
    )

###histogram plot for the DATE_DIFF ----
#view variable stats
summary(webFeatures$DATE_DIFF)
str(webFeatures$DATE_DIFF)

#plot variable
ggplot(
  data = webFeatures, 
  mapping = aes(
    x = DATE_DIFF, 
    fill = Type
    )) +
  xlim(
    0,
    30
    ) +
  geom_histogram(
    binwidth = 3, 
    color = "black"
    ) + 
  scale_fill_manual(values = c(
    "Malicious" = "#CC6666",
    "Benign" = "#9999CC"
    )) +
  labs(
    title = "Update vs Registered Year Differences Coloured by Class",
    y = "Observations",
    x = "Year Differences",
    fill = "Class") +
  theme(
    text = element_text(size = 20)
  )

###histogram plot for the WHOIS_UPDATED_DATE ----
#view variable stats
summary(webFeatures$WHOIS_UPDATED_DATE)
str(webFeatures$WHOIS_UPDATED_DATE)

#correct the date format
webFeatures$WHOIS_UPDATED_DATE <- as.numeric(webFeatures$WHOIS_UPDATED_DATE)

#plot variable
ggplot(
  data = webFeatures, 
  mapping = aes(
    x = WHOIS_UPDATED_DATE, 
    fill = Type
    )) +
  geom_histogram(
    binwidth = 1, 
    color = "black"
    ) + 
  scale_fill_manual(values = c(
    "Malicious" = "#CC6666",
    "Benign" = "#9999CC"
    )) +
  labs(
    title = "Updated Year Coloured by Class",
    y = "Observations",
    x = "Updated Year",
    fill = "Class") +
  theme(
    text = element_text(size = 20)
  ) 


###histogram plot for the WHOIS_REGDATE ----
#view variable stats
summary(webFeatures$WHOIS_REGDATE)
str(webFeatures$WHOIS_REGDATE)

#correct the format
webFeatures$WHOIS_REGDATE <- as.numeric(webFeatures$WHOIS_REGDATE)

#plot variable
ggplot(
  data = webFeatures, 
  mapping = aes(
    x = WHOIS_REGDATE, 
    fill = Type
  )) +
  geom_histogram(
    binwidth = 3, 
    color = "black"
  ) + 
  scale_fill_manual(values = c(
    "Malicious" = "#CC6666",
    "Benign" = "#9999CC"
  )) +
  labs(
    title = "Registered Year Coloured by Class",
    y = "Observations",
    x = "Registered Year",
    fill = "Class") +
  theme(
    text = element_text(size = 20)
  ) 
