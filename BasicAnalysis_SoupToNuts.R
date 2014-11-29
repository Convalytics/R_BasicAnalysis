###########################################################
# A basic analysis from beginning to end.
# Jason Green - Convalytics
# 11/29/2014
###########################################################

# This initial version is based on my gist of favorite R snippets:
# https://gist.github.com/Convalytics/9212091

# Google Style Guide:
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml


# Set the working directory:
setwd("~/GitHub/R_BasicAnalysis")

# Load Packages:
require(dplyr)       # To Manipulate and Summarize Data
require(ggplot2)     # Data Visualization
require(openxlsx)    # Work with Excel Files
require(RODBC)       # Connect to Databases

# Load a CSV file:
mydata <- read.csv("C:/Users/jasongreen/Documents/R_Files/mydata.csv")
mydata <- read.csv("~/R_Files/mydata.csv")      # Shortened version, based from root directory.

# Load CSV with File Chooser!
data <- read.csv(file.choose())

# Write data to a CSV file:
write.csv(mydata, file = "mydatafile.csv", row.names=F)

# Load Excel File: (requires openxlsx)
myDataFrame <- read.xlsx("FileName.xlsx",sheet="SheetName",startRow=1,colNames=TRUE)

# Write Excel File:
write.xlsx(dataFrame, file="FileName.xlsx")

# Get Input From User:
variableToStoreResponse <- readline("Question to user?: ")

# Add a column to a data frame with the value based on a subset of that data frame:
mydata$goodScore[mydata$score >= 9] <- "Y"
mydata$goodScore[mydata$score < 9] <- "N"

# List the first n records in a data fram, along with header names.
head(mydata, n=10)

# Substring: (from character 3 to 5 of the string)
mydata$partoftext <- substr(mydata$fulltext,3,5)    


# Merge two data frames:
bothreports <- merge(file1, file2, by.x="file1join", by.y="file2join", all.x=TRUE)  # Left Join: all.x=TRUE ... Outer Join: all=TRUE

# Merge / Join on Multiple Fields
mydata.merged <- merge(x = mydata, y = otherdata, by.x=c("lowTeam","season"), by.y=c("team","season"))


# Regular expression to include only numbers from a string. Regex Regexp gsub :
dataframe$numbersonly <- gsub("[^0-9]","",dataframe$lettersandnumbers)


# If/Else : Controlling the value of a column based on another column:
tourney_results$lowTeamWon <- ifelse(tourney_results$wteam < tourney_results$lteam, 1, 0)

# ddply to summarize counts by season and wteam:
Reg.Wins <- ddply(regular_season_results,c("season", "wteam"), summarise, N=length(wteam))

# Get the sum of a value for a group.
ddply(cogs.trimmed, "Code", summarise, SumOfFees = sum(SumFeeAmount))

# Interpolate the data into 1000 separate values. (to smooth out the choppiness of having only a few values)
interp <- approx(SampleROI$Month, SampleROI$TotalCostVsSavings, n=1000)

# Database Connection -----------------------------------------------------------
myDatabase <- odbcDriverConnect('driver={SQL Server};server=(local);database=MyDatabaseName;trusted_connection=true')

# Query for data
results <- sqlQuery(myDatabase,queryText )   #   dataconnection,query  

# Close Data Connection
odbcCloseAll()  # Used to close all connections.

## Simple box/violin plot with ggplot2:
qplot(A,A,data=train.selection, geom="violin", na.rm=T)

# Histogram with ggplot2:
ggplot(train.selection, aes(x=A, na.rm=T)) + geom_histogram(binwidth=1) 
# OR:
ggplot(cogs.trimmed, aes(x=Code, na.rm=F)) + geom_histogram(fill="navy", color="black") + theme_bw() 

# Boxplot with ggplot2:
ggplot(cogs.trimmed, aes(x=Code, y=SumFeeAmount)) + geom_boxplot(fill = "yellow", color="black") + theme_bw()

# Use gridExtra to show multiple plots together.
grid.arrange(Aplot, Bplot,Cplot,Dplot,Eplot,Fplot,Gplot,ncol=4)

# Subset your data:
train.quotes <- subset(train, train$record_type == 0)


# Function for converting Excel dates. ------------------------------------------------
excelToDate <- function(excelDate)  ### Function to convert from excel's numeric date format to an actual date format.
{
   as.POSIXct(excelDate * 60 * 60 * 24, origin="1899-12-30", tz="GMT")
}
# -------------------------------------------------------------------------------------
