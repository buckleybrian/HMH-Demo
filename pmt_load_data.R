# Brian Buckley February 2015
# updated by Martin Richard May 2015 (fix issues with number of attempts and timeOnItem)

## Load the data

#setwd("C:/Users/wonga/Downloads/")

# load plyr to get performant bind
library(plyr)


# The Knowledge Graphs

readKG = function(fs) {
  df<-data.frame()
  for (i in 1:length(fs)) {
    tmp<-read.csv(fs[i], head=TRUE)
    df<-rbind.fill(df, tmp)
  }   
  return(df)
}

files<-list.files("./knowledge_graphs", full.names=TRUE, pattern = "^.*_modules\\.csv$")
modules<-readKG(files)

files<-list.files("./knowledge_graphs", full.names=TRUE, pattern = "^.*_concept_module\\.csv$")
concept.module<-readKG(files)

files<-list.files("./knowledge_graphs", full.names=TRUE, pattern = "^.*_concept_concept\\.csv$")
concept.concept<-readKG(files)

files<-list.files("./knowledge_graphs", full.names=TRUE, pattern = "^.*_concepts\\.csv$")
concepts<-readKG(files)

files<-list.files("./knowledge_graphs", full.names=TRUE, pattern = "^.*_module_module\\.csv$")
module.module<-readKG(files)


#used to assign timeOnItem, which is in a variety of formats depending on length
#converts to seconds
timeParse <- function (isoTime) {
  days<-0; hours<-0; mins<-0; secs<-0; #make initial values
  isoTime<-strsplit(isoTime, "P")[[1]][2] #remove starting P
  if (grepl('D', isoTime)) {
    days<-as.numeric(strsplit(isoTime, "D")[[1]][1])
    isoTime<-strsplit(isoTime, "D")[[1]][2]
  }
  isoTime<-strsplit(isoTime, "T")[[1]][2] #remove T (and anything before it, but it should be first)
  if (grepl('H', isoTime)) {
    hours<-as.numeric(strsplit(isoTime, "H")[[1]][1])
    isoTime<-strsplit(isoTime, "H")[[1]][2]
  }
  if (grepl('M', isoTime)) {
    mins<-as.numeric(strsplit(isoTime, "M")[[1]][1])
    isoTime<-strsplit(isoTime, "M")[[1]][2]
  }
  secs<-as.numeric(strsplit(isoTime, "S")[[1]][1])
  return (days*24*3600+hours*3600+mins*60+secs)
}


# The message data

# convenience function to read a set of files and merge into a single data frame
# added count after each 1000 messages processed to check how far the thing has gotten too as it is slow
readData = function (fs) {
  # data frame to hold all of the data from multiple files
  dataFrame<-data.frame(studentId=character(0), 
                        learningInstance=character(0), 
                        knowledgeGraph=character(0), 
                        isCorrect=character(0), 
                        isComplete=character(0), 
                        numberAttempts=integer(0), 
                        timeOnItem=numeric(0),
                        contentId=character(0), 
                        itemScore=numeric(0), 
                        interactionEndTime=character(0),
                        receivedRecommendation=character(0),
                        knewtonModuleId=character(0),
                        adaptiveType=integer(0),
                        stringsAsFactors=FALSE)
  for (i in 1:length(fs)) {
    if (exists('dataItem')) { rm(dataItem) }
    dataItem<-scan(file=fs[i], fileEncoding='UTF8', what=character(), sep='\n', allowEscapes='TRUE')
    
    # parse each line of each file - as the data is not proper JSON we have
    # to use string manipulation which is very computationally intensive
    k<-length(dataItem)
    
    # for performance create a set of vectors to hold the messages
    studentId=character(k)
    learningInstance=character(k) 
    knowledgeGraph=character(k) 
    isCorrect=character(k) 
    isComplete=character(k) 
    numberAttempts=integer(k)
    timeOnItem=numeric(k)
    contentId=character(k) 
    itemScore=numeric(k) 
    interactionEndTime=character(k)
    receivedRecommendation=character(k)
    knewtonModuleId=character(k)
    adaptiveType=integer(k)
    
    for (j in 1:k) {
      # indicate how far along we are extracting the mesage file
      if (j %% 1000 == 0) {
        count<-paste("Extracting message ", j, " of ", k)
        print(count)
      }
      
      x<-strsplit(dataItem[j], '\\{')
      y<-strsplit(x[[1]][2], "<")
      
      # We could generically extract the internal SIF message but we need to make sense
      # of it so instead we extract the specific elements we are expecting 
      studentId[j]<-strsplit(y[[1]][10], ">")[[1]][2]
      learningInstance[j]<-strsplit(y[[1]][4], ">")[[1]][2]
      knowledgeGraph[j]<-strsplit(y[[1]][6], ">")[[1]][2]
      isCorrect[j]<-strsplit(y[[1]][12], ">")[[1]][2]
      isComplete[j]<-strsplit(y[[1]][14], ">")[[1]][2] #labeled as attempt status
      
      if (grepl('numberOfAttempts', y[[1]][16])) { #numberofAttempts not in every entry 
        numberAttempts[j]<-as.integer(strsplit(y[[1]][16], ">")[[1]][2]) 
        #not in every entry, will mess up position of any term after
        iso8601Time<-strsplit(y[[1]][18], ">")[[1]][2]
        timeOnItem[j]<-timeParse(iso8601Time)
        contentId[j]<-strsplit(y[[1]][20], ">")[[1]][2]
        itemScore[j]<-as.numeric(strsplit(y[[1]][22], ">")[[1]][2])
        interactionEndTime[j]<-as.character(strptime(strsplit(y[[1]][28], ">")[[1]][2], "%FT%T"))
        
        # Check if a Recommendation was returned by Knewton
        #reading wrong cell, increase indices by 2
        if (!is.na(strsplit(y[[1]][38], ">")[[1]][2])) { 
          receivedRecommendation[j]<-"TRUE"
          knewtonModuleId[j]<-strsplit(strsplit(y[[1]][40], ">")[[1]][2], ",")[[1]][1]
          adaptiveType[j]<-as.integer(strsplit(strsplit(y[[1]][40], ">")[[1]][2], ",")[[1]][2])
        } else {
          receivedRecommendation[j]<-"FALSE"
          knewtonModuleId[j]<-NA
          adaptiveType[j]<-0
        } 
      }
      else{ #move everything up 2 slots
        numberAttempts[j]<-NA 
        iso8601Time<-strsplit(y[[1]][16], ">")[[1]][2]
        timeOnItem[j]<-timeParse(iso8601Time)
        contentId[j]<-strsplit(y[[1]][18], ">")[[1]][2]
        itemScore[j]<-as.numeric(strsplit(y[[1]][20], ">")[[1]][2])
        interactionEndTime[j]<-as.character(strptime(strsplit(y[[1]][26], ">")[[1]][2], "%FT%T"))
        
        # Check if a Recommendation was returned by Knewton
        #reading wrong cell, increase indices by 2
        if (!is.na(strsplit(y[[1]][36], ">")[[1]][2])) {
          receivedRecommendation[j]<-"TRUE"
          knewtonModuleId[j]<-strsplit(strsplit(y[[1]][38], ">")[[1]][2], ",")[[1]][1]
          adaptiveType[j]<-as.integer(strsplit(strsplit(y[[1]][38], ">")[[1]][2], ",")[[1]][2])
        } else {
          receivedRecommendation[j]<-"FALSE"
          knewtonModuleId[j]<-NA
          adaptiveType[j]<-0
          
        }
      }
    }
    fileData<-data.frame(studentId, 
                         learningInstance, 
                         knowledgeGraph, 
                         isCorrect, 
                         isComplete, 
                         numberAttempts, 
                         timeOnItem, 
                         contentId, 
                         itemScore, 
                         interactionEndTime,
                         receivedRecommendation,
                         knewtonModuleId,
                         adaptiveType, 
                         stringsAsFactors=FALSE)
    dataFrame<-rbind.fill(dataFrame, fileData)
  }
  return(dataFrame)
}

# Read in the student data files
#
# note the commented output in this code comes from one data file - 5971d01b-fbaa-425d-b0ce-1c18fab915c9_000007


files<-list.files("./adaptive_data", full.names=TRUE, pattern = "^5971.*$")
files
system.time(dataAll<-readData(files))

# read the knowledge graphs 

# Check for dirty data - empty fields
sum(is.na(dataAll$studentId))           # 0
sum(is.na(dataAll$learningInstance))    # 0
sum(is.na(dataAll$knowledgeGraph))      # 0
sum(is.na(dataAll$isCorrect))           # 0
sum(is.na(dataAll$isComplete))          # 0
sum(is.na(dataAll$numberAttempts))      # 10673 // LSI bugs, functionality??
sum(is.na(dataAll$timeOnItem))          # 47627 // LSI bugs, functionality??
sum(is.na(dataAll$contentId))           # 0
sum(is.na(dataAll$itemScore))           # 10673
sum(is.na(dataAll$interactionEndTime))  # 10673

# Knewton recommendation data
recs<-subset(dataAll, !is.na(knewtonModuleId), select=c(studentId, knewtonModuleId))
str(recs)

#'data.frame':    207184 obs. of  2 variables:
#    $ studentId      : chr  "2ae5f91a-3e87-4371-9e8f-ce774ba80d25" "2ae5f91a-3e87-4371-9e8f-ce774ba80d25" "2ae5f91a-3e87-4371-9e8f-ce774ba80d25" "2ae5f91a-3e87-4371-9e8f-ce774ba80d25" ...
#$ knewtonModuleId: chr  "5936584916610318339" "5936584916610318339" "5936585205195210755" "5936585620347420672" ...

# We note that the knewton recommendation id is a different format (decimal string) to that 
# stored in the KG (canonical UUID).  Doh!

##################################################################################################
# Added by Alice Wong, primarily to create timeids in later scripts.

# Convert interactionEndTime to a useable format for creating timeids.
dataAll$dataconv<-as.POSIXlt(strptime(as.character(dataAll$interactionEndTime),format="%Y-%m-%d %H:%M:%S"))

start<-as.POSIXlt(strptime(as.character("2014-08-01 00:00:00"),format="%Y-%m-%d %H:%M:%S"))

dataAll$minsincestart<-difftime(dataAll$dataconv,start,units='mins')

dataAll$minsincestart<-as.numeric(dataAll$minsincestart)

# Have to delete these columns.  Otherwise, the creation of timeids below will not work.
dataAllclean<-dataAll[,-c(10,14)]

write.csv(dataAllclean,'datatimeall.csv')

datatime<-dataAll[dataAll$numberAttempts==1,]

write.csv(datatime,'datatime2.csv')