## copy local text file to hdfs
#system("/dev/hadoop/bin/hadoop fs -copyFromLocal /dev/PMT_Data_Feb_2015/adaptive_data /user/hduser/pmt")

Sys.setenv("HADOOP_PREFIX"="/dev/hadoop")
Sys.setenv("HADOOP_CMD"="/dev/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/dev/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.7.1.jar")
Sys.setenv("CPLUS_INCLUDE_PATH"="/usr/local/include/thrift")
Sys.setenv("PKG_CONFIG_PATH"="/usr/local/lib/pkgconfig:/usr/local/lib/pkgconfig/thrift")
Sys.setenv("JAVA_HOME"="/usr/lib/jvm/java")

library(rmr2) 

## map function

map <- function(null, lineText) {
    
    x<-strsplit(lineText, '\\{')
    y<-strsplit(x[[1]][2], "<")
    
    # We could generically extract the internal SIF message but we need to make sense
    # of it so instead we extract the specific elements we are expecting 
    studentId<-strsplit(y[[1]][10], ">")[[1]][2]
    learningInstance<-strsplit(y[[1]][4], ">")[[1]][2]
    knowledgeGraph<-strsplit(y[[1]][6], ">")[[1]][2]
    isCorrect<-strsplit(y[[1]][12], ">")[[1]][2]
    isComplete<-strsplit(y[[1]][14], ">")[[1]][2]
    numberAttempts<-as.integer(strsplit(y[[1]][16], ">")[[1]][2])
    iso8601Time<-strsplit(y[[1]][18], ">")[[1]][2]
    mins<-NA
    secs<-strsplit(iso8601Time, "M")[[1]][2]
    if (is.na(secs)) {
        # There are no minutes in the duration
        secs<-substring(strsplit(iso8601Time, "PT")[[1]][2], 1)
    } else {
        mins<-substring(strsplit(iso8601Time, "M")[[1]][1], 3)
    }
    secs<-substring(secs, 1, nchar(secs)-1) # remove the trailing S
    timeOnItem<-as.numeric(secs)
    if (!is.na(mins)) {
        timeOnItem<-timeOnItem+(as.numeric(mins)*60)
    }
    contentId<-strsplit(y[[1]][20], ">")[[1]][2]
    itemScore<-as.numeric(strsplit(y[[1]][22], ">")[[1]][2])
    interactionEndTime<-as.character(strptime(strsplit(y[[1]][28], ">")[[1]][2], "%FT%T"))
    
    # Check if a Recommendation was returned by Knewton
    if (!is.na(strsplit(y[[1]][36], ">")[[1]][2])) {
        receivedRecommendation<-"TRUE"
        knewtonModuleId<-strsplit(strsplit(y[[1]][38], ">")[[1]][2], ",")[[1]][1]
        adaptiveType<-as.integer(strsplit(strsplit(y[[1]][38], ">")[[1]][2], ",")[[1]][2])
    } else {
        receivedRecommendation<-"FALSE"
        knewtonModuleId<-NA
        adaptiveType<-0
    }
    lineData<-data.frame(studentId,
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
    
    keyval(NULL, lineData)
}

## reduce function
reduce <- function(key, lineData) { 
    keyval(key, lineData)
}

combiner <- function (input, output=NULL) { 
    mapreduce(input=input, output=output, input.format="text",
              map=map, reduce=NULL)
}


## delete previous result if any
system("/dev/hadoop/bin/hadoop fs -rmr /user/hduser/pmt-out")

## Submit job
hdfs.root <- '/user/hduser'
hdfs.data <- file.path(hdfs.root, 'pmt/adaptive_data') 
hdfs.out <- file.path(hdfs.root, 'pmt-out') 
out <- combiner(hdfs.data, hdfs.out)

## Fetch results from HDFS
results <- from.dfs(out)

results.df <- as.data.frame(results, stringsAsFactors=F) 
colnames(results.df) <- c('data', 'count') 
head(results.df[order(results.df$count, decreasing=T), ], 3000)
