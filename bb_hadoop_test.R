## copy local text file to hdfs
#/dev/hadoop/bin/hadoop fs -copyFromLocal /tmp/gutenberg /user/hduser/gutenberg

Sys.setenv("HADOOP_PREFIX"="/dev/hadoop")
Sys.setenv("HADOOP_CMD"="/dev/hadoop/bin/hadoop")
Sys.setenv("HADOOP_STREAMING"="/dev/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.7.1.jar")
Sys.setenv("CPLUS_INCLUDE_PATH"="/usr/local/include/thrift")
Sys.setenv("PKG_CONFIG_PATH"="/usr/local/lib/pkgconfig:/usr/local/lib/pkgconfig/thrift")
Sys.setenv("JAVA_HOME"="/usr/lib/jvm/java")

library(rmr2) 

## map function
map <- function(k,lines) {
    words.list <- strsplit(lines, '\\s') 
    words <- unlist(words.list)
    return( keyval(words, 1) )
}

## reduce function
reduce <- function(word, counts) { 
    keyval(word, sum(counts))
}

wordcount <- function (input, output=NULL) { 
    mapreduce(input=input, output=output, input.format="text", 
              map=map, reduce=reduce)
}


## delete previous result if any
system("/dev/hadoop/bin/hadoop fs -rmr /user/hduser/gutenberg-output")

## Submit job
hdfs.root <- '/user/hduser'
hdfs.data <- file.path(hdfs.root, 'gutenberg') 
hdfs.out <- file.path(hdfs.root, 'gutenberg-out') 
out <- wordcount(hdfs.data, hdfs.out)

## Fetch results from HDFS
results <- from.dfs(out)

## check top 30 frequent words
results.df <- as.data.frame(results, stringsAsFactors=F) 
colnames(results.df) <- c('word', 'count') 
head(results.df[order(results.df$count, decreasing=T), ], 30)