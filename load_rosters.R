# Brian Buckley November 4 2015

setwd("C:/Users/buckleyb/Documents/Data Science/K12-Identity-Rosters/K12-Identity-Rosters")

# load plyr to get performant bind
library(plyr)

roster_path_1 <- "/To_HMH/Processed/"
roster_path_2 <- "/To_HMH/"
roster_path_3 <- "/"

files<-list.files(".", full.names=TRUE)
users = character(0)
classes = character(0)
count_users = 1
count_classes = 1

for (i in files) {
  if (file.info(i)$isdir) {
    #csv_files<-list.files(paste(i, roster_path, sep=''), full.names=TRUE, pattern = "^.*\\.csv$", ignore.case=T)
    csv_files<-list.files(paste(i, roster_path_1, sep=''), full.names=TRUE, pattern = "^.*\\.csv$", ignore.case=T)
    csv_files_2<-list.files(paste(i, roster_path_2, sep=''), full.names=TRUE, pattern = "^.*\\.csv$", ignore.case=T)
    csv_files_3<-list.files(paste(i, roster_path_3, sep=''), full.names=TRUE, pattern = "^.*\\.csv$", ignore.case=T)
    
    if (length(csv_files_2) > 0) {
      csv_files = append(csv_files, csv_files_2)
    }
    
    if (length(csv_files_3) > 0) {
      csv_files = append(csv_files, csv_files_3)
    }
    
    for (j in csv_files) {
      x<-grep("class", j, ignore.case = T)
      if (length(x) == 0) { # Deal with classes separately
        users[count_users] = j
        count_users = count_users + 1
      } else {
        classes[count_classes] = j
        count_classes = count_classes + 1
      }
    }
  }
}

readRosters = function(fs) {
  df<-data.frame()
  
  for (i in 1:length(fs)) {
    #tryCatch(tmp<-read.csv(fs[i], head=TRUE, row.names=NULL), error=function() next)
    x<-try(tmp<-read.csv(fs[i], head=TRUE, row.names=NULL), silent=T)
    if ('try-error' %in% class(x)) next
    df<-rbind.fill(df, tmp)
    df$district<-strsplit(fs[i], '/')[[1]][2]
  }   
  return(df)
}

df_users<-readRosters(users)
df_classes<-readRosters(classes)

# Now add the State
maint_schedule<-read.csv("./Maintenance Schedule v2.csv")
df_users<-merge(x=df_users, y=maint_schedule[,c("STATE", "DISTRICT")], by.x="district", by.y="DISTRICT")

# Make one big csv out of all rosters - need separate one for HRW, TC and CUS as CSV files different

createOneCsv<- function (target) {
    if (target == 'hrw') {
      firstFile = users[1]
      outFile = "./one_roster_hrw.csv"
      dataItem<-read.csv(firstFile, head=T, row.names=NULL)
    } else if (target == 'tc') {
      firstFile = users[10]
      dataItem<-read.csv(firstFile, head=T, row.names=NULL)   
      outFile = "./one_roster_tc.csv"
    } else { # CUS
      #firstFile = users[1]
      #dataItem<-read.csv(firstFile, head=T, row.names=NULL)    
    }
    
    x<-strsplit(firstFile, '/')[[1]][2][1]
    y<-maint_schedule[which(maint_schedule$DISTRICT == x),]$STATE
    dataItem$DISTRICT<-x
    dataItem$STATE<-y
    write.table(dataItem, file=outFile, append = T, col.names = T, row.names = F, sep = ',')
    
    for (i in 2:length(users)) {
      x<-try(dataItem<-read.csv(users[i], head=F, row.names=NULL), silent=T)
      if ('try-error' %in% class(x)) next
      
      if (target == 'hrw') {
        if (ncol(dataItem) != 18) {
          rm(dataItem)
          next
        }
      } else if (target == 'tc') {
        if (ncol(dataItem) != 25) {
          rm(dataItem)
          next      
      } else { # CUS
        
      }
      
      # Check for header - skip if we found one
      if (dataItem[1,1] == 'UserType' | dataItem[1,1] == 'Action') {
        dataItem = dataItem[-1,]
      }
      x<-strsplit(users[i], '/')[[1]][2][1]
      y<-maint_schedule[which(maint_schedule$DISTRICT == x),]$STATE
      if (nrow(dataItem) > 0) {
        dataItem$DISTRICT<-x
        dataItem$STATE<-y
        write.table(dataItem, file=outFile, append = T, col.names = F, row.names = F,sep = ',')    
      }
    }
  }
}
   
# TC
createOneCsv('tc')

# Read in the big one csv file
hrwRosters<-read.csv("./one_roster_hrw.csv", head=T, row.names=NULL, sep=',')
tcRosters<-read.csv("./one_roster_tc.csv", head=T, row.names=NULL, sep=',')

# Exploratory analysis - unique and duplicated
x<-hrwRosters[!(duplicated(hrwRosters[c("Last","First","DISTRICT")]) | duplicated(hrwRosters[c("Last","First","DISTRICT")], fromLast = TRUE)), ]
z<-arrange(x, Student.ID, DISTRICT, STATE, Last, First)

#x<-unique(hrwRosters[c('Student.ID','STATE', 'DISTRICT')])
str(x) # 862691 obs. of  3 variables

str(subset(hrwRosters, STATE == 'FL'))

barplot(summary(x$STATE), col=rainbow(length(summary(x$STATE))))
sort(summary(x$DISTRICT), decreasing=T)

barplot(summary(x$DISTRICT), col=rainbow(length(summary(x$DISTRICT))), horiz=T)

par(mar=c(5.1, max(4.1,max(nchar(names(summary(x$DISTRICT))))/1.8) ,4.1 ,2.1))
barplot(summary(x$DISTRICT), col=rainbow(length(summary(x$DISTRICT))), horiz=T, las=1, cex.names=0.5, main='MyHRW Unique registered users/District', xlab='# Unique registered users')


cus_path='C:/Users/buckleyb/Documents/Data Science/K12-Identity-Rosters/CUS Rosters/00057015_FresnoUnified_8_19/'
cus_fresno<-read.csv(paste(cus_path, "Student.csv", sep=''), head=T, row.names=NULL, sep=',')

# CTS Roster data

all_rosters = data.frame()

setwd("C:/Users/buckleyb/Documents/Data Science/HMOF_RosterData")
files<-list.files(".", full.names=TRUE, pattern = "^.*\\.csv$", ignore.case=T)

for (i in files) {
  tmpdf<-read.csv(i, head=T, row.names=NULL, sep=',', stringsAsFactors = F)
  tmpdf$platform<-'HRW'
  all_rosters<-rbind.fill(all_rosters, tmpdf)
}

setwd("C:/Users/buckleyb/Documents/Data Science/TC_RosterData")
files<-list.files(".", full.names=TRUE, pattern = "^.*\\.csv$", ignore.case=T)

for (i in files) {
  tmpdf<-read.csv(i, head=T, row.names=NULL, sep=',', stringsAsFactors = F)
  tmpdf$platform<-'TC'
  all_rosters<-rbind.fill(all_rosters, tmpdf)
}

num_students<-sum(all_rosters$STUDENT_COUNT,na.rm=T) #14,281,759
num_teachers<-sum(all_rosters$TEACHER_COUNT,na.rm=T) #678,464

all_rosters$STATE<-as.factor(all_rosters$STATE)
all_rosters$DISTRICT_PID<-as.factor(all_rosters$DISTRICT_PID)
all_rosters$SCHOOL_PID<-as.factor(all_rosters$SCHOOL_PID)
all_rosters$platform<-as.factor(all_rosters$platform)

saveRDS(all_rosters, file="C:/Users/buckleyb/Documents/Data Science/Platform-Weather-Map/data/ctsrosters.rds")








