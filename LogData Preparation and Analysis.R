library(reshape2)
library(cluster)
library(hyperSpec)
#install.packages("dplyr")
library(dplyr)
library(reshape2)
LogData <- read.csv("C:/Users/mmesgari/Desktop/Processed Logs (3)/All anonomyized logs.csv",na.strings = "NA")
#Loading Action-Affordance Match data
AcAfData <- read.csv("C:/Users/mmesgari/Desktop/EventName-Affordance-Match.csv",na.strings = "NA")

#Converting datetime format
LogData$DateTime <- as.POSIXct(LogData$DateTime)

#Calculating the last submission date for each person for each assignment
SubDate = aggregate(DateTime ~ studentLogID + EventContext, data = subset(LogData, EventName %in% c("A submission has been submitted.","assign_submit")) ,max)

#Creating ActionName2 column for before and after assign submission
LogData <- cbind(LogData,LogData$EventName)
colnames(LogData)[16] <- "EventName2"
LogData$EventName2 <- as.character(LogData$EventName2)
LogData$EventName2[LogData$EventName2=="The status of the submission has been viewed."] <- "The status of the submission has been viewed (before submission)."
LogData$EventName2[LogData$EventName2=="assign_view"] <- "assign_view (before submission)"
LogData <- merge(LogData, SubDate, by=c("studentLogID", "EventContext"),all.x = TRUE)
LogData$EventName2[(LogData$EventName2=="The status of the submission has been viewed (before submission).") & 
                        (LogData$DateTime.x > LogData$DateTime.y)] <- "The status of the submission has been viewed (after submission)."
LogData$EventName2[(LogData$EventName2=="assign_view (before submission)") & 
                           (LogData$DateTime.x > LogData$DateTime.y)] <- "assign_view (after submission)"
names(LogData)[names(LogData)=="DateTime.x"] <- "DateTime"
names(LogData)[names(LogData)=="DateTime.y"] <- "SubDate"

#Adding "/component" to the events named "course module viewed" and "Course module instance list viewed"
LogData <- transform(LogData, EventName2 = ifelse(EventName2=="Course module viewed", paste(EventName2,Component,sep = "/"), EventName2))
LogData <- transform(LogData, EventName2 = ifelse(EventName2=="Course module instance list viewed", paste(EventName2,Component,sep = "/"), EventName2))

#Assing Practice/Graded to the actions in quiz component to identify if the quiz was for practice or grade
LogData <- transform(LogData, EventName2 = ifelse(Component=="Quiz", ifelse(grepl("Quiz: Quiz",as.character(EventContext)), paste(EventName2,"(Graded)"), paste(EventName2,"(Practice)")), EventName2))

#Adding the column for affordance of each action
LogData <- merge(LogData, AcAfData, by.x ="EventName2", by.y ="EventName",all.x = TRUE)

#Aggregarting actions to affordances
LogDataCount <- aggregate( DateTime~ studentLogID + Affordance , data=LogData, FUN = length)
names(LogDataCount)[names(LogDataCount)=="DateTime"] <- "Frequency"

#Removing users with less than 40 actions in total
temp <- aggregate( DateTime~ studentLogID , data=LogData, FUN = length)
names(temp)[names(temp)=="DateTime"] <- "Frequency"
removal <- subset(temp, Frequency < 40)
LogDataCount <- subset(LogDataCount, !studentLogID %in% removal$studentLogID)
remove(temp,removal)

#############CLUSTER ANALYSIS#################

#Subsetting log data counts for certain sections
#LogDataCountSub <- LogDataCount
LogDataCountSub <- subset(LogDataCount, grepl("F5", as.character(studentLogID))) #for certain sections (for Persona paper)
SampleSize <- aggregate(Frequency~studentLogID, data=LogDataCountSub, FUN=sum)

LogDataCountSub <- subset(LogDataCount, as.character(studentLogID) %in% as.character(SurData.Agg.Sub$studentLogID)) #for survey participants (for TechnoNiche paper)

#reformating log data as Matrix and getting distance matrix
LogDataCountMatrix <- acast(LogDataCountSub, studentLogID~Affordance, value.var="Frequency", fill = 0)
Eucdist <- dist(LogDataCountMatrix, method = "euclidean")
hclust.euc.ave <- hclust(Eucdist,method = "average")
plot(hclust.euc.ave)
abline(h=260,col="red")

# Pearson Coorelation Distance and cluster analysis
peardist <- pearson.dist(LogDataCountMatrix)
hclust.pear.ave <- hclust(peardist,method = "average")
plot(hclust.pear.ave)
abline(h=0.069,col="red")

#extracting cluster memberships for specified number of clusters or height
LogDataCountMatrix.Clustnum <-data.frame(LogDataCountMatrix, as.factor(cutree(hclust.euc.ave, h=260)), 
                                                                as.factor(cutree(hclust.pear.ave, h=0.069)))
names(LogDataCountMatrix.Clustnum)[6] <- "ClustNo.euc"
names(LogDataCountMatrix.Clustnum)[7] <- "ClustNo.pear"
summary(LogDataCountMatrix.Clustnum$ClustNo.euc)
summary(LogDataCountMatrix.Clustnum$ClustNo.pear)
#removing extra data points- clusters with less than 10 members
#AffData.clustered.clean <- subset(AffData.Clustnum,AffData.Clustnum$clustnumber %in% c("1","3","4"))
#AffData.clustered.clean$clustnumber <- factor(AffData.clustered.clean$clustnumber, levels = c("1","3","4"), labels = c("Species 1","Species 2","Species 3"))
temp <- LogDataCountMatrix.Clustnum %>% 
        group_by(ClustNo.pear) %>%
        summarise(no_rows = length(ClustNo.pear))
temp <- subset(temp,no_rows>18)
LogDataCountMatrix.Clustnum <- transform(LogDataCountMatrix.Clustnum, ClustNo.pear=ifelse(ClustNo.pear %in% temp$ClustNo.pear, ClustNo.pear, NA))
LogDataCountMatrix.Clustnum$ClustNo.pear <- as.factor(LogDataCountMatrix.Clustnum$ClustNo.pear)
temp <- LogDataCountMatrix.Clustnum %>% 
        group_by(ClustNo.euc) %>%
        summarise(no_rows = length(ClustNo.euc))
temp <- subset(temp,no_rows>18)
LogDataCountMatrix.Clustnum <- transform(LogDataCountMatrix.Clustnum, ClustNo.euc=ifelse(ClustNo.euc %in% temp$ClustNo.euc, ClustNo.euc, NA))
LogDataCountMatrix.Clustnum$ClustNo.euc <- as.factor(LogDataCountMatrix.Clustnum$ClustNo.euc)
remove(temp)

#go back to survey code for Technoniche paper

#Aggregating and averaging actions for clusters)
AFA.Agg.Clust.euc <- aggregate(cbind(CNT,SBM,CMU,PRC,FDB)~ClustNo.euc,data=LogDataCountMatrix.Clustnum,FUN = "mean")
AFA.Agg.Clust.pear <- aggregate(cbind(CNT,SBM,CMU,PRC,FDB)~ClustNo.pear,data=LogDataCountMatrix.Clustnum,FUN = "mean")

#reshaping pivot table
AFA_reshape.euc <- melt(AFA.Agg.Clust.euc, id.vars = "ClustNo.euc", 
                    measure.vars = c("CNT","SBM","CMU","PRC","FDB"))
AFA_reshape.pear <- melt(AFA.Agg.Clust.pear, id.vars = "ClustNo.pear", 
                    measure.vars = c("CNT","SBM","CMU","PRC","FDB"))

#plotting affordance actualization across species
ggplot(AFA_reshape.euc, aes(x=variable,y=value,colour=ClustNo.euc)) +  
        geom_line(aes(group=ClustNo.euc),size=1.2) +
        geom_point(size=3)+
        xlab("Affordance Actualization")+
        ylab("Average Number of Actions")+
        labs(title="Affordance Actualization Across Species (Euclidean)")
ggplot(AFA_reshape.pear, aes(x=variable,y=value,colour=ClustNo.pear)) +  
        geom_line(aes(group=ClustNo.pear),size=1.2) +
        geom_point(size=3)+
        xlab("Affordance Actualization")+
        ylab("Average Number of Actions")+
        labs(title="Affordance Actualization Across Species (Pearson)")

#ANOVA and Tukey test  
for (j in c("ClustNo.pear")) {
        for (i in c("CNT","SBM","CMU","PRC","FDB")){
                print(c(i,j))
                anovaAF <- aov(LogDataCountMatrix.Clustnum[,i]~LogDataCountMatrix.Clustnum[,j], data = LogDataCountMatrix.Clustnum)
                print(summary(anovaAF))
                print(TukeyHSD(anovaAF))
        }
}


#######Recycled codes#######
        
#plotting distribution probability of the three species across for affordance
ggplot(data=LogDataCountMatrix.Clustnum,aes(CMU,color=clustnumber))+ 
        geom_density(size=1.5)+
        xlab("Number of Actions")+
        ylab("Density")+
        ggtitle("Communication Affordance Actualization Across Species")+
        guides(colour=guide_legend(title=NULL))+
        theme(plot.title = element_text(face="bold"))+
        theme(axis.title = element_text(face="bold"),axis.text = element_text(color="black",face = "bold"))+
        theme(legend.position="none",legend.text = element_text(face = "bold"))


s <- summary(t$EventName)
write.table(y, file = "C:/Users/mmesgari/Desktop/UserCodesMatch.csv", sep=",")
#write.table(AffData.Clustnum,  file=" AffData-Clustnum.xls", row.names=T, sep="\t")

#########Getting the student-respondent-codes-match############
y <- aggregate(DateTime~studentLogID+respondentID ,data=LogData, FUN="mean")
write.table(UserData, file = "C:/Users/mmesgari/Desktop/UserData.csv", sep=",")