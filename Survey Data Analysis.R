#install.packages("data.table")
library(data.table)

SurData <- read.csv("C:/Users/mmesgari/Dropbox/Mostafa/Affordances/ETS/Essay2.Technoniche/Survey/Survey Data/All Surveys-For R.csv",na.strings = "NA")
#Adding studentLogID to survey data
CodeMatch <- aggregate(DateTime~studentLogID+respondentID ,data=LogData, FUN="mean")
SurData <- merge(CodeMatch[,c("studentLogID","respondentID")],SurData, by="respondentID")
#reshaping the data.frame
SurData.melt <- melt(SurData, id.vars = c("Status", "respondentID", "studentLogID"))

#aggregation: averaging AFP for each participant
SurData.Agg <- SurData[,c("Status","studentLogID")]
for (i in c("CNT","SBM","CMU","PRC","FDB")) {
        temp <- aggregate(value~studentLogID,
                  FUN= mean,
                  data=subset(SurData.melt,grepl(i, as.character(SurData.melt$variable))))
        SurData.Agg <- merge(SurData.Agg,temp,by="studentLogID")
        names(SurData.Agg)[length(SurData.Agg)] <- i
}
#aggregation: calculating learning style for each participant
for (i in c("ACT","SNS","VIS","SEQ")) {
        temp <- aggregate(value~studentLogID,
                       FUN= length,
                       data=subset(SurData.melt,grepl(i, as.character(SurData.melt$variable))&value==1))
        temp2 <- aggregate(value~studentLogID,
                          FUN= length,
                          data=subset(SurData.melt,grepl(i, as.character(SurData.melt$variable))&value==2))
        temp3 <- merge(temp,temp2,all=TRUE,by="studentLogID")
        temp3[is.na(temp3)] <- 0
        temp3 <- data.frame(temp3, temp3$value.x-temp3$value.y)
        temp3[,2] <- NULL
        temp3[,2] <- NULL
        SurData.Agg <- merge(SurData.Agg,temp3,by="studentLogID",all.x=TRUE)
        names(SurData.Agg)[length(SurData.Agg)] <- i
}
remove(temp)
remove(temp2)
remove(temp3)
SurData.Agg <- merge(SurData.Agg,SurData[,c("studentLogID","Age","Gender","Major","Exp")], by="studentLogID")
# For now: removing three wrong studentLogID
#SurData.Agg <- SurData.Agg[!SurData.Agg$studentLogID %in% c("PF525 B 034","PF525 B 035","PF525 B 023"),]

# Subsetting the survey participants we want to include
SurData.Agg.Sub <- subset(SurData.Agg, Status=="In") #grepl("CF5", as.character(SurData.Agg$studentLogID)) &

####################Go to Clustering code, subset and cluster survey data, then back to hear

#compile full UserData table
UserData <- merge(SurData.Agg,LogDataCountMatrix.Clustnum,by.x="studentLogID",by.y="row.names")
setnames(UserData, old = c('CNT.x','SBM.x','CMU.x','PRC.x','FDB.x','CNT.y','SBM.y','CMU.y','PRC.y','FDB.y'), 
                        new = c('CNT.AFP','SBM.AFP','CMU.AFP','PRC.AFP','FDB.AFP','CNT.AFA','SBM.AFA','CMU.AFA','PRC.AFA','FDB.AFA'))

#UserData$Gender <- as.factor(UserData$Gender)
#UserData$Major <- as.factor(UserData$Major)

#Aggregating and averaging actions for clusters)
AFA.Agg.Clust.euc <- aggregate(cbind(CNT.AFA,SBM.AFA,CMU.AFA,PRC.AFA,FDB.AFA)~ClustNo.euc,data=UserData,FUN = "mean")
AFA.Agg.Clust.pear <- aggregate(cbind(CNT.AFA,SBM.AFA,CMU.AFA,PRC.AFA,FDB.AFA)~ClustNo.pear,data=UserData,FUN = "mean")
AFP.Agg.Clust.euc <- aggregate(cbind(CNT.AFP,SBM.AFP,CMU.AFP,PRC.AFP,FDB.AFP)~ClustNo.euc,data=UserData,FUN = "mean")
AFP.Agg.Clust.pear <- aggregate(cbind(CNT.AFP,SBM.AFP,CMU.AFP,PRC.AFP,FDB.AFP)~ClustNo.pear,data=UserData,FUN = "mean")

#reshaping UserData table
AFA_reshape.euc <- melt(AFA.Agg.Clust.euc, id.vars = "ClustNo.euc", 
                        measure.vars = c("CNT.AFA","SBM.AFA","CMU.AFA","PRC.AFA","FDB.AFA"))
AFA_reshape.pear <- melt(AFA.Agg.Clust.pear, id.vars = "ClustNo.pear", 
                         measure.vars = c("CNT.AFA","SBM.AFA","CMU.AFA","PRC.AFA","FDB.AFA"))
AFP_reshape.euc <- melt(AFP.Agg.Clust.euc, id.vars = "ClustNo.euc", 
                        measure.vars = c("CNT.AFP","SBM.AFP","CMU.AFP","PRC.AFP","FDB.AFP"))
AFP_reshape.pear <- melt(AFP.Agg.Clust.pear, id.vars = "ClustNo.pear", 
                         measure.vars = c("CNT.AFP","SBM.AFP","CMU.AFP","PRC.AFP","FDB.AFP"))

#plotting affordance actualization across species
ggplot(AFA_reshape.euc, aes(x=variable,y=value,colour=ClustNo.euc)) +  
        geom_line(aes(group=ClustNo.euc),size=1.2) +
        geom_point(size=3)+
        xlab("Affordance Actualization")+
        ylab("Average Number of Actions")+
        labs(title="Affordance Actualization Across Species (Euclidean)")
ggplot(AFP_reshape.euc, aes(x=variable,y=value,colour=ClustNo.euc)) +  
        geom_line(aes(group=ClustNo.euc),size=1.2) +
        geom_point(size=3)+
        xlab("Affordance Perception")+
        ylab("Average Likert Scale")+
        labs(title="Affordance Perception Across Species (Euclidean)")
ggplot(AFA_reshape.pear, aes(x=variable,y=value,colour=ClustNo.pear)) +  
        geom_line(aes(group=ClustNo.pear),size=1.2) +
        geom_point(size=3)+
        xlab("Affordance Actualization")+
        ylab("Average Number of Actions")+
        labs(title="Affordance Actualization Across Species (Pearson)")
ggplot(AFP_reshape.pear, aes(x=variable,y=value,colour=ClustNo.pear)) +  
        geom_line(aes(group=ClustNo.pear),size=1.2) +
        geom_point(size=3)+
        xlab("Affordance Perception")+
        ylab("Average Likert Scale")+
        labs(title="Affordance Perception Across Species (Pearson)")


#ANOVA and Tukey test
for (j in c("ClustNo.euc","ClustNo.pear")) {
        for (i in c("CNT.AFA","CNT.AFP","SBM.AFA","SBM.AFP","CMU.AFA","CMU.AFP","PRC.AFA","PRC.AFP","FDB.AFA","FDB.AFP","ACT","SNS","VIS","SEQ","Age","Gender","Major","Exp")){
                print(c(i,j))
                anovaAF <- aov(UserData[,i]~UserData[,j], data = UserData)
                print(summary(anovaAF))
                print(TukeyHSD(anovaAF))
        }
}
# the table of average actions/perception for each cluster
AFA.Agg.Ave <- aggregate(cbind(CNT.AFA,SBM.AFA,CMU.AFA,PRC.AFA,FDB.AFA)~ClustNo.euc, data=UserData, FUN=mean)
AFP.Agg.Ave <- aggregate(cbind(CNT.AFP,SBM.AFP,CMU.AFP,PRC.AFP,FDB.AFP)~ClustNo.euc, data=UserData, FUN=mean)


### multivariable logistics regression 
library(mlogit)
install.packages("mlogit")
model <- glm(ClustNo.euc ~Gender,family=binomial(link='logit'),data=UserData)
model <- glm(Gender~ClustNo.euc ,family=binomial(link='logit'),data=UserData)
