data<-read.csv(file.choose())
data$Year<-as.factor(data$Year)
data1<-data[data$Hospital!="Statewide",]
data1$X..of.Cases..ICD.9.10.[is.na(data1$X..of.Cases..ICD.9.10.)]<-0

##question 1##
ggplot() +
geom_bar(data=data1,aes(x = data1$Year,y=data1$X..of.Cases..ICD.9.10.,fill = data1$Surgery),stat = "identity",
                position = "stack")

##question 2###
q2<-aggregate(data1$X..of.Cases..ICD.9.10., by=list(Category=data1$Surgery), FUN=sum)
ggplot() +
  geom_bar(data=q2,aes(x = q2$Category,y=q2$x),stat = "identity")

##question 4##
q4<-aggregate(data1$X..of.Cases..ICD.9.10., by=list(Category=data1$Hospital), FUN=sum)
#----Top 5 hospitals with most surgeries--
head(q4[order(q4$x,decreasing = TRUE),],5)

##question 5##
q5<-aggregate(data1$X..of.Cases..ICD.9.10., by=list(Category=data1$Year), FUN=sum)
q5[order(q5$x,decreasing = TRUE),]




