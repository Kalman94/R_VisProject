install.packages("devtools")
library("devtools")
install_github("ropensci/plotly", force=TRUE)

library(ggplot2)
library(plotly)
install.packages("dplyr")
install.packages("ggplot")
library(dplyr)
library(ggplot)
reshape2 
install.packages("dplyr")
library(reshape2 )

MyData <- read.csv(file="C:/Users/teo/Downloads/metal_bands_2017.csv", header=TRUE, sep=",")

#====================Preprocess==============================

df<-count(MyData,origin)
df <- df[order((df$n),decreasing=T),]
df <- df[-which(df$origin == ""), ]

df2<-count(MyData,as.factor(formed))
df2 <- df2[order((df2$n),decreasing=T),]
colnames(df2)[1] <- "formed"
df2_yearsort<- df2[order((df2$formed),decreasing=F),]

df2 <- df2[-which(df2$formed == "-"), ]

df3<-count(MyData,as.factor(split))
df3 <- df2[order((df3$n),decreasing=F),]
df3_yearsort <- df3[order((df3$formed),decreasing=F),]
colnames(df3)[1] <- "split"

x <- df[1:20,"origin"]
y <- df[1:20,"n"]
data <- data.frame(x, y)

x2 <- df2["formed"]
y2 <- df2["n"]
data2 <- data.frame(x2, y2)
col=c("red","blue")
bands_formed<-head(df2$n,-2)
bands_split<-df3$n
data22 <- data.frame(bands_formed,bands_split)
#========================Visualizations========================
#scatter
p <- ggplot(data22, aes(bands_formed,bands_split,colour="red"))
# plotting as it is
p + geom_point()
# adjusting size of the points
p + geom_point(aes(size=2))
# making the size dynamic
p + geom_point(aes(size=bands_split))
#barplot
ggplot(data=data, aes(x=origin, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=0.8, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))





#barplot2
ggplot(data=data2, aes(x=formed, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=-0.8, color="black", size=3.5)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


#preprocess2
df3_yearsort$n <- df3_yearsort$n * -1
merged <- merge(x = df3_yearsort,
                y = df2_yearsort,
                by = c("formed"))
colnames(merged)[2] <- "bands_formed"
colnames(merged)[3] <- "bands_split"

dfm <- melt(merged[,c('formed','bands_formed','bands_split')],id.vars = 1)

dev.off()




#contrast barplot
ggplot(data=dfm,aes(x=formed, y=value, fill=variable, color=variable, alpha=variable)) +
  geom_bar(stat="identity",position ="identity") +
  scale_colour_manual(values=c("lightblue4","red")) +
  scale_fill_manual(values=c("lightblue","pink")) +
  scale_alpha_manual(values=c(.3 ,.8))+ theme(axis.text.x = element_text(angle = 90, hjust = 5,vjust = 0.5))

