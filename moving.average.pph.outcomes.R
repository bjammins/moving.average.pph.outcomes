
library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)


setwd('L:/PPH Project/data/Final Raw data/')
df <- read.csv("outcome_measures_05_05_2016.csv", head=TRUE)

## Paste a month to the character and covert the period to a date
df$Period <- as.Date(paste('01', df$Period, sep = "-"), format='%d-%b-%y')

#Remove 2016 period dates
df <- df[df$Period < "2016-01-01", ]

timeser <- df %>% filter(Complete. == TRUE) %>% group_by(Period) %>% summarize(count=n(),sum_Q1=sum(Q1), sum_Q2=sum(Q2), 
                                                                               sum_Q3=sum(Q3), sum_Q4=sum(Q4), 
                                                                               sum_Q5=sum(Q5), sum_Q6=sum(Q6), 
                                                                               sum_Q7=sum(Q7), sum_Q8=sum(Q8),
                                                                               sum_Q9=sum(Q9), sum_Q12=sum(Q12), PRBC=(sum_Q3/sum_Q1)*100, mass_trans=(sum_Q8/sum_Q1)*100, hyst=(sum_Q9/sum_Q1)*100, ICU_admits=(sum_Q12/sum_Q1)*100)

#Create a moving average function
#calculate moving average
# x is the time series data
# n is the number of time series elements to average
ma <- function(x,n=12){stats::filter(x,rep(1/n,n), sides=1)}

#calculate ma for each item

timeser$PRBC_ma <- round(ma(timeser$PRBC),digits=1)
#create a PRBC moving average percent label for the y axis
#timeser$PRBC_ma_lab <- paste(timeser$PRBC_ma,"%", sep="")

timeser$mass_trans_ma <- ma(timeser$mass_trans)
timeser$hyst_ma <- ma(timeser$hyst)
timeser$ICU_admits_ma <- ma(timeser$ICU_admits)

## Create graphs of each of the moving averages
## PRBC
pdf("Packed Red Blood Cells.pdf",width=7,height=5)
c <- ggplot(timeser, aes(x=Period, y=PRBC_ma))
c <- c + geom_point() + scale_x_date(limits = c(as.Date("2014-06-01"), as.Date("2015-12-01")), 
                                     labels = date_format("%m/%y"),
                                     breaks=seq(as.Date("2014-06-01"), as.Date("2015-12-01"), by="2 months"))
c <- c + scale_y_continuous(breaks=seq(0,4.5, by=0.5))
c <- c + expand_limits(y=c(0,4.5))
c <- c + geom_smooth(se=FALSE)
c <- c + ylab("Percent")+ggtitle("Packed Red Blood Cells") + theme_minimal()
c <- c + theme(plot.title = element_text(hjust = 0.5))
c
dev.off()

## Massive Transfusion
pdf("Massive Transfusions.pdf",width=7,height=5)
c <- ggplot(timeser, aes(x=Period, y=mass_trans_ma))
c <- c + geom_point() + scale_x_date(limits = c(as.Date("2014-06-01"), as.Date("2015-12-01")), 
                                     labels = date_format("%m/%y"),
                                     breaks=seq(as.Date("2014-06-01"), as.Date("2015-12-01"), by="2 months"))
c <- c + scale_y_continuous(breaks=seq(0,0.3, by=0.05))
c <- c + expand_limits(y=c(0,0.3))
c <- c + geom_smooth(se=FALSE)
c <- c + ylab("Percent")+ggtitle("Massive Transfusions") + theme_minimal()
c <- c + theme(plot.title = element_text(hjust = 0.5))
c
dev.off()

## Hysterectomies
pdf("Hysterectomies.pdf",width=7,height=5)
c <- ggplot(timeser, aes(x=Period, y=hyst_ma))
c <- c + geom_point() + scale_x_date(limits = c(as.Date("2014-06-01"), as.Date("2015-12-01")), 
                                     labels = date_format("%m/%y"),
                                     breaks=seq(as.Date("2014-06-01"), as.Date("2015-12-01"), by="2 months"))
c <- c + scale_y_continuous(breaks=seq(0,0.3, by=0.05))
c <- c + expand_limits(y=c(0,0.3))
c <- c + geom_smooth(se=FALSE)
c <- c + ylab("Percent")+ggtitle("Hysterectomies") + theme_minimal()
c <- c + theme(plot.title = element_text(hjust = 0.5))
c
dev.off()

## PPH Related ICU Admissions
pdf("PPH Related ICU Admissions.pdf",width=7,height=5)
c <- ggplot(timeser, aes(x=Period, y=ICU_admits_ma))
c <- c + geom_point() + scale_x_date(limits = c(as.Date("2014-06-01"), as.Date("2015-12-01")), 
                                     labels = date_format("%m/%y"),
                                     breaks=seq(as.Date("2014-06-01"), as.Date("2015-12-01"), by="2 months"))
c <- c + scale_y_continuous(breaks=seq(0,0.3, by=0.05))
c <- c + expand_limits(y=c(0,0.3))
c <- c + geom_smooth(se=FALSE)
c <- c + ylab("Percent")+ggtitle("PPH-Related ICU Admissions") + theme_minimal()
c <- c + theme(plot.title = element_text(hjust = 0.5))
c
dev.off()
