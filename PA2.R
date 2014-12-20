##Reproducible Research: Project Assignment 2

#Remember to set working directory before beginning data download

#Loading the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm data

if(!file.exists("./NOAA_Data")){
        dir.create("./NOAA_Data")
}

fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl,destfile="./NOAA_Data/StormData.csv.bz2")
#MAC users must add [method="curl"] to the download.file() arguments list

Sys.setlocale("LC_ALL","C")
dat<-read.csv(bzfile("./NOAA_Data/StormData.csv.bz2"))


#Which type of events are most harmful with respect to population health?

attach(dat)
pop.health<-FATALITIES+INJURIES
dat<-data.frame(dat,pop.health)
detach(dat)

library(dplyr)

df<-tbl_df(dat)

sum.dat<-df%>%
        group_by(EVTYPE)%>%
        summarize(sum(pop.health))

colnames(sum.dat)<-c("EventType","PopHealth")
sum.dat<-data.frame(sum.dat)

sum.dat<-sum.dat[order(sum.dat$PopHealth,decreasing=TRUE),]

dat.sub<-sum.dat[1:5,]

dat.sub$EventType<-as.character(dat.sub$EventType)
dat.sub$EventType<-factor(dat.sub$EventType,
                          levels=c("TORNADO","EXCESSIVE HEAT","TSTM WIND","FLOOD","LIGHTNING"))

barplot(height=dat.sub$PopHealth,
        names.arg=c("TORNADO","EXCESSIVE HEAT","THUNDERSTORM WIND","FLOOD","LIGHTNING"),
        main="Top 5 Threats to Population Health",ylab="Total Number of Injuries and Fatalities Combined",
        xlab="Type of Weather Event",col="red")
abline(h=mean(sum.dat$PopHealth),lty=2,lwd=1.5)
legend("topright",legend="Overall average number of injuries & fatalities combined by event type",lty=2,lwd=1,cex=0.7)

summary(sum.dat$PopHealth)
summary(dat.sub$PopHealth)


#Which type of events have the greatest economic consequences?

multiplier<-function(x) if(is.null(x) || is.na(x)){1
        }else if(x == 'K' || x== 'k'){1000
        }else if(x == 'M' || x == 'm'){1000000
        }else if(x == 'B' || x == 'b'){1000000000
        }else 1

vmulti <- Vectorize(multiplier)
total_prop_dmg <- dat$PROPDMG * vmulti(dat$PROPDMGEXP)
total_crop_dmg <- dat$CROPDMG * vmulti(dat$CROPDMGEXP)

dat2<-data.frame(dat,total_prop_dmg,total_crop_dmg)

str(dat2)

attach(dat2)
total.dmg<-total_prop_dmg + total_crop_dmg
dat2<-data.frame(dat2,total.dmg)
detach(dat2)

df2<-tbl_df(dat2)

sum.dat2<-df2%>%
        group_by(EVTYPE)%>%
        summarize(sum(total.dmg))

colnames(sum.dat2)<-c("EventType","TotalDamage")
sum.dat2<-data.frame(sum.dat2)

sum.dat2<-sum.dat2[order(sum.dat2$TotalDamage,decreasing=TRUE),]
dat.sub2<-sum.dat2[1:5,]

dat.sub2$EventType<-as.character(dat.sub2$EventType)
dat.sub2$EventType<-factor(dat.sub2$EventType,
                          levels=c("FLOOD","HURRICANE/TYPHOON","TORNADO","STORM SURGE","HAIL"))

barplot(height=dat.sub2$TotalDamage,
        names.arg=c("FLOOD","HURRICANE","TORNADO","STORM SURGE","HAIL"),
        main="Top 5 Most Costly Weather Threats",ylab="Total Value of Property and Crop Damage Combined ($)",
        xlab="Type of Weather Event",col="blue")
abline(h=mean(sum.dat2$TotalDamage),lty=2,lwd=1.5)
legend("topright",legend="Overall average value of property and crop damage combined by event type",lty=2,lwd=1,cex=0.7)

summary(sum.dat2$TotalDamage)
summary(dat.sub2$TotalDamage)
