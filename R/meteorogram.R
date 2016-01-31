library(StreamMetabolism)
library(plyr)

# read sample WRF dataset
dataset <- read.csv("http://openmeteo.pl/meta/wrf/ASCII.txt", sep=";", na.strings="-999000000.00 ")
dataset$czas <- sub("Z"," ",as.character(dataset$czas))
dataset$czas <- sub("JAN","-01-",as.character(dataset$czas))
dataset$czas <- sub("FEB","-02-",as.character(dataset$czas))
dataset$czas <- as.POSIXlt(strptime(dataset$czas, "%H %d-%m-%Y", tz="UTC"))

# creating sample datasets:
#dates <- seq(ISOdatetime(2016,1,29,1,0,0, tz="UTC"), ISOdatetime(2016,2,1,0,0,0, tz="UTC"), "hours")
dates <- dataset$czas
len <- length(dates)

temp <- dataset$temp
temp2 <- dataset$dpt2m
# TODO


# merging onto data.frame and calculating simple stats
DF <- data.frame(dates,temp)
DF$Date <- as.Date(DF$dates, "%m/%d/%Y")
stats <- aggregate(temp ~ Date, DF, mean) 
stats$max<- round(aggregate(temp ~ Date, DF, max)[,2],1)
stats$min<- round(aggregate(temp ~ Date, DF, min)[,2],1)

head(stats)

# creating layout (similarly as presented @ meteo.pl)
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.70,0.90), new=F, mar = c(0, 0, 0, 0))
ylim=round(c(range(c(temp,temp2))))
ylim[1] <- ylim[1]-3
ylim[2] <- ylim[2]+3

plot(dates,temp, xaxt='n', xlab='', type='l', col='blue', lwd=3, ylim=ylim,xaxs = "i", cex.axis=0.8, ylab="Temperature")
lines(dates,temp2, type='l', col='red', lwd=2, yaxs = "i")

polygon(x=c(dates[1]-100000,dates[1]-100000,max(dates)+10000,max(dates)+10000), y = c(-30,0,0,-30), col="#0011FF30", border=NA)

abline(h=c(-20:30*5), lty=3)
abline(v =seq(dates[1],max(dates), by="6 hour"), col="black", lty=3)


aty <- interwal_x <- seq(1, len, by=3)
axis(3, at=seq(dates[1],max(dates), by="3 hour"), labels = format(seq(dates[1],max(dates), by="3 hour"),"%H"), padj = 1.5, cex.axis=0.75)
axis(3, at=seq(dates[12],max(dates), by="24 hour"), labels = format(seq(dates[12],max(dates), by="24 hour"),"%a, %m-%d"), padj = 0, cex.axis=0.8, hadj=)

axis(1, at=seq(dates[12],max(dates), by="24 hour")[1:3], labels = paste("Tmax = ",format(stats[1:3,3]+0.5,digits = 2)), padj = -3, cex.axis=0.7, col.axis="red", tick = FALSE,lwd.ticks = 0,line = NA)
axis(1, at=seq(dates[12],max(dates), by="24 hour")[1:3], labels = paste("Tmin = ",format(stats[1:3,4]-0.5,digits = 2)), padj = -2, cex.axis=0.7, col.axis="blue", tick = FALSE,lwd.ticks = 0,line = NA)


# delimiting day and night periods and adding results as shaded polygons
daynight <- sunrise.set(52.4, 16.9, format(dates[1]-60*60*24, "%Y/%m/%d"), timezone="UTC", 5)
#abline(v=c(daynight$sunrise, daynight$sunset))
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.03,0.95), new=TRUE, mar = c(0, 0, 0, 0))
for (i in 1:4) polygon(x = c(daynight$sunset[i], daynight$sunset[i], daynight$sunrise[i+1], daynight$sunrise[i+1]),  y=c(-40,50,50,-40 ), col=rgb(0.1,0.1,0.1,0.15), border = NA)

# dodanie stopki:
par(fig=c(0.10,0.90,0.00,1), new=TRUE, mar = c(0, 0, 0, 0))
text(max(dates), y=-2,"(c) Zakład Klimatologii UAM (2016)", cex=0.6, pos=2)
text(max(dates), y=-2.5,"Bartosz Czernecki & Mateusz Taszarek", cex=0.6, pos=2)



#        rysowanie wykresu nr 2
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.50,0.67), new=TRUE, mar = c(0, 0, 0, 0))
a <-sin(1:72)# wygenerowanie sztucznej serii
x <- dates
y <- dataset$opad
if(is.na(y[1])) y[1]=0
#x <- seq_along(y)
y2 <- rep(y, each=2)
y2 <- y2[-length(y2)]
x2 <- rep(dates, each=2)[-1]
x3 <- c(min(x2), x2, max(x2))
y3 <- c(0, y2, 0)

# because polygon() is dumb and wants a pre-existing plot
plot(x, y, ylim=c(0, max(y)+1), type="n", xaxs = "i", xaxt='n',  cex.axis=0.8)


polygon(x3, y3, border=NA, col="#0000FF50", yaxs = "i", xaxs='i')
lines(x2, y2)
lines(dates,dataset$dpt2m/dataset$temp, xaxt='n', xlab='', type='l', col='coral', lwd=2)
abline(h=c(0,0.5,1,2,5,10), lty=3)
abline(v =seq(dates[1],max(dates), by="6 hour"), col="black", lty=3)


par(fig=c(0.10,0.90,0.30,0.47), new=TRUE, mar = c(0, 0, 0, 0))
slp_range <- round(range(dataset$cisn_msl))
slp_range[1] <- slp_range[1]-6
slp_range[2] <- slp_range[2]+6
barplot(dataset$cisn_msl, ylim=slp_range, col=add_slp_color(76),names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0)
box()

x_max <- length(dates)-1
lines(x = seq(0,x_max+1, length.out=x_max+1) ,dataset$cisn_msl,lty=1,col="black",xaxs='i', lwd=3)
abline(h=1:240*5, lty=3)
abline(v =0:12*6, col="black", lty=3)

