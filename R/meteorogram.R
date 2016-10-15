library(StreamMetabolism)
library(plyr)
setwd("/home/bartosz/bitbucket/meteorogram/R")
# read sample WRF dataset
# need to be fixed in future for reading raw grib/netcdf files; by now it is done seperately...
dataset <- read.csv("http://openmeteo.pl/meta/wrf/poznan.txt", sep=";", header=F,na.strings=c("-999000000.000 ","  Entire grid contents are set to missing data "))
colnames(dataset) <- c("data_init","czas","lon","lat","t2m","td","press","slp","ws","u","v","wd","ventilation")
dataset$czas <- sub("Z"," ",as.character(dataset$czas))
dataset$czas <- sub("JAN","-01-",as.character(dataset$czas))
dataset$czas <- sub("FEB","-02-",as.character(dataset$czas))
dataset$czas <- sub("MAR","-03-",as.character(dataset$czas))
dataset$czas <- as.POSIXlt(strptime(dataset$czas, "%H %d-%m-%Y", tz="UTC"))

# creating sample datasets:
#dates <- seq(ISOdatetime(2016,1,29,1,0,0, tz="UTC"), ISOdatetime(2016,2,1,0,0,0, tz="UTC"), "hours")

DF1 <- head(dataset, n = 72)

dates <- DF1$czas
len <- length(dates)

temp <- DF1$t2m
temp2 <-DF1$td
# TODO


# merging onto data.frame and calculating simple stats
DF <- data.frame(dates,temp)
DF$Date <- as.Date(DF$dates, "%m/%d/%Y")
stats <- aggregate(temp ~ Date, DF, mean) 
stats$max<- round(aggregate(temp ~ Date, DF, max)[,2],1)
stats$min<- round(aggregate(temp ~ Date, DF, min)[,2],1)

head(stats)


png(filename="../test.png", width = 630, height = 660)

##############################
## chart no.1 starting here:
##############################

# creating layout (similarly as presented @ meteo.pl)
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.79,0.91), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
ylim=round(c(range(c(temp,temp2))))
ylim[1] <- ylim[1]-3
ylim[2] <- ylim[2]+3

plot(dates,temp, xaxt='n', xlab='', type='l', col='blue', lwd=3, ylim=ylim,xaxs = "i", yaxt='n')

daynight <- sunrise.set(52.4, 16.9, format(dates[1]-60*60*24, "%Y/%m/%d"), timezone="UTC", 5)
# delimiting day and night periods and adding results as shaded polygons
par(fig=c(0.10,0.90,0.03,0.95), new=F, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
for (i in 1:4) polygon(x = c(daynight$sunset[i], daynight$sunset[i], daynight$sunrise[i+1], daynight$sunrise[i+1]),  y=c(-40,50,50,-40 ), col="#e0e0e0", border = NA)
# col=rgb(0.1,0.1,0.1,0.15), 


par(fig=c(0.10,0.90,0.79,0.91), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
box()
axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=1.0, tck=-0.04)
lines(dates,temp, type='l', col='red', lwd=2, yaxs = "i")
lines(dates,DF1$td, type='l', col='blue', lwd=1, yaxs = "i")

polygon(x=c(dates[1]-100000,dates[1]-100000,max(dates)+10000,max(dates)+10000), y = c(-30,0,0,-30), col="#0080FF80", border=NA)
mtext(text = "Temperatura (C)", side = 2,padj = -3, cex=0.9)
#mtext(text = "Temperatura (C)", side = 4, cex=0.8)
p <- par('usr')
text(p[2]+20000, mean(p[3:4]), labels = 'Temperatura (C)', xpd = NA, srt = -90, cex=0.9)

abline(h=c(-20:30*5), lty=3)
abline(v =seq(dates[1],max(dates), by="3 hour"), col="black", lty=3)

axis(3, at=seq(dates[1],max(dates), by="6 hour"), labels = format(seq(dates[1],max(dates), by="6 hour"),"%H"), padj = 1.3, cex.axis=1.05, tck = -0.04)
axis(3, at=seq(dates[12],max(dates), by="24 hour"), labels = format(seq(dates[12],max(dates), by="24 hour"),"%a, %m-%d"), padj = 0, cex.axis=0.9,tick = FALSE)

axis(1, at=seq(dates[12],max(dates), by="24 hour")[1:3], labels = paste("Tmax = ",format(stats[1:3,3]+0.5,digits = 2)), padj = -3, cex.axis=0.7, col.axis="red", tick = FALSE,lwd.ticks = 0,line = NA)
axis(1, at=seq(dates[12],max(dates), by="24 hour")[1:3], labels = paste("Tmin = ",format(stats[1:3,4]-0.5,digits = 2)), padj = -2, cex.axis=0.7, col.axis="blue", tick = FALSE,lwd.ticks = 0,line = NA)


par(fig=c(0.08,0.18,0.92,0.96), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
mtext(paste0("wschód słońca: ", format(daynight[1,1],"%H:%M"), " UTC\nzachód słońca: ", format(daynight[1,2],"%H:%M"), " UTC"), cex = 0.9)
par(fig=c(0.82,0.95,0.90,0.95), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
mtext(("Poznań\n"), cex = 1.05, adj=0)

#daynight <- sunrise.set(52.4, 16.9, format(dates[1]-60*60*24, "%Y/%m/%d"), timezone="UTC", 5)


#par(fig=c(0.10,0.90,0.03,0.95), new=F, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
#for (i in 1:4) polygon(x = c(daynight$sunset[i], daynight$sunset[i], daynight$sunrise[i+1], daynight$sunrise[i+1]),  y=c(-40,50,50,-40 ), col="#e0e0e090", border = NA)
                      # col=rgb(0.1,0.1,0.1,0.15), 





##############################
## chart no.2 starting here:
##############################
#        rysowanie wykresu nr 2
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.66,0.78), new=TRUE, mar = c(0, 0, 0, 0))
a <-sin(1:72)# wygenerowanie sztucznej serii
#barplot(a,add=T, xaxs='i')
x <- dates
y <- a
if(is.na(y[1])) y[1]=0
#x <- seq_along(y)
y2 <- rep(y, each=2)
y2 <- y2[-length(y2)]
x2 <- rep(dates, each=2)[-1]
x3 <- c(min(x2), x2, max(x2))
y3 <- c(0, y2, 0)

# because polygon() is dumb and wants a pre-existing plot
plot(x, y, ylim=c(0, max(y)+1), type="n", xaxs = "i",yaxs = "i", xaxt='n',  cex.axis=0.8, yaxt='n')
labs <- axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.7, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=0.5, tck=-0.04)


polygon(x3, y3, border=NA, col="#0000FF50", yaxs = "i", xaxs='i')
lines(x2, y2)
lines(dates,DF1$td/DF1$t2m, xaxt='n', xlab='', type='l', col='coral', lwd=2)
abline(h=labs, lty=3)
abline(v =seq(dates[1],max(dates), by="3 hour"), col="black", lty=3)


mtext(text = "Opady\n(mm/godz)", side = 2,padj = -1.5, cex=0.9)
p <- par('usr')
text(p[2]+20000, mean(p[3:4]), labels = 'Opady [mm/godz]', xpd = NA, srt = -90, cex=0.9)



##############################
## chart no.3 starting here:
##############################


par(fig=c(0.10,0.90,0.53,0.65), new=TRUE, mar = c(0, 0, 0, 0))
slp_range <- round(range(DF1$slp))
slp_range[1] <- slp_range[1]-6
slp_range[2] <- slp_range[2]+6
source("add_slp_color.R")
barplot(DF1$slp, ylim=slp_range, col=add_slp_color(76),names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0, yaxt='n')
#barplot(DF1$slp, ylim=slp_range, names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0)
box()
axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.7, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)

x_max <- length(dates)-1
lines(x = seq(0,x_max+1, length.out=x_max+1) ,DF1$slp,lty=1,col="white",xaxs='i', lwd=3)
lines(x = seq(0,x_max+1, length.out=x_max+1) ,DF1$slp,lty=1,col="black",xaxs='i', lwd=2)
abline(h=1:240*5, lty=3)
abline(v =0:100*3.04, col="black", lty=3)


#mtext(text = "Temperatura (C)", side = 4, cex=0.8)
mtext(text = "Ciśnienie [hPa]", side = 2,padj = -4.5, cex=0.9)
p <- par('usr')
text(p[2]+(p[2]/72)*6, mean(p[3:4]), labels = 'Ciśnienie [hPa]', xpd = NA, srt = -90, cex=0.9)
## end of chart no.3 (SLP)



############
# chart no.4
############
par(fig=c(0.10,0.90,0.40,0.52), new=TRUE, mar = c(0, 0, 0, 0))
wind_range <- c(0, max(ceiling(range(DF1$ws, DF1$u, na.rm=T))))
source("add_slp_color.R")
barplot(DF1$ws, ylim=wind_range, names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0, yaxt='n')
points(DF1$u , pch="-")
#barplot(DF1$slp, ylim=slp_range, names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0)
box()
axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.7, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)

x_max <- length(dates)-1
lines(x = seq(0,x_max+1, length.out=x_max+1) ,DF1$slp,lty=1,col="white",xaxs='i', lwd=3)
lines(x = seq(0,x_max+1, length.out=x_max+1) ,DF1$slp,lty=1,col="black",xaxs='i', lwd=2)
abline(h=1:240*5, lty=3)
abline(v =0:100*3.04, col="black", lty=3)


#mtext(text = "Temperatura (C)", side = 4, cex=0.8)
mtext(text = "Wiatr [m/s]", side = 2,padj = -4.5, cex=0.9)
p <- par('usr')
text(p[2]+(p[2]/72)*6, mean(p[3:4]), labels = 'Wiatr [m/s]', xpd = NA, srt = -90, cex=0.9)
## end of chart no.4 (Wind speed)



############
# chart no.5 -> wind barbs
############
par(fig=c(0.10,0.90,0.34,0.39), new=TRUE, mar = c(0, 0, 0, 0))
x0 <- 1:72
y0 <- rep(0,length(x0))
x1 <- sort(x0+rnorm(length(x0))) # domyslnie jakas kolumna z wartosciami dla skladowej 'u' wiatru
y1 <- y0+rnorm(length(x0)) # domyslnie jakas kolumna z wartosciami dla skladowej 'u' wiatru

plot(1:72, DF1$ws, ylim=range(c(y0,y1)), xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0, yaxt='n', type='n')
arrows(x0=x0, y0=y0, x1=x1, y1=y1, length = 0.05 ,lwd = 1.7)
#barplot(DF1$slp, ylim=slp_range, names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0)
box()

abline(v =0:100*3.04, col="black", lty=3)

mtext(text = "kierunek", side = 2,padj = -4.5, cex=0.9)
p <- par('usr')
text(p[2]+(p[2]/72)*6, mean(p[3:4]), labels = 'kierunek', xpd = NA, srt = -90, cex=0.9)
## end of chart no.5 (Wind direction)





###########################
# zachmurzenie
#########################

par(fig=c(0.10,0.90,0.21,0.33), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
# wygenerujmy pewne ciagi danych
niskie <- (round(rnorm(72,4),2))
srednie <- (round(rnorm(72,3),2))
wysokie <- (round(rnorm(72,2.5),2))

plot(niskie, type='n', xaxt='n', yaxt='n', yaxs='i', xaxs='i', ylim=c(0,8))
polygon(x=c(0,0,10000,10000), y = c(100,0,0,100), col="#0080FF80", border=NA)

x <- 1:72
y <- wysokie
if(is.na(y[1])) y[1]=0
y2 <- rep(y, each=2); y2 <- y2[-length(y2)] ; x2 <- rep(x, each=2)[-1]; x3 <- c(min(x2), x2, max(x2)) ;y3 <- c(0, y2, 0)
polygon(x3, y3, border=NA, col="#ffffff", yaxs = "i", xaxs='i')

y <- srednie
if(is.na(y[1])) y[1]=0
y2 <- rep(y, each=2); y2 <- y2[-length(y2)] ; x2 <- rep(x, each=2)[-1]; x3 <- c(min(x2), x2, max(x2)) ;y3 <- c(0, y2, 0)
polygon(x3, y3, border=NA, col="#474747", yaxs = "i", xaxs='i')

y <- niskie
if(is.na(y[1])) y[1]=0
y2 <- rep(y, each=2); y2 <- y2[-length(y2)] ; x2 <- rep(x, each=2)[-1]; x3 <- c(min(x2), x2, max(x2)) ;y3 <- c(0, y2, 0)
polygon(x3, y3, border=NA, col="#636363", yaxs = "i", xaxs='i')

lines(niskie, col="white")
lines(srednie, col="#474747")
lines(wysokie, col="#636363")

axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=1.0, tck=-0.04)

mtext(text = "Zachmurzenie (0-8)", side = 2,padj = -3, cex=0.9)
#mtext(text = "Temperatura (C)", side = 4, cex=0.8)
p <- par('usr')
text(p[2]+20000, mean(p[3:4]), labels = 'Zachmurzenie (0-8)', xpd = NA, srt = -90, cex=0.9)

abline(h=c(0:4*2), lty=3)
abline(v =0:72*3, col="black", lty=3)
##########################################
##########################################



###########################
# chemizm
#########################

par(fig=c(0.10,0.90,0.06,0.18), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
# wygenerujmy pewne ciagi danych
pm10 <- round((sin(1:72*5)*15)+40)
plot(pm10, type='n', xaxt='n', yaxt='n', yaxs='i', xaxs='i', ylim=c(0,max(pm10)+10))

abline(col="green", h=25, lwd=2, lty=2)
abline(col="red", h=50, lwd=2, lty=2)
box()

lines(pm10, lwd=1.5)

axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=1.0, tck=-0.04)

mtext(text = "PM10 (uq/m3)", side = 2,padj = -3, cex=0.9)
#mtext(text = "Temperatura (C)", side = 4, cex=0.8)
p <- par('usr')
text(p[2]+20000, mean(p[3:4]), labels = 'PM10 (uq/m3)', xpd = NA, srt = -90, cex=0.9)

abline(h=c(0:100*20), lty=3)
abline(v =0:72*3, col="black", lty=3)


# dodanie stopki:
#        left, right, bottom, top
par(fig=c(0.70,0.95,0.075,1), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
mtext("(c) Zakład Klimatologii UAM \nBartosz Czernecki & Mateusz Taszarek",side=1, cex=0.75, padj=2, adj = 0)
par(fig=c(0.05,0.35,0.075,1), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
mtext("\nhttp://www.klimat.amu.edu.pl",side=1, cex=0.75, padj=2, adj = 0)

dev.off()

system(command = "convert ../test.png ../ICM.png +append ../wyjscie.png")
system(command = "eog ../wyjscie.png")