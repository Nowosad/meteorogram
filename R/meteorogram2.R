library(StreamMetabolism)
library(plyr)
setwd("/home/bartosz/github//meteorogram/R")
# read sample WRF dataset
# need to be fixed in future for reading raw grib/netcdf files; by now it is done seperately...
# dataset <- read.csv("http://openmeteo.pl/meta/wrf/poznan.txt", sep=";", header=F,na.strings=c("-999000000.000 ","  Entire grid contents are set to missing data "))
# colnames(dataset) <- c("data_init","czas","lon","lat","t2m","td","press","slp","ws","u","v","wd","ventilation")
# dataset$czas <- sub("Z"," ",as.character(dataset$czas))
# dataset$czas <- sub("JAN","-01-",as.character(dataset$czas))
# dataset$czas <- sub("FEB","-02-",as.character(dataset$czas))
# dataset$czas <- sub("MAR","-03-",as.character(dataset$czas))
# dataset$czas <- as.POSIXlt(strptime(dataset$czas, "%H %d-%m-%Y", tz="UTC"))

# creating sample datasets:
#dates <- seq(ISOdatetime(2016,1,29,1,0,0, tz="UTC"), ISOdatetime(2016,2,1,0,0,0, tz="UTC"), "hours")


DF1 <- head(test, n = 73)
library(plyr)
#UZUPELNIENIE BRAKOW DANYCH:
DF1[1,which(is.na(DF1[1,]))] <- (DF1[2,which(is.na(DF1[1,]))])
dates <- DF1$date2
len <- length(dates)

DF1$TMP <- DF1$TMP-273.15
DF1$TMAX <- DF1$TMAX-273.15
DF1$TMIN <- DF1$TMIN-273.15
DF1$DPT <- DF1$DPT-273.15
DF1$PRMSL <- DF1$PRMSL/100
DF1$VGRD <- ifelse(is.na(DF1$VGRD), 0, DF1$VGRD)
DF1$WS <- sqrt((DF1$UGRD^2)+(DF1$VGRD^2))
DF1$WD <- round((180/pi*atan2(DF1$UGRD,DF1$VGRD)+180))

DF1$PRATE <- DF1$PRATE*3600 # units: kg/m^2/s -> to mm/h
# TODO


# merging onto data.frame and calculating simple stats
DF <- data.frame(dates,DF1$TMAX, DF1$TMIN, DF1$TMP, DF1$PRATE)
DF$Date <- as.Date(DF$dates, "%m/%d/%Y")
stats <- aggregate(DF1.TMP ~ Date, DF, mean) 
stats$max<- round(aggregate(DF1.TMAX ~ Date, DF, max)[,2],1)
stats$min<- round(aggregate(DF1.TMIN ~ Date, DF, min)[,2],1)
stats$prec<- round(aggregate(DF1.PRATE ~ Date, DF, sum)[,2],1)

head(stats)


png(filename="../test.png", width = 630, height = 660)

##############################
## chart no.1 starting here:
##############################

# creating layout (similarly as presented @ meteo.pl)
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.79,0.91), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
ylim <- round(c(range(c(DF1$TMP,DF1$DPT))))
ylim[1] <- ylim[1]-2
ylim[2] <- ylim[2]+2

plot(DF1$date2,DF1$TMP, xaxt='n', xlab='', type='l', col='blue', lwd=2, ylim=ylim,xaxs = "i", yaxt='n')
daynight <- sunrise.set(52.4, 16.9, format(dates[1]-60*60*24, "%Y/%m/%d"), timezone="UTC", 5)
# delimiting day and night periods and adding results as shaded polygons
par(fig=c(0.10,0.90,0.06,0.95), new=F, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
for (i in 1:4) polygon(x = c(daynight$sunset[i], daynight$sunset[i], daynight$sunrise[i+1], daynight$sunrise[i+1]),  y=c(-40,50,50,-40 ), col="#e0e0e0", border = NA)
# col=rgb(0.1,0.1,0.1,0.15), 


par(fig=c(0.10,0.90,0.79,0.91), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
box()
axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=1.0, tck=-0.04)
lines(DF1$date2,DF1$TMP, type='l', col='red', lwd=3, yaxs = "i")
points(DF1$date2,DF1$TMAX, pch="|", col='#FF0000', lwd=3, yaxs = "i", cex=0.5)
points(DF1$date2,DF1$TMIN, pch="|", col='#FF0000', lwd=3, yaxs = "i", cex=0.5)
lines(DF1$date2,DF1$DPT, type='l', col='blue', lwd=2, yaxs = "i")

polygon(x=c(dates[1]-100000,dates[1]-100000,max(dates)+10000,max(dates)+10000), y = c(-30,0,0,-30), col="#0080FF80", border=NA)
#mtext(text = "Temperatura\n(C)", side = 2,padj = -3, cex=0.9)
mtext(text = "Temperatura\n na wys. 2m [C]", side = 2,padj = -1.5, cex=0.9)
p <- par('usr')
text(p[2]+20000, mean(p[3:4]), labels = 'Temperatura (C)', xpd = NA, srt = -90, cex=0.9)

abline(h=c(-20:30*5), lty=3)
abline(v =seq(dates[1],max(dates), by="3 hour"), col="black", lty=3)

axis(3, at=seq(dates[1],max(dates), by="6 hour"), labels = format(seq(dates[1],max(dates), by="6 hour"),"%H"), padj = 1.3, cex.axis=1.05, tck = -0.04)
axis(3, at=seq(dates[12],max(dates), by="24 hour"), labels = format(seq(dates[12],max(dates), by="24 hour"),"%a, %m-%d"), padj = 0, cex.axis=0.9,tick = FALSE)

axis(1, at=seq(dates[12],max(dates), by="24 hour")[1:3], labels = paste("Tmax = ",round(stats[1:3,3],1)), padj = -4, cex.axis=0.8, col.axis="red", tick = FALSE,lwd.ticks = 0,line = NA)
axis(1, at=seq(dates[12],max(dates), by="24 hour")[1:3], labels = paste("Tmin = ",round(stats[1:3,4],2)), padj = -3, cex.axis=0.8, col.axis="blue", tick = FALSE,lwd.ticks = 0,line = NA)


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
a <- DF1$PRATE
#barplot(a,add=T, xaxs='i')
x <- dates
y <- a
#barplot(a,add=T, xaxs='i')
if(is.na(y[1])) y[1]=0
#x <- seq_along(y)
y2 <- rep(y, each=2)
y2 <- y2[-length(y2)]
x2 <- rep(dates, each=2)[-1]
x3 <- c(min(x2), x2, max(x2))
y3 <- c(0, y2, 0)

zakres <- c(0, max(y)+5)
if(max(zakres)<5) zakres[2] <- 5
# because polygon() is dumb and wants a pre-existing plot
plot(x, y, ylim=zakres, type="n", xaxs = "i",yaxs = "i", xaxt='n',  cex.axis=0.8, yaxt='n')
labs <- axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.7, tck=-0.04)
#axis(4, cex.axis=0.9, outer = F,las=1,hadj=0.5, tck=-0.04)
polygon(x3, y3, border=NA, col="#0000FF50", yaxs = "i", xaxs='i')
lines(x2, y2, col="blue")

par(new = T)
zakres <- c(min(DF1$RH)-5, 100)
plot(dates,DF1$RH, col="coral", ylim=zakres, yaxs = "i", xaxs='i', axes=F, xlab=NA, ylab=NA, lty=1,  type="l", lwd=2)
axis(side = 4, cex.axis=0.9, outer=F, las=1, hadj=0.5, tck=-0.04)
abline(v =seq(dates[1],max(dates), by="3 hour"), col="black", lty=3)


mtext(text = "Opady\n(mm/godz)", side = 2,padj = -1.5, cex=0.9)
p <- par('usr')
text(p[2]+20000, mean(p[3:4]), labels = 'Wilgotność wzgl. [%]', xpd = NA, srt = -90, cex=0.9)
box()


##############################
## chart no.3 starting here:
##############################


par(fig=c(0.10,0.90,0.53,0.65), new=TRUE, mar = c(0, 0, 0, 0))
slp_range <- round(range(DF1$PRMSL))
slp_range[1] <- slp_range[1]-4
slp_range[2] <- slp_range[2]+4
source("/home/bartosz/github/meteorogram/R/add_slp_color.R")
barplot(DF1$PRMSL, ylim=slp_range, col=add_slp_color(76),names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0, yaxt='n')
#barplot(DF1$slp, ylim=slp_range, names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0)
box()
axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.7, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)

x_max <- length(dates)-1
lines(x = seq(0,x_max+1, length.out=x_max+1) ,DF1$PRMSL,lty=1,col="white",xaxs='i', lwd=3)
lines(x = seq(0,x_max+1, length.out=x_max+1) ,DF1$PRMSL,lty=1,col="black",xaxs='i', lwd=2)
abline(h=1:240*5, lty=3)
abline(v =0:100*3.04, col="black", lty=3)


#mtext(text = "Temperatura (C)", side = 4, cex=0.8)
mtext(text = "Ciśnienie [hPa]", side = 2,padj = -4.5, cex=0.9)
p <- par('usr')
text(p[2]+(p[2]/len)*6, mean(p[3:4]), labels = 'Ciśnienie [hPa]', xpd = NA, srt = -90, cex=0.9)
## end of chart no.3 (SLP)



############
# chart no.4
############
par(fig=c(0.10,0.90,0.40,0.52), new=TRUE, mar = c(0, 0, 0, 0))
wind_range <- c(0, max(ceiling(range(DF1$WS, DF1$GUST, na.rm=T)))+3)
if(max(wind_range)<9) wind_range[2] <- 9
#source("add_slp_color.R")
barplot(DF1$WS, ylim=wind_range, names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0, yaxt='n')
points(DF1$GUST , pch="-")
#barplot(DF1$slp, ylim=slp_range, names.arg = dates, xaxt='n', yaxs='i',xaxs='i', border=NA, cex.axis=0.8,space = 0.0)
box()
axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.7, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)

x_max <- length(dates)-1
lines(x = seq(0,x_max+1, length.out=x_max+1) ,DF1$WS,lty=1,col="white",xaxs='i', lwd=3)
lines(x = seq(0,x_max+1, length.out=x_max+1) ,DF1$WS,lty=1,col="black",xaxs='i', lwd=2)
abline(h=1:240*5, lty=3)
abline(v =0:100*3.04, col="black", lty=3)


#mtext(text = "Temperatura (C)", side = 4, cex=0.8)
mtext(text = "Wiatr [m/s]", side = 2,padj = -4.5, cex=0.9)
p <- par('usr')
text(p[2]+(p[2]/len)*6, mean(p[3:4]), labels = 'Wiatr [m/s]', xpd = NA, srt = -90, cex=0.9)
## end of chart no.4 (Wind speed)



############
# chart no.5 -> wind barbs
############
par(fig=c(0.10,0.90,0.34,0.39), new=TRUE, mar = c(0, 0, 0, 0))
x0 <- 1:len
y0 <- rep(0,length(x0))
Ugeo = round(-1 * sin(DF1$WD * pi/180),2)+1:len
Vgeo = round(-1 * cos(DF1$WD * pi/180),2)

plot(1:len, DF1$WS, ylim=c(-1,1), xaxt='n', yaxs='i',xaxs='i', cex.axis=0.8,yaxt='n', type='n')
arrows(x0=x0, y0=y0, x1=Ugeo, y1=Vgeo, length = 0.05 ,lwd = 1.5)
box()

abline(v =0:100*3.04, col="black", lty=3)

mtext(text = "Kierunek\nadwekcji", side = 2,padj = -1.5, cex=0.9)
p <- par('usr')
text(p[2]+(p[2]/len)*6, mean(p[3:4]), labels = 'kierunek', xpd = NA, srt = -90, cex=0.9)
## end of chart no.5 (Wind direction)





###########################
# zachmurzenie
#########################

par(fig=c(0.10,0.90,0.21,0.33), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
# wygenerujmy pewne ciagi danych
niskie <- DF1$TCDC_low/12.5
srednie <- DF1$TCDC_mid/12.5
wysokie <- DF1$TCDC_hig/12.5

plot(niskie, type='n', xaxt='n', yaxt='n', yaxs='i', xaxs='i', ylim=c(0,8))
polygon(x=c(0,0,10000,10000), y = c(100,0,0,100), col="#0080FF80", border=NA)

x <- 1:len
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

lines(niskie, col="#636363", lwd=1)
lines(srednie, col="#474747", lwd=1)
lines(wysokie, col="white", lwd=1)
box()

axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=1.0, tck=-0.04)

mtext(text = "Zachmurzenie\n(0-8 oktantów)", side = 2,padj = -1.5, cex=0.9)
p <- par('usr')
text(p[2]+20000, mean(p[3:4]), labels = 'Zachmurzenie (0-8)', xpd = NA, srt = -90, cex=0.9)

abline(h=c(0:4*2), lty=3)
abline(v =0:len*3, col="black", lty=3)
##########################################
##########################################



###########################
# chemizm
#########################
par(fig=c(0.10,0.90,0.10,0.20), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
# wygenerujmy pewne ciagi danych
pm10 <- round((sin(1:len*5)*15)+40)
plot(dates,pm10, type='n', xaxt='n', yaxt='n', yaxs='i', xaxs='i', ylim=c(0,max(pm10)+10))
abline(col="green", h=25, lwd=2, lty=2)
abline(col="red", h=50, lwd=2, lty=2)
box()
lines(dates,pm10, lwd=1.5, col=NA)
abline(v =seq(dates[1],max(dates), by="3 hour"), col="black", lty=3)
text(40, y=40,"Prognoza dostępna w ciągu najbliższych tygodni...")

axis(2, cex.axis=0.9, outer = F,las=1,hadj=0.3, tck=-0.04)
axis(4, cex.axis=0.9, outer = F,las=1,hadj=1.0, tck=-0.04)

axis(1, at=seq(dates[1]+delta,max(dates), by="6 hour"), labels = format(seq(from=dates[1], by="6 hour", length.out = 12),"%H"), padj = -1.5, cex.axis=1.05, tck = -0.04)
axis(1, at=seq(dates[12]+delta,max(dates), by="24 hour"), labels = format(seq(dates[12],max(dates), by="24 hour"),"%a, %m-%d"), padj = 0, cex.axis=0.9,tick = FALSE)

mtext(text = "PM10\n(uq/m3)", side = 2,padj = -1.5, cex=0.9)
p <- par('usr')
text(p[2]+20000, mean(p[3:4]), labels = 'PM10 (uq/m3)', xpd = NA, srt = -90, cex=0.9)

abline(h=c(0:100*20), lty=3)
abline(v =0:len*3, col="black", lty=3)


# dodanie stopki:
#        left, right, bottom, top
par(fig=c(0.70,0.95,0.075,1), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
mtext("(c) Zakład Klimatologii UAM \nBartosz Czernecki & Mateusz Taszarek",side=1, cex=0.75, padj=2, adj = 0)
par(fig=c(0.05,0.35,0.075,1), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
mtext("GFS grib (0.25x0.25)\nhttp://www.klimat.amu.edu.pl",side=1, cex=0.75, padj=2, adj = 0)

# dodanie czasu:
par(fig=c(0.88,0.95,0.12,0.95), new=T, mar = c(0, 0, 0, 0), oma=c(0,0,0,0))
mtext("CEST",side=1, cex=0.95, padj=2, adj = 0)

dev.off()

#system(command = "convert ../test.png ../ICM.png +append ../wyjscie.png")
#system(command = "eog ../test.png")