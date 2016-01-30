library(StreamMetabolism)

# creating sample datasets:
dates <- seq(ISOdatetime(2016,1,29,1,0,0, tz="UTC"), ISOdatetime(2016,2,1,0,0,0, tz="UTC"), "hours")
len <- length(dates)

temp <- sin(1:len/pi)*3
temp2 <- jitter(sin(1:len/pi),factor = 400)*3
# TODO


# creating layout (similarly as presented @ meteo.pl)


#        left, right, bottom, top
par(fig=c(0.10,0.90,0.70,0.90), new=F, mar = c(0, 0, 0, 0))
plot(dates,temp, xaxt='n', xlab='', type='l', col='blue', lwd=2, ylim=c(range(c(temp,temp2))),yaxs = "i")
lines(dates,temp2, type='l', col='red', lwd=1.5, yaxs = "i")

abline(h=c(-20:30*2), lty=3)
abline(v = dates[seq(1, len+2, by=3)], col="black", lty=3)

axis(3, at=dates[seq(1, len, by=3)], labels = format(dates[seq(1, len, by=3)],"%H"), padj = 1.5, cex.axis=0.75)
axis(3, at=dates[seq(10, len, by=24)], labels = format(dates[seq(10, len, by=24)],"%a, %m-%d"), padj = 0, cex.axis=0.8)


# delimiting day and night periods and adding results as shaded polygons
daynight <- sunrise.set(52.4, 16.9, format(dates[1]-60*60*24, "%Y/%m/%d"), timezone="UTC", 5)
#abline(v=c(daynight$sunrise, daynight$sunset))
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.03,0.95), new=TRUE, mar = c(0, 0, 0, 0))
for (i in 1:4) polygon(x = c(daynight$sunset[i], daynight$sunset[i], daynight$sunrise[i+1], daynight$sunrise[i+1]),  y=c(-40,50,50,-40 ), col=rgb(0.1,0.1,0.1,0.15), border = NA)

# dodanie stopki:
par(fig=c(0.10,0.90,0.00,1), new=TRUE, mar = c(0, 0, 0, 0))
text(dates[50], y=-2,"(c) Zakład Klimatologii UAM (2016)", cex=0.6, pos=4)
text(dates[50], y=-2.5,"Bartosz Czernecki & Mateusz Taszarek", cex=0.6, pos=4)



#        rysowanie wykresu nr 2
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.50,0.67), new=TRUE, mar = c(0, 0, 0, 0))
a <-sin(1:72)# wygenerowanie sztucznej serii
x <- dates
y <- temp/10
#x <- seq_along(y)
y2 <- rep(y, each=2)
y2 <- y2[-length(y2)]
x2 <- rep(dates, each=2)[-1]
x3 <- c(min(x2), x2, max(x2))
y3 <- c(0, y2, 0)

# because polygon() is dumb and wants a pre-existing plot
plot(x, y, ylim=c(0, max(y)+1), type="n", yaxs = "i", xaxt='n')

polygon(x3, y3, border=NA, col="#0000FF50", yaxs = "i")
lines(x2, y2)
lines(dates,cos(temp/6), xaxt='n', xlab='', type='l', col='coral', lwd=2, ylim=c(range(c(temp,temp2))))
abline(h=c(0,0.5,1,2,5,10), lty=3)
abline(v = dates[seq(1, len+2, by=3)], col="black", lty=3)

