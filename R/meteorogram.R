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
plot(dates,temp, xaxt='n', xlab='', type='l', col='blue', lwd=2, ylim=c(range(c(temp,temp2))))
lines(dates,temp2, type='l', col='red', lwd=1.5)

abline(h=c(-20:30*2), lty=3)
abline(v = dates[seq(1, len+2, by=3)], col="black", lty=3)

axis(3, at=dates[seq(1, len, by=3)], labels = format(dates[seq(1, len, by=3)],"%H"), padj = 1.5, cex.axis=0.75)
axis(3, at=dates[seq(10, len, by=24)], labels = format(dates[seq(10, len, by=24)],"%a, %m-%d"), padj = 0, cex.axis=0.8)





# delimiting day and night periods and adding results as shaded polygons
daynight <- sunrise.set(52.4, 16.9, format(dates[1]-60*60*24, "%Y/%m/%d"), timezone="UTC", 5)
#abline(v=c(daynight$sunrise, daynight$sunset))

par(fig=c(0.10,0.90,0.05,0.95), new=F, mar = c(0, 0, 0, 0))

for (i in 1:4) polygon(x = c(daynight$sunset[i], daynight$sunset[i], daynight$sunrise[i+1], daynight$sunrise[i+1]),  y=c(-40,50,50,-40 ), col=rgb(0.1,0.1,0.1,0.15), border = NA)

