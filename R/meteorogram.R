
# creating sample datasets:
dates <- seq(ISOdatetime(2016,1,1,0,0,0, tz="UTC"), ISOdatetime(2016,1,3,0,0,0, tz="UTC"), "hours")

temp <- sin(1:length(dates)/pi)
temp2 <- jitter(sin(1:length(dates)/pi),factor = 400)
# TODO

plot(temp2, type='l')
lines(temp)
# creating layout (similarly as presented @ meteo.pl)
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.70,0.95), new=F, mar = c(0, 0, 0, 0))
plot(temp, xaxt='n', xlab='', type='l', col='blue', lwd=2, ylim=c(range(c(temp,temp2))))
lines(temp2, type='l', col='red', lwd=1.5)
grid(nx = length(dates)/3, ny=0, col="black")
  


par(fig=c(0.47,0.93,0.67,1.00), new=TRUE)
rysowanie_bezlinii(rhum850_noc_ave,brejks,tytul="")
linie(rhum850_noc_ave,brejks2)
par(fig=c(0.98,1.00,0.68,0.98), new=TRUE)
legenda(brejks)


par(mfrow = c(1, 3))
par(cex = 0.6)
par(mar = c(3, 3, 0, 0), oma = c(1, 1, 1, 1))

for (i in 1:6) {
  +     plot(1, 1, type = "n")
  +     mtext(letters[i], side = 3, line = -1, adj = 0.1, cex = 0.6)
  + }
=======
>>>>>>> 336096e54d4805ffc0a71bbedc44e8f8ab1b390b
