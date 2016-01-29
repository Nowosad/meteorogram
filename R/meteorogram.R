
# creating sample datasets:

<<<<<<< HEAD
# TODO



# creating layout (similarly as presented @ meteo.pl)
#        left, right, bottom, top
par(fig=c(0.10,0.90,0.70,0.95), new=F, mar = c(0, 0, 0, 0))
plot(matrix(runif(6),3,2), xaxt='n', xlab='')
linie(rhum850_dzien_ave,brejks2)
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
