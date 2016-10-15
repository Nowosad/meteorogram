

add_slp_color <- function(no_col){
kolory = colorRampPalette(colors=c("gray","purple","blue","skyblue3","yellow","green3","coral","red3","purple","gray"))
  
skala <- (data.frame(cisnienie2=965:1040,kolor=kolory(no_col)))
DF1$cisnienie2 <- floor(DF1$slp)
wynik <- join(DF1,skala,by="cisnienie2")

as.character(wynik[,dim(wynik)[2]])
}
