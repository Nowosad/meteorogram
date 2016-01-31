

add_slp_color <- function(no_col){
kolory = colorRampPalette(colors=c("gray","purple","blue","skyblue3","yellow","green3","coral","red3","purple","gray"))
  
skala <- (data.frame(cisnienie2=965:1040,kolor=kolory(no_col)))
dataset$cisnienie2 <- floor(dataset$cisnienie)
wynik <- join(dataset,skala,by="cisnienie2")

as.character(wynik[,dim(wynik)[2]])
}
