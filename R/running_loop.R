# run entire procedure:
setwd("/home/bartosz/github/meteorogram/")
source("R/read_gfs_from_file.R")
source("R/add_slp_color.R")
source("R/meteorogram2.R")

for (lon1 in seq(from=14,to=25,by=0.25)){
  for(lat1 in seq(from=49,to=55,by=0.25)){
    
    cat(paste(lon1,lat1,"\n"))
    
    DF1 <- create_df(gfs, lon1=lon1, lat1=lat1)
    tytul <- paste0(format(lon1, nsmall = 2),"_",format(lat1, nsmall = 2),".png")
    create_png(DF1 = DF1,tytul = tytul,lon1=lon1, lat1=lat1)
  }
}

system(command = "ls *png | wc", intern = T)
#system(command = "mv *png ../")