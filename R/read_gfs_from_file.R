setwd("/home/bartosz/gfs/")
kolumny <- c("date1","date2","var","lev","lon","lat","value")
klasy <- c("POSIXct","POSIXct","character","character","numeric","numeric","numeric")

gfs <- read.table("gfs.gz",header=F, sep=",", col.names = kolumny, 
                  colClasses = klasy)
print(object.size(gfs), units="auto")

delta <- round(as.numeric(format(Sys.time(), tz='UTC', "%H"))-as.numeric(format(Sys.time(), "%H")))*-3600

gfs$date1 <- .POSIXct(gfs$date1, tz='UTC')+delta
gfs$date2 <- .POSIXct(gfs$date2, tz='UTC')+delta


library(dplyr)
library(tidyr)
tmp <- gfs %>% filter(.,  lon==14.25, lat==54.5, date1==min(gfs$date1), var!="TCDC") %>% select(-lev, -date1, -lon, -lat) %>% arrange(var,date2)

test <- tidyr::spread(data = tmp,key = var,value = value)
rm(tmp)
rm(gfs)
