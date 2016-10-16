library(dplyr)
library(tidyr)
kolumny <- c("date1","date2","var","lev","lon","lat","value")
klasy <- c("POSIXct","POSIXct","character","character","numeric","numeric","numeric")
gfs <- read.table("data/gfs.gz",header=F, sep=",", col.names = kolumny, colClasses = klasy)
#print(object.size(gfs), units="auto")
delta <- round(as.numeric(format(Sys.time(), tz='UTC', "%H"))-as.numeric(format(Sys.time(), "%H")))*-3600 # differences between local time zone and

gfs$date1 <- .POSIXct(gfs$date1, tz='UTC')+delta
gfs$date2 <- .POSIXct(gfs$date2, tz='UTC')+delta

create_df <- function(gfs,lon1,lat1){
test <- gfs %>% 
  filter(.,  lon==lon1, lat==lat1, date1==min(gfs$date1), 
         var!="TCDC", var!="ACPCP", var!="APCP", var!="CPRAT") %>% 
  select(-lev, -date1) %>% 
  arrange(var,date2) %>% 
  tidyr::spread(data = .,key = var,value = value)
return(head(test, n = 73))
}


