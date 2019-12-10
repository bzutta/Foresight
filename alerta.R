Sys.time()
## print with possibly greater accuracy:
op <- options(digits.secs = 6)
Sys.time()
options(op)

## locale-specific version of date()
format(Sys.time(), "%a %b %d %X %Y")



year <- format(Sys.Date(), "%Y")





init <- "https://storage.cloud.google.com/earthenginepartners-hansen/GLADalert/"
# year <- format(Sys.Date(), "%Y")
# month <- format(Sys.Date(), "%m")
# day <- format(Sys.Date(), "%d")
date <- format(Sys.Date(), "%Y/%m_%d/")

# paste0(init,year,"/",month,"_",day,"/")
file1 <-  "alert19_090W_10S_080W_00N.tif"
url <- paste0(init,date,file1)

# 2019/03_27/

library(RCurl)

x <- getURL(url)
class(x)
download.file(url,destfile="alert19_090W_10S_080W_00N.tif")





download.file(url=url,destfile='nombre.tif', method='auto')

nose <- raster("GLADalert_2019_03_27_alert19_070W_20S_060W_10S.tif")
nlayers(nose)
plot(nose)
library(raster)

test <- raster("alerata/GLADalert_2019_03_28_alert18_080W_10S_070W_00N.tif")
salida <- as.data.frame( rasterToPoints(test) )
plot(test)
hist(test)