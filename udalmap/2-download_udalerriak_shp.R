library(maptools)

## Download shape file and unzip
if (!file.exists("udalerriak/udalerriak.shp"))
{ udalerriak.url <- "http://www.euskalgeo.net/sites/euskalgeo.net/files/fitxategi-eranskin/udalerriak_0.zip"
  download.file(udalerriak.url, "udalerriak.zip")
  unzip("udalerriak.zip", exdir="udalerriak")
}

## Load the shape file to R
udalerriak.shape<-readShapePoly("udalerriak/udalerriak.shp")

## Change encoding from Windows-1252 to UTF-8
for ( i in 1:ncol(udalerriak.shape@data))
  udalerriak.shape@data[,i] <- iconv(udalerriak.shape@data[,i], "Windows-1252", "UTF-8")

## View map
plot(udalerriak.shape)
