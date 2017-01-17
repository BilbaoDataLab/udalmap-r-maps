library(XML)      # readHTMLTable
library(rgeos)
library(maptools) # readShapePoly
library(dplyr)    # awesome data manipulation
library(tidyr)    # reshaping data
library(scales)   # for 'percent'
library(stringi)  # string manipulation
library(stringr)  # string manipulation
library(ggplot2)  # plotting
library(ggthemes) # theme_map

## Parameters

udalmap <- read.csv("udalmap2016.csv")

## Download shape file and unzip from Euskalgeo
if (!file.exists("udalerriak/udalerriak.shp"))
{ udalerriak.url <- "http://www.euskalgeo.net/sites/euskalgeo.net/files/fitxategi-eranskin/udalerriak_0.zip"
  download.file(udalerriak.url, "udalerriak.zip")
  unzip("udalerriak.zip", exdir="udalerriak")
}

udalerriak.shape <- readShapePoly("udalerriak/udalerriak.shp")

## Change encoding from Windows-1252 to UTF-8
for ( i in 1:ncol(udalerriak.shape@data))
  udalerriak.shape@data[,i] <- iconv(udalerriak.shape@data[,i], "Windows-1252", "UTF-8")

# rownames(udalmap[[1]]) %in% map.names
map.names <- gsub(" \\/ ","\\/",udalerriak.shape@data$iz_ofizial)

# Changes names for municipalities to match the names of Euskalgeo with Udalmap data.
map.names[336] <- "Salvatierra/Agurain"
map.names[460] <- "Sopela"
map.names[587] <- "Arratzua-Ubarrundia"
map.names[666] <- "Urduliz"
map.names[682] <- "Donostia/San Sebastián"

udalerriak.shape@data$name <- map.names

udalerriak_eae <- unique(as.character(udalmap$name))
udalerriak_eae.shape <- udalerriak.shape[udalerriak.shape$name %in% udalerriak_eae,]
rownames(udalerriak_eae.shape@data) <- udalerriak_eae.shape$name

udalerriak_eae.map <- fortify(udalerriak_eae.shape, region="name")

## Plot

## Breaks for percentages
per_scale_breaks <- c(0, 2.5, 5, 10, 25, 50, 75, 80, 100)

sprintf("%2.1f-%s", per_scale_breaks, percent(lead(per_scale_breaks/100))) %>%
  stri_replace_all_regex(c("^0.0", "-NA%"), c("0", "%"), vectorize_all=FALSE) %>%
  head(-1) -> breaks_labels

# Colour pallet
colour_pal <- c("#eaecd8", "#d6dab3", "#c2c98b", "#949D48", "#6e7537", "#494E24", "#BB792A", "#7C441C")

if (!dir.exists("eae"))
  dir.create("eae")

indicators <- unique(udalmap$indid)

for (indicator in indicators) ##change "length(udalmap)" to small number like "3" to test generation of only 3 maps
{ ## Plot each indicators colour map (by years) to out dir

  df <- udalmap %>%
    filter(indid==indicator)

  ## Get indicator id and name
  indicator.id <- unique(as.character(df$indid))
  indicator.name <- unique(as.character(df$indname_eus))

  print(paste(i,indicator.id))
  print(indicator.name)

  if ( grepl("%", indicator.name) | grepl("‰", indicator.name))
  {
    df %>%
      gather(year, value, -indid, -name, -indname_eus, -indname_en, -indname_es) %>%
      mutate(value=as.numeric(value)) %>%
      filter(!is.na(value)) %>%
      mutate(`%`=cut(value,
        c(0, 2.5, 5, 10, 25, 50, 75, 80, 100),
        breaks_labels))-> indicator_dat

      colour_pal.tmp <- colour_pal[breaks_labels %in% indicator_dat$'%']

  } else if (grepl("Índice", indicator.name) |
             grepl("indizea", indicator.name) |
             grepl("index", indicator.name) )
  {
    df %>%
      gather(year, value, -indid, -name, -indname_eus, -indname_en, -indname_es) %>%
      mutate(value=as.numeric(value)) %>%
      filter(!is.na(value)) -> indicator_dat

      cont_scale_breaks <- round(c(0:4,1/0),2)

      sprintf("%s-%s", cont_scale_breaks, lead(cont_scale_breaks)) %>%
        head(-1) -> value_breaks_labels


      indicator_dat %>% mutate(Indizea=cut(value,
        cont_scale_breaks,
        value_breaks_labels))-> indicator_dat

      colour_pal.tmp <- colour_pal[4:8]

  } else if (grepl("Superficie", indicator.name) |
             grepl("azalera", indicator.name) |
             grepl("area", indicator.name) )
  {
     df %>%
      gather(year, value, -indid, -name, -indname_eus, -indname_en, -indname_es) %>%
      mutate(value=as.numeric(value)) %>%
      filter(!is.na(value)) -> indicator_dat

      m2_scale_breaks <- unique(round(fivenum(indicator_dat$value,na.rm=T),2))

      sprintf("%s-%s", m2_scale_breaks, lead(m2_scale_breaks)) %>%
        head(-1) -> m2_breaks_labels


      indicator_dat %>% mutate(`m²`=cut(value,
        m2_scale_breaks,
        m2_breaks_labels))-> indicator_dat

      colour_pal.tmp <- colour_pal[5:(4+length(m2_breaks_labels))]

  } else
  {
     df %>%
      gather(year, value, -indid, -name, -indname_eus, -indname_en, -indname_es) %>%
      mutate(value=as.numeric(value)) %>%
      filter(!is.na(value)) -> indicator_dat

      cont_scale_breaks <- unique(round(fivenum(indicator_dat$value,na.rm=T),2))

      sprintf("%s-%s", cont_scale_breaks, lead(cont_scale_breaks)) %>%
        head(-1) -> value_breaks_labels

      colour_pal.tmp <- colour_pal[5:(4+length(value_breaks_labels))]

      if (grepl("€", indicator.name))
      { indicator_dat %>% mutate("€"=cut(value,
        cont_scale_breaks,
        value_breaks_labels))-> indicator_dat
      } else
      { indicator_dat %>% mutate(Kopurua=cut(value,
        cont_scale_breaks,
        value_breaks_labels))-> indicator_dat
      }

  }

  ##############################################################
  gg <- ggplot()

  gg <- gg + geom_map(data=udalerriak_eae.map, map=udalerriak_eae.map,
                    aes(x=long, y=lat, group=group, map_id=id),
                    color="#7f7f7f", fill="white", size=0.15)


  if ( grepl("%", indicator.name) | grepl("‰", indicator.name) )
  { gg <- gg + geom_map(data=indicator_dat, map=udalerriak_eae.map,
                    aes(map_id=name, fill=`%`),
                    color="#7f7f7f", size=0.15)
  } else if (grepl("€", indicator.name))
  { gg <- gg + geom_map(data=indicator_dat, map=udalerriak_eae.map,
                    aes(map_id=name, fill=`€`),
                    color="#7f7f7f", size=0.15)
  } else if (grepl("Índice", indicator.name) |
             grepl("indizea", indicator.name) |
             grepl("index", indicator.name) )
  { gg <- gg + geom_map(data=indicator_dat, map=udalerriak_eae.map,
                    aes(map_id=name, fill=Indizea),
                    color="#7f7f7f", size=0.15)
  } else if (grepl("Superficie", indicator.name) |
             grepl("azalera", indicator.name) |
             grepl("area", indicator.name) )
  { gg <- gg + geom_map(data=indicator_dat, map=udalerriak_eae.map,
                    aes(map_id=name, fill=`m²`),
                    color="#7f7f7f", size=0.15)
  } else
  { gg <- gg + geom_map(data=indicator_dat, map=udalerriak_eae.map,
                    aes(map_id=name, fill=Kopurua),
                    color="#7f7f7f", size=0.15)
  }

  gg <- gg + scale_fill_manual(values=colour_pal.tmp)

  gg <- gg + guides(fill=guide_legend(override.aes=list(colour=NA)))


  gg <- gg + labs(title=paste(indicator.name,"\n",indicator.id,"\n"))

  gg <- gg + facet_wrap(~year)

  gg <- gg + coord_equal()

  gg <- gg + theme_map()

  gg <- gg + theme(panel.margin=unit(1, "lines"))
  gg <- gg + theme(plot.title=element_text(face="bold", size=14))
  gg <- gg + theme(legend.title=element_text(face="bold", hjust=0, size=9))
  gg <- gg + theme(legend.text=element_text(size=10))
  gg <- gg + theme(strip.text=element_text(face="bold", size=10))
  gg <- gg + theme(strip.background=element_blank())
  gg <- gg + theme(legend.position="bottom")

  #gg
  filename <- paste("eae/", indicator.id,".png",sep="")
  print(filename)

  ggsave(filename, gg, dpi=600)
}
