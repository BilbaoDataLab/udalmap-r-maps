library(XML)      # readHTMLTable
library(rgeos)
library(rgdal) # readShapePoly
library(dplyr)    # awesome data manipulation
library(tidyr)    # reshaping data
library(scales)   # for 'percent'
library(stringi)  # string manipulation
library(stringr)  # string manipulation
library(ggplot2)  # plotting
library(ggthemes) # theme_map

## Parameters

adierazleak <- read.csv("../data/adierazleak.csv")
adids <- adierazleak[,1]

eskualdeak.dat <- read.csv("../data/eskualdeka/1.csv") %>%
  mutate(adierazlea = as.character(adierazleak[1,2]))

for (adid in adids)
{ if (file.exists(paste0("../data/eskualdeka/",adid,".csv")))
  { print(paste(adid, adierazleak[adierazleak$id==adid,2]))

  eskualdeak.dat <- read.csv(paste0("../data/eskualdeka/",adid,".csv")) %>%
    #as.data.frame() %>%
    mutate(adierazlea = as.character(adierazleak[adierazleak$id==adid,2] )) %>%
    bind_rows(eskualdeak.dat)
  }
}

## Download shape file and unzip from Euskalgeo
if (!file.exists("../data/shp/eskualdeak/Eskualdeak.shp"))
{ eskualdeak.url <- "http://euskalgeo.net/sites/euskalgeo.net/files/fitxategi-eranskin/Eskualdeak_0.zip"
  if (!dir.exists("data/shp"))
    dir.create("../data/shp")
  download.file(eskualdeak.url, "../data/shp/eskualdeak.zip")
  unzip("../data/shp/eskualdeak.zip", exdir="../data/shp/eskualdeak")
}

eskualdeak.shp <- readOGR("../data/shp/eskualdeak/Eskualdeak.shp")
udalerriak.shp <- readOGR("../data/shp/udalerriak/udalerriak.shp")

## Change encoding from Windows-1252 to UTF-8
for ( i in 1:ncol(eskualdeak.shp@data))
  eskualdeak.shp@data[,i] <- iconv(eskualdeak.shp@data[,i], "Windows-1252", "UTF-8")

# Fix codes in araba (add starting with 0)
for (i in 1:nrow(eskualdeak.dat))
  if (nchar(eskualdeak.dat[i,1])==4)
    eskualdeak.dat[i,1] <- paste0("0",eskualdeak.dat[i,1])

eskualdeak.dat$Eskualde.kodea <- NA
for (i in 1:nrow(eskualdeak.shp@data))
  if (sum(grepl(eskualdeak.shp$es_iz_e_2[i], eskualdeak.dat$Lurralde)))
  eskualdeak.dat[grepl(eskualdeak.shp$es_iz_e_2[i], eskualdeak.dat$Lurralde),]$Eskualde.kodea <- eskualdeak.shp$es_kod_2[i]

eskualdeak.dat[grepl("Markina-Ondarroa", eskualdeak.dat$Lurralde),]$Eskualde.kodea <- "E33" # Lea Artibai
eskualdeak.dat[grepl("Arratia Nerbioi", eskualdeak.dat$Lurralde),]$Eskualde.kodea <- "E07" # Arrati Nerbioi
eskualdeak.dat[grepl("Deba Beherea", eskualdeak.dat$Lurralde),]$Eskualde.kodea <- "E13" # Debabarrena

eskualdeak_eae.shp <- eskualdeak.shp[eskualdeak.shp$es_kod_2 %in% eskualdeak.dat$Eskualde.kodea,]
rownames(eskualdeak_eae.shp@data) <- eskualdeak_eae.shp$es_iz_e_2
eskualdeak_eae.shp$id <- eskualdeak_eae.shp$es_kod_2

eskualdeak_eae.map <- fortify(eskualdeak_eae.shp, region="id")

## Plot

## Breaks for percentages
per_scale_breaks <- c(0, 2.5, 5, 10, 25, 50, 75, 80, 100)

sprintf("%2.1f-%s", per_scale_breaks, percent(lead(per_scale_breaks/100))) %>%
  stri_replace_all_regex(c("^0.0", "-NA%"), c("0", "%"), vectorize_all=FALSE) %>%
  head(-1) -> breaks_labels

# Colour pallet
colour_pal <- c("#eaecd8", "#d6dab3", "#c2c98b", "#949D48", "#6e7537", "#494E24", "#BB792A", "#7C441C")

if (!dir.exists("../out/eskualdeka"))
  dir.create("../out/eskualdeka")

indicators <- unique(eskualdeak.dat$adierazlea)

for (indicator in indicators)
{ ## Plot each indicators colour map (by years) to out dir

  ## Get indicator id and name
  indicator.id <- adierazleak %>% filter(izena==indicator) %>% select(id) %>% as.numeric()
  indicator.name <- str_trim(strsplit(indicator,"\\(")[[1]][1])
  indicator.subname <- ifelse(!is.na(strsplit(indicator,"\\(")[[1]][2]), str_trim(paste0("(",strsplit(indicator,"\\(")[[1]][2])),"")

  print(indicator.name)

#
if ( !(grepl(indicator.name,"Uraren industri eskaria. biztanleko eta eguneko (l/bizt/egunak)") ||
      grepl(indicator.name,"Erreferentziako ospitalerako joan-etorrien batez besteko denbora") ||
      grepl(indicator.name,"Gizarte Zerbitzuetan egindako gastu osoa biztanle bakoitzeko")
    ))
{

  df <- eskualdeak.dat %>%
    filter(adierazlea==indicator)

  if ( grepl("%", indicator) | grepl("‰", indicator))
  {
    df %>%
      as.data.frame() %>%
      gather(year, value, starts_with("X")) %>%
      mutate(year=gsub("X","",year)) %>%
      mutate(value=as.numeric(value)) %>%
      filter(!is.na(value)) %>%
      mutate(`%`=cut(value,
        c(0, 2.5, 5, 10, 25, 50, 75, 80, 100),
        breaks_labels, right=F))-> indicator_dat

      colour_pal.tmp <- colour_pal[breaks_labels %in% indicator_dat$'%']

  } else if (grepl("Índice", indicator) |
             grepl("indizea", indicator) |
             grepl("index", indicator) )
  {
    df %>%
      gather(year, value, starts_with("X")) %>%
      mutate(year=gsub("X","",year)) %>%
      mutate(value=as.numeric(value)) %>%
      filter(!is.na(value)) -> indicator_dat

      cont_scale_breaks <- round(c(0:4,1/0),2)

      sprintf("%s-%s", cont_scale_breaks, lead(cont_scale_breaks)) %>%
        head(-1) -> value_breaks_labels


      indicator_dat %>% mutate(Indizea=cut(value,
        cont_scale_breaks,
        value_breaks_labels))-> indicator_dat

      colour_pal.tmp <- colour_pal[4:8]

  } else if (grepl("Superficie", indicator) |
             grepl("azalera", indicator) |
             grepl("area", indicator) )
  {
     df %>%
      gather(year, value, starts_with("X")) %>%
      mutate(year=gsub("X","",year)) %>%
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
      gather(year, value, starts_with("X")) %>%
      mutate(year=gsub("X","",year)) %>%
      mutate(value=as.numeric(value)) %>%
      filter(!is.na(value)) -> indicator_dat

      cont_scale_breaks <- unique(round(fivenum(indicator_dat$value,na.rm=T),2))

      sprintf("%s-%s", cont_scale_breaks, lead(cont_scale_breaks)) %>%
        head(-1) -> value_breaks_labels

      colour_pal.tmp <- colour_pal[5:(4+length(value_breaks_labels))]

      if (grepl("€", indicator))
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

  gg <- gg + geom_map(data=eskualdeak_eae.map, map=eskualdeak_eae.map,
                    aes(x=long, y=lat, group=group, map_id=id),
                    color="#7f7f7f", fill="white", size=0.15)


  if ( grepl("%", indicator) | grepl("‰", indicator) )
  { gg <- gg + geom_map(data=indicator_dat, map=eskualdeak_eae.map,
                    aes(map_id=Eskualde.kodea, fill=`%`),
                    color="#7f7f7f", size=0.15)
  } else if (grepl("€", indicator))
  { gg <- gg + geom_map(data=indicator_dat, map=eskualdeak_eae.map,
                    aes(map_id=Eskualde.kodea, fill=`€`),
                    color="#7f7f7f", size=0.15)
  } else if (grepl("Índice", indicator) |
             grepl("indizea", indicator) |
             grepl("index", indicator) )
  { gg <- gg + geom_map(data=indicator_dat, map=eskualdeak_eae.map,
                    aes(map_id=Eskualde.kodea, fill=Indizea),
                    color="#7f7f7f", size=0.15)
  } else if (grepl("Superficie", indicator) |
             grepl("azalera", indicator) |
             grepl("area", indicator) )
  { gg <- gg + geom_map(data=indicator_dat, map=eskualdeak_eae.map,
                    aes(map_id=Eskualde.kodea, fill=`m²`),
                    color="#7f7f7f", size=0.15)
  } else
  { gg <- gg + geom_map(data=indicator_dat, map=eskualdeak_eae.map,
                    aes(map_id=Eskualde.kodea, fill=Kopurua),
                    color="#7f7f7f", size=0.15)
  }

  gg <- gg + scale_fill_manual(values=colour_pal.tmp)

  gg <- gg + guides(fill=guide_legend(override.aes=list(colour=NA)))

  gg <- gg + labs(title=indicator.name, subtitle=indicator.subname)
  gg <- gg + labs(caption = "Iturria: OpenDataEuskadi. Egilea(k): BilbaoDataLab")

  gg <- gg + facet_wrap(~year)

  gg <- gg + coord_equal()

  gg <- gg + theme_map()

  #gg <- gg + theme(panel.margin=unit(1, "lines"))
  gg <- gg + theme(plot.title=element_text(face="bold", size=13, hjust = 0.5))
  gg <- gg + theme(plot.subtitle=element_text(size=11, hjust = 0.5))
  gg <- gg + theme(legend.title=element_text(face="bold", hjust=0, size=9))
  gg <- gg + theme(legend.text=element_text(size=10))
  gg <- gg + theme(strip.text=element_text(face="bold", size=10))
  gg <- gg + theme(strip.background=element_blank())
  gg <- gg + theme(legend.position="bottom")
  gg <- gg + theme(legend.justification="center")

  #gg
  filename <- paste("../out/eskualdeka/eskualdeka-", indicator.id,".png",sep="")
  print(filename)

  ggsave(filename, gg, dpi=600)
}
}
