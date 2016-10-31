library(XML)      # readHTMLTable
library(maptools) # readShapePoly
library(dplyr)    # awesome data manipulation
library(tidyr)    # reshaping data
library(scales)   # for 'percent'
library(stringi)  # string manipulation
library(stringr)  # string manipulation
library(ggplot2)  # plotting
library(ggthemes) # theme_map

## Download udalmap data
if ( !file.exists("Adierazleak udalerrika.html")  |
      file.size("Adierazleak udalerrika.html")==0 )
{ udalmap.url <- "http://www.ogasun.ejgv.euskadi.net/r51-t64cont/eu/t64amVisorWar/t64aservlet/t64aIndicadoresServlet?cods=48001|20001|48002|20002|20016|20003|48911|20004|20005|01001|20006|48912|20906|20007|20008|48003|48004|01002|20009|20010|20011|01049|48005|20012|01003|48006|48093|20013|01006|01037|48009|20055|48914|01008|48010|48011|48023|48008|01004|01009|20014|20903|20015|48091|48070|01010|20017|48012|20018|20904|48090|01011|48013|48014|01013|48015|20019|48092|20020|20021|48016|01014|20022|48017|01016|48018|48019|20023|20024|48020|48021|01017|20029|48901|48026|20069|48027|20030|48028|48031|01021|01022|20031|20033|20032|48032|01023|48902|48033|48034|20067|20066|48079|20034|48029|48030|20035|48906|48035|20038|20037|48036|48037|48038|48039|48040|48041|20907|48046|48044|20039|48047|48042|48043|48045|01056|20040|20041|20036|20042|48048|20043|48094|20044|20045|01901|20046|01027|48049|20047|48910|48050|48022|48907|01019|01020|01028|01030|01031|01032|48051|01902|01033|48052|20048|20902|01036|48053|20049|20050|20051|20052|01058|20068|48054|48057|48055|48056|01034|48081|20053|20054|48903|48058|48059|48061|48060|20901|48062|48063|48064|01039|48066|48068|48069|48007|48908|48071|20057|20056|48067|48909|01041|20063|01042|20058|48073|20059|20076|20905|20060|20061|20062|48075|48083|48072|01043|20064|01044|48077|48078|01046|01051|01047|01052|01053|20070|48084|48904|48086|48085|20065|48076|20071|48087|48088|48065|48089|48074|01054|20072|20077|20073|01055|48080|20075|01057|01059|01060|48095|20078|01061|48096|01062|48905|48097|20079|48024|48025|20025|20026|20027|48913|01018|48915|01063|20081|20080&selindics=0|1|2|3|4|5|6|7|8|9|91|10|11|12|92|50|144|105|173|174|181|182|183|192|193|194|195|196|197|198|199|200|13|14|55|169|170|15|16|93|17|18|19|20|21|56|164|106|153|165|22|24|23|57|58|59|25|26|27|60|184|185|166|61|186|187|167|62|188|189|64|190|191|63|65|94|203|204|205|206|28|29|30|31|32|33|34|35|36|37|38|39|201|202|162|40|41|42|43|44|45|176|177|46|66|47|74|48|49|179|180|178|68|69|70|71|175|86|163|108|72|73|75|77|111|126|76|67|78|79|116|117|118|119|120|121|87|129|127|80|115|88|124|125|84|104|130|81|82|83|131|132|109|110|90|89|157|134|85|135|136|137|138|139|140|51|145|146|114|148|172|159|160|161|96|97|98|99|53|168|95|112|113|141|100|101|102|158|103|152|123|122|151|149|150|142|143|52|&mode1=1&language1=1&type1=0&R01HNoPortal=true"
  download.file(udalmap.url, "Adierazleak udalerrika.html")
}

doc <- htmlParse("Adierazleak udalerrika.html")
udalmap <- readHTMLTable(doc)

indicator.names <- xpathSApply(doc,'//h3',xmlValue)
names(udalmap) <- iconv(indicator.names, "UTF-8")

for (i in 1:length(udalmap)) {
  table <- matrix(0,nr=nrow(udalmap[[i]]), nc=ncol(udalmap[[i]]))

  colnames(table) <- c("name",colnames(udalmap[[i]])[-1])

  table[,"name"] <- as.character(udalmap[[i]][,1])
  rownames(table) <-table[,"name"]

  for (j in 2:ncol(udalmap[[i]]))
  {
    tmp.col <- gsub("\\.","",udalmap[[i]][,j]) #ex: 1.000.000 -> 1000000
    tmp.col <- gsub(",",".",tmp.col) #ex: 0,53 -> 0.53
    table[,j] <- as.numeric(as.character(tmp.col))
  }

  udalmap[[i]] <- table
}

## Download shape file and unzip
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

# rownames(udalmap[[1]]) %in% map.names
map.names[336] <- "Salvatierra/Agurain"
map.names[460] <- "Sopela"
map.names[587] <- "Arratzua-Ubarrundia"
map.names[666] <- "Urduliz"
map.names[682] <- "Donostia/San Sebastián"

udalerriak.shape@data$name <- map.names

udalerriak_eae <- rownames(udalmap[[i]])
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

col_perc <- sort(c(grep("%",names(udalmap)),grep("‰",names(udalmap))))
col_eur <- grep("€",names(udalmap))
col_ind <- grep("indizea",names(udalmap))
col_m2 <- grep("azalera",names(udalmap))

if (!dir.exists("eae"))
  dir.create("eae")

for (i in 1:length(udalmap))
{ ## Plot each indicators colour map (by years) to out dir

#i <- 128 #TODO:Error because of NA columns
if (i!=128)
{

  ## Get indicator id and name
  indicator.id <- strsplit(names(udalmap)[[i]],":")[[1]][1]
  indicator.name <- strsplit(names(udalmap)[[i]],":")[[1]][2]

  print(paste(i,indicator.id))
  print(indicator.name)

  if ( i %in% col_perc)
  { as.data.frame(udalmap[[i]][,!is.na(colnames(udalmap[[i]]))]) %>%
      gather(year, value, starts_with("2")) %>%
      mutate(value=as.numeric(value)) %>%
      mutate(`%`=cut(value,
        c(0, 2.5, 5, 10, 25, 50, 75, 80, 100),
        breaks_labels))-> indicator_dat

      colour_pal.tmp <- colour_pal[breaks_labels %in% indicator_dat$'%']

  } else if (i %in% col_ind)
  { as.data.frame(udalmap[[i]]) %>%
      gather(year, value, starts_with("2")) %>%
      mutate(value=as.numeric(value)) -> indicator_dat

      cont_scale_breaks <- round(c(0:4,1/0),2)

      sprintf("%s-%s", cont_scale_breaks, lead(cont_scale_breaks)) %>%
        head(-1) -> value_breaks_labels


      indicator_dat %>% mutate(Indizea=cut(value,
        cont_scale_breaks,
        value_breaks_labels))-> indicator_dat

      colour_pal.tmp <- colour_pal[4:8]

  } else if (i %in% col_m2)
  { as.data.frame(udalmap[[i]]) %>%
      gather(year, value, starts_with("2")) %>%
      mutate(value=as.numeric(value)) -> indicator_dat

      m2_scale_breaks <- unique(round(fivenum(indicator_dat$value,na.rm=T),2))

      sprintf("%s-%s", m2_scale_breaks, lead(m2_scale_breaks)) %>%
        head(-1) -> m2_breaks_labels


      indicator_dat %>% mutate(`m²`=cut(value,
        m2_scale_breaks,
        m2_breaks_labels))-> indicator_dat

      colour_pal.tmp <- colour_pal[5:(4+length(m2_breaks_labels))]

  } else
  { as.data.frame(udalmap[[i]]) %>%
      gather(year, value, starts_with("2")) %>%
      mutate(value=as.numeric(value)) -> indicator_dat

      cont_scale_breaks <- unique(round(fivenum(indicator_dat$value,na.rm=T),2))

      sprintf("%s-%s", cont_scale_breaks, lead(cont_scale_breaks)) %>%
        head(-1) -> value_breaks_labels

      colour_pal.tmp <- colour_pal[5:(4+length(value_breaks_labels))]

      if (i %in% col_eur)
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


  if ( i %in% col_perc)
  { gg <- gg + geom_map(data=indicator_dat, map=udalerriak_eae.map,
                    aes(map_id=name, fill=`%`),
                    color="#7f7f7f", size=0.15)
  } else if (i %in% col_eur)
  { gg <- gg + geom_map(data=indicator_dat, map=udalerriak_eae.map,
                    aes(map_id=name, fill=`€`),
                    color="#7f7f7f", size=0.15)
  } else if (i %in% col_ind)
  { gg <- gg + geom_map(data=indicator_dat, map=udalerriak_eae.map,
                    aes(map_id=name, fill=Indizea),
                    color="#7f7f7f", size=0.15)
  } else if (i %in% col_m2)
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
}
