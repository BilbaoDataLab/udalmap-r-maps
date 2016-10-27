library(tidyr)    # reshaping data
library(dplyr)    # awesome data manipulation

library(scales)   # for 'percent'
library(stringi)  # string manipulation
library(stringr)  # string manipulation

library(ggplot2)  # plotting
library(ggthemes) # theme_map

## Get list of percent indicators
col_perc <- sort(c(grep("%",names(udalmap)),grep("‰",names(udalmap))))
names(udalmap)[col_perc]

## Select one indicator (e.g. 1) and get name and id.
indicator.id <- strsplit(names(udalmap)[[1]],":")[[1]][1]
indicator.name <- strsplit(names(udalmap)[[1]],":")[[1]][2]

## Define breaks for colouring.
per_scale_breaks <- c(0, 2.5, 5, 10, 25, 50, 75, 80, 100)

sprintf("%2.1f-%s", per_scale_breaks, percent(lead(per_scale_breaks/100))) %>%
  stri_replace_all_regex(c("^0.0", "-NA%"), c("0", "%"), vectorize_all=FALSE) %>%
  head(-1) -> breaks_labels

## Colour pallet
colour_pal <- c("#eaecd8", "#d6dab3", "#c2c98b", "#949D48", "#6e7537", "#494E24", "#BB792A", "#7C441C", "#ffffff")


## Process data to fit ggplot format.
udalmap[[1]] %>%
  as.data.frame() %>%
  tibble::rownames_to_column("name") %>%
  gather(year, value, starts_with("2"), -name) %>%
  mutate(value=as.numeric(value)) %>%
  mutate(`%`=cut(value,
    c(0, 2.5, 5, 10, 25, 50, 75, 80, 100),
    breaks_labels))-> indicator_dat

udalerriak.df <- fortify(udalerriak_eae.shape, region="name")


## Plot
gg <- ggplot()

gg <- gg + geom_map(data=udalerriak.df, map=udalerriak.df,
                    aes(x=long, y=lat, group=group, map_id=id),
                    color="#7f7f7f", fill="white", size=0.15)

gg <- gg + geom_map(data=indicator_dat, map=udalerriak.df,
                    aes(map_id=name, fill=`%`),
                    color="#7f7f7f", size=0.15)

gg <- gg + scale_fill_manual(values=colour_pal)

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

gg

# Save plot as png
dir.create("out",recursive=T)
filename <- paste("out/", indicator.id,".png",sep="")
ggsave(filename, gg, dpi=600)
