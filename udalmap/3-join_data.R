## Identify the towns that are in both objects (tables and map).
rownames(udalmap[[1]]) %in% udalerriak.shape@data$iz_ofizial

diff1 <- which(!gsub(" \\/ ","\\/",rownames(udalmap[[1]])) %in% udalerriak.shape@data$iz_ofizial)
rownames(udalmap[[1]])[diff1]

## Remove additional space around the slash.
map.names <- gsub(" \\/ ","\\/",udalerriak.shape@data$iz_ofizial)

diff2 <- which(!gsub(" \\/ ","\\/",rownames(udalmap[[1]])) %in% map.names)
rownames(udalmap[[1]])[diff2]

## Rename the rest of the names.
map.names[336] <- "Salvatierra/Agurain"
map.names[460] <- "Sopela"
map.names[587] <- "Arratzua-Ubarrundia"
map.names[666] <- "Urduliz"
map.names[682] <- "Donostia/San Sebastián"

rownames(udalmap[[1]]) %in% map.names

## Add a new variable with town names to the map data.
udalerriak.shape@data$name <- map.names

## Reduce map to EAE
udalerriak_eae <- rownames(udalmap[[i]])
udalerriak_eae.shape <- udalerriak.shape[udalerriak.shape$name %in% udalerriak_eae,]
rownames(udalerriak_eae.shape@data) <- udalerriak_eae.shape$name

## View EAE map
plot(udalerriak_eae.shape)
