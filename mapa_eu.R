# install.packages("mapproj")
# install.packages("rworldmap")

library(tidyverse)
library(grid)
library(rworldmap)
library(eurostat)
library(lubridate)

# Get the world map
worldMap <- getMap()

# Member States of the European Union
reg = data.frame(ref_area=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","GB"),country=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Rep.","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom"),regija=c("Other EU","Other EU","CEE","HR","Other EU","CEE","Other EU","CEE","Other EU","Other EU","Other EU","Other EU","CEE","Other EU","Other EU","CEE","CEE","Other EU","Other EU","Other EU","CEE","Other EU","CEE","CEE","CEE","Other EU","Other EU","Other EU"))

# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME %in% reg$country)

# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

# 1. Mapa regija ####
europeanUnionTable <- data.frame(country = reg$country, value = reg$regija)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

# Plot the map
P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value), colour = "black", size = 0.1) + coord_map(xlim = c(-13, 35),  ylim = c(32, 71))
P <- P + scale_fill_discrete(name = "Regija", na.value = "grey50")
P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
P

P <- P + scale_fill_gradient(name = "Growth Rate", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")

# 2. Mapa kumulativni rast BDP-a od 2010.

pom <- get_eurostat(id="namq_10_gdp") %>% filter(na_item=="B1GQ" & s_adj=="NSA" & unit=="CLV10_MNAC") %>% select(time,geo,bdp=values) %>% arrange(time) %>% as.data.frame() %>% group_by(geo) %>% mutate(bdp_y = (bdp + lag(bdp,1)+ lag(bdp,2) + lag(bdp,3))) %>% filter(time>="2007-10-01") %>% mutate(datum = make_date(ifelse(month(time)>=10,year(time)+1,year(time)),ifelse(month(time)>=10,1,month(time)+3),1) - 1) %>% select(datum,ref_area=geo,bdp_y) %>% na.omit() %>% ungroup()
baza <- pom %>% filter(datum=="2010-12-31") %>% select(ref_area,baza=bdp_y)
delta_bdp <- left_join(pom,baza,by=c("ref_area")) %>% mutate(indeks=bdp_y/baza*100) %>% filter(datum=="2019-09-30") %>% left_join(reg,by="ref_area") %>% na.omit()

europeanUnionTable <- data.frame(country = delta_bdp$country, value = delta_bdp$indeks)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

# Plot the map
P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value), colour = "black", size = 0.1) + coord_map(xlim = c(-13, 35),  ylim = c(32, 71))
P <- P + scale_fill_gradient(name = "Kumulativni rast BDP-a od 2010.", low = "red", high = "green", na.value = "grey50")
P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
P
