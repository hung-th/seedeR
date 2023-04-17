run <- function(sp, ssp, time, site.x, site.y){

  ## Configuration, shouldn't change
  load(paste0("./data/", sp, "/pred.adaptive.RData"))
  #load("./data/hills.df.RData")
  #load("./data/slope.df.RData")
  load("./data/world.RData")
  
  showModal(modalDialog(
    title = "Running... Please do not close this window.",
    "Starting..."
  ))

site.x <- as.numeric(site.x)
site.y <- as.numeric(site.y)

  showModal(modalDialog(
    title = "Running...",
    "Loading the species distribution..."
  ))

sp.map <- raster(paste0("./data/", sp, "/sp.map.tif"))
NAvalue(sp.map) <- 128

pred.adaptive.future.list <- dir(paste0('./data/', sp), full.names = T, recursive = T, pattern = glob2rx("pred.adaptive.future.RData"))
pred.adaptive.future.list <- pred.adaptive.future.list[grep(ssp, pred.adaptive.future.list)]
pred.adaptive.future.list <- pred.adaptive.future.list[grep(time, pred.adaptive.future.list)]

my.rast.list <- list()

showModal(modalDialog(
  title = "Running...",
  paste0("Producing the offset layers... (0%)")
))

for (a in 1:length(pred.adaptive.future.list)){

	load(pred.adaptive.future.list[a])
	
	if(nrow(pred.adaptive) != nrow(pred.adaptive.future)){
	common <- intersect(row.names(pred.adaptive), row.names(pred.adaptive.future))
	pred.adaptive <- pred.adaptive[row.names(pred.adaptive) %in% common, ]
	pred.adaptive.future <- pred.adaptive.future[row.names(pred.adaptive.future) %in% common, ]
	}
	
	cell.id <- cellFromXY(sp.map, c(site.x, site.y))

	if(is.na(cell.id) == T){    # out of the species range
	  
	  showModal(modalDialog(
	      title = "Error!",
	      "The site is out of the species range. While this is possible, we do not support this functionality in this version of the App."
	   ))
	  
	  return(NA)
	  
	}
	
	site.future <- pred.adaptive.future[toString(cell.id), ]
	n_env <- ncol(site.future)
	genetic.offset.adaptive <- replicate(nrow(pred.adaptive.future),0)
	
	for (n in 1:n_env){
  	nth.term <- (replicate(nrow(pred.adaptive.future),site.future[[n]]) - pred.adaptive[,n])^2
  	genetic.offset.adaptive <- genetic.offset.adaptive + nth.term
	}
	genetic.offset.adaptive <- sqrt(genetic.offset.adaptive)
	names(genetic.offset.adaptive) <- row.names(pred.adaptive.future)
	
	rast.offset <- sp.map
	names(rast.offset) <- "Offset"
	values(rast.offset) <- NA
	rast.offset[as.numeric(names(genetic.offset.adaptive))] <- genetic.offset.adaptive
  
	my.rast.list[[a]] <- rast.offset
	
	progress <- a/length(pred.adaptive.future.list)*100
	
	showModal(modalDialog(
	  title = "Running...",
	  paste0("Producing the offset layers... (", progress, "%)")
	))
	
}

showModal(modalDialog(
  title = "Running...",
  "Calculating average offsets across the GCMs..."
))

rast.stack <- stack(my.rast.list)
rast.avg <- calc(rast.stack, fun = mean, na.rm = T)
#writeRaster(rast.avg, filename = paste0("tmp_output/rast.offset.tif"), format = "GTiff", overwrite = T)

mnv <- min(minValue(rast.avg))
mxv <- max(maxValue(rast.avg))
	
f <- function(i){
	x <- 1 - ((i - mnv) / (mxv - mnv))
	x
}

showModal(modalDialog(
  title = "Running...",
  "Scaling the offsets..."
))

rast.scaled <- calc(rast.avg, f)
names(rast.scaled) <- "Similarity"

rm(rast.offset)
rm(my.rast.list)
rm(rast.avg)
rm(rast.stack)
rm(pred.adaptive)
rm(pred.adaptive.future)

site <- data.frame(Longitude = site.x, Latitude = site.y)

showModal(modalDialog(
  title = "Running...",
  "Drawing the graph..."
))

p <- ggplot() +
	  #geom_raster(data=hills.df,aes(Longitude,Latitude,fill=hills,group=1),alpha=.6,interpolate=TRUE) +
	  #geom_raster(data=slope.df,aes(Longitude,Latitude,fill=slope,group=2),alpha=.2,interpolate=TRUE) +
	  scale_fill_gradientn(colors=grey.colors(100,start=0,end=.95),guide = 'none') +
	  new_scale("fill") +
	  geom_raster(data = rast.scaled, aes(x = x, y = y, fill = Similarity)) +
	  scale_fill_gradient(low = "white", high = "#006400", na.value = NA, limits=c(0, 1)) +
	  #scale_fill_viridis_c(option = "plasma", na.value = "transparent", alpha = 0.8) +
	  geom_sf(data = world, fill = NA) +
	  coord_sf(xlim = c(round(extent(sp.map)@xmin-1.5), round(extent(sp.map)@xmax+1.5)),
		ylim = c(round(extent(sp.map)@ymin-1.5), round(extent(sp.map)@ymax+1.5)), expand = FALSE) +
	  geom_point(data = site, aes(x = Longitude, y = Latitude), color = "white", size = 3, shape = 8) +
	  annotation_scale(location = "br", width_hint = 0.4) +
	  xlab("Longitude") +
	  ylab("Latitude") +
	  labs(title = paste0("ssp", ssp, " ", time)) +
	  theme(axis.text = element_text(size=12),
		axis.title = element_text(size=18),
		panel.border = element_rect(color = "black", fill = NA, size = 1),
		panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
		panel.background = element_rect(fill = "aliceblue"))

return(p)

}
