library(terra)

dem <- rast("data/dem_aragon.tif")

pendiente_cat <- clase_pendiente(dem)

plot(pendiente_cat)

levels(pendiente_cat)
