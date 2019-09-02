# Packages Used
library(raster)
library(rgdal)
library(RColorBrewer)
library(elevatr)

# Experimental ways that probably need to be deleted

dpath<-"C:/Users/ag236526/Box Sync/sd_dat/DEM/South Dakota DEM/42096/42096/d42096d4/dblbnd.adf"
x <- new("GDALReadOnlyDataset", dpath)
getDriver(x)
getDriverLongName(getDriver(x))


xx<-asSGDF_GROD(x, output.dim=c(200, 200))
spplot(xx, "band1", 
       at=c(0, 10, 50, 100, 200, 500, 1000, 2500, max(xx$band1,
                                                      na.rm=TRUE)),
       col.regions=brewer.pal(8,"Oranges") )


mt_wash <- data.frame(x = -71.3036, y = 44.2700)
mt_mans <- data.frame(x = -72.8145, y = 44.5438)
mts <- rbind(mt_wash,mt_mans)

mts_sp <- sp::SpatialPoints(sp::coordinates(mts),
                            proj4string = sp::CRS(ll_prj))
get_elev_point(locations = mt_wash, prj = ll_prj)
get_elev_point(locations = mt_wash, units="feet", prj = ll_prj
               
# sd tri     
# SD shape file
               
sd_shp <- readOGR("C:/Users/ag236526/Box Sync/sd_dat/DAUDissolve", "Dissolve")
               
#subsetting the DAU to West River and East River
               
wr <- subset(sd_shp, DAU %in% c(1, 2, 3, 4, 5, 7))
er <- subset(sd_shp, DAU %in% c(6, 8, 9, 10, 11))
               
 # Setting the Correct Projection
               
ll_prj <- projection(sd_shp)              
 
# finding the extent of both west river and east river

 wr_ext <- t(as.matrix(extent(wr))) %>%
              data.frame() %>%
              tidyr::expand(x, y) %>%
              data.frame
 er_ext <- t(as.matrix(extent(er))) %>%
              data.frame() %>%
              tidyr::expand(x, y) %>%
              data.frame
               
 # sd <- data.frame(x = c(88142.83, 88142.83, 709728.3, 709728.3),
 #                  y = c(4705901, 5100080, 4705901, 5100080))

 # Downdloading the DEM
 
wr_dem <- get_elev_raster(locations = wr_ext, z=10, prj = ll_prj);gc()
er_dem <- get_elev_raster(locations = er_ext, z=10, prj = ll_prj);gc()
               
# The Clean map of wr and er

wr_elev <- purrr::map(c(1,2,3,4,5,7), 
                  ~ subset(sd_shp, DAU == .x) %>% mask(wr_dem, .) %>% trim()
               )
# save(wr_elev, file = "C:/Users/ag236526/Documents/temp_gis_sd/wr_elev.RData")
# save(er_elev, file = "C:/Users/ag236526/Documents/temp_gis_sd/er_elev.RData") 
               
er_elev <- purrr::map(c(6, 8, 9, 10, 11), 
                ~ subset(sd_shp, DAU == .x) %>% mask(er_dem, .) %>% trim()
               )
#save(er_elev, file = )

# WR and ER TRI               
wr_tri <- purrr::map(wr_elev, ~ terrain(.x, opt = "TRI"))
               
er_tri <- purrr::map(er_elev, ~ terrain(.x, opt = "TRI"))
               
plot(wr_tri[[1]])
plot(er_tri[[1]])
               
# save(wr_tri, file = "C:/Users/ag236526/Documents/temp_gis_sd/wr_tri.RData")
# save(er_tri, file = "C:/Users/ag236526/Documents/temp_gis_sd/er_tri.RData") 

#experimenting with a file               
tmp_wr <- wr_tri
 # one wy that you suggested to change the resolution but I could not get it to work
res(tmp_wr) <- 250 
wr_tri_250<- resample(wr_tri, tmp_wr, method = "bilinear")

#I got this way to work but it can only do one layer at a time and since the 
#wr_tri have mutiple layers it fails
tmp_wr <- aggregate(wr_tri[[1]], fact=5)

# This is some code that I found that I was going to try and custimize the 
#color bands for my stratas. The following are the breaks for the tri levels 
#that I am currently thinking about red = >2.5, Yellow 2.4 - 1.5, Green 1.4<.

breakpoints <- c(94,100,120,140,160,180,195)
colors <- c("red","white","white","white","white","blue")
plot(volcanoR,breaks=breakpoints,col=colors)
               