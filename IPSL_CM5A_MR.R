library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(ggplot2) # package for plottingin
library(rasterVis)


historical <- nc_open('wam_IPSL_CM5A_MR_historical_r3i1p1_swh_yearmean.nc')
{
  sink('wam_IPSL_CM5A_MR_historical_r3i1p1_swh_yearmean.txt')
  print(historical)
  sink()
}

lon_historical <- ncvar_get(historical, "longitude")
lat_historical <- ncvar_get(historical, "latitude", verbose = F)
t <- ncvar_get(historical, "time")

array.historical <- ncvar_get(historical, "significant_wave_height") # store the data in a 3-dimensional array
dim(array.historical)

fillvalue_historical <- ncatt_get(historical, "significant_wave_height", "_FillValue")
fillvalue_historical

nc_close(historical) 


array.historical[array.historical == fillvalue_historical$value] <- NA

slice_historical <- array.historical[, , 1]

lonrowshistorical <- apply(lon_historical, 1, function(row) any(row >= -5 & row <= 10))
loncolshistorical <- apply(lon_historical, 2, function(col) any(col >= 4 & col <= 5))

latrowshistorical <- apply(lat_historical, 1, function(row) any(row >= 50 & row <= 51))
latcolshistorical <- apply(lat_historical, 2, function(col) any(col >= 20 & col <= 50))

filtered_lon_historical <- lon_historical[lonrowshistorical, loncolshistorical]
filtered_lat_historical <- lat_historical[latrowshistorical, latcolshistorical]

filtered_historical <- slice_historical[lonrowshistorical, latcolshistorical]

r <- raster(t(filtered_historical), xmn=min(filtered_lon_historical), 
            xmx=max(filtered_lon_historical), ymn=min(filtered_lat_historical),
            ymx=max(filtered_lat_historical), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
dim(slice_historical)

r <- flip(r, direction='y')

plot(r)

r_brick <- brick(array.historical, xmn=min(lat_historical), xmx=max(lat_historical),
                 ymn=min(lon_historical), ymx=max(lon_historical), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_brick <- flip(t(r_brick), direction='y')

zeebrugge_lon <- 3.186747
zeebrugge_lat <- 51.328358
zeebrugge_series <- extract(r_brick, SpatialPoints(cbind(zeebrugge_lon,zeebrugge_lat)), method='simple')
zeebrugge_series <- zeebrugge_series[1:29]

zeebrugge_df_IPSL_CM5A_MR<- data.frame(year= seq(from=1971, to=1999, by=1), SWH=zeebrugge_series)
ggplot(data=zeebrugge_df_IPSL_CM5A_MR, aes(x=year, y=SWH, group=1)) +
  geom_line(color = "black") + # make this a line plot
  ggtitle("Significant Waveheight Historical") +     # Set title
  theme_bw()+
  ylim(2, 5) + ylab('Significant waveheight (m)') + 
  geom_smooth(method = lm, color = 'black')





#rcp45
rcp45 <- nc_open('wam_IPSL_CM5A_MR_rcp45_r1i1p1_swh_yearmean.nc')
{
  sink('wam_IPSL_CM5A_MR_rcp45_r1i1p1_swh_yearmean.txt')
  print(rcp45)
  sink()
}

lon_rcp45 <- ncvar_get(rcp45, "longitude")
lat_rcp45 <- ncvar_get(rcp45, "latitude", verbose = F)
t <- ncvar_get(rcp45, "time")

array.rcp45 <- ncvar_get(rcp45, "significant_wave_height") # store the data in a 3-dimensional array
dim(array.rcp45)

fillvalue_rcp45 <- ncatt_get(rcp45, "significant_wave_height", "_FillValue")
fillvalue_rcp45

nc_close(rcp45) 


array.rcp45[array.rcp45 == fillvalue_rcp45$value] <- NA

slice_rcp45 <- array.rcp45[, , 1]

lonrowsrcp45 <- apply(lon_rcp45, 1, function(row) any(row >= -5 & row <= 10))
loncolsrcp45 <- apply(lon_rcp45, 2, function(col) any(col >= 4 & col <= 5))

latrowsrcp45 <- apply(lat_rcp45, 1, function(row) any(row >= 50 & row <= 51))
latcolsrcp45 <- apply(lat_rcp45, 2, function(col) any(col >= 20 & col <= 50))

filtered_lon_rcp45 <- lon_rcp45[lonrowsrcp45, loncolsrcp45]
filtered_lat_rcp45 <- lat_rcp45[latrowsrcp45, latcolsrcp45]

filtered_rcp45 <- slice_rcp45[lonrowsrcp45, latcolsrcp45]

rcp45 <- raster(t(filtered_rcp45), xmn=min(filtered_lon_rcp45), 
                xmx=max(filtered_lon_rcp45), ymn=min(filtered_lat_rcp45),
                ymx=max(filtered_lat_rcp45), 
                crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
dim(slice_rcp45)

rcp45 <- flip(rcp45, direction='y')

plot(rcp45)

r_brick_rcp45 <- brick(array.rcp45, xmn=min(lat_rcp45), xmx=max(lat_rcp45),
                       ymn=min(lon_rcp45), ymx=max(lon_rcp45), 
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_brick_rcp45 <- flip(t(r_brick_rcp45), direction='y')

zeebrugge_lon_rcp45 <- 3.186747
zeebrugge_lat_rcp45 <- 51.328358



zeebrugge_series_rcp45 <- extract(r_brick_rcp45, SpatialPoints(cbind(zeebrugge_lon_rcp45,zeebrugge_lat_rcp45)),
                                  method='simple')

zeebrugge_rcp45_IPSL_CM5A_MR_df <- data.frame(year= seq(from=2071, to=2100, by=1), SWH_rcp45=t(zeebrugge_series_rcp45))
ggplot(data=zeebrugge_rcp45_IPSL_CM5A_MR_df, aes(x=year, y=SWH_rcp45, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("SWH rcp45") +     # Set title
  theme_bw()+
  ylim(2, 5)



#rcp85
rcp85 <- nc_open('wam_IPSL_CM5A_MR_rcp85_r1i1p1_swh_yearmean.nc')
{
  sink('wam_IPSL_CM5A_MR_rcp85_r1i1p1_swh_yearmean.txt')
  print(rcp85)
  sink()
}

lon_rcp85 <- ncvar_get(rcp85, "longitude")
lat_rcp85 <- ncvar_get(rcp85, "latitude", verbose = F)
t <- ncvar_get(rcp85, "time")

array.rcp85 <- ncvar_get(rcp85, "significant_wave_height") # store the data in a 3-dimensional array
dim(array.rcp85)

fillvalue_rcp85 <- ncatt_get(rcp85, "significant_wave_height", "_FillValue")
fillvalue_rcp85

nc_close(rcp85) 


array.rcp85[array.rcp85 == fillvalue_rcp85$value] <- NA

slice_rcp85 <- array.rcp85[, , 1]

lonrowsrcp85 <- apply(lon_rcp85, 1, function(row) any(row >= -5 & row <= 10))
loncolsrcp85 <- apply(lon_rcp85, 2, function(col) any(col >= 4 & col <= 5))

latrowsrcp85 <- apply(lat_rcp85, 1, function(row) any(row >= 50 & row <= 51))
latcolsrcp85 <- apply(lat_rcp85, 2, function(col) any(col >= 20 & col <= 50))

filtered_lon_rcp85 <- lon_rcp85[lonrowsrcp85, loncolsrcp85]
filtered_lat_rcp85 <- lat_rcp85[latrowsrcp85, latcolsrcp85]

filtered_rcp85 <- slice_rcp85[lonrowsrcp85, latcolsrcp85]

rcp85 <- raster(t(filtered_rcp85), xmn=min(filtered_lon_rcp85), 
                xmx=max(filtered_lon_rcp85), ymn=min(filtered_lat_rcp85),
                ymx=max(filtered_lat_rcp85), 
                crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
dim(slice_rcp85)

rcp85 <- flip(rcp85, direction='y')

plot(rcp85)

r_brick_rcp85 <- brick(array.rcp85, xmn=min(lat_rcp85), xmx=max(lat_rcp85),
                       ymn=min(lon_rcp85), ymx=max(lon_rcp85), 
                       crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_brick_rcp85 <- flip(t(r_brick_rcp85), direction='y')

zeebrugge_lon_rcp85 <- 3.186747
zeebrugge_lat_rcp85 <- 51.328358


zeebrugge_series_rcp85 <- extract(r_brick_rcp85, SpatialPoints(cbind(zeebrugge_lon_rcp85,zeebrugge_lat_rcp85)),
                                  method='simple')


colors <- c("RCP-4.5" = "red", "RCP-8.5" = "blue")




zeebrugge_rcp85_IPSL_CM5A_MR_df <- data.frame(year= seq(from=2071, to=2100, by=1), SWH_rcp85=t(zeebrugge_series_rcp85))
ggplot(data=zeebrugge_rcp85_IPSL_CM5A_MR_df, aes(x=year, y=SWH_rcp85, group=1)) +
  geom_line(aes(color = 'RCP-8.5'))+   # Set title
  theme_bw()+
  ylim(2, 5)+ylab('Significant waveheight (m)') + 
  geom_line(data = zeebrugge_rcp45_IPSL_CM5A_MR_df, aes(y = SWH_rcp45, color = 'RCP-4.5'))+
  geom_smooth(method=lm, aes(color = 'RCP-8.5')) + 
  geom_smooth(data = zeebrugge_rcp45_IPSL_CM5A_MR_df, method = lm, aes(y = SWH_rcp45, color = 'RCP-4.5'))+
  labs(title = "Significant Waveheight RCP-4.5 and RCP-8.5",
       color = "Scenario's")+ 
  theme(legend.position = c(0.05, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))



