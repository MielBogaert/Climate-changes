library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(ggplot2) # package for plottingin
library(rasterVis)


nc_data_2006 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2006_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2006_grid5km_v1.txt')
  print(nc_data_2006)
  sink()
}

lon_2006 <- ncvar_get(nc_data_2006, "lon")
lat_2006 <- ncvar_get(nc_data_2006, "lat", verbose = F)
t <- ncvar_get(nc_data_2006, "time")

head(lon_2006)

ndvi.array.2006 <- ncvar_get(nc_data_2006, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2006) 

fillvalue_2006 <- ncatt_get(nc_data_2006, "prAdjust", "_FillValue")

fillvalue_2006



nc_close(nc_data_2006) 


ndvi.array.2006[ndvi.array.2006 == fillvalue_2006$value] <- NA
avgData_2006 <- apply(ndvi.array.2006, c(1,2), mean) 


lonrows2006 <- apply(lon_2006, 1, function(row) any(row >= 3 & row <= 6))
loncols2006 <- apply(lon_2006, 2, function(col) any(col >= 4 & col <= 5))

latrows2006 <- apply(lat_2006, 1, function(row) any(row >= 50 & row <= 51))
latcols2006 <- apply(lat_2006, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2006 <- lon_2006[lonrows2006, loncols2006]
filtered_lat_2006 <- lat_2006[latrows2006, latcols2006]

avgData_filtered_2006 <- avgData_2006[lonrows2006, latcols2006]

avgData_filtered_2006_mm <- avgData_filtered_2006 * 60*60*24*365

r_2006 <- raster(t(avgData_filtered_2006_mm), xmn=min(filtered_lon_2006), xmx=max(filtered_lon_2006), ymn=min(filtered_lat_2006),
            ymx=max(filtered_lat_2006), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_2006 <- flip(r_2006, direction='y')
plot(r_2006, xlim = c(min(filtered_lon_2006), max(filtered_lon_2006)))+
  title('Precipitation in mm/m² 2010')



nc_data_2010 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2010_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2010_grid5km_v1.txt')
  print(nc_data_2010)
  sink()
}

lon_2010 <- ncvar_get(nc_data_2010, "lon")
lat_2010 <- ncvar_get(nc_data_2010, "lat", verbose = F)
t <- ncvar_get(nc_data_2010, "time")

head(lon_2010)

ndvi.array.2010 <- ncvar_get(nc_data_2010, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2010) 

fillvalue_2010 <- ncatt_get(nc_data_2010, "prAdjust", "_FillValue")
fillvalue_2010



nc_close(nc_data_2010) 


ndvi.array.2010[ndvi.array.2010 == fillvalue$value] <- NA
avgData_2010 <- apply(ndvi.array.2010, c(1,2), mean) 


lonrows2010 <- apply(lon_2010, 1, function(row) any(row >= 3 & row <= 6))
loncols2010 <- apply(lon_2010, 2, function(col) any(col >= 4 & col <= 5))

latrows2010 <- apply(lat_2010, 1, function(row) any(row >= 50 & row <= 51))
latcols2010 <- apply(lat_2010, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2010 <- lon_2010[lonrows2010, loncols2010]
filtered_lat_2010 <- lat_2010[latrows2010, latcols2010]

avgData_filtered_2010 <- avgData_2010[lonrows2010, latcols2010]

r <- raster(t(avgData_filtered_2010), xmn=min(filtered_lon_2010), xmx=max(filtered_lon_2010), ymn=min(filtered_lat_2010),
            ymx=max(filtered_lat_2010), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r <- flip(r, direction='y')
plot(r)


nc_data <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2100_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2100_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(lon)

ndvi.array <- ncvar_get(nc_data, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array) 

fillvalue <- ncatt_get(nc_data, "prAdjust", "_FillValue")
fillvalue



nc_close(nc_data) 


ndvi.array[ndvi.array == fillvalue$value] <- NA

lonrows <- apply(lon, 1, function(row) any(row >= 3 & row <= 6))
loncols <- apply(lon, 2, function(col) any(col >= 4 & col <= 5))

latrows <- apply(lat, 1, function(row) any(row >= 50 & row <= 51))
latcols <- apply(lat, 2, function(col) any(col >= 45 & col <= 46))

avgData <- apply(ndvi.array, c(1,2), mean)
avgData_filtered <- avgData[lonrows, latcols]




filtered_lon <- lon[lonrows, loncols]
filtered_lat<- lat[latrows, latcols]
avgData_filtered <- avgData[lonrows, latcols]
avgData_filtered_mm <- avgData_filtered*60*60*24*365

r_2100 <- raster(t(avgData_filtered_mm), xmn=min(filtered_lon), xmx=max(filtered_lon),
                 ymn=min(filtered_lat),
            ymx=max(filtered_lat),
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))


r_2100 <- flip(r_2100, direction='y')
plot(r_2100) + title('Precipitation in mm/m² 2100')
plot(r)


difference <- avgData_filtered - avgData_filtered_2010

r_diff <- raster(t(difference), xmn=min(filtered_lon), xmx=max(filtered_lon),
                  ymn=min(filtered_lat),
                  ymx=max(filtered_lat),
                  crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_diff <- flip(r_diff, direction='y')

plot(r_diff)





nc_data_2010 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2010_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2010_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2010 <- ncvar_get(nc_data_2010, "lon")
lat_2010 <- ncvar_get(nc_data_2010, "lat", verbose = F)
t <- ncvar_get(nc_data_2010, "time")

head(lon_2010)

ndvi.array.2010 <- ncvar_get(nc_data_2010, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2010) 

fillvalue_2010 <- ncatt_get(nc_data_2010, "prAdjust", "_FillValue")
fillvalue_2010



nc_close(nc_data_2010) 


ndvi.array.2010[ndvi.array == fillvalue$value] <- NA
avgData_2010 <- apply(ndvi.array.2010, c(1,2), mean) 


lonrows2010 <- apply(lon_2010, 1, function(row) any(row >= 3 & row <= 6))
loncols2010 <- apply(lon_2010, 2, function(col) any(col >= 4 & col <= 5))

latrows2010 <- apply(lat_2010, 1, function(row) any(row >= 50 & row <= 51))
latcols2010 <- apply(lat_2010, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2010 <- lon[lonrows2010, loncols2010]
filtered_lat_2010 <- lat[latrows2010, latcols2010]

avgData_filtered_2010 <- avgData_2010[lonrows2010, latcols2010]



nc_data_2020 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2020_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2020_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2020 <- ncvar_get(nc_data_2020, "lon")
lat_2020 <- ncvar_get(nc_data_2020, "lat", verbose = F)
t <- ncvar_get(nc_data_2020, "time")

head(lon_2020)

ndvi.array.2020 <- ncvar_get(nc_data_2020, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2020) 

fillvalue_2020 <- ncatt_get(nc_data_2020, "prAdjust", "_FillValue")
fillvalue_2020



nc_close(nc_data_2020) 


ndvi.array.2020[ndvi.array == fillvalue$value] <- NA
avgData_2020 <- apply(ndvi.array.2020, c(1,2), mean) 


lonrows2020 <- apply(lon_2020, 1, function(row) any(row >= 3 & row <= 6))
loncols2020 <- apply(lon_2020, 2, function(col) any(col >= 4 & col <= 5))

latrows2020 <- apply(lat_2020, 1, function(row) any(row >= 50 & row <= 51))
latcols2020 <- apply(lat_2020, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2020 <- lon[lonrows2020, loncols2020]
filtered_lat_2020 <- lat[latrows2020, latcols2020]

avgData_filtered_2020 <- avgData_2020[lonrows2020, latcols2020]





# 2030
nc_data_2030 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2030_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2030_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2030 <- ncvar_get(nc_data_2030, "lon")
lat_2030 <- ncvar_get(nc_data_2030, "lat", verbose = F)
t <- ncvar_get(nc_data_2030, "time")

head(lon_2030)

ndvi.array.2030 <- ncvar_get(nc_data_2030, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2030) 

fillvalue_2030 <- ncatt_get(nc_data_2030, "prAdjust", "_FillValue")
fillvalue_2030



nc_close(nc_data_2030) 


ndvi.array.2030[ndvi.array == fillvalue$value] <- NA
avgData_2030 <- apply(ndvi.array.2030, c(1,2), mean) 


lonrows2030 <- apply(lon_2030, 1, function(row) any(row >= 3 & row <= 6))
loncols2030 <- apply(lon_2030, 2, function(col) any(col >= 4 & col <= 5))

latrows2030 <- apply(lat_2030, 1, function(row) any(row >= 50 & row <= 51))
latcols2030 <- apply(lat_2030, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2030 <- lon[lonrows2030, loncols2030]
filtered_lat_2030 <- lat[latrows2030, latcols2030]

avgData_filtered_2030 <- avgData_2030[lonrows2030, latcols2030]



#2040
nc_data_2050 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2050_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2050_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2050 <- ncvar_get(nc_data_2050, "lon")
lat_2050 <- ncvar_get(nc_data_2050, "lat", verbose = F)
t <- ncvar_get(nc_data_2050, "time")

head(lon_2050)

ndvi.array.2050 <- ncvar_get(nc_data_2050, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2050) 

fillvalue_2050 <- ncatt_get(nc_data_2050, "prAdjust", "_FillValue")
fillvalue_2050



nc_close(nc_data_2050) 


ndvi.array.2050[ndvi.array == fillvalue$value] <- NA
avgData_2050 <- apply(ndvi.array.2050, c(1,2), mean) 


lonrows2050 <- apply(lon_2050, 1, function(row) any(row >= 3 & row <= 6))
loncols2050 <- apply(lon_2050, 2, function(col) any(col >= 4 & col <= 5))

latrows2050 <- apply(lat_2050, 1, function(row) any(row >= 50 & row <= 51))
latcols2050 <- apply(lat_2050, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2050 <- lon[lonrows2050, loncols2050]
filtered_lat_2050 <- lat[latrows2050, latcols2050]

avgData_filtered_2050 <- avgData_2050[lonrows2050, latcols2050]



#2060

nc_data_2060 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2060_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2060_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2060 <- ncvar_get(nc_data_2060, "lon")
lat_2060 <- ncvar_get(nc_data_2060, "lat", verbose = F)
t <- ncvar_get(nc_data_2060, "time")

head(lon_2060)

ndvi.array.2060 <- ncvar_get(nc_data_2060, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2060) 

fillvalue_2060 <- ncatt_get(nc_data_2060, "prAdjust", "_FillValue")
fillvalue_2060



nc_close(nc_data_2060) 


ndvi.array.2060[ndvi.array == fillvalue$value] <- NA
avgData_2060 <- apply(ndvi.array.2060, c(1,2), mean) 


lonrows2060 <- apply(lon_2060, 1, function(row) any(row >= 3 & row <= 6))
loncols2060 <- apply(lon_2060, 2, function(col) any(col >= 4 & col <= 5))

latrows2060 <- apply(lat_2060, 1, function(row) any(row >= 50 & row <= 51))
latcols2060 <- apply(lat_2060, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2060 <- lon[lonrows2060, loncols2060]
filtered_lat_2060 <- lat[latrows2060, latcols2060]

avgData_filtered_2060 <- avgData_2060[lonrows2060, latcols2060]


#2070
nc_data_2070 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2070_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2070_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2070 <- ncvar_get(nc_data_2070, "lon")
lat_2070 <- ncvar_get(nc_data_2070, "lat", verbose = F)
t <- ncvar_get(nc_data_2070, "time")

head(lon_2070)

ndvi.array.2070 <- ncvar_get(nc_data_2070, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2070) 

fillvalue_2070 <- ncatt_get(nc_data_2070, "prAdjust", "_FillValue")
fillvalue_2070



nc_close(nc_data_2070) 


ndvi.array.2070[ndvi.array == fillvalue$value] <- NA
avgData_2070 <- apply(ndvi.array.2070, c(1,2), mean) 


lonrows2070 <- apply(lon_2070, 1, function(row) any(row >= 3 & row <= 6))
loncols2070 <- apply(lon_2070, 2, function(col) any(col >= 4 & col <= 5))

latrows2070 <- apply(lat_2070, 1, function(row) any(row >= 50 & row <= 51))
latcols2070 <- apply(lat_2070, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2070 <- lon[lonrows2070, loncols2070]
filtered_lat_2070 <- lat[latrows2070, latcols2070]

avgData_filtered_2070 <- avgData_2070[lonrows2070, latcols2070]

#2080
nc_data_2080 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2080_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2080_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2080 <- ncvar_get(nc_data_2080, "lon")
lat_2080 <- ncvar_get(nc_data_2080, "lat", verbose = F)
t <- ncvar_get(nc_data_2080, "time")

head(lon_2080)

ndvi.array.2080 <- ncvar_get(nc_data_2080, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2080) 

fillvalue_2080 <- ncatt_get(nc_data_2080, "prAdjust", "_FillValue")
fillvalue_2080



nc_close(nc_data_2080) 


ndvi.array.2080[ndvi.array == fillvalue$value] <- NA
avgData_2080 <- apply(ndvi.array.2080, c(1,2), mean) 


lonrows2080 <- apply(lon_2080, 1, function(row) any(row >= 3 & row <= 6))
loncols2080 <- apply(lon_2080, 2, function(col) any(col >= 4 & col <= 5))

latrows2080 <- apply(lat_2080, 1, function(row) any(row >= 50 & row <= 51))
latcols2080 <- apply(lat_2080, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2080 <- lon[lonrows2080, loncols2080]
filtered_lat_2080 <- lat[latrows2080, latcols2080]

avgData_filtered_2080 <- avgData_2080[lonrows2080, latcols2080]


nc_data_2080 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2080_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2080_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2080 <- ncvar_get(nc_data_2080, "lon")
lat_2080 <- ncvar_get(nc_data_2080, "lat", verbose = F)
t <- ncvar_get(nc_data_2080, "time")

head(lon_2080)

ndvi.array.2080 <- ncvar_get(nc_data_2080, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2080) 

fillvalue_2080 <- ncatt_get(nc_data_2080, "prAdjust", "_FillValue")
fillvalue_2080



nc_close(nc_data_2080) 


ndvi.array.2080[ndvi.array == fillvalue$value] <- NA
avgData_2080 <- apply(ndvi.array.2080, c(1,2), mean) 


lonrows2080 <- apply(lon_2080, 1, function(row) any(row >= 3 & row <= 6))
loncols2080 <- apply(lon_2080, 2, function(col) any(col >= 4 & col <= 5))

latrows2080 <- apply(lat_2080, 1, function(row) any(row >= 50 & row <= 51))
latcols2080 <- apply(lat_2080, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2080 <- lon[lonrows2080, loncols2080]
filtered_lat_2080 <- lat[latrows2080, latcols2080]

avgData_filtered_2080 <- avgData_2080[lonrows2080, latcols2080]


#2090

nc_data_2090 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2090_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2090_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2090 <- ncvar_get(nc_data_2090, "lon")
lat_2090 <- ncvar_get(nc_data_2090, "lat", verbose = F)
t <- ncvar_get(nc_data_2090, "time")

head(lon_2090)

ndvi.array.2090 <- ncvar_get(nc_data_2090, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2090) 

fillvalue_2090 <- ncatt_get(nc_data_2090, "prAdjust", "_FillValue")
fillvalue_2090



nc_close(nc_data_2090) 


ndvi.array.2090[ndvi.array == fillvalue$value] <- NA
avgData_2090 <- apply(ndvi.array.2090, c(1,2), mean) 


lonrows2090 <- apply(lon_2090, 1, function(row) any(row >= 3 & row <= 6))
loncols2090 <- apply(lon_2090, 2, function(col) any(col >= 4 & col <= 5))

latrows2090 <- apply(lat_2090, 1, function(row) any(row >= 50 & row <= 51))
latcols2090 <- apply(lat_2090, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2090 <- lon[lonrows2090, loncols2090]
filtered_lat_2090 <- lat[latrows2090, latcols2090]

avgData_filtered_2090 <- avgData_2090[lonrows2090, latcols2090]


#2040

nc_data_2040 <- nc_open('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2040_grid5km_v1.nc')

{
  sink('prAdjust_day_QM-EFAS-Meteo_EUR-11_MOHC-HadGEM2-ES_rcp85_r1i1p1_KNMI-RACMO22E-v2_2040_grid5km_v1.txt')
  print(nc_data)
  sink()
}

lon_2040 <- ncvar_get(nc_data_2040, "lon")
lat_2040 <- ncvar_get(nc_data_2040, "lat", verbose = F)
t <- ncvar_get(nc_data_2040, "time")

head(lon_2040)

ndvi.array.2040 <- ncvar_get(nc_data_2040, "prAdjust") # store the data in a 3-dimensional array
dim(ndvi.array.2040) 

fillvalue_2040 <- ncatt_get(nc_data_2040, "prAdjust", "_FillValue")
fillvalue_2040



nc_close(nc_data_2040) 


ndvi.array.2040[ndvi.array == fillvalue$value] <- NA
avgData_2040 <- apply(ndvi.array.2040, c(1,2), mean) 


lonrows2040 <- apply(lon_2040, 1, function(row) any(row >= 3 & row <= 6))
loncols2040 <- apply(lon_2040, 2, function(col) any(col >= 4 & col <= 5))

latrows2040 <- apply(lat_2040, 1, function(row) any(row >= 50 & row <= 51))
latcols2040 <- apply(lat_2040, 2, function(col) any(col >= 45 & col <= 46))

filtered_lon_2040 <- lon[lonrows2040, loncols2040]
filtered_lat_2040 <- lat[latrows2040, latcols2040]

avgData_filtered_2040 <- avgData_2040[lonrows2040, latcols2040]


combined <- array(c(avgData_filtered_2010, avgData_filtered_2020, avgData_filtered_2030,
                    avgData_filtered_2040, avgData_filtered_2050, avgData_filtered_2060,
                    avgData_filtered_2070, avgData_filtered_2080, avgData_filtered_2090,
                    avgData_filtered), dim = c(111, 219, 10))


r_brick <- brick(combined, xmn=min(filtered_lat), xmx=max(filtered_lat),
                 ymn=min(filtered_lon), ymx=max(filtered_lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

# note that you may have to play around with the transpose (the t() function) and flip() before the data are oriented correctly. In this example, the netcdf file recorded latitude on the X and longitude on the Y, so both a transpose and a flip in the y direction were required.
r_brick <- flip(t(r_brick), direction='y')

zeebrugge_lon <- 3.186747
zeebrugge_lat <- 51.328358
zeebrugge_series <- extract(r_brick, SpatialPoints(cbind(zeebrugge_lon,zeebrugge_lat)), method='simple')


zeebrugge_df <- data.frame(year= seq(from=2010, to=2100, by=10), precipitation=t(zeebrugge_series))
ggplot(data=zeebrugge_df, aes(x=year, y=precipitation, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("Precipitation in Zeebrugge") +     # Set title
  theme_bw() # use the black and white theme

plot(r, main = 'Precipitation 2010')
plot(r_2100, main = 'Precipitation 2100')
