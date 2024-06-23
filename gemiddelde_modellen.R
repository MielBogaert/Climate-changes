library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(ggplot2) # package for plottingin
library(rasterVis)
library(data.table)


#historical
saveRDS(zeebrugge_GFDL_CM3_df,file="zeebrugge_GFDL_CM3_df")
GFDL_CM3 <- readRDS("zeebrugge_GFDL_CM3_df")

saveRDS(zeebrugge_EC_EARTH_df,file="zeebrugge_EC_EARTH_df")
EC_EARTH <- readRDS("zeebrugge_EC_EARTH_df")

saveRDS(zeebrugge_HadGEM2_ES_df,file="zeebrugge_HadGEM2_ES_df")
HadGEM2_ES <- readRDS("zeebrugge_HadGEM2_ES_df")

saveRDS(zeebrugge_df_IPSL_CM5A_MR,file="zeebrugge_df_IPSL_CM5A_MR")
IPSL_CM5A <- readRDS("zeebrugge_df_IPSL_CM5A_MR")

saveRDS(zeebrugge_df_MRI_CGCM3,file="zeebrugge_df_MRI_CGCM3")
MRI_CGCM3 <- readRDS("zeebrugge_df_MRI_CGCM3")

saveRDS(zeebrugge_df_MIROC,file="zeebrugge_df_MIROC")
MIROC5 <- readRDS("zeebrugge_df_MIROC")

mean_historical <- rbindlist(list(GFDL_CM3, EC_EARTH, HadGEM2_ES, IPSL_CM5A, MRI_CGCM3, MIROC5))[,lapply(.SD,mean), 
                                                                  list(year)]
ggplot(data=mean_historical, aes(x=year, y=SWH, group=1)) +
  geom_line(color = "black") + # make this a line plot
  ggtitle("Significant Waveheight Historical (ensemble)") +     # Set title
  theme_bw()+
  ylim(2, 5) + ylab('Significant waveheight (m)') + 
  geom_smooth(method = lm, color = 'black')

#rcp45
saveRDS(zeebrugge_rcp45_GFDL_CM3_df,file="zeebrugge_rcp45_GFDL_CM3_df")
GFDL_CM3_rcp45 <- readRDS("zeebrugge_rcp45_GFDL_CM3_df")
GFDL_CM3_rcp45 <- GFDL_CM3_rcp45[1:30, ]

saveRDS(zeebrugge_rcp45_EC_EARTH_df,file="zeebrugge_rcp45_EC_EARTH_df")
EC_EARTH_rcp45 <- readRDS("zeebrugge_rcp45_EC_EARTH_df")
EC_EARTH_rcp45 <- EC_EARTH_rcp45[1:30, ]



saveRDS(zeebrugge_rcp45_IPSL_CM5A_MR_df,file="zeebrugge_rcp45_IPSL_CM5A_MR_df")
IPSL_CM5A_rcp45 <- readRDS("zeebrugge_rcp45_IPSL_CM5A_MR_df")
IPSL_CM5A_rcp45 <- IPSL_CM5A_rcp45[1:30, ]

saveRDS(zeebrugge_rcp45_MRI_CGCM3_df,file="zeebrugge_rcp45_MRI_CGCM3_df")
MRI_CGCM3_rcp45 <- readRDS("zeebrugge_rcp45_MRI_CGCM3_df")
MRI_CGCM3_rcp45 <- MRI_CGCM3_rcp45[1:30, ]

saveRDS(zeebrugge_rcp45_MIROC_df,file="zeebrugge_rcp45_MIROC_df")
MIROC5_rcp45 <- readRDS("zeebrugge_rcp45_MIROC_df")
MIROC5_rcp45 <- MIROC5_rcp45[1:30, ]

mean_rcp45 <- rbindlist(list(GFDL_CM3_rcp45, EC_EARTH_rcp45, IPSL_CM5A_rcp45, MRI_CGCM3_rcp45, MIROC5_rcp45))[,lapply(.SD,mean), 
                                                                                              list(year)]
mean_rcp45 <- mean_rcp45[1:30, ]
ggplot(data=mean_rcp45, aes(x=year, y=SWH_rcp45, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("SWH rcp45") +     # Set title
  theme_bw()+
  ylim(2, 5)


#rcp85
saveRDS(zeebrugge_rcp85_GFDL_CM3_df,file="zeebrugge_rcp85_GFDL_CM3_df")
GFDL_CM3_rcp85 <- readRDS("zeebrugge_rcp85_GFDL_CM3_df")
GFDL_CM3_rcp85 <- GFDL_CM3_rcp85[1:30, ]

saveRDS(zeebrugge_rcp85_EC_EARTH_df,file="zeebrugge_rcp85_EC_EARTH_df")
EC_EARTH_rcp85 <- readRDS("zeebrugge_rcp85_EC_EARTH_df")
EC_EARTH_rcp85 <- EC_EARTH_rcp85[1:30, ]



saveRDS(zeebrugge_rcp85_IPSL_CM5A_MR_df,file="zeebrugge_rcp85_IPSL_CM5A_MR_df")
IPSL_CM5A_rcp85 <- readRDS("zeebrugge_rcp85_IPSL_CM5A_MR_df")
IPSL_CM5A_rcp85 <- IPSL_CM5A_rcp85[1:30, ]

saveRDS(zeebrugge_rcp85_MRI_CGCM3_df,file="zeebrugge_rcp85_MRI_CGCM3_df")
MRI_CGCM3_rcp85 <- readRDS("zeebrugge_rcp85_MRI_CGCM3_df")
MRI_CGCM3_rcp85 <- MRI_CGCM3_rcp85[1:30, ]

saveRDS(zeebrugge_rcp85_MIROC_df,file="zeebrugge_rcp85_MIROC_df")
MIROC5_rcp85 <- readRDS("zeebrugge_rcp85_MIROC_df")
MIROC5_rcp85 <- MIROC5_rcp85[1:30, ]

mean_rcp85 <- rbindlist(list(GFDL_CM3_rcp85, EC_EARTH_rcp85, IPSL_CM5A_rcp85, MRI_CGCM3_rcp85, MIROC5_rcp85))[,lapply(.SD,mean), 
                                                                                                              list(year)]
mean_rcp85 <- mean_rcp85[1:30, ]

ggplot(data=mean_rcp85, aes(x=year, y=SWH_rcp85, group=1)) +
  geom_line(aes(color = 'RCP-8.5'))+   # Set title
  theme_bw()+
  ylim(2, 5)+ylab('Significant waveheight (m)') + 
  geom_line(data = mean_rcp45, aes(y = SWH_rcp45, color = 'RCP-4.5'))+
  geom_smooth(method=lm, aes(color = 'RCP-8.5')) + 
  geom_smooth(data = mean_rcp45, method = lm, aes(y = SWH_rcp45, color = 'RCP-4.5'))+
  labs(title = "Significant Waveheight RCP-4.5 and RCP-8.5 (ensemble)",
       color = "Scenario's")+ 
  theme(legend.position = c(0.05, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
