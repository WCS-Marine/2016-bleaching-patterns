##-------------------------------------------------------
##Import NOAA bleaching alerts raster and organize/summarize

#download NOAA CRW netCDF files - from FTP server
# download.file("ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1/nc/v1.0/annual/ct5km_baa-max_v3.1_2016.nc",
#               destfile="2016 alerts.nc")

??nc_open
nc.alerts <- nc_open(here::here("2016 alerts.nc"))
head(nc.alerts)
str(nc.alerts)
names(nc.alerts$var)


# now, grab stuff out of the netcdf file and return it in a list
#sstFile <- nc.ocean
sstFile <- nc.alerts
ret2 <- list()
ret2$lat <- ncvar_get(sstFile, "lat")
ret2$lon <- ncvar_get(sstFile, "lon")
#ret2$time <- ncvar_get(sstFile, "time")
ret2$baa <- ncvar_get(sstFile, "bleaching_alert_area")
str(ret2)
hist(ret2$baa)

#function to melt
melt_sst2 <- function(L) {
  dimnames(L$baa) <- list(lon = L$lon, lat = L$lat)
  ret2 <- melt(L$baa, value.name = "baa")
}

alerts <- melt_sst2(ret2)
head(alerts)
str(alerts)

#pull up to coarser file and filter to tropical aresa
alerts.coarse <- alerts %>% 
  mutate_at(1:2, round, 1) %>% 
  as_tibble() %>% 
  group_by(lon, lat) %>% 
  summarize(max.baa = max(baa, na.omit = TRUE))
alerts.coarse

#Google is using [0,360] longitude values, 360 - value = converted lonitude   
alerts.coarse <- alerts.coarse %>% 
  mutate(X = ifelse(lon < 0, 360 - abs(lon), lon), 
         Y = lat) %>% 
  rename("BAA.max" = max.baa) %>% 
  filter(Y > -42 & Y < 42) %>% 
  filter(X > 23 & X < 188) %>% 
  mutate(BAA.max = factor(BAA.max))

alerts.coarse
