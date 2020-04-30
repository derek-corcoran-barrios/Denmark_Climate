library(tidyverse)
library(raster)
library(sf)
library(rworldxtra)
library(foreach)
library(doParallel)


data("countriesHigh")

e <- new("Extent", xmin = -18.1643245238217, xmax = 47.8410512467405, 
         ymin = 34.5230470172154, ymax = 73.2069290580954)

Europe <- getData('worldclim', var='bio', res=2.5) %>% crop(e)



Europe_sf <- countriesHigh %>% st_as_sf() %>% lwgeom::st_make_valid() %>% st_crop(st_bbox(e)) %>% st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

Denmark <- Europe_sf %>% dplyr::filter(SU_A3 == "DNK") %>% dplyr::select(SU_A3) %>% rasterize(Europe, field = 1)


Europe_Future_85 <- getData('CMIP5', var='bio', res=2.5, model = "IP", rcp = 85, year = 70) %>% crop(e)

Europe_Future_45 <- getData('CMIP5', var='bio', res=2.5, model = "IP", rcp = 45, year = 70) %>% crop(e)

library(sp)

#DF1 <-  Europe[[c(1,4,5,7, 12, 16,17)]]  %>% as("SpatialPixelsDataFrame") %>% as.data.frame() #%>% rename(Presente = bio1)
#PCAs <- preProcess(DF1[,c(1:7)], method = "pca")

DF1 <-  Europe[[1]]  %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% rename(Presente = bio1)

DF2 <-  Europe_Future_45[[1]]  %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% rename(Future_45 = ip45bi701)

DF3 <-  Europe_Future_85[[1]]  %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% rename(Future_85 = ip85bi701)

DF4 <-  Denmark  %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% rename(Denmark = layer)

DF <- list(DF1, DF2, DF3, DF4) %>% reduce(full_join)  %>% mutate(NewDenmark_85 =NA,NewDenmark_45 =NA , Denmark = ifelse(is.na(Denmark), "No", "Yes"))


p <- DF %>% dplyr::pull(Presente)
f <- DF %>% dplyr::filter(Denmark == "Yes") %>% dplyr::pull(Future_85)
t <-(range(DF$Present)[2] - range(DF$Present)[1])/nclass.scott(DF$Present)


for(i in 1:length(f)){
  
  DF$NewDenmark_85[abs(p- f[i]) <t] <- "RCP85"
  if(i %% 500 == 0){
    message(paste(i, "of", length(f)))
    #saveRDS(DF, "DF.rds")
  }
}


p <- DF %>% dplyr::pull(Presente)
f <- DF %>% dplyr::filter(Denmark == "Yes") %>% dplyr::pull(Future_45)
t <-(range(DF$Present)[2] - range(DF$Present)[1])/nclass.scott(DF$Present)


for(i in 1:length(f)){
  
  DF$NewDenmark_45[abs(p- f[i]) <t] <- "RCP45"
  if(i %% 500 == 0){
    message(paste(i, "of", length(f)))
    #saveRDS(DF, "DF.rds")
  }
}


#ggplot() + geom_sf(data = Europe_sf) + geom_raster(data = DF, aes(fill = NewDenmark_85, x = x , y = y)) + theme_bw() + xlab("") + ylab ("")

#ggplot() + geom_sf(data = Europe_sf) + geom_raster(data = DF, aes(fill = NewDenmark_45, x = x , y = y)) + theme_bw() + xlab("") + ylab ("")


DF_Final <- DF %>% dplyr::filter(Denmark == "Yes" | NewDenmark_85 == "RCP85" | NewDenmark_45 == "RCP45") %>% mutate(Climate = case_when(Denmark == "Yes" ~ "Present",
                                                                                                                                       NewDenmark_45 == "RCP45" & is.na(NewDenmark_85) ~ "Low Emissions 2070",
                                                                                                                                       NewDenmark_85 == "RCP85" & is.na(NewDenmark_45) ~ "High Emissions 2070",
                                                                                                                                       NewDenmark_85 == "RCP85" & NewDenmark_45 == "RCP45" ~ "Both 2070")) %>% mutate(Climate = fct_relevel(Climate, c("Present", "Low Emissions 2070", "High Emissions 2070", "Both 2070")))

ggplot() + geom_sf(data = Europe_sf) + geom_raster(data = DF_Final, aes(fill = Climate, x = x , y = y)) + theme_bw() + xlab("") + ylab ("")


#### Rápido