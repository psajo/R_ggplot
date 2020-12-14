library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(dplyr)
setwd("C:/PSJ/R_ggmap") #디렉토리 경로 지정해야됨
seoul_in9 <- read.csv("seoul_in9.csv", header =TRUE)
sample_data <- read.csv("sample.csv", header=TRUE)
gu_name <- read.csv("gu_name.csv", header = TRUE)
map <- shapefile("SIG_201703/TL_SCCO_SIG.shp") #서울시 지리정보
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
seoul_total <- merge(seoul_map, sample_data, by='id')
seoul_total_in9 <- merge(seoul_total, seoul_in9, by='gu')
seoul_total_in9$in9 <- gsub(",","",seoul_total_in9$in9)
seoul_total_in9$in9 <- as.numeric(seoul_total_in9$in9)
corona <- read.csv("corona.csv")
corona <- corona %>%  group_by(gu) %>% summarise(확진자 = n())
total <- merge(gu_name, corona, by="gu")
options(scipen = 99)
ggplot() + 
  geom_polygon(data = seoul_total_in9, aes(x=long, y=lat, group=group, fill = in9), color='black')+
  scale_fill_gradient(low = "#D4F4FA", high = "#002266", space = "Lab", guide = "colourbar")  +
  geom_point(data = total, aes(x=long, y= lat,size=확진자,color=확진자), shape=16) +
  scale_color_gradient(low="#FFDCDC", high = "#DB0000")+
  scale_size_area(max_size = 22, guide=FALSE) +
  geom_text(data = total ,aes(x = long,y = lat,label= paste(gu, 확진자,sep='\n'))) +
  labs(title="각 구별 인구수 및 코로나 확진자 수",fill="인구수") +
  theme(plot.title = element_text(size=28,hjust = 0.5)) 
