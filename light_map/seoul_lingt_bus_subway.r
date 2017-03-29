# Idea is from http://sharpsightlabs.com/blog/mapping-france-night/
# Seoul map shp file is here. http://data.seoul.go.kr/openinf/mapview.jsp?infId=OA-11677
# Seoul subway stations location is here. https://data.seoul.go.kr/openinf/sheetview.jsp?infId=OA-118
# Seoul bus station location is here. topis.seoul.go.kr/excel/stationlist.xlsx
# Bus users info is here. http://data.seoul.go.kr/openinf/openapiview.jsp?infId=OA-12912
# Subway users info is here. http://data.seoul.go.kr/openinf/sheetview.jsp?infId=OA-12914&tMenu=11

# Ref1) http://lumiamitie.github.io/r/visualization/mapping-with-ggplot2
# Ref2) http://m.blog.naver.com/goldenezkang/220061647790
# Ref3) http://sape.inf.usi.ch/quick-reference/ggplot2/shape

library(dplyr)
library(ggplot2)
library(maps)
library(maptools)
library(xml2)
library(rvest)
theme_set(theme_gray(base_family='NanumGothic'))

# Seoul map
seoul <- readShapePoly(file.choose())
plot(seoul)

seoul_gg <- fortify(seoul)

max(seoul_gg$long)
min(seoul_gg$long)

max(seoul_gg$lat)
min(seoul_gg$lat)

# Seoul subway sation location
subway_loc <- read.csv(file.choose())
subway_loc <-subset(subway_loc, X좌표.WGS.<=max(seoul_gg$lat)&X좌표.WGS.>=min(seoul_gg$lat))
subway_loc <-subset(subway_loc, Y좌표.WGS.<=max(seoul_gg$long)&Y좌표.WGS.>=min(seoul_gg$long))
subway_loc_df<- subway_loc %>% group_by(전철역명) %>% summarise(X좌표.WGS.=mean(X좌표.WGS.), Y좌표.WGS.=mean(Y좌표.WGS.))

# Seoul bus sation location
bus <- read.csv(file.choose())
bus_loc <- bus %>% group_by(X__, Y__) %>% summarise(n=n())
bus_loc <-subset(bus_loc, Y__<=max(seoul_gg$lat)&Y__>=min(seoul_gg$lat))
bus_loc <-subset(bus_loc, X__<=max(seoul_gg$long)&X__>=min(seoul_gg$long))

# Getting data from API
api_key<-'XXXX'

date<-c(20170310)

# Subway api
url<-list()
for (i in 1:length(date)){
  url[i]<-paste0('http://openapi.seoul.go.kr:8088/',api_key,'/xml/CardSubwayStatsNew/1/1000/',date)
}

url_xml<-list()
for(i in 1:length(url)){
  url_xml[[i]] <- read_xml(url[[i]])
}

time<-list()
line<-list()
station<-list()
num.ride<-list()
num.alight<-list()

for(i in 1:length(url_xml)){
  time[[i]]<- url_xml[[i]] %>% xml_find_all("//USE_DT") %>% xml_text()
  line[[i]]<- url_xml[[i]] %>% xml_find_all("//LINE_NUM") %>% xml_text()
  station[[i]]<- url_xml[[i]] %>% xml_find_all("//SUB_STA_NM") %>% xml_text()
  num.ride[[i]]<- url_xml[[i]] %>% xml_find_all("//RIDE_PASGR_NUM") %>% xml_text()
  num.alight[[i]]<- url_xml[[i]] %>% xml_find_all("//ALIGHT_PASGR_NUM") %>% xml_text()
}

time<-unlist(time)
line<-unlist(line)
station<-unlist(station)
num.ride<-unlist(num.ride)
num.alight<-unlist(num.alight)

subway<-cbind(time, line, station, num.ride, num.alight)
subway<-data.frame(subway)
subway$num.ride<-as.numeric(as.character(subway$num.ride))
subway$num.alight<-as.numeric(as.character(subway$num.alight))

subway.alight<- subway %>% group_by(station) %>% summarise(alight=sum(num.alight))

names(subway_loc_df)[1]<-'station'
subway_loc_df<-merge(subway_loc_df, subway.alight, c('station'),all.x=T)
subway_loc_df <- subway_loc_df %>% arrange(alight)

# Bus api
url_bus<-list()
seq1<-seq(1,38048, 1000)
seq2<-seq(1000,38048, 1000)
seq2<-c(seq2,38048)

for (i in 1:length(seq1)){
  url_bus[i]<-paste0('http://openapi.seoul.go.kr:8088/',api_key,'/xml/CardBusStatisticsServiceNew/',seq1[i],'/',seq2[i],'/',date)
}

url_xml_bus<-list()
for(i in 1:length(url_bus)){
  url_xml_bus[[i]] <- read_xml(url_bus[[i]])
}

time_bus<-list()
route_bus<-list()
route_no<-list()
route_nm<-list()
bus_std_station_id<-list()
bus_station_id<-list()
station_nm<-list()
bus_ride<-list()
bus_alight<-list()

for(i in 1:length(url_xml_bus)){
  time_bus[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//USE_DT") %>% xml_text()
  route_bus[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_ROUTE_ID") %>% xml_text()
  route_no[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_ROUTE_NO") %>% xml_text()
  route_nm[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_ROUTE_NM") %>% xml_text()
  bus_std_station_id[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//STND_BSST_ID") %>% xml_text()
  bus_station_id[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_STA_ID") %>% xml_text()
  station_nm[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_STA_NM") %>% xml_text()
  bus_ride[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//RIDE_PASGR_NUM") %>% xml_text()
  bus_alight[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//ALIGHT_PASGR_NUM") %>% xml_text()
}

time_bus<-unlist(time_bus)
route_bus<-unlist(route_bus)
route_no<-unlist(route_no)
route_nm<-unlist(route_nm)
bus_std_station_id<-unlist(bus_std_station_id)
bus_station_id<-unlist(bus_station_id)
station_nm<-unlist(station_nm)
bus_ride<-unlist(bus_ride)
bus_alight<-unlist(bus_alight)

bus_df<-cbind(time_bus, route_bus, route_no, route_nm, bus_std_station_id,bus_station_id,station_nm,bus_ride,bus_alight)
bus_df<-data.frame(bus_df)
bus_df$bus_ride<-as.numeric(as.character(bus_df$bus_ride))
bus_df$bus_alight<-as.numeric(as.character(bus_df$bus_alight))

names(bus)[7]<-'id'
names(bus_df)[5]<-'id'
bus_loc_df<-merge(bus, bus_df, c('id'),all.x=T)
bus_loc_df <- bus_loc_df %>% group_by(X__, Y__) %>% summarise(n=n(), alight=sum(bus_alight))
bus_loc_df <-subset(bus_loc_df, Y__<=max(seoul_gg$lat)&Y__>=min(seoul_gg$lat))
bus_loc_df <-subset(bus_loc_df, X__<=max(seoul_gg$long)&X__>=min(seoul_gg$long))
bus_loc_df <- bus_loc_df %>% arrange(alight)
bus_loc_df$alight <- bus_loc_df$alight+1

subway_loc_df %>% head()
bus_loc_df %>% head()

seooul_y<-subway_loc_df$X좌표.WGS.
seooul_x<-subway_loc_df$Y좌표.WGS.
seooul_size<-subway_loc_df$alight

seooul_y<-c(seooul_y, bus_loc_df$Y__)
seooul_x<-c(seooul_x, bus_loc_df$X__)
seooul_size<-c(seooul_size, bus_loc_df$alight)

seoul_people<-data.frame(seooul_y, seooul_x, seooul_size)
seoul_people<- seoul_people %>% arrange(seooul_size)

seooul_y<-c(seoul_people$seooul_y, rep(NA,38546-nrow(seoul_people)))
seooul_x<-c(seoul_people$seooul_x, rep(NA,38546-nrow(seoul_people)))
seooul_size<-c(seoul_people$seooul_size, rep(NA,38546-nrow(seoul_people)))

# Create map with size 550 * 450
ggplot(seoul_gg, aes(x=long, y=lat, group=group)) + geom_polygon(colour="#000233", fill="#000233") +
  geom_point(aes(x=seooul_x, y=seooul_y, size=seooul_size, colour=seooul_size), shape=16) + 
  scale_colour_gradientn(colours = c("#000233","#f9cf86","#fceccf","white")) + 
  scale_fill_gradientn(colours = c("#000233","#f9cf86","#fceccf","white")) +
  theme(text = element_text(family = "NanumGothic", color = "#E1E1E1")
        ,plot.title = element_text(size = 18, color = "#E1E1E1")
        ,plot.subtitle = element_text(size = 10)
        ,plot.background = element_rect(fill = "#000223")
        ,panel.background = element_rect(fill = "#000223")
        ,panel.grid.major = element_line(color = "#000223")
        ,panel.grid.minor = element_line(color = "#000223")
        ,legend.key = element_rect(fill = "#000223")
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )

# Create gif animation
library(gganimate)
library(animation)

date<-c(20161029,20161105,20161112,20161119,20161126,20161203,20161210,20161217,20161224,20161231,20170107,20170114,
        20170121,20170128,20170204,20170211,20170218,20170225,20170304,20170311)
url<-list()
for (i in 1:length(date)){
  url[i]<-paste0('http://openapi.seoul.go.kr:8088/',api_key,'/xml/CardSubwayStatsNew/1/1000/',date[i])
}

url_xml<-list()
for(i in 1:length(url)){
  url_xml[[i]] <- read_xml(url[[i]])
}

time<-list()
line<-list()
station<-list()
num.ride<-list()
num.alight<-list()

for(i in 1:length(url_xml)){
  time[[i]]<- url_xml[[i]] %>% xml_find_all("//USE_DT") %>% xml_text()
  line[[i]]<- url_xml[[i]] %>% xml_find_all("//LINE_NUM") %>% xml_text()
  station[[i]]<- url_xml[[i]] %>% xml_find_all("//SUB_STA_NM") %>% xml_text()
  num.ride[[i]]<- url_xml[[i]] %>% xml_find_all("//RIDE_PASGR_NUM") %>% xml_text()
  num.alight[[i]]<- url_xml[[i]] %>% xml_find_all("//ALIGHT_PASGR_NUM") %>% xml_text()
}

time<-unlist(time)
line<-unlist(line)
station<-unlist(station)
num.ride<-unlist(num.ride)
num.alight<-unlist(num.alight)

subway<-cbind(time, line, station, num.ride, num.alight)
subway<-data.frame(subway)
subway$num.ride<-as.numeric(as.character(subway$num.ride))
subway$num.alight<-as.numeric(as.character(subway$num.alight))

subway.alight<- subway %>% group_by(time, station) %>% summarise(alight=sum(num.alight))

names(subway_loc_df)[1]<-'station'
subway_loc_df<-merge(subway_loc_df, subway.alight, c('station'),all.x=T)
subway_loc_df <- subway_loc_df %>% arrange(alight)

# bus api
url_bus<-list()
seq1<-seq(1,39000, 1000)
seq2<-seq(1000,39000, 1000)
seq2<-c(seq2,39000)
seq_date <-data.frame(seq1=rep(seq1,length(date)),seq2=rep(seq2,length(date)),date=rep(date,length(seq1)))

for (i in 1:nrow(seq_date)) {
  url_bus[i]<-paste0('http://openapi.seoul.go.kr:8088/',api_key,'/xml/CardBusStatisticsServiceNew/',seq_date[i,1],'/'
                     ,seq_date[i,2],'/',seq_date[i,3])
}

url_xml_bus<-list()
for(i in 1:length(url_bus)){
  url_xml_bus[[i]] <- read_xml(url_bus[[i]])
}

url_xml_bus[[1]]

time_bus<-list()
route_bus<-list()
route_no<-list()
route_nm<-list()
bus_std_station_id<-list()
bus_station_id<-list()
station_nm<-list()
bus_ride<-list()
bus_alight<-list()

for(i in 1:length(url_xml_bus)){
  time_bus[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//USE_DT") %>% xml_text()
  route_bus[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_ROUTE_ID") %>% xml_text()
  route_no[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_ROUTE_NO") %>% xml_text()
  route_nm[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_ROUTE_NM") %>% xml_text()
  bus_std_station_id[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//STND_BSST_ID") %>% xml_text()
  bus_station_id[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_STA_ID") %>% xml_text()
  station_nm[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//BUS_STA_NM") %>% xml_text()
  bus_ride[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//RIDE_PASGR_NUM") %>% xml_text()
  bus_alight[[i]]<- url_xml_bus[[i]] %>% xml_find_all("//ALIGHT_PASGR_NUM") %>% xml_text()
}

time_bus<-unlist(time_bus)
route_bus<-unlist(route_bus)
route_no<-unlist(route_no)
route_nm<-unlist(route_nm)
bus_std_station_id<-unlist(bus_std_station_id)
bus_station_id<-unlist(bus_station_id)
station_nm<-unlist(station_nm)
bus_ride<-unlist(bus_ride)
bus_alight<-unlist(bus_alight)

bus_df<-cbind(time_bus, route_bus, route_no, route_nm, bus_std_station_id,bus_station_id,station_nm,bus_ride,bus_alight)
bus_df<-data.frame(bus_df)
bus_df$bus_ride<-as.numeric(as.character(bus_df$bus_ride))
bus_df$bus_alight<-as.numeric(as.character(bus_df$bus_alight))

names(bus)[7]<-'id'
names(bus_df)[5]<-'id'
bus_loc_df<-merge(bus, bus_df, c('id'),all.x=T)
bus_loc_df <- bus_loc_df %>% group_by(X__, Y__,time_bus) %>% summarise(n=n(), alight=sum(bus_alight))
bus_loc_df <-subset(bus_loc_df, Y__<=max(seoul_gg$lat)&Y__>=min(seoul_gg$lat))
bus_loc_df <-subset(bus_loc_df, X__<=max(seoul_gg$long)&X__>=min(seoul_gg$long))
bus_loc_df <- bus_loc_df %>% arrange(alight)
bus_loc_df$alight <- bus_loc_df$alight+1

seooul_y<-subway_loc_df$X좌표.WGS.
seooul_x<-subway_loc_df$Y좌표.WGS.
seooul_size<-subway_loc_df$alight
seoul_date<-as.character(subway_loc_df$time)
  
seooul_y<-c(seooul_y, bus_loc_df$Y__)
seooul_x<-c(seooul_x, bus_loc_df$X__)
seooul_size<-c(seooul_size, bus_loc_df$alight)
seoul_date<-c(seoul_date, as.character(bus_loc_df$time_bus))
  
seoul_people<-data.frame(seooul_y, seooul_x, seooul_size, seoul_date)
seoul_people<- seoul_people %>% arrange(seooul_size)

seoul_people<-seoul_people[complete.cases(seoul_people[1:ncol(seoul_people)]),]
seoul_people$seoul_date<-as.Date(seoul_people$seoul_date, "%Y%m%d")

plot<-ggplot(seoul_people, aes(x=seooul_x, y=seooul_y, frame=seoul_date)) + geom_point(aes(size=seooul_size,colour=seooul_size)) +
  scale_colour_gradientn(colours = c("#000233","#f9cf86","#fceccf","white")) + 
  scale_fill_gradientn(colours = c("#000233","#f9cf86","#fceccf","white")) +
  theme(text = element_text(family = "NanumGothic", color = "#E1E1E1")
        ,plot.title = element_text(size = 18, color = "#E1E1E1")
        ,plot.subtitle = element_text(size = 10)
        ,plot.background = element_rect(fill = "#000223")
        ,panel.background = element_rect(fill = "#000223")
        ,panel.grid.major = element_line(color = "#000223")
        ,panel.grid.minor = element_line(color = "#000223")
        ,legend.key = element_rect(fill = "#000223")
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )

ani.options(interval = .2)
gganimate(plot, 'seoul.gif')
