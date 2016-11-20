# Seoul subway users by week

library(xml2)
library(rvest)
library(ggplot2)
library(tidyr)
library(dplyr)

#subway num from seoul openapi
date<-c(20161031,20161101:20161106)
url<-list()
for (i in 1:length(date)){
url[i]<-paste0('http://openapi.seoul.go.kr:8088/68796451636b6867393077596f564b/xml/CardSubwayStatsNew/1/1000/',date[i])
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

theme_set(theme_gray(base_family='NanumGothic'))
ggplot(data=subway, aes(x=line, y=num.ride)) + geom_bar(stat="identity") + facet_grid(~time)
ggplot(data=subway, aes(x=time, y=num.ride)) + geom_bar(stat="identity")

#location
url_daum<-paste0('http://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=',subway[1:nrow(subway),2], subway[1:nrow(subway),3])
url_daum<-unique(url_daum)
info<-list()
for(i in 1:length(url_daum)){
  info[[i]]<-read_html(url_daum[i]) %>% html_nodes('.detail') %>% html_text()
}

for(i in 1:length(info)){
  info[[i]][2]<-c('num')
}

info_add<-do.call(rbind, info)
info_add<-data.frame(info_add)

info_add<-strsplit(as.character(info_add$X1),'주소')
info_add<-do.call(rbind, info_add)
info_add<-data.frame(info_add)
info_add<-strsplit(as.character(info_add$X2),'전화번호')
info_add<-do.call(rbind, info_add)
info_add<-data.frame(info_add)
info_add<-info_add[c(1)]

info_name<-paste0(subway[1:nrow(subway),2],c(' '),subway[1:nrow(subway),3])
info_name<-unique(info_name)
info_name<-data.frame(info_name)
info_name<-cbind(info_name, info_add)
names(info_name)<-c('name','address')

info_name_level <- info_name %>% separate(address, into = c("lv0","lv1","lv2","lv3"), sep = " ")
info_name_level <- subset(info_name_level, lv1==c('서울특별시'))
info_name_level <- info_name_level[c(1,4)]

#merge
subway$name <- with(subway, paste0(line,c(' '),station))
subway_loc<-merge(subway, info_name_level, c('name'))

theme_set(theme_gray(base_family='NanumGothic'))
ggplot(data=subway_loc, aes(x=line, y=num.ride)) + geom_bar(stat="identity") #+ facet_grid(~time)
ggplot(data=subway_loc, aes(x=time, y=num.ride)) + geom_bar(stat="identity")

subway_loc_lv2<-subway_loc %>% group_by(time,lv2) %>% summarise(num.ride=sum(num.ride),num.alight=sum(num.alight))
ggplot(data=subset(subway_loc_lv2,time==20161031), aes(x=lv2, y=num.ride)) + geom_bar(stat="identity") #+ facet_grid(~time)
ggplot(data=subset(subway_loc_lv2,time==20161105), aes(x=lv2, y=num.ride)) + geom_bar(stat="identity")

library(cartogram)
library(tmap)
library(maptools)
library(Kormaps)

Seoul2 <- submap(korpopmap2,"서울")

people<-function(Seoul2,subway_loc_lv2,date) {
  name_order<-Seoul2$name
  name_order<-data.frame(name=name_order)
  subway_loc_lv2_sub<-subset(subway_loc_lv2,time==date)
  names(subway_loc_lv2_sub)[2]<-'name'
  name_order<-left_join(name_order, subway_loc_lv2_sub,c('name'))
  people<-name_order[1:nrow(name_order),3]
  return(people)
}

people_1031<-people(Seoul2,subway_loc_lv2,20161031)
people_1105<-people(Seoul2,subway_loc_lv2,20161105)

Seoul2$people_1031<-people_1031
Seoul2$people_1105<-people_1105

Seoul2 <- spTransform(Seoul2, CRS("+init=epsg:3395"))
Seoul2 <- cartogram(Seoul2, "people_1031", itermax=200, threshold=0.01)
ani1<-qtm(Seoul2, "people_1031")+tm_layout(fontfamily="AppleGothic")

Seoul2 <- spTransform(Seoul2, CRS("+init=epsg:3395"))
Seoul2 <- cartogram(Seoul2, "people_1105", itermax=200, threshold=0.01)
ani2<-qtm(Seoul2, "people_1105")+tm_layout(fontfamily="AppleGothic")

Seoul2 <- spTransform(Seoul2, CRS("+init=epsg:3395"))
Seoul2 <- cartogram(Seoul2, "people_1104", itermax=200, threshold=0.01)
qtm(Seoul2, "people_1104")+tm_layout(fontfamily="AppleGothic")

