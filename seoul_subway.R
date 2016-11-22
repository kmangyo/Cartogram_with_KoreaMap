# 2016 촛불 시위 기간동안 서울 지하철 승객 데이터를 사용하여, 서울시의 카토그램(cargotram) 지도 만들기

library(xml2)
library(rvest)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cartogram)
library(tmap)
library(maptools)
library(Kormaps)

# 서울시 오픈API를 사용하여 승하차 데이터를 수집할 수 있다. 10/22 부터 11/12까지 매주 토요일 승하차 데이터를 수집
# http://data.seoul.go.kr/openinf/sheetview.jsp?tMenu=11&leftSrvType=S&infId=OA-12914
# 위의 URL에가면 데이터에 대한 자세한 정보 수집 가능

date<-c(20161022,20161029,20161105,20161112)
url<-list()
for (i in 1:length(date)){
  url[i]<-paste0('http://openapi.seoul.go.kr:8088/{api_key}/xml/CardSubwayStatsNew/1/1000/',date[i])
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

subway_name <- subway %>% separate(name, into = c("lv0","lv1"), sep = "\\(")
subway_name <- subway_name[c(-7)]
names(subway_name)[6] <-'name'

# 호선별, 일자별로 승차 인원 시각화
theme_set(theme_gray(base_family='NanumGothic'))
ggplot(data=subway_name, aes(x=line, y=num.ride)) + geom_bar(stat="identity") + facet_grid(~time)
ggplot(data=subway_name, aes(x=time, y=num.ride)) + geom_bar(stat="identity")

# 오픈API에는 지하철역의 위치정보를 확인할 수 없음. 그러므로 다음 검색을 사용하여 해당역의 주소정보를 획득
url_daum<-paste0('http://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=',subway_name[1:nrow(subway_name),6])
url_daum<-unique(url_daum)
url_daum<-gsub(" ", "", url_daum)

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
info_name_level_name <- info_name_level %>% separate(name, into = c("lv0","lv1"), sep = "\\(")
info_name_level_name <- info_name_level_name[c(-2)]
names(info_name_level_name)[1] <-'name'

# 지하철역의 승하차 인원수 정보와 해당역의 위치정보를 통합
subway_loc<-merge(subway_name, info_name_level_name, c('name'))

theme_set(theme_gray(base_family='NanumGothic'))
ggplot(data=subway_loc, aes(x=line, y=num.ride)) + geom_bar(stat="identity") #+ facet_grid(~time)
ggplot(data=subway_loc, aes(x=time, y=num.ride)) + geom_bar(stat="identity")

subway_loc_lv2<-subway_loc %>% group_by(time,lv2) %>% summarise(num.ride=sum(num.ride),num.alight=sum(num.alight))
ggplot(data=subset(subway_loc,time==20161029), aes(x=lv2, y=num.ride)) + geom_bar(stat="identity") #+ facet_grid(~time)
ggplot(data=subset(subway_loc,time==20161112), aes(x=lv2, y=num.ride)) + geom_bar(stat="identity")

# Kormaps패키지를 사용하여 서울 구별 지도를 불러옴
Seoul2 <- submap(korpopmap2,"서울")

# 해당 일자의 하차인원을 구별로 정리하는 함수. Kormaps의 구별 순서와 매칭되게 정리
people<-function(Seoul2,subway_loc_lv2,date) {
  name_order<-Seoul2$name
  name_order<-data.frame(name=name_order)
  subway_loc_lv2_sub<-subset(subway_loc_lv2,time==date)
  names(subway_loc_lv2_sub)[2]<-'name'
  name_order<-left_join(name_order, subway_loc_lv2_sub,c('name'))
  people<-name_order[1:nrow(name_order),4]
  return(people)
}

people_1022<-people(Seoul2,subway_loc_lv2,20161022)
people_1029<-people(Seoul2,subway_loc_lv2,20161029)
people_1105<-people(Seoul2,subway_loc_lv2,20161105)
people_1112<-people(Seoul2,subway_loc_lv2,20161112)

# 구별로 해당일자의 하차인원 정보를 입력
Seoul2$people_1022<-people_1022
Seoul2$people_1029<-people_1029
Seoul2$people_1105<-people_1105
Seoul2$people_1112<-people_1112

# 일자별 카토그램(cartogram) 서울시 지도 생성
Seoul2 <- spTransform(Seoul2, CRS("+init=epsg:3395"))
Seoul2 <- cartogram(Seoul2, "people_1022", itermax=200, threshold=0.01)
qtm(Seoul2, "people_1022")+tm_layout(fontfamily="AppleGothic")

Seoul2 <- spTransform(Seoul2, CRS("+init=epsg:3395"))
Seoul2 <- cartogram(Seoul2, "people_1029", itermax=200, threshold=0.01)
qtm(Seoul2, "people_1029")+tm_layout(fontfamily="AppleGothic")

Seoul2 <- spTransform(Seoul2, CRS("+init=epsg:3395"))
Seoul2 <- cartogram(Seoul2, "people_1105", itermax=200, threshold=0.01)
qtm(Seoul2, "people_1105")+tm_layout(fontfamily="AppleGothic")

Seoul2 <- spTransform(Seoul2, CRS("+init=epsg:3395"))
Seoul2 <- cartogram(Seoul2, "people_1112", itermax=200, threshold=0.01)
qtm(Seoul2, "people_1112")+tm_layout(fontfamily="AppleGothic")
