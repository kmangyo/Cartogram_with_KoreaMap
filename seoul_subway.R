# Seoul subway users by week
# animation packages 

library(animation)
# https://cran.r-project.org/web/packages/animation/animation.pdf
library(xml2)
library(rvest)

date<-c(20161008:20161014)
url<-list()
for (i in 1:length(date)){
url[i]<-paste0('http://openapi.seoul.go.kr:8088/{key}/xml/CardSubwayStatsNew/1/1000/',date[i])
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

theme_set(theme_gray(base_family='NanumGothic'))
ggplot(data=subway, aes(x=line, y=num.alight)) + geom_bar(stat="identity") #+ facet_grid(~time)
