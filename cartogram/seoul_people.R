
#Kormaps packages
#http://rstudio-pubs-static.s3.amazonaws.com/159183_794d3326ca09428eb519206440ff863d.html

library(cartogram)
library(tmap)
library(maptools)
library(Kormaps)

Seoul2 <- submap(korpopmap2,"서울")

# 주 평균 유동인구
# data from http://data.seoul.go.kr/visual/content/4221?filtered=true
people<-c(10386,
                 15791,
                 5583,
                 3815,
                 4643,
                 4040,
                 2848,
                 2943,
                 4453,
                 4209,
                 3548,
                 1426,
                 6660,
                 5117,
                 2526,
                 3924,
                 5650,
                 4547,
                 4553,
                 8359,
                 6797,
                 5809,
                 6656,
                 4015,
                 6327)

Seoul2$people<-people

Seoul3 <- spTransform(Seoul2, CRS("+init=epsg:3395"))
Seoul3 <- cartogram(Seoul3, "people", itermax=10)
qtm(Seoul3, "people")+tm_layout(fontfamily="AppleGothic")
qtm(Seoul2, "people")+tm_layout(fontfamily="AppleGothic")
