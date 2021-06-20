#ERGM MODEL---------------------------------------------------------------------
library(tidyverse)
library(statnet)
library(ergm)
library(sna)
library(stargazer)
library(readxl)
library(countrycode)
library(xtable)

detach("package:igraph")

load("~/Desktop/R Directory/Project_Network/AgreementScores.Rdata")
agree_all <- dfAgree

duplic80_85 <- read.csv("duplic80_85.csv")
duplic14_19 <- read.csv("duplic14_19.csv")
northsouth <- read_excel("north_south.xlsx")

## Working for 1980-85----------------------------------------------------------

high.cor1 <- subset(duplic80_85, subset = cor > 0.6)
edgelist1 <- high.cor1[,1:2]

unnet1 <- network(edgelist1, directed = F, matrix.type= "edgelist")

isolates1 <- sna::isolates(unnet1)
network::delete.vertices(unnet1, isolates1)

vertices1 <- sort(unique(c(high.cor1$ccode1, high.cor1$ccode2)))

vertex_list1 <- agree_all %>% 
  subset(select = c(ccode1, ccode2, IdealPointAll.x, IdealPointAll.y), 
         subset = year == 1985) %>%
  subset(select = c(ccode1, IdealPointAll.x),
         subset = ccode1 %in% vertices1) %>%
  distinct()

northsouth$ccode1 <- countrycode(northsouth$country, "country.name", "cown")
ns_sub1 <- subset(northsouth, select = c("country","ccode1","global_south"), 
                 subset = ccode1 %in% vertex_list1$ccode1)

vertex_list1 <- full_join(vertex_list1, ns_sub1[,2:3])
vertex_list <- distinct(vertex_list)

vertex_list1[vertex_list1$ccode1 == 678, 3] = "Global South"
vertex_list1[vertex_list1$ccode1 == 680, 3] = "Global South"
vertex_list1[vertex_list1$ccode1 == 315, 3] = "Global North"
vertex_list1[vertex_list1$ccode1 == 260, 3] = "Global North"
vertex_list1[vertex_list1$ccode1 == 265, 3] = "Global North"

vertex_list1$region <- countrycode(vertex_list1$ccode1, "cown", "un.region.name")

vertex_list1[vertex_list1$ccode1 == 678, 4] = "Asia"
vertex_list1[vertex_list1$ccode1 == 680, 4] = "Asia"
vertex_list1[vertex_list1$ccode1 == 315, 4] = "Europe"
vertex_list1[vertex_list1$ccode1 == 260, 4] = "Europe"
vertex_list1[vertex_list1$ccode1 == 265, 4] = "Europe"

network.vertex.names(unnet1) <- countrycode(vertex_list1$ccode1, "cown", "country.name")
set.vertex.attribute(unnet1, "ideal", vertex_list1$IdealPointAll.x)
set.vertex.attribute(unnet1, "north_south", vertex_list1$global_south)
set.vertex.attribute(unnet1, "region", vertex_list1$region)

est1_1 <- ergm(unnet1 ~ edges)
summary(est1_1)

est1_2 <- ergm(unnet1 ~ edges + nodecov("ideal") + absdiff("ideal"))
summary(est1_2)

plogis(coef(est1_2)[[1]])
plogis(coef(est1_2)[[1]]+coef(est1_2)[[2]])

est1_3 <- ergm(unnet1 ~ edges + nodematch("north_south"))
summary(est1_3)

plogis(coef(est1_3)[[1]])
plogis(coef(est1_3)[[1]] + coef(est1_3)[[2]])

est1_4 <- ergm(unnet1 ~ edges + nodematch("region"))
summary(est1_4)

est1_5 <- ergm(unnet1 ~ edges + nodecov("ideal") + absdiff("ideal") + 
                 nodematch("north_south") + nodematch("region"))
summary(est1_5)

gof1 <- gof(est1_5)
par(mfrow=c(2,2))
plot(gof1)

stargazer(est1_1,est1_3,est1_4,est1_2,est1_5)

## Working for 2014-19----------------------------------------------------------

high.cor2 <- subset(duplic14_19, subset = cor > 0.75)
edgelist2 <- high.cor2[,1:2]

unnet2 <- network(edgelist2, directed = F, matrix.type= "edgelist")

isolates2 <- sna::isolates(unnet2)
network::delete.vertices(unnet2, isolates2)

vertices2 <- sort(unique(c(high.cor2$ccode1, high.cor2$ccode2)))

vertex_list2 <- agree_all %>% 
  subset(select = c(ccode1, ccode2, IdealPointAll.x, IdealPointAll.y), 
         subset = year == 2019) %>%
  subset(select = c(ccode1, IdealPointAll.x),
         subset = ccode1 %in% vertices2) %>%
  distinct()

northsouth$ccode1 <- countrycode(northsouth$country, "country.name", "cown")
ns_sub2 <- subset(northsouth, select = c("country","ccode1","global_south"), 
                  subset = ccode1 %in% vertex_list2$ccode1)

vertex_list2 <- full_join(vertex_list2, ns_sub2[,2:3])
vertex_list2 <- distinct(vertex_list2)
View(vertex_list2)

vertex_list2[vertex_list2$ccode1 == 345, 3] = "Global North"

vertex_list2$region <- countrycode(vertex_list2$ccode1, "cown", "un.region.name")

vertex_list2[vertex_list2$ccode1 == 345, 4] = "Europe"

network.vertex.names(unnet2) <- countrycode(vertex_list2$ccode1, "cown", "country.name")
set.vertex.attribute(unnet2, "ideal", vertex_list2$IdealPointAll.x)
set.vertex.attribute(unnet2, "north_south", vertex_list2$global_south)
set.vertex.attribute(unnet2, "region", vertex_list2$region)

est2_1 <- ergm(unnet2 ~ edges)
summary(est2_1)

est2_2 <- ergm(unnet2 ~ edges + nodecov("ideal") + absdiff("ideal"))
summary(est2_2)

plogis(coef(est2_2)[[1]]) #0.45
plogis(coef(est2_2)[[1]] + coef(est2_2)[[2]]) #0.73


est2_3 <- ergm(unnet2 ~ edges + nodematch("north_south"))
summary(est1_3)

plogis(coef(est2_3)[[1]]) #0.0019
plogis(coef(est2_3)[[1]] + coef(est2_3)[[2]]) #0.23

est2_4 <- ergm(unnet2 ~ edges + nodematch("region"))
summary(est2_4)

est2_5 <- ergm(unnet2 ~ edges + nodecov("ideal") + absdiff("ideal") + 
                 nodematch("north_south") + nodematch("region"))
summary(est2_5)

gof2 <- gof(est2_5)
par(mfrow=c(2,2))
plot(gof2)

stargazer(est2_1,est2_2,est2_3,est2_4,est2_5)

stargazer(est1_3, est1_2, est1_5, est2_3, est2_2, est2_5)

# Mixing-matrix
unnet1_ns_tab <- mixingmatrix(unnet2, "north_south")
xtable(as.table(unnet1_ns_tab$matrix))

unnet2_ns_tab <- mixingmatrix(unnet1, "north_south")
xtable(as.table(unnet2_ns_tab$matrix))



