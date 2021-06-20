#FINDING SPEARMAN COORELATION - VOTING SIMILARITY EDGELIST

library(tidyverse)
library(igraph)
library(countrycode)
library(httr)
library(ggplot2)
library(countrycode)
library(unvotes)
library(readr)


setwd("/Users/egemenbuyukkaya/Desktop/R Directory/Project_Network")
rm(list = ls())

load("~/Desktop/R Directory/Project_Network/AgreementScores.Rdata")
load("~/Desktop/R Directory/Project_Network/UNVotes-1.RData")

votes_all <- completeVotes
agree_all <- dfAgree

# Getting the data between 1980 and 1985----------------------------------------
vote_2019 <- subset(votes_all, select = c("rcid","ccode","vote"), 
                    subset = (year>1979) & (year<1986))
accept_vote <- 1:3
vote_2019 <- subset(vote_2019, subset = vote %in% accept_vote)
rcid_array <- unique(vote_2019$rcid)

node_list <- data.frame(ccode = unique(vote_2019$ccode), name = countrycode(unique(vote_2019$ccode), "cown", "country.name"))

node_list[node_list$ccode == 260, ] = "German Federal Republic" #generating final vertex list

agree_2020 <- subset(agree_all, 
                     select = c("ccode1", "ccode2", "agree", "IdealPointDistance", "Countryname.x", "Countryname.y"),
                     subset = year == 1985)

agree_2020 <- agree_2020[!duplicated(agree_2020$IdealPointDistance),] #generating all possible dyads,

dyads <- agree_2020[,1:2]

get_correlation <- function(ccode1, ccode2) {
  country1 <- subset(vote_2019, subset = ccode == ccode1)
  colnames(country1) <- c("rcid","country1","vote1")
  country2 <- subset(vote_2019, subset = ccode == ccode2)
  colnames(country2) <- c("rcid","country2","vote2")
  dyadic_unit <- full_join(country1, country2, by = "rcid")
  dyadic_unit$vote1 <- as.factor(dyadic_unit$vote1)
  dyadic_unit$vote2 <- as.factor(dyadic_unit$vote2)
  dyadic_unit <- na.omit(dyadic_unit)
  cor_dyad <- cor(as.numeric(dyadic_unit$vote1), as.numeric(dyadic_unit$vote2), method = "spearman")
  return(as.numeric(cor_dyad))
}

corr_matrix <- matrix(ncol = 3, nrow = dim(dyads)[[1]])
for (i in 1:dim(dyads)[[1]]) {
  corr_matrix[i,1] <- dyads$ccode1[[i]]
  corr_matrix[i,2] <- dyads$ccode2[[i]]
  corr_matrix[i,3] <- get_correlation(dyads$ccode1[[i]],dyads$ccode2[[i]])
}
corr_matrix

View(corr_matrix)
corr80_85 <- as.data.frame(corr_matrix)

colnames(corr80_85) <- c("ccode1","ccode2","cor")


duplic80_85 <- full_join(corr80_85, 
                    subset(agree_2020, 
                           select = c("ccode1","ccode2","IdealPointDistance"), 
                           by = c("ccode1", "ccode2"))
)

duplic80_85 <- duplic80_85[!duplicated(duplic80_85$IdealPointDistance),]

write_csv(duplic80_85, file = "duplic80_85.csv")

duplic80_85 <- read.csv("duplic80_85.csv")

high_cor80_85 <- subset(duplic80_85, subset = cor > 0.6)

nodes.spearman80_85 <- sort(unique(c(high_cor80_85$ccode1, high_cor80_85$ccode2)))

graph_spearman80_85 <- graph_from_data_frame(d=high_cor80_85[,1:2], 
                                        vertices=nodes.spearman80_85, 
                                        directed=F)

regions <- as.factor(countrycode(nodes.spearman80_85, "cown", "un.region.name"))
my_pal <- hcl.colors(n=length(unique(regions)), palette = "viridis", alpha = 0.7)
my_color <- my_pal[as.numeric(as.factor(regions))]

plot(graph_spearman80_85,
     vertex.label.cex = 0.3,
     vertex.size = log(degree(graph_spearman80_85)),
     vertex.label = countrycode(nodes.spearman80_85, "cown", "country.name"),
     vertex.color = my_color,
     edge.width = (high_cor80_85$cor)^2,
     vertex.label.color = "black",
     vertex.label.font=2,
     vertex.frame.color = my_color,
     legend = TRUE,
     main= paste("Period 1980-85, Voting Correlation > 0.6"))

# Getting the data between 2014 and 2019----------------------------------------

vote_new<- subset(votes_all, select = c("rcid","ccode","vote"), 
                    subset = (year>2013) & (year<2020))
accept_vote <- 1:3
vote_new <- subset(vote_new, subset = vote %in% accept_vote)
rcid_array <- unique(vote_new$rcid)

node_list_new <- data.frame(ccode = unique(vote_new$ccode), name = countrycode(unique(vote_new$ccode), "cown", "country.name"))

View(node_list) #look for missing values

#node_list[node_list$ccode == 260, ] = "German Federal Republic" #generating final vertex list

agree_new <- subset(agree_all, 
                     select = c("ccode1", "ccode2", "agree", "IdealPointDistance", "Countryname.x", "Countryname.y"),
                     subset = year == 2019)

agree_new <- agree_new[!duplicated(agree_new$IdealPointDistance),] #generating all possible dyads,

dyads_new <- agree_new[,1:2]

get_correlation_new <- function(ccode1, ccode2) {
  country1 <- subset(vote_new, subset = ccode == ccode1)
  colnames(country1) <- c("rcid","country1","vote1")
  country2 <- subset(vote_new, subset = ccode == ccode2)
  colnames(country2) <- c("rcid","country2","vote2")
  dyadic_unit <- full_join(country1, country2, by = "rcid")
  dyadic_unit$vote1 <- as.factor(dyadic_unit$vote1)
  dyadic_unit$vote2 <- as.factor(dyadic_unit$vote2)
  dyadic_unit <- na.omit(dyadic_unit)
  cor_dyad <- cor(as.numeric(dyadic_unit$vote1), as.numeric(dyadic_unit$vote2), method = "spearman")
  return(as.numeric(cor_dyad))
}

corr_matrix_new <- matrix(ncol = 3, nrow = dim(dyads_new)[[1]])
for (i in 1:dim(dyads_new)[[1]]) {
  corr_matrix_new[i,1] <- dyads_new$ccode1[[i]]
  corr_matrix_new[i,2] <- dyads_new$ccode2[[i]]
  corr_matrix_new[i,3] <- get_correlation_new(dyads_new$ccode1[[i]],dyads_new$ccode2[[i]])
}
corr_matrix_new

View(corr_matrix_new)
corr14_19 <- as.data.frame(corr_matrix_new)

colnames(corr14_19) <- c("ccode1","ccode2","cor")


duplic14_19 <- full_join(corr14_19, 
                         subset(agree_new, 
                                select = c("ccode1","ccode2","IdealPointDistance"), 
                                by = c("ccode1", "ccode2"))
)

duplic14_19 <- duplic14_19[!duplicated(duplic14_19$IdealPointDistance),]

write_csv(duplic14_19, file = "duplic14_19.csv")

duplic14_19 <- read.csv("duplic14_19.csv")

high_cor14_19 <- subset(duplic14_19, subset = cor > 0.75)

nodes.spearman14_19 <- sort(unique(c(high_cor14_19$ccode1, high_cor14_19$ccode2)))

graph_spearman14_19 <- graph_from_data_frame(d=high_cor14_19[,1:2], 
                                             vertices=nodes.spearman14_19, 
                                             directed=F)

regions_new <- as.factor(countrycode(nodes.spearman14_19, "cown", "un.region.name"))
my_pal_new <- hcl.colors(n=length(unique(regions_new)), palette = "viridis", alpha = 0.7)
my_color_new <- my_pal_new[as.numeric(as.factor(regions_new))]

plot(graph_spearman14_19,
     vertex.label.cex = 0.3,
     vertex.size = log(degree(graph_spearman14_19)),
     vertex.label = countrycode(nodes.spearman14_19, "cown", "country.name"),
     vertex.color = my_color_new,
     edge.width = (high_cor14_19$cor)^2,
     vertex.label.color = "black",
     vertex.label.font=2,
     vertex.frame.color = my_color_new,
     legend = TRUE,
     main= paste("Period 2014-19, Voting Correlation > 0.75"))

comp <- components(graph_spearman14_19)

sub1 <- induced_subgraph(graph_spearman14_19, comp$membership == 2)

names <- as.numeric(V(sub1)$name)
names_label <- countrycode(names, "cown", "country.name")

plot(sub1, vertex.label.cex = 0.3,
     vertex.size = log(degree(sub1)),
     vertex.label = names_label,
     vertex.color = my_color_new,
     edge.width = (high_cor14_19$cor)^2,
     vertex.label.color = "black",
     vertex.label.font=2,
     vertex.frame.color = my_color_new,
     legend = TRUE,
     main= paste("Period 2014-19, Voting Correlation > 0.75"))
)



