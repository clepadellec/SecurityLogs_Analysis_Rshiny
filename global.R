####################################################################
################### Installation des dépendances ###################
####################################################################

list.of.packages <-
  c(
    "shiny",
    "shinydashboard",
    "shinyWidgets",
    "dplyr",
    "plotly",

    "RColorBrewer",
    "ggplot2",
    "igraph",
    "ggraph",
    
    "sqldf",
    "reshape2",
    "fastDummies",
    "ape",
    "httr",
    "htmltools"
    
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)


rm(list=ls(all=TRUE))
setwd("C:/code_challenge_secu/code_2_0/code_2_0")
#setwd("C:\\Users\\rpic\\Documents\\PHYTEM\\2020-2021\\Cours\\Data Mining et Sécurité Informatique\\Challenge\\code_Romain")


##################################################################
################### Pré traitement des données ###################
##################################################################
# 
# # Chargement des données 
data <- read.table("data/firewall_secu.log",sep=";",header = TRUE,nrows=50000)

# data$ipsrc <- as.factor(data$ipsrc


ports=(data %>% group_by(dstport) %>% count(sort=T))$dstport
port_list <- ports[1:10]

data$dstport <- as.character(data$dstport)

analyse<-sqldf("select ipsrc, count(*) as nombre, 
    count(distinct dstport) as cndstport,
    sum( case when action like 'Permit' then 1 else 0 ENd) as permit,
    sum( case when action like 'Permit' and dstport < 1024 then 1 else 0 ENd) as inf1024permit,
    sum( case when action like 'Permit' and dstport >= 1024 then 1 else 0 ENd) as sup1024permit,
    sum( case when action like 'Permit' and (dstport = 21 OR dstport = 22 OR dstport = 3389 OR dstport = 3306) 
    then 1 else 0 ENd) as adminpermit,
    sum(case when action like 'Deny' then 1 else 0 ENd) as deny,
    sum(case when action like 'Deny' and dstport < 1024 then 1 else 0 ENd) as inf1024deny,
    sum(case when action like 'Deny' and dstport >= 1024 then 1 else 0 ENd) as sup1024deny,
    sum( case when action like 'Deny' and (dstport = 21 OR dstport = 22 OR dstport = 3389 OR dstport = 3306) 
    then 1 else 0 ENd) as admindeny from data group by ipsrc ")

rownames(analyse)<- analyse$ipsrc
analyse <- analyse[,-1]

data$datetime <-strptime(data$datetime, "%Y-%m-%d %H:%M:%S")
data$datetime_month<-format(data$datetime, format = "%m")
data$datetime_year<-format(data$datetime, format = "%Y")
data$datetime_day<-format(data$datetime, format = "%d")
data$datetime_hour<-data$datetime$hour
data$datetime_min<-data$datetime$min
data$datetime_year<- as.numeric(data$datetime_year)
data$datetime_day<- as.numeric(data$datetime_day)
data$datetime_month<- as.numeric(data$datetime_month)

data$ipsrc<-as.character(data$ipsrc)
