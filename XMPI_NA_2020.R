##---TESTING NZ's FEWS PACKAGE ON NAMIBIA 2020 DATA---##

#Working directory same as where script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)

"%notin%" <- Negate("%in%") #creating notin operator

#Read Namibia data for 2019
df_orig = read.csv("NA_2016_2020.txt", sep = "\t",
                   colClasses = c(rep("character", 2),
                                  rep("NULL", 7),
                                  rep("character", 2),
                                  "NULL",
                                  rep("character", 6),
                                  rep("numeric", 2),
                                  "character",
                                  "numeric"),
                   na.strings='NULL')

df_orig = df_orig[df_orig$NetWeight %notin% c(0,NA) & df_orig$CValue %notin% c(0,NA), ] #Use records with valid UV

##--Regression to determine which columns influence UV most--##
#Imports only
df = df_orig[df_orig$Flow=="I", ]

#Remove main df to save space
rm(df_orig)

df$UV = df$CValue/df$NetWeight
df$HS4 = substr(df$HS, 1, 4)
df$HS2 = substr(df$HS, 1, 2)
#df = df[!is.na(df$UV), ]
df$Trader = as.character(df$Trader)
names(df)[names(df)=="ï..Year"] = "Year"

#data=df[df$Year=="2019" & df$HS4=="8431" & is.finite(df$UV), ]
#regression = lm(UV~Trader+HS+Partner, df[df$ï..Year==2019, ]) #memory fails without subsetting
#regression = lm(UV~Trader,
#                data=data) #memory fails without subsetting

##With car package
library(car)
library(plm)

#Find most frequently traded HS
#f = as.data.frame(table(df$HS))
#hs = f[f$Freq==max(f$Freq),]$Var1

#rm(df, AOV.df, AOV.lm, f, regression, hs, hs2)

#data = df[df$HS==as.character(hs), ]
#write.table(data, "HS39269090.txt", sep = "\t", quote = TRUE, row.names = FALSE)
#data = data[data$Year=="2019", ]
#data = data %>% select("UV", "Trader", "Partner")

data = read.csv("HS39269090.txt", sep="\t")
#dat = data[sample(nrow(data), 20), ]
#dat = dat %>% select("Trader", "Partner", "UV")
#dat$Trader = droplevels(as.factor(dat$Trader))
#dat$Partner = droplevels(as.factor(dat$Partner))
data = data %>% select("Trader", "Partner", "UV")
data$Trader = droplevels(as.factor(data$Trader))
data$Partner = droplevels(as.factor(data$Partner))

#Use MMI function to see if controlling for Trader affects multimodality
source("Function_MMI.R")


model = plm(UV~Trader+Partner,
                data=dat)

model = lm(UV~Trader+Partner,
           data=data)

#Run FEWS
library(FEWS)

#df$Period = sprintf("%02d", df$Period)
df$refPeriod = paste0(df$Year, df$Period)
df$UV = df$CValue/df$NetWeight
#df$UV = as.numeric(df$CValue)/as.numeric(df$NetWeight)
#df = df[!is.na(df$UV), ]

#Change column type to use FEWS function
#df$UnitValNW = as.numeric(df$UnitValNW)
#df$NetWeight = as.numeric(df$NetWeight)
df$refPeriod = as.numeric(df$refPeriod)

#Exports
dfX = df[df$Flow== "E", ]
#dfX = dfX[!is.na(dfX$UnitValNW) & !is.na(dfX$NetWeight), ]

OutputX_NA = FEWS(times = dfX$refPeriod,
                 logprice = log(dfX$UV),
                 id = dfX$HS,
                 window_length = 3,
                 weight = dfX$UV*dfX$NetWeight,
                 splice_pos = "mean",
                 num_cores = NULL)

XMPI_NAX = OutputX_NA$fews

#Exports
dfM = df[df$Flow== "I", ]
dfM = dfM[dfM$refPeriod %in% c(201906,201905,201904,201903,201902,201901), ] #downsizing to handle memory issue
#rm(list=c("df","df_orig", "dfX", "OutputX_NA"))
#dfM = dfM[!is.na(dfM$UnitValNW) & !is.na(dfX$NetWeight), ]

OutputM_NA = FEWS(times = dfM$refPeriod,
                 logprice = log(dfM$UV),
                 id = dfM$HS,
                 window_length = 5,
                 weight = dfM$UV*dfM$NetWeight,
                 splice_pos = "mean",
                 num_cores = NULL)

XMPI_NAM = OutputM_NA$fews
##-------------------------------------##

#rm(list = ls())

##-------------------------------------##
#Example from R-bloggers
n = 30
sigma = 2.0

AOV.df <- data.frame(category = c(rep("category1", n)
                                  , rep("category2", n)
                                  , rep("category3", n)),

                     j = c(1:n
                           , 1:n
                           , 1:n),

                     y = c(8.0  + sigma*rnorm(n)
                           , 9.5  + sigma*rnorm(n)
                           , 11.0 + sigma*rnorm(n))
)

AOV.lm <- lm(y ~ category, data = AOV.df)
summary(AOV.lm)

