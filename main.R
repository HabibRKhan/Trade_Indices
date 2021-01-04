##-----------------------------------------------##

##---RUNNING NZ's FEWS PACKAGE ON NAMIBIA DATA---##

#1. You can use this script after you have run the "Run_Once.R" script once.

#2. This script will read data in the same format as was shared by Elijah with UNSD.
    # If the format changes, this script may not work.

#3. More instructions and comments within the script

##-----------------------------------------------##

# **Save everything (data, this script, other scripts) in one folder
  # Following command will set the working directory same as where this script is saved

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# **Load package "dplyr" for data manipulation

library(dplyr)

# **Load package "FEWS" for Indices calculation

library(FEWS)

# **Create a "notin" operator

"%notin%" <- Negate("%in%")

# **Read Namibia data for 2016-2020

df_orig = read.csv("NA_2016_2020.txt",
                   # **if title is different, change accordingly
                   # **txt and csv file types can be used

                   sep = "\t",
                   # **the text file is tab separated
                   # **if csv, use sep=","

                   colClasses = c(rep("character", 2),
                                  rep("NULL", 7),
                                  rep("character", 2),
                                  "NULL",
                                  rep("character", 6),
                                  rep("numeric", 2),
                                  "character",
                                  "numeric"),
                   # **this command sets data type for each command

                   na.strings='NULL'
                   # **this changes all NULL values to NA
                   )

# **Exclude 0 and NA Net Weight or Value

df_orig = df_orig[df_orig$NetWeight %notin% c(0,NA) & df_orig$CValue %notin% c(0,NA), ] #Use records with valid UV

# **Choose either Imports or Exports
  # Remove "#" sign from the respective command below and run

#df = df_orig[df_orig$Flow=="E", ]
#df = df_orig[df_orig$Flow=="I", ]

# **Optional: remove df_orig if memory space is an issue

#rm(df_orig)

# **Optional: subsetting for a specific commodity

# **For two digit level
#df = df[substr(df$HS, 1, 2) == "01", ]

# **For four digit level
#df = df[substr(df$HS, 1, 4) == "0101", ]

# **For six digit level
#df = df[substr(df$HS, 1, 6) == "010101", ]

# **Create an unit values columns

df$UV = df$CValue/df$NetWeight

# **In the text file, the Year column has a strange name. Change it to "Year"
  # Comment out below line by adding a "#" if not needed

names(df)[names(df)=="ï..Year"] = "Year"

# **Remove outliers. Load the Outliers function

source("Function_Outlier.R")

# **Running the function generates a table with upper and lower bound for each commodity
  # Any unit values outside this range are outliers

outliers = outlier(df, cmdCodeColname="HS", UV_Colname="UV", constant=1.5)

# ** Merge outliers table with the data

df = merge(df, outliers, by = "HS")

# **Remove records falling outside outlier ranges

df = df %>%
  mutate(outlier_indicator= ifelse((df$UV>df$upper) | (df$UV<df$lower) , 1, 0))

df = df[df$outlier_indicator == 0, ]

# **Run FEWS
# **Create a reference period column by combining Year and Month

df$refPeriod = paste0(df$Year, df$Period)

# **FEWS require that the reference period column is numeric

df$refPeriod = as.numeric(df$refPeriod)

# **Below function calculates the Indices

OutputX_NA = FEWS(times = df$refPeriod,
                  logprice = log(df$UV),
                  id = df$HS,
                  # **this is the most important argument
                    # **ideally, the id column will be one which captures "quality"
                    # **for NZ, it's created by combining HS, description, partner, and SU

                  window_length = 12,
                  # **number of periods during which the unique IDs will appear at least twice

                  weight = df$UV*df$NetWeight,
                  splice_pos = "mean",
                  num_cores = NULL)

# **Get indices table

XMPI_NAX = OutputX_NA$fews

# **Write result to working directory

write.table(XMPI_NAX, "Results_FEWS_NA.txt", sep = "\t", row.names = FALSE)
