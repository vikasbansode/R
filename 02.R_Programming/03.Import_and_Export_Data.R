
# Set working Directory

setwd("D:\\from E\\R")

# Import Data

# Method 1

data <- read.csv(file.choose(),header = T,sep = "\t")
head(data)

# Method 2

data1 <- read.csv("LungCapData.txt",header = T,sep = "\t")
head(data1)

# Method 3

data2 <- read.table("LungCapData.txt",header = T,sep = "\t")
head(data2)

# Importing Data from Excel

library(readxl)
# Check sheets if you don't know

excel_sheets("meat.xls")
meat <- read_excel("meat.xls",sheet =  "Sheet1")
head(meat)

# Export Data from R

write.csv(df,file = "emp_data.csv",sep = ",",row.names = FALSE)
write.csv(meat,file = "mat.csv",row.names = FALSE)

write.csv()
write.csv2()
write.dcf()
write.ftable()
write.socket()
write.table()

