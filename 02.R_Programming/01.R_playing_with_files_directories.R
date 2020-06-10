# Setting working directory

setwd("D:\\\from E\\\ML")

# getting working directory

getwd()

# Listing history objects

ls()

# listing files and directories

list.files()

# removing objects 

rm(a)

#Creating  Directories

dir.create("new_folder")

#Creating files

file.create("new_text_file.txt")
file.create("new_word_file.docx")
file.create("new_csv_file.csv")

sapply(paste0("file", 1:100, ".txt"), file.create)

#Copying a file / folder

file.copy("new_text_file.txt", "D:\\from E\\ML\\new_folder")

# list all files in current directory

list.files()

# list all files in another directory

list.files("D:\\from E\\ML\\new_folder")

list.files("D:\\from E\\ML\\new_folder", recursive = TRUE)
list.files("D:\\from E\\ML\\new_folder", full.names = TRUE, recursive = TRUE)

# list all CSV files non-recursively

list.files(pattern = ".csv")

# list all CSV files recursively through each sub-folder

list.files(pattern = ".csv", recursive = TRUE)


# read in all the CSV files

all_data_frames <- lapply(list.files(pattern = ".csv"), read.csv)

# Open file

file.show("filename.csv")

# Writing lines to the file

writeLines("Hello world",file("new1.csv"))

# stack all data frames together

single_data_frame <- Reduce(rbind, all_data_frames)

# How to get created / modified times and other details about files
# get file snapshot of current directory

snapshot <- fileSnapshot()

# or file snapshot of another directory

snapshot <- fileSnapshot("C:/some/other/directory")

snapshot$info

# File info

file.info("some_file.csv")

# File ctime

file.ctime("C:/path/to/file/some_file.txt")

# File mtime

file.mtime("C:/path/to/file/some_file.txt")

# How to delete files
# delete a file

unlink("some_file.csv")

# delete another file

file.remove("some_other_file.csv")

# delete a directory -- must add recursive = TRUE

unlink("some_directory", recursive = TRUE)

# check if a file exists

file.exists("C:/path/to/file/some_file.txt")

# check if a folder exists

file.exists("C:/path/to/file/some_folder")

# alternatively, check if a folder exists with dir.exists

dir.exists("C:/path/to/file/some_folder")

# How to get the base name of a file

basename("C:/path/to/file.txt")

# How to get the directory name of a file

dirname("D:\\from E\\ML\\new_folder\\new_text_file.txt")

# How to get a file's extension

library(tools)

file_ext("D:\\from E\\ML\\new_folder\\new_text_file.txt") # returns "txt"

file_ext("C:/path/to/file.csv") # returns "csv"

# How to physically open a file
# use shell.exec...

shell.exec("C:/path/to/file/some_file.txt")

# or file.show to launch a file

file.show("C:/path/to/file/some_file.txt")

# How to open a file selection window

file.choose()

# How to move a file

install.packages('filesstrings')
library(filesstrings)

file.move("C:/path/to/file/some_file.txt", "C:/some/other/path")

#Download files

download.file('https://raw.githubusercontent.com/GauthamBest/Training_Data/master/utilities.csv',
              destfile = "D:\\from E\\ML\\new_folder\\utilities.csv")
#rename file

file.rename("utilities.csv","vikas.csv")

#Rename directory

file.rename("vik/","vikas/")

# Remove list of objects from R environment

rm(list =ls())

# will clear all objects includes hidden objects.

rm(list = ls(all.names = TRUE))

#  free up memrory and report the memory usage.

gc()

# check no of sheets in excel

library(readr)
excel_sheets()

