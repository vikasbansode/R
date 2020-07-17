library(RPostgres)
library(DBI)


pw <- {
  "vikas@123"
}

con <- dbConnect(RPostgres::Postgres()
                 , host='localhost'
                 , port='5432'
                 , dbname='testdb'
                 , user='postgres'
                 , password=pw)

# Checking tables in database

dbListTables(con)

# writing tables to database

dbWriteTable(con,"mtcars",mtcars)

# making more readable

data("mtcars")
my_data <- data.frame(carname = rownames(mtcars),mtcars,row.names = NULL)
my_data$carname <- as.character(my_data$carname)
rm(mtcars)
dbWriteTable(con,name = 'cars',value = my_data)

# Reading back from db
dbReadTable(con,'cars')

# working with query

dbGetQuery()
dbSendQuery()
dbFetch()

dbGetQuery(con, 'ALTER TABLE cars ADD CONSTRAINT cars_pk PRIMARY KEY ("carname")')

dbReadTable(con,'cars')

dbGetQuery(con,'select * from cars')

# Creating Basic query

cars_query <- dbSendQuery(con,'SELECT carname,cyl,gear FROM cars WHERE cyl >=5 and gear >= 4')
result <-dbFetch(cars_query)
dbClearResult(cars_query)
dbWriteTable(con,'newtable',result)
dbReadTable(con,"newtable")


# Connection must be closed

dbDisconnect(con)
