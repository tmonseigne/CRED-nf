library(data.table)

id <- c("1","2","3")
l <- list(1,c(10,20,30),c(2,3))

data.table("n" =id, "v" = l) # Create a table

