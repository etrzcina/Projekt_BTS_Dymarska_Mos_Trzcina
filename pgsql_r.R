#install.packages("RPostgreSQL")
#install.packages("dbscan")
require("RPostgreSQL")
require("dbscan")

# load the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# create a connection to the postgres database
con <- dbConnect(drv, dbname = "tele",
                host = "localhost", port = 5432,
                user = "user", password = "user")

# check for the transmitters
dbExistsTable(con, "transmitters")
dbListConnections(drv)
dbGetInfo(drv)
summary(con)

# get coordinates
xy <- dbSendQuery(con,"select x, y from transmitters")
xy <- fetch(xy, n=-1)
x <- as.matrix(xy[,1:2])

# dbscan
klastry <- dbscan(x, eps=5000, minPts = 20)

# plot result
plot(x, col = klastry$cluster + 1L, pch = 16)
