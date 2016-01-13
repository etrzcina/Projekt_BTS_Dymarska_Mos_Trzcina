#install.packages("RPostgreSQL")
#install.packages("dbscan")
#install.packages("SDMTools")
#install.packages("flexclust")
#install.packages("FNN")
#install.packages("psych")
#install.packages("stats")
require("stats")
require("psych")
require("FNN")
require("flexclust")
require("SDMTools")
require("RPostgreSQL")
require("dbscan")

# create a connection
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
xy <- dbSendQuery(con,"select x, y from transmitters")
xy <- fetch(xy, n=-1)
x <- as.matrix(xy[,1:2])

#dbscan
klastry <- dbscan(x, eps=1000, minPts = 60, borderPoints=FALSE)

#match number of cluster to each point (station)
p2cl = matrix(c(x[,1], x[,2], klastry$cluster), nrow=263633, ncol=3)

#calculate center of gravity for each cluster
cluster_info_temp <- matrix(, nrow = 0, ncol = 6)
for (i in 1:max(klastry$cluster))
{
 k<-0
 temp = matrix(, nrow = 0, ncol = 2)
 temp2 = matrix(, nrow = 0, ncol = 6)
 for (j in 1:dim(x)[1])
 {
   if (p2cl[j,3]==i)
   {
     temp <- rbind(temp, p2cl[j,1:2])
     k<-k+1
   }
 }
 cog <- COGravity(temp[,1], temp[,2])
 temp2 <- matrix(c(i, k, cog[1], cog[2], cog[3], cog[4]), nrow=1, ncol=6)
 cluster_info_temp <- rbind(cluster_info_temp, temp2)
}
cluster_info = matrix(, nrow = 0, ncol = 6)

#filter clusters (remove having less than 60 stations)
survivor = matrix( , nrow=0, ncol = 3)
for (i in 1:dim(cluster_info_temp)[1])
{
 if (cluster_info_temp[i,2]>60)
 {
   cluster_info <- rbind(cluster_info, cluster_info_temp[i,])
   for (j in 1:dim(p2cl)[1])
   {
     if (p2cl[j, 3]==i)
     {
       survivor <- rbind(survivor, p2cl[j,])
     }
   }
 }
 print(i)
}
xy_centers = matrix(c(cluster_info[,3], cluster_info[,5]), nrow = dim(cluster_info)[1], ncol=2)

#plot clusters and centers of gravity
plot(survivor[,1], survivor[,2], col = survivor[,3], pch = 16)
points(cluster_info[,3],cluster_info[,5], pch = 8, cex=1)

# check for the sites
dbExistsTable(con, "miejscowosci")
dbListConnections(drv)
dbGetInfo(drv)
summary(con)
dane_miejsc <- dbSendQuery(con,"select x, y, naz from miejscowosci")
dane_miejsc <- fetch(dane_miejsc, n=-1)
x_miejsc <- as.matrix(dane_miejsc[,1:2])
naz <- as.matrix(dane_miejsc[,3])

#plot sites
points(x_miejsc, pch=15, cex=1)

#find distance to the nearest site for each cluster
nearest_miejsc <- get.knnx(xy_centers, x_miejsc, k=1, algorithm = "kd_tree")
nearest_nr <- do.call(rbind, nearest_miejsc[1])
nearest_dist <- do.call(rbind, nearest_miejsc[2])
nearest_pairs <- matrix(, nrow = 0, ncol = 2)
for (i in 1:dim(cluster_info)[1])
{
 nearest = 10000000
 for (j in 1:dim(nearest_nr)[1])
 {
   if (nearest_nr[j,1]==i && nearest_dist[j,1] < nearest)
   {
     nearest <- nearest_dist[j,1]
   }
 }
 temp3 <- matrix(c(i, nearest), nrow=1, ncol=2)
 nearest_pairs <- rbind(nearest_pairs, temp3)
}
for (i in 1:dim(nearest_pairs)[1])
{
 if (nearest_pairs[i,2]==10000000)
 {
  nearest_pairs[i,2] <- NA
 }
}

#statistics
srednia <- colMeans(nearest_pairs, na.rm = TRUE, dims = 1L)[2]
srednia
maksymalna <- max(nearest_pairs[,2], na.rm = TRUE, dims = 1L)
maksymalna
minimalna <- min(nearest_pairs[,2], na.rm = TRUE, dims = 1L)
minimalna
odchylenie <- SD(nearest_pairs, na.rm = TRUE)[2]
odchylenie
mediana <- median(nearest_pairs[,2], na.rm=TRUE)
mediana
