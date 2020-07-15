
# Load Libraries ----------------------------------------------------------

library(marmap)
library(lattice)

# Map Buoys ---------------------------------------------------------------

# read in xyz data
socal <- read.bathy(xyz = "socal_2.xyz", header = FALSE, sep = " ")

# inital map of bathy data
plot(socal,image = TRUE, drawlabels = TRUE)

# create data and dataframe for san clemente basin buoy on map
station <- paste("station", c(46086, 46221), sep = " ")

x <- c(-118.052, -118.634)
y <- c(32.499, 33.855)
z <- c("red", "green")

buoy_location <- data.frame(x, y, z, station)

# plot san clemente buoy on map
points(buoy_location$x, buoy_location$y, pch = 21, col = "black",
       bg = as.character(buoy_location$z), cex = 2)

legend(x = "topright", 
       legend = c("San Clemente Basin (46086)", "Santa Monica Bay (46221)"),
       pch = 19,
       col = c("red", "green"),
       cex = .60)

# plot depth profile from san clemente basin buoy to shore
trsect <- get.transect(socal, -118, 32, -117, 33, distance = TRUE)
marmap::plotProfile(trsect)

# 3d plot of socal region
wireframe(unclass(socal), 
          shade = TRUE, 
          aspect = c(1, 0.1), 
          zlab = "", xlab = "", ylab = "",
          scales = list(draw = FALSE))
