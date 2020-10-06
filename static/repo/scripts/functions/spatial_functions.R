#============================= Spatial Functions ===============================
#====================== Spatial Functions in Collection ========================

# Notes:
#
## This script contains some functions about spatial analyses which are directly related to estimation stage.
## Some of these functions have specific purposes and should not be used separately.
## Information for some functions is given in its dedicated space.

#===============================================================================
# North arrow function to add maps.
## Taken from Auxiliary Cartographic Functions in R: North Arrow, Scale Bar, and Label with a Leader Arrow, Tanimura, 2007.
northarrow <- function(loc, size, bearing = 0, cols, cex = 1, ...) {
    # Checking arguments.
    if (missing(loc))
        stop("loc is missing")
    if (missing(size))
        stop("size is missing")

    # Default colors are white and black.
    if (missing(cols)) {
        cols <- rep(c("white", "black"), 8)
    }

    # Calculating coordinates of polygons.
    radii <- rep(size/c(1, 4, 2, 4), 4)
    x <- radii[(0:15) + 1] * cos((0:15) * pi/8 + bearing) + loc[1]
    y <- radii[(0:15) + 1] * sin((0:15) * pi/8 + bearing) + loc[2]

    # Drawing polygons.
    for (i in 1:15) {
        x1 <- c(x[i], x[i + 1], loc[1])
        y1 <- c(y[i], y[i + 1], loc[2])
        polygon(x1, y1, col = cols[i])
    }

    # Drawing the last polygon.
    polygon(c(x[16], x[1], loc[1]), c(y[16], y[1], loc[2]), col = cols[16])

    # Drawing letters
    b <- c("E", "N", "W", "S")
    for (i in 0:3) {
        text((size + par("cxy")[1]) * cos(bearing + i * pi/2) + loc[1], (size + par("cxy")[2]) * sin(bearing + i * pi/2) + loc[2], b[i + 1], cex = cex)
    }
}

#===============================================================================
# Scalebar function to add maps.
## Taken from Auxiliary Cartographic Functions in R: North Arrow, Scale Bar, and Label with a Leader Arrow, Tanimura, 2007.
## For an unprojected map in R, we can use the map.scale() function in the maps package. This is applicable to a map created not only by the maps package but also by other cartographic packages. However, in R, there is no scale bar applicable to a projected map. So, use it if your shapefile is projected.
## It should be noted that using scalebar() for a decimal degree or the entire world is not logical because the function cannot convert units and retains them in the unconverted form in the current graphic device. Therefore, if necessary, users must project and coordinate the map appropriately prior to the execution of scalebar().
scalebar <- function(loc, length, unit = "km", division.cex = .8, ...) {
    if (missing(loc))
        stop("loc is missing")
    if (missing(length))
        stop("length is missing")
    x <- c(0, length/c(4, 2, 4/3, 1), length * 1.1) + loc[1]
    y <- c(0, length/(10 * 3:1)) + loc[2]
    cols <- rep(c("black", "white"), 2)
    for (i in 1:4) {
        rect(x[i], y[1], x[i + 1], y[2], col = cols[i])
    }
    for (i in 1:5) {
        segments(x[i], y[2], x[i], y[3])
    }
    labels <- x[c(1, 3)] - loc[1]
    labels <- append(labels, paste(x[5] - loc[1], unit))
    text(x[c(1, 3, 5)], y[4], labels = labels, adj = .5, cex = division.cex)
}

#===============================================================================
# Converts kilometers to miles.
km2ml <- function(km) {
    out <- km * 0.621374
    return(out)
}

#===============================================================================
# Converts miles to kilometers.
ml2km <- function(ml) {
    out <- ml / 0.621374
    return(out)
}

#===============================================================================
# Converts km to degrees by the selected latitude degree. Base latitude is selected as the mean latitude of shapefile.
km2d <- function(km, base.latitude = 38.280479) {
    one.degree.dist <- rdist.earth(matrix(c(0, base.latitude), ncol = 2), matrix(c(1, base.latitude), ncol = 2), miles = FALSE) ## 1 degree longitude distance in kilometers.
    out <- km / one.degree.dist
    return(out)
}

#===============================================================================
# Converts miles to degrees by the selected latitude degree. Base latitude is selected as the mean latitude of shapefile.
ml2d <- function(ml, base.latitude = 38.280479) {
    one.degree.dist <- rdist.earth(matrix(c(0, base.latitude), ncol = 2), matrix(c(1, base.latitude), ncol = 2), miles = TRUE) ## 1 degree longitude distance in miles.
    out <- ml / one.degree.dist
    return(out)
}

#===============================================================================
# Converts degrees to kilometers by the selected latitude degree. Base latitude is selected as the mean latitude of shapefile.
d2km <- function(d, base.latitude = 38.280479) {
    one.degree.dist <- rdist.earth(matrix(c(0, base.latitude), ncol = 2), matrix(c(1, base.latitude), ncol = 2), miles = FALSE) ## 1 degree longitude distance in kilometers.
    out <- d * one.degree.dist
    return(out)
}

#===============================================================================
# Converts degrees to miles by the selected latitude degree. Base latitude is selected as the mean latitude of shapefile.
d2ml <- function(d, base.latitude = 38.280479) {
    one.degree.dist <- rdist.earth(matrix(c(0, base.latitude), ncol = 2), matrix(c(1, base.latitude), ncol = 2), miles = TRUE) ## 1 degree longitude distance in miles.
    out <- d * one.degree.dist
    return(out)
}

#===============================================================================
# Converts radians to degrees.
rad2deg <- function(rad) {
    degree <- (rad * 180) / (pi)
    return(degree)
}

#===============================================================================
# Converts degrees to radians.
deg2rad <- function(deg) {
    radian <- (deg * pi) / (180)
    return(radian)
}

#==================================== END ======================================



