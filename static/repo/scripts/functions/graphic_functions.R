#============================= Graphic Functions ===============================
#====================== Graphic Functions in Collection ========================

# Notes:
#
## This script contains some functions about graphics which are directly related to estimation stage.
## Some of these functions have specific purposes and should not be used separately.
## Information for some functions is given in its dedicated space.

#===============================================================================
# This function is necessary for extracting the legend from ggplot2 plots.
g_legend <- function(a.gplot) {
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

#==================================== END ======================================



