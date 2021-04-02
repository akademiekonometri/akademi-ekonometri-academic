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

#===============================================================================
# This function adds text to the specified places (using compas style) of a ggplot.
## For more information see: https://stackoverflow.com/questions/47916307/specify-position-of-geom-text-by-keywords-like-top-bottom-left-right

# Example:
#
## g + annotation_compass(label = "sometext", position = "NE")
## You can always use:
## 1. g + geom_text(data=data.frame(), aes(label = "sometext", x = -Inf, y = Inf), hjust = 0, vjust = 1)
## 2. g + ggplot2::annotate(geom = "text", label = "sometext", x = Inf, y = Inf, hjust = 1, vjust = 1, size = 24)
annotation_compass <- function(label,
                               position = c('N','NE','E','SE','S','SW','W','NW'),
                               padding = grid::unit(c(0.5,0.5),"line"), ...){
    position <- match.arg(position)
    x <- switch (position,
                 N = 0.5,
                 NE = 1,
                 E = 1,
                 SE = 1,
                 S = 0.5,
                 SW = 0,
                 W = 0,
                 NW = 0
    )
    y <- switch (position,
                 N = 1,
                 NE = 1,
                 E = 0.5,
                 SE = 0,
                 S = 0,
                 SW = 0,
                 W = 0.5,
                 NW = 1
    )
    hjust <- switch (position,
                     N = 0.5,
                     NE = 1,
                     E = 1,
                     SE = 1,
                     S = 0.5,
                     SW = 0,
                     W = 0,
                     NW = 0
    )
    vjust <- switch (position,
                     N = 1,
                     NE = 1,
                     E = 0.5,
                     SE = 0,
                     S = 0,
                     SW = 0,
                     W = 0.5,
                     NW = 1
    )
    f1 <- switch (position,
                  N = 0,
                  NE = -1,
                  E = -1,
                  SE = -1,
                  S = 0,
                  SW = 1,
                  W = 1,
                  NW = 1
    )
    f2 <- switch (position,
                  N = -1,
                  NE = -1,
                  E = 0,
                  SE = 1,
                  S = 1,
                  SW = 1,
                  W = 0,
                  NW = -1
    )
    annotation_custom(grid::textGrob(label,
                                     x=grid::unit(x,"npc") + f1*padding[1] ,
                                     y=grid::unit(y,"npc") + f2*padding[2],
                                     hjust=hjust,vjust=vjust, ...))
}

#==================================== END ======================================



