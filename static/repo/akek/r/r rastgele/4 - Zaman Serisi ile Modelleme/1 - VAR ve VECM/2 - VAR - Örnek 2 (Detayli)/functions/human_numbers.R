#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#=============================== human_numbers =================================
#==================== Convert Number to Human Readable Way =====================

# Notes:
#
## Formatting functions for ggplot graph axis.
## Human Numbers: Format numbers so they're legible for humans. Use this in ggplot for labels where you might use the comma or percent functions from the Scales package.
## Function checks whether numbers are positive or negative. It allows up to 1 significant figure and sapply used for element-wise application of the humanity function as a vector may include numbers where billions, millions or thousands are appropriate.
## It returns a character vector the same length as the input vector.
## These functions are taken from https://github.com/fdryan/R/blob/master/ggplot2_formatter.r

# Usage:
#
## x: a numeric vector to format.
## smbl: a symbol you'd like to prefix your numbers by e.g. "$".
## signif: = the number of significant places you want the function to return.

# Examples:
#
## human_numbers(c(1000000 , 1500000, 10000000000))
## human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
## ggplot2 + scale_y_continuous(labels = human_numbers)
## ggplot2 + scale_x_continuous(labels = human_numbers)
## ggplot2 + scale_x_continuous(labels = human_gbp)

# require(plyr)
# require(scales)
human_numbers <- function(x = NULL, smbl = "", signif = 1) {
    humanity <- function(y) {
        if (!is.na(y)) {
            tn <- round(abs(y) / 1e12, signif)
            b <- round(abs(y) / 1e9, signif)
            m <- round(abs(y) / 1e6, signif)
            k <- round(abs(y) / 1e3, signif)

            if (y >= 0) {
                y_is_positive <- ""
            } else {
                y_is_positive <- "-"
            }
            if (k < 1) {
                paste0(y_is_positive, smbl, round(abs(y), signif))
            } else if (m < 1) {
                paste0(y_is_positive, smbl, k , "k")
            } else if (b < 1) {
                paste0(y_is_positive, smbl, m ,"m")
            } else if (tn < 1) {
                paste0(y_is_positive, smbl, b ,"bn")
            } else {
                paste0(y_is_positive, smbl, comma(tn), "tn")
            }
        } else if (is.na(y) | is.null(y)) {
            "-"
        }
    }
    sapply(x, humanity)
}

## Human versions of large currency numbers - extensible via smbl.
human_num <- function(x) {
    human_numbers(x, smbl = "")
}
human_per <- function(x) {
    human_numbers(x, smbl = "%")
}
human_gbp <- function(x) {
    human_numbers(x, smbl = "£")
}
human_usd <- function(x) {
    human_numbers(x, smbl = "$")
}
human_euro <- function(x) {
    human_numbers(x, smbl = "€")
}
human_tl <- function(x) {
    human_numbers(x, smbl = "TL")
}

#==================================== SON ======================================
