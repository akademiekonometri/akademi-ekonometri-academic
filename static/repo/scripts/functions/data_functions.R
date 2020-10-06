#=============================== Data Functions ================================
#======================== Data Functions in Collection =========================

# Notes:
#
## This script contains some functions which are directly related to data stage but also might be use in estimation stage.
## Some of these functions have specific purposes and should not be used separately.
## Information for some functions is given in its dedicated space.

#================================== Head.Tail ==================================
#================== Selects the Head and Tail of a Data Frame ==================

# Notes:
#
## Shows the first (head) and last (tail) selected items of a data frame.

# Usage:
#
# Head.Tail(x, Select)
#
## x: Data as in data.frame class.
## Select: Number of the first and the last items in the data frame.

# Examples:
#
## Head.Tail(data, 5)
## Head.Tail(data, 10)

Head.Tail <- function(x, Select) {
    if (Select %% 1 != 0)
        stop("Invalid Select. Please choose a whole number as Select.\n")

    rbind(head(x, Select), tail(x, Select))
}

#=============================== Impute.NA.Mean ================================
#===================== Impute Missing Values (NA) with Mean ====================

# Notes:
#
## A function to fill NA values with mean value.

# Usage:
#
# Impute.NA.Mean(x)
#
## x: a numeric vector.

# Examples:
#
## Impute.NA.Mean(my.vector)

Impute.NA.Mean <- function(x) {
    replace(x, is.na(x), mean(x, na.rm = TRUE))
}

#============================== Decimal.Num.Count ==============================
#========================== Number of Decimal Places ===========================

# Notes:
#
## Gives the number of decimal places of a number written as character string or numeric object.

# Usage:
#
# Decimal.Num.Count(x)
#
## x: Number in character or in numeric.

# Examples:
#
## Decimal.Num.Count(12.898978978978)
## Decimal.Num.Count("12.898978978978")

Decimal.Num.Count <- function(x) {
    if (class(x) == "numeric") {
        x <- as.character(x)
    }
    stopifnot(class(x) == "character")
    x <- gsub("(.*)(\\.)|([0]*$)", "", x)
    nchar(x)
}

#============================ Conversion Functions =============================
Dash.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] == "-") { ## Converts "-" into NA.
                temp[, variable][i] <- NA
            }
        }
    }
    return(temp)
}

Dash.Convert.Zero <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] == "-") { ## Converts "-" into "0".
                temp[, variable][i] <- "0"
            }
        }
    }
    return(temp)
}

Zero.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] == 0) { ## Converts "0" into NA.
                temp[, variable][i] <- NA
            }
        }
    }
    return(temp)
}

Null.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] == "null") { ## Converts "null" into NA.
                temp[, variable][i] <- NA
            }
        }
    }
    return(temp)
}

NA.Convert.Zero <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (is.na(temp[, variable][i])) {
            temp[, variable][i] <- "0" ## Converts NA into "0".
        }
    }
    return(temp)
}

Negative.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] < 0) { ## Converts negative value into NA.
                temp[, variable][i] <- NA
            }
        }
    }
    return(temp)
}

NaN.Convert.Zero <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (is.nan(temp[, variable][i])) {
            temp[, variable][i] <- "0" ## Converts NaN into "0".
        }
    }
    return(temp)
}

NaN.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (is.nan(temp[, variable][i])) {
            temp[, variable][i] <- NA ## Converts NaN into NA.
        }
    }
    return(temp)
}


#==================================== END ======================================
