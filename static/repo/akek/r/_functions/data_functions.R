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

#========================= Cubic.Spline.interpolation ==========================
#=============== Interpolates Selected Columns with Cubic Spline ===============

# Notes:
#
## Interpolates the selected columns with cubic spline interpolation.
## Aim is to interpolate the values between two entries.
## Argument maxgap is internally selected with the below code to interpolate all missing values.
## na.rm = FALSE is selected since other NA values should be kept, if there is any.

# Usage:
#
# Cubic.Spline.Interpolation(Data, Column.Names)
#
## Data: Data as in data.frame class.
## Column.Names: Name of the columns to be interpolated.

# Examples:
#
## Cubic.Spline.Interpolation(data, c("Open", "Close"))
## Cubic.Spline.Interpolation(data, c("Open", "High", "Low", "Close"))

Cubic.Spline.Interpolation <- function(Data, Column.Names) {
    # Checks Data argument.
    if (!is.data.frame(Data))
        stop("Invalid Data. Please choose a data.frame object for Data.\n")

    # Checks Column.Names argument.
    if (ncol(Data) < length(Column.Names))
        stop("Invalid Column.Names. Please choose a Column.Names object with lower length then number of columns in Data.\n")
    if (sum(!(Column.Names %in% colnames(Data))) != 0)
        stop(paste0("Invalid Column.Names. ", paste0(dQuote(Column.Names[!(Column.Names %in% colnames(Data))]), collapse = " and "), " columns does not exists in Data.\n"))

    # Storing the original colnames.
    col.names <- colnames(Data)

    # Interpolating.
    for (j in 1:length(Column.Names)) {
        k <- Column.Names[j]
        if (!(is.na(Data[1, k]))) {
            Data$count[1] <- ifelse(is.na(Data[1, k]) == TRUE, 1, 0)
            for (i in 2:nrow(Data)) {
                Data$count[i] <- ifelse(is.na(Data[i, k]) == TRUE, Data$count[i - 1] + 1, 0)
            }
        }
        if (is.na(Data[1, k])) {
            Data$count <- NA
            Data$count[as.numeric(rownames(Data[(is.na(Data[, k]) == FALSE),])[1])] <- ifelse(is.na(Data[as.numeric(rownames(Data[(is.na(Data[, k]) == FALSE),])[1]), k]) == TRUE, 1, 0)
            for (i in (as.numeric(rownames(Data[(is.na(Data[, k]) == FALSE),])[1]) + 1):nrow(Data)) {
                Data$count[i] <- ifelse(is.na(Data[i, k]) == TRUE, Data$count[i - 1] + 1, 0)
            }
        }
        max <- max(Data$count, na.rm = TRUE)
        Data <- Data[, -(grep("count", names(Data)))]
        variable <- Data[, k]
        if (j == 1) {
            main <- data.frame(na.spline(variable, maxgap = max, na.rm = FALSE))
            main <- cbind(Data[, colnames(Data)[!(colnames(Data) %in% Column.Names)]], main[, 1])
            colnames(main)[ncol(main)] <- k
        } else {
            temp <- data.frame(na.spline(variable, maxgap = max, na.rm = FALSE))
            main <- cbind(main, temp[, 1])
            colnames(main)[ncol(main)] <- k
        }
    }
    Data <- main[, col.names]

    # Naming and extracting the Data file to the general environment.
    assign("data.interpolate", Data, envir = globalenv()) ## Output data will be named as "data.interpolate".
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
