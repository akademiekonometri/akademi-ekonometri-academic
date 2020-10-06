#============================ Estimation Functions =============================
#===================== Estimation Functions in Collection ======================

# Notes:
#
## This script contains some functions which are directly related to estimation stage.
## Some of these functions have specific purposes and should not be used separately.
## Information for some functions is given in its dedicated space.

#================================= SummaryR.lm =================================
#==================== Summary of OLS with Robust Std Erros =====================

# Notes:
#
## Summary for OLS with different type of robust std errors.

# Usage:
#
# SummaryR.lm(model, type = c("hc3", "hc0", "hc1", "hc2", "hc4"), ...)
#
## model: Name of the model estimated with lm() function.
## type: Type of the robust std error calculation. See hccm() function in "car" pacakge.
## ...: Arguments passed from other functions.

# Examples:
#
## SummaryR.lm(my.model, type = "hc4")
## SummaryR.lm(your.model, type = "hc1")

SummaryR.lm <- function(model, type = c("hc3", "hc0", "hc1", "hc2", "hc4"), ...) {
    if (!require(car)) stop("Required car package is missing.")
    type <- match.arg(type)
    V <- hccm(model, type = type)
    sumry <- summary(model)
    table <- coef(sumry)
    table[, 2] <- sqrt(diag(V))
    table[, 3] <- table[,1]/table[, 2]
    table[, 4] <- 2 * pt(abs(table[, 3]), df.residual(model), lower.tail = FALSE)
    sumry$coefficients <- table
    p <- nrow(table)
    hyp <- cbind(0, diag(p - 1))
    sumry$fstatistic[1] <- linearHypothesis(model, hyp, white.adjust = type)[2,"F"]
    print(sumry)
    cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
}

#===============================================================================
# Percentage increase/decrease for log variables. The variable should be in logs. This function makes sure that the raw variable of interest increases/decrease, 10% i.e., while you are dealing with the log form.
log.percent <- function(Variable, Percent) {
    Variable.New <- Variable + log(1 + Percent/100)
    return(Variable.New)
}

#===============================================================================
# Lag function with padding.
lagpad <- function(x, k) {
    if (!is.vector(x))
        stop('x must be a vector')
    if (!is.numeric(x))
        stop('x must be numeric')
    if (!is.numeric(k))
        stop('k must be numeric')
    if (1 != length(k))
        stop('k must be a single number')
    c(rep(NA, k), x)[1:length(x)]
}

#===============================================================================
# Takes the difference of specified columns. Takes only ts (time-series) object as input.
## Ex: Diff.Col(data.ts, c("SP.Shiller", "CL", "57"), Output = "data.pdiff")
Diff.Col <- function(Data, Diff.ColNames, Output) {
    temp.level <- Data[-1, colnames(Data)[!(colnames(Data) %in% Diff.ColNames)], drop = FALSE]
    temp.diff <- diff(Data[ , colnames(Data)[colnames(Data) %in% Diff.ColNames], drop = FALSE], lag = 1, diff = 1)
    main <- cbind(temp.level, temp.diff)
    if ((ncol(Data) - length(Diff.ColNames)) == 1) {
        colnames(main)[1] <- c(paste0(colnames(main)[1], ".", colnames(Data)[!(colnames(Data) %in% Diff.ColNames)]))
    }
    if (length(Diff.ColNames) == 1) {
        colnames(main)[ncol(main)] <- c(paste0(colnames(main)[ncol(main)], ".", Diff.ColNames))
    }
    colnames(main) <- gsub("(temp.level.)|(temp.diff.)", "", colnames(main))
    main <- main[, colnames(Data)]
    assign(Output, main, envir = globalenv()) ## Output data will be named as "data.ts".
}

#===============================================================================
# Takes the difference of the given time series data and pads it with the selected lag length while maintaining the attributes of the input data.
pad.diff <- function(x, lag = 1) {
    output <- rbind(rep(NA, lag), diff(x, lag = lag, diff = 1))
    attributes(output) <- attributes(x)
    return(output)
}

#===============================================================================
# Takes the difference of the given time series data without padding it while maintaining the attributes of the input data.
nopad.diff <- function(x) {
    output <- diff(x, lag = 1, diff = 1)
    attributes(output) <- attributes(x)
    return(output)
}

#===============================================================================
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

#==================================== END ======================================
