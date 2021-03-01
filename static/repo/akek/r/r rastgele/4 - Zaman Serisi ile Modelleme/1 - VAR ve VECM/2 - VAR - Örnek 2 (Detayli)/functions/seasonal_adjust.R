#============================= Akademi Ekonometri ==============================
#============================== Seasonal.Adjust ================================
#========================== Seasonally Adjusts Data ============================

# Notes:
#
## Function tests the seasonality with X-13ARIMA-SEATS method and creates two data sets which are season.pvalues (pvalues for QS tests) and season.results (decision of QS tests) as default. Also, if desired, function can adjust the original data with the above mentioned method, if and only if the QS test with the selected significance level rejects the null hypothesis (no seasonality) in the original data and does not reject the null after the data is seasonally adjusted. This is done to make sure seasonality is necessary and useful.
## Function requires a time series (ts) object.
## Significance.Level is the significance level which is used in all QS seasonality tests.
## The default for Adjust argument is FALSE which does not adjusts the original data with X-13ARIMA-SEATS seasonal adjustment. If user is sure that there is seasonality in at least one series (check season.results and season.pvalues files generated with this function), TRUE for Adjust should be used.

# Usage:
#
# Seasonal.Adjust(Data, Significance.Level = 5, Adjust = FALSE)
#
## Data: Time series (ts) object. It can be multivariate or univariate.
## Significance Level: Significance level for all QS seasonality tests.
## Adjust: Adjusts the original data with seasonality.
## To run the function without any argument matching confusion make sure to specify Significance.Level and Convert arguments with their names always (if non-default values are selected).

# Examples:
#
## Seasonal.Adjust(data.ts, 5)
## Seasonal.Adjust(data.ts, Significance.Level = 1)
## Seasonal.Adjust(data.ts, Significance.Level = 5, Adjust = TRUE)

Seasonal.Adjust <- function(Data, Significance.Level, Adjust = FALSE) {
    # Saves the current working directory for further use.
    WD.temp <- getwd()

    # Checks Data argument.
    if (!is.ts(Data))
        stop("Invalid Data. Please choose a time series (ts) data as input.\n")
    if (is.null(ncol(Data))) { ## Data is in ts vector format.
        ncol <- 1
        colname <- "Univariate Series"
    }
    if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
        ncol <- ncol(Data)
        colname <- colnames(Data)
    }

    if (frequency(Data) == 1)
        stop("X-13ARIMA-SEATS seasonal adjustment does not allow data frequency equals to 1 (yearly data).\n")

    if (frequency(Data) > 12)
        stop("X-13ARIMA-SEATS seasonal adjustment does not allow data frequency more than 12 (monthly data).\n")

    # Checks Significance.Level argument.
    if (length(Significance.Level) != 1)
        stop("Invalid Significance.Level. Please choose only one Significance.Level.\n")
    if (!is.numeric(Significance.Level))
        stop("Invalid Significance.Level. Please choose a numeric Significance.Level value.\n")
    if (!(Significance.Level %in% c(1, 5, 10)))
        stop("Invalid Significance.Level. Please choose one of the followings: 1) For %0.01: 1. 2) For %0.05: 5. 3) For %0.1: 10.\n")
    s.level <- Significance.Level/100 ## Significance level for manually written regressions.

    # Checks Adjust argument.
    if (length(Adjust) != 1)
        stop("Invalid Adjust. Please choose only one Adjust.\n")
    if (!(Adjust %in% c("TRUE", "FALSE")))
        stop("Invalid Adjust. Please choose one of the followings: 1) To seasonally adjust the date with X-13ARIMA-SEATS automatic adjustment: TRUE. 2) To leave data as is: FALSE.\n")

    # Changing the working directory and loading x13binary.
    setwd("./functions/") ## This is where X13 binary is stored.
    Sys.setenv(X13_PATH = "./x13binary/bin")

    # Generating the data frame containing the p-values.
    pvalues <- data.frame(Variable = rep(NA, ncol), Ori = rep(NA, ncol), S.Adj = rep(NA, ncol), stringsAsFactors = FALSE)
    for (i in 1:ncol) {
        pvalues$Variable[i] <- colname[i]
        if (ncol > 1) {
            ts <- Data[, colname[i], drop = FALSE]
        }
        if (ncol == 1) {
            ts <- Data
        }
        sadj <- seasonal::seas(ts) ## Automatic version is selected here.
        pvalues$Ori[i] <- seasonal::qs(sadj)[1, 2] ## QS statistic's p-value for original series.
        pvalues$S.Adj[i] <- seasonal::qs(sadj)[4, 2] ## QS statistic's p-value for seasonally adjusted series with extreme-value adjusted.

        # Seasonal adjustment.
        if (Adjust == TRUE) {
            if (seasonal::qs(sadj)[1, 2] <= s.level & seasonal::qs(sadj)[4, 2] > s.level) {
                if (ncol > 1) {
                    Data[, i] <- seasonal::final(sadj)
                }
                if (ncol == 1) {
                    Data <- seasonal::final(sadj)
                }
            }
        }
    }

    # Generating the data frame containing the results.
    season.results <- pvalues
    for (i in 1:nrow(season.results)) {
        for (j in 2:ncol(season.results)) {
            if (season.results[i, j] > s.level) {
                season.results[i, j] <- "NS"
            }
            else {
                season.results[i, j] <- "S"
            }
        }
    }

    # Naming and extracting files.
    assign("season.pvalues", pvalues, envir = globalenv()) ## Output data will be named as "season.pvalues".
    assign("season.results", season.results, envir = globalenv()) ## Output data will be named as "season.results".
    if (Adjust == TRUE) {
        assign("ts.seasonal.adj", Data, envir = globalenv()) ## Output data will be named as "data.ts".
    }

    # Revert the working directory to initial path.
    setwd(WD.temp)
}

#==================================== END ======================================
