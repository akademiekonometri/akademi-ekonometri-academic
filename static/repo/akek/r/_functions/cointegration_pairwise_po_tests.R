#=========================== PO.Coint.Pairwise.Test ============================
#============= Phillips-Ouliaris (PO) Cointegration Pairwise Tests =============

# Notes:
#
## Function runs the Phillips-Ouliaris (PO) cointegration pairwise tests.
## Phillips-Ouliaris (PO) cointegration test is a 2-step method proposed by Engle and Granger (1987) which is a asymmetric/symmetric test (in this function only the symmetric version is considered). Instead of using Augmented Dickey-Fuller (ADF) unit root test in the second stage, Phillips-Perron (PP) unit root test is used. Deterministic term might be added to cointegration regression (1st step) is specified, but no deterministic term is used in the auxiliary regression (2nd step).
## Function requires a time series (ts) object, either bivariate or multivariate.
## Lag length (for 2nd step) is selected with Lag argument with default as Short. Lag specification is either Short = trunc[4(N/100)^0.25] or Long = trunc[12(N/100)^0.25] where N = Length of series.
## The default for Deterministic is "Constant" which adds only intercept to 1st regression. User can also select "None" and "Trend". Using Deterministic = "Trend" adds constant and trend to to 1st regression.
## The default for LaTeX is FALSE which produces output only for printing.
## All the test results will be on the upper triangle of the output table. The diagonal of the table will be empty.
## The outputs of the function is named as "po.coint.pairwise.results" data frames.

# Usage:
#
# PO.Coint.Pairwise.Test(Data, Lag = "Short", Deterministic = "Constant", LaTeX = FALSE)

#
## Data: Time series (ts) object. It can be bivariate or multivariate. Cannot be univariate.
## Lag: Character. Lag length selection in PO tests. Options are Short and Long.
## Deterministic: Character. Deterministic parts for the cointegration relationship. Options are None, Constant, Trend.
## LaTeX: Logical. Output is corrected for LaTeX.
## To run the function without any argument matching problem, make sure to specify all arguments with their names always (if non-default values are selected).

# Examples:
#
## PO.Coint.Pairwise.Test(Data = data.ts)
## PO.Coint.Pairwise.Test(Data = data.ts, Lag = "Long", Deterministic = "Trend", LaTeX = TRUE)
## PO.Coint.Pairwise.Test(Data = data.ts, Lag = "Long", Deterministic = "None", LaTeX = TRUE)

PO.Coint.Pairwise.Test <- function(Data, Lag = "Short", Deterministic = "Constant", LaTeX = FALSE) {
    # Saves the current working directory for further use.
    WD.temp <- getwd()

    # Checks Data argument.
    if (!is.ts(Data))
        stop("Invalid Data. Please choose a time series (ts) data as input.\n")
    if (is.null(ncol(Data))) ## Data is in vector format and thus univariate.
        stop("Invalid Data. Please choose either a bivariate or multivariate time series (ts) data as input.\n")
    if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
        if (ncol(Data) == 1) ## Data is univariate.
            stop("Invalid Data. Please choose either a bivariate or multivariate time series (ts) data as input.\n")
        ncol <- ncol(Data)
        colname <- colnames(Data)
    }

    # Checks Lag argument.
    if (length(Lag) != 1)
        stop("Invalid Lag. Please choose only one Lag.\n")
    if (!(Lag %in% c("Long", "Short")))
        stop("Invalid Lag. Please choose one of the followings: 1) Short: To select short lag with predefined formula. 2) Long: To select long lag with predefined formula.\n")
    lag <- tolower(Lag)

    # Checks Deterministic argument.
    if (length(Deterministic) != 1)
        stop("Invalid Deterministic. Please choose only one Deterministic.\n")
    if (!(Deterministic %in% c("None", "Constant", "Trend")))
        stop("Invalid Deterministic. Please choose one of the followings: 1) None: For model without intercept and trend. 2) Constant: For a model with intercept only. 3) Trend: For a model with intercept and trend.\n")
    deterministic <- tolower(Deterministic)

    # Checks LaTeX argument.
    if (length(LaTeX) != 1)
        stop("Invalid LaTeX. Please choose only one LaTeX.\n")
    if (!(LaTeX %in% c("TRUE", "FALSE")))
        stop("Invalid LaTeX. Please choose one of the followings: 1) To generate output data frame for LaTeX: TRUE. 2) To generate output data frame for printing: FALSE.\n")

    # Generating the data frame containing the results.
    temp <- as.data.frame(matrix(NA, nrow = ncol, ncol = ncol)) ## Data frame containing the results.
    colnames(temp) <- colname
    rownames(temp) <- colname

    # PO Cointegration tests.
    ## Note that there is no option for adding exogenous variable in PO test.
    for (i in 1:ncol) {
        for (j in 1:ncol) {
            if (j <= i) { ## Leaving the lower triangle of the output data frame empty.
                temp[i, j] <- ""
            }
            if (i < j) {
                ts <- Data[, c(i, j)]
                test <- ca.po(ts, demean = deterministic, lag = lag, type = "Pz") ## Symmetric test.
                test.stat <- test@teststat[1]
                test.decision <- ifelse(test.stat > test@cval[1, 3], "^{***}", ifelse(test.stat > test@cval[1, 2], "^{**}", ifelse(test.stat > test@cval[1, 1], "^{*}", "")))
                test.stat <- format(round(test@teststat[1], 2), nsmall = 2)
                po.result <- paste0("$", test.stat, test.decision, "$")
                temp[i, j] <- po.result

            }
        }
    }

    # Saving the results as main data frame.
    main <- temp

    # Correcting the results if not used for LaTeX.
    if (LaTeX == FALSE) {
        for (i in 1:ncol(main)) {
            main[, i] <- gsub("(\\$)|(\\^\\{)|(\\})", "", main[, i])
        }
    }

    # Output data will be named as "po.coint.pairwise.results"
    assign("po.coint.pairwise.results", main, envir = globalenv())

    # Revert the working directory to initial path.
    setwd(WD.temp)
}

#==================================== END ======================================
