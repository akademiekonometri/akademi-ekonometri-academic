#============================ UR.Stationary.Tests ==============================
#====================== Unit Root and Stationary Tests =========================

# Notes:
#
## Function takes the input multivariate/univariate time series object and runs some pre-specified unit root and stationary tests such as, Augmented Dickey Fuller (ADF), Phillips-Perron (PP), Elliott-Rothenberg-Stock (ERS), and Kwiatkowski–Phillips–Schmidt–Shin (KPSS) tests on each univariate time series with the selected lags and significance level.
## For all specifications please see the source code of this function.
## Function requires a time series (ts) object.
## Function runs the tests with the specified lag lengths by tests. The form of the Lags argument should be "list(ADF = c("AIC", "BIC", 2), PP = c("Short", "Long", 3), ERS = c("Short", "Long", 3), KPSS = c("Short", "Long", 4))". AIC and BIC is only for ADF test. Short and Long are for PP, ERS, and KPSS tests. Lag length selection: An important practical issue for the implementation of the ADF, PP, ERS, and KPSS tests is the specification of the lag length p. If p is too small, then the remaining serial correlation in the errors will bias the test. If p is too large, then the power of the test will suffer. Therefore, while running these tests different lag lengths can be used: AIC, BIC, Short, Long, and user selected numeric lag value. For lag selection with Short and Long: In the literature two type of common lags are used for PP, ERS, and KPSS tests: Short = trunc[4(N/100)^0.25] and Long = trunc[12(N/100)^0.25] where N = Length of series.
## Function optionally takes the first difference of the input data and applies the same procedure mentioned above. The default for Diff argument is False.
## Function optionally subsets the results with specified lag selections methods. The form of the Subset argument should be "list(Test = c("ADF", "PP", "KPSS"), Model = c("Constant"), Lag = c("AIC", "Short"))".
## The default for Reshape argument is FALSE which produces a desired output without reshaping the results. If Reshape = TRUE, then the desired output is reshaped with results only.
## The default for LaTeX is FALSE which produces output only for printing.
## The output of the function is named as "ur.stationary.tests.results" data frame for the raw output and "ur.stationary.tests.results.conv" data frame for the reshaped output.

# Usage:
#
# UR.Stationary.Tests(Data, Lags, Diff = FALSE, Subset = NULL, Reshape = FALSE, LaTeX = FALSE)
#
## Data: Time series (ts) object. It can be multivariate or univariate.
## Lags: List. Specified lags by tests in the form of "list(ADF = c("AIC", "BIC", 2), PP = c("Short", "Long", 3), ERS = c("Short", "Long", 3), KPSS = c("Short", "Long", 4))". AIC and BIC is only for ADF test. Short and Long are for PP, ERS, and KPSS tests.
## Diff: Logical. Takes the first difference of all time series.
## Subset: List. Subsets some of the test results with specified test, model and lags in the form of "list(Test = c("ADF", "PP", "KPSS"), Model = c("Constant"), Lag = c("AIC", "Short"))".
## Reshape: Logical. Output is reshaped for better a view.
## LaTeX: Logical. Output is corrected for LaTeX.
## To run the function without any argument matching problem, make sure to always specify all arguments with their names (if non-default values are selected).

# Examples:
#
## UR.Stationary.Tests(Data = data.ts)
## UR.Stationary.Tests(Data = data.ts, Lags = list(ADF = c("AIC", "BIC"), PP = c("Short", "Long"), ERS = c(4), KPSS = c("Short", "Long")))
## UR.Stationary.Tests(Data = data.ts, Subset = list(Test = c("ADF", "PP", "KPSS"), Model = c("Constant"), Lag = c("AIC", "Short")), Reshape = TRUE, LaTeX = FALSE)
## UR.Stationary.Tests(Data = data.ts, Diff = TRUE, Reshape = TRUE, LaTeX = TRUE)

UR.Stationary.Tests <- function(Data, Lags = NULL, Diff = FALSE, Subset = NULL, Reshape = FALSE, LaTeX = FALSE) {
    # Saves the current working directory for further use.
    WD.temp <- getwd()

    # Type of the test and lag selection procedure.
    tests <- list(
        test1 = c("ADF", "Constant"),
        test2 = c("ADF", "Trend"),

        test3 = c("PP", "Constant"),
        test4 = c("PP", "Trend"),

        test5 = c("ERS", "Constant"),
        test6 = c("ERS", "Trend"),

        test7 = c("KPSS", "Constant"),
        test8 = c("KPSS", "Trend")
        )

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

    # Checking the Lags argument.
    if (!is.null(Lags)) {
        if (!is.list(Lags)) {
            stop("Invalid Lags. Please choose a list as Lags.\n")
        }
        if (length(Lags) != 4) {
            stop("Invalid Lags. Please choose a Lags with 4 categories in a list.\n")
        }
        if (sum(names(unclass(Lags)) %in% c("ADF", "PP", "ERS", "KPSS")) != 4) {
            stop("Invalid Lags. Please choose a Lags with 4 unique categories (ADF, PP, ERS, KPSS) in a list.\n")
        }
        lag.unique <- unique(c(unlist(Lags, use.names = FALSE)))
        lag.numeric <- lag.unique[!(lag.unique %in% c("AIC", "BIC", "Short", "Long"))]
        if (sum(is.na(as.numeric(lag.numeric))) != 0 | sum(!(as.numeric(lag.numeric) > 0)) != 0) {
            stop("Invalid Lags. Please choose a Lags with either a positive integer values or pre-specified categories (AIC, BIC, Short, Long) in a list.\n")
        }
    }
    if (is.null(Lags)) {
        Lags <- list(ADF = c("AIC", "BIC"), PP = c("Short", "Long"), ERS = c(4), KPSS = c("Short", "Long")) ## The default Lags.
    }

    # Checks Diff argument.
    if (length(Diff) != 1)
        stop("Invalid Diff. Please choose only one Diff.\n")
    if (!(Diff %in% c("TRUE", "FALSE")))
        stop("Invalid Diff. Please choose one of the followings: 1) To take the first difference of input data: TRUE. 2) To use the input data as is: FALSE.\n")

    # Checks Subset argument.
    if (!is.null(Subset)) {
        if (!is.list(Subset)) {
            stop("Invalid Subset. Please choose a list as Subset.\n")
        }
        if (length(Subset) != 3) {
            stop("Invalid Subset. Please choose a Subset with 3 categories in a list.\n")
        }
        if (sum(names(unclass(Subset)) %in% c("Test", "Model", "Lag")) != 3) {
            stop("Invalid Subset. Please choose a Subset with 4 unique categories (Test, Model, Lag) in a list.\n")
        }
    }

    # Checks Reshape argument.
    if (length(Reshape) != 1)
        stop("Invalid Reshape. Please choose only one Reshape.\n")
    if (!(Reshape %in% c("TRUE", "FALSE")))
        stop("Invalid Reshape. Please choose one of the followings: 1) To reshape output data frame only with results: TRUE. 2) Without reshaping: FALSE.\n")

    # Checks LaTeX argument.
    if (length(LaTeX) != 1)
        stop("Invalid LaTeX. Please choose only one LaTeX.\n")
    if (!(LaTeX %in% c("TRUE", "FALSE")))
        stop("Invalid LaTeX. Please choose one of the followings: 1) To generate output data frame for LaTeX: TRUE. 2) To generate output data frame for printing: FALSE.\n")

    # Running the unit root and stationarity tests with selected arguments for all univariate time series in input data.
    for (i in 1:ncol) {
        if (is.null(ncol(Data))) { ## Data is in ts vector format.
            ts <- Data ## Univariate time series.
        }
        if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
            ts <- Data[, colname[i]] ## Univariate time series.
        }
        if (Diff == TRUE) { ## Taking the first difference.
            ts <- diff(ts, lag = 1, diff = 1)
        }

        ## Some common used lags can be applied which is taken directly from the literature.
        lag.short <- trunc((4 * (length(ts) / 100)^0.25)) ## Short specified lags generally used for PP and KPSS tests.
        lag.long <- trunc((12 * (length(ts) / 100)^0.25)) ## Long specified lags generally used for PP and KPSS tests.

        # Running the Augmented Dickey Fuller, Phillips-Perron, Elliott-Rothenberg-Stock, and KPSS tests with possible specifications.
        for (j in 1:length(tests)) {
            ## Number of unique tests with different lags.
           n.lags <- length(Lags[[tests[[j]][1]]])

            ## Data frame containing the results.
            temp <- data.frame(Variable = rep(NA, n.lags), Test = rep(NA, n.lags), Model = rep(NA, n.lags), Lag = rep(NA, n.lags), Statistic = rep(NA, n.lags), Result = rep(NA, n.lags), stringsAsFactors = FALSE)

            # For different lag selection criteria.
            for (k in 1:n.lags) {
                ## Criteria for the lag selection.
                lag.criteria <- Lags[[tests[[j]][1]]][k]

                ## Filling same variables.
                temp$Variable[k] <- colname[i]
                temp$Test[k] <- tests[[j]][1]
                temp$Model[k] <- tests[[j]][2]

                # Augmented Dickey Fuller Tests:
                if (tests[[j]][1] == "ADF") {
                    ## AIC and BIC lag selection.
                    if (lag.criteria %in% c("AIC", "BIC")) {
                        lag <- lag.short
                        test <- ur.df(ts, type = ifelse(tests[[j]][2] == "Constant", "drift", "trend"), lags = lag, selectlags = lag.criteria) ## lag.short is used for maximum number of lags in AIC or BIC lag selection process.
                        temp$Lag[k] <- paste0(lag.criteria)
                    }
                    ## Short and Long lag selection.
                    if (lag.criteria %in% c("Short", "Long")) {
                        lag <- ifelse(lag.criteria == "Short", lag.short, lag.long)
                        test <- ur.df(ts, type = ifelse(tests[[j]][2] == "Constant", "drift", "trend"), lags = lag, selectlags = "Fixed")
                        temp$Lag[k] <- paste0(lag.criteria, " ", "(", lag, ")")
                    }
                    ## User (Numeric) lag selection.
                    if (!(lag.criteria %in% c("AIC", "BIC", "Short", "Long"))) {
                        lag <- as.numeric(lag.criteria)
                        test <- ur.df(ts, type = ifelse(tests[[j]][2] == "Constant", "drift", "trend"), lags = lag, selectlags = "Fixed")
                        temp$Lag[k] <- paste0("User", " ", "(", lag, ")")
                    }
                    test.stat <- test@teststat[1]
                    test.decision <- ifelse(test.stat < test@cval[1, 1], "^{***}", ifelse(test.stat < test@cval[1, 2], "^{**}", ifelse(test.stat < test@cval[1, 3], "^{*}", "")))
                    test.stat <- format(round(test@teststat[1], 2), nsmall = 2)
                    temp$Statistic[k] <- paste0("$", test.stat, test.decision, "$")
                    temp$Result[k] <- ifelse(test.decision %in% c("", "^{*}"), "Unit Root at 5%", "Stationary at 5%") ## The result is given by using the 5% significance level only.
                }

                # Phillips-Perron Tests:
                if (tests[[j]][1] == "PP") {
                    ## Short and Long lag selection.
                    if (lag.criteria %in% c("Short", "Long")) {
                        lag <- ifelse(lag.criteria == "Short", lag.short, lag.long)
                        test <- ur.pp(ts, type = "Z-tau", model = tolower(tests[[j]][2]), use.lag = lag)
                        temp$Lag[k] <- paste0(lag.criteria, " ", "(", lag, ")")
                        summary(test)
                    }
                    ## User (Numeric) lag selection.
                    if (!(lag.criteria %in% c("Short", "Long"))) {
                        lag <- as.numeric(lag.criteria)
                        test <- ur.pp(ts, type = "Z-tau", model = tolower(tests[[j]][2]), use.lag = lag)
                        temp$Lag[k] <- paste0("User", " ", "(", lag, ")")
                    }
                    test.stat <- test@teststat[1]
                    test.decision <- ifelse(test.stat < test@cval[1, 1], "^{***}", ifelse(test.stat < test@cval[1, 2], "^{**}", ifelse(test.stat < test@cval[1, 3], "^{*}", "")))
                    test.stat <- format(round(test@teststat[1], 2), nsmall = 2)
                    temp$Statistic[k] <- paste0("$", test.stat, test.decision, "$")
                    temp$Result[k] <- ifelse(test.decision %in% c("", "^{*}"), "Unit Root at 5%", "Stationary at 5%") ## The result is given by using the 5% significance level only.
                }

                # Elliott-Rothenberg-Stock Tests:
                if (tests[[j]][1] == "ERS") {
                    ## Short and Long lag selection.
                    if (lag.criteria %in% c("Short", "Long")) {
                        lag <- ifelse(lag.criteria == "Short", lag.short, lag.long)
                        test <- ur.ers(ts, type = c("DF-GLS"), model = tolower(tests[[j]][2]), lag.max = lag)
                        temp$Lag[k] <- paste0(lag.criteria, " ", "(", lag, ")")
                    }
                    ## User (Numeric) lag selection.
                    if (!(lag.criteria %in% c("Short", "Long"))) {
                        lag <- as.numeric(lag.criteria)
                        test <- ur.ers(ts, type = c("DF-GLS"), model = tolower(tests[[j]][2]), lag.max = lag)
                        temp$Lag[k] <- paste0("User", " ", "(", lag, ")")
                    }
                    test.stat <- test@teststat[1]
                    test.decision <- ifelse(test.stat < test@cval[1, 1], "^{***}", ifelse(test.stat < test@cval[1, 2], "^{**}", ifelse(test.stat < test@cval[1, 3], "^{*}", "")))
                    test.stat <- format(round(test@teststat[1], 2), nsmall = 2)
                    temp$Statistic[k] <- paste0("$", test.stat, test.decision, "$")
                    temp$Result[k] <- ifelse(test.decision %in% c("", "^{*}"), "Unit Root at 5%", "Stationary at 5%") ## The result is given by using the 5% significance level only.
                }

                # Kwiatkowski–Phillips–Schmidt–Shin Tests:
                if (tests[[j]][1] == "KPSS") {
                    ## Short and Long lag selection.
                    if (lag.criteria %in% c("Short", "Long")) {
                        lag <- ifelse(lag.criteria == "Short", lag.short, lag.long)
                        test <- ur.kpss(ts, type = ifelse(tests[[j]][2] == "Constant", "mu", "tau"), use.lag = lag)
                        summary(test)
                        temp$Lag[k] <- paste0(lag.criteria, " ", "(", lag, ")")
                    }
                    ## User (Numeric) lag selection.
                    if (!(lag.criteria %in% c("Short", "Long"))) {
                        lag <- as.numeric(lag.criteria)
                        test <- ur.kpss(ts, type = ifelse(tests[[j]][2] == "Constant", "mu", "tau"), use.lag = lag)
                        temp$Lag[k] <- paste0("User", " ", "(", lag, ")")
                    }
                    test.stat <- test@teststat[1]
                    test.decision <- ifelse(test.stat > test@cval[1, 4], "^{***}", ifelse(test.stat > test@cval[1, 2], "^{**}", ifelse(test.stat > test@cval[1, 1], "^{*}", "")))
                    test.stat <- format(round(test@teststat[1], 2), nsmall = 2)
                    temp$Statistic[k] <- paste0("$", test.stat, test.decision, "$")
                    temp$Result[k] <- ifelse(test.decision %in% c("", "^{*}"), "Stationary at 5%", "Unit Root at 5%") ## The result is given by using the 5% significance level only.
                }
            }
            if (j == 1) {
                main1 <- temp
            } else {
                main1 <- rbind(main1, temp)
            }
        }
        if (i == 1) {
            main <- main1
        } else {
            main <- rbind(main, main1)
        }
    }

    # Output data will be named as "ur.stationary.tests.results".
    assign("ur.stationary.tests.results", main, envir = globalenv())

    # Subsetting the results with specified lag selection methods.
    if (!is.null(Subset)) {
        for (i in 1:length(Subset)) {
            subset <- paste0("(", paste0(Subset[[i]], collapse = ")|("), ")")
            main <- main[c(grep(subset, main[, names(Subset[i])])), ]
        }
        rownames(main) <- 1:nrow(main)
    }

    # Reshaping results for final use.
    if (Reshape == TRUE) {
        main <- main[, -c(grep("(Result)", colnames(main)))] ## Deleting the Result column.
        # main$Lag <- gsub(" \\([0-9]{1,2}\\)", "", main$Lag)
        main <- reshape2::dcast(main, Test + Model + Lag ~ Variable, value.var = "Statistic") ## Reshaping the main data frame.
        main <- main[, c(1:3, match(colname, colnames(main)))] ## Arranging the column order.
        main <- main[c(grep("(ADF)", main$Test), grep("(PP)", main$Test), grep("(ERS)", main$Test), grep("(KPSS)", main$Test)), ] ## Arranging the row order.
        rownames(main) <- 1:nrow(main)
    }

    # Correcting the results if not used for LaTeX.
    if (LaTeX == FALSE) {
        for (i in 1:ncol(main)) {
            main[, i] <- gsub("(\\$)|(\\^\\{)|(\\})", "", main[, i])
        }
    }

    # Output data will be named as "ur.stationary.tests.results.conv".
    assign("ur.stationary.tests.results.conv", main, envir = globalenv())

    # Revert the working directory to initial path.
    setwd(WD.temp)
}

#==================================== END ======================================
