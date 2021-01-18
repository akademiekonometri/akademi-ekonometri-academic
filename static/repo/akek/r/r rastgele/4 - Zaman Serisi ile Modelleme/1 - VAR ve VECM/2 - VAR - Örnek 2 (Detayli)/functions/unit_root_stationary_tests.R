#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#============================ UR.Stationary.Tests ==============================
#====================== Unit Root and Stationary Tests =========================

# Notes:
#
## Function takes the input multivariate/univariate time series object and runs some prespecified unit root and stationary tests such as, Phillips-Perron (PP) test (with 10 different specifications), Elliott-Rothenberg-Stock (ERS) tests (with 10 different specifications) and Kwiatkowski–Phillips–Schmidt–Shin (KPSS) test (with 10 different specifications) on each univariate time series with the selected significance level. For all specifications please see the source code of this function.
## Function requires a time series (ts) object.
## Significance.Level is the significance level which is used in all tests in this function.
## Function optionally takes the first difference of the input data and applies the same procedure mentioned above. The default for Diff argument is False.
## The default for Reshape argument is FALSE which produces a desired output without reshaping the results. If Reshape = TRUE, then the desired output is reshaped with results only.
## Function optionally subsets the results with specified lag selections methods.
## Lag length selection: An important practical issue for the implementation of the PP, ERS and KPSS tests is the specification of the lag length p. If p is too small, then the remaining serial correlation in the errors will bias the test. If p is too large, then the power of the test will suffer. Therefore, while runing these tests 5 types of lag length selection criteria will be used: BIC, AIC, AIC*, Short, and Long. For lag selection with BIC and AIC: Lag length is selected with SelectModel function in FitAR. The function selects the best order(lag) for AR model with BIC or AIC criterion by utilizing from "McLeod, A.I. and Zhang, Y. (2006) Partial Autocorrelation Parameterization for Subset Autoregression. Journal of Time Series Analysis, 27, 599-612". Note that in SelectModel function lag.max argument is defined as frequency of univariate series (e.g., for weekly it is 52, and for monthly it is 12). For lag selection with AIC*: Lag length is selected with AIC in AR model estimated by yule-walker method. Note that for AIC, BIC, and AIC*, the selected lag length is decreased by 1 since AIC, BIC and AIC* chooses lags in levels with either SelectModel or ar function but unit root tests uses series in first difference. For lag selection with Short and Long: In the literature two type of common lags are used by tests: For PP and ERS tests: Short = trunc((4 * (length(data) / 100) ^ 0.25)) and Long = trunc((12 * (length(data) / 100) ^ 0.25)); for KPSS tests: Short = trunc(3 * (length(data) ^ 0.5) / 13) and Long = trunc(10 * (length(data) ^ 0.5) / 14).
## The output of the function is named as "ur.stationary.tests" data frame.

# Usage:
#
# UR.Stationary.Tests(Data, Significance.Level, Diff = FALSE, Reshape = FALSE, Subset = NULL)
#
## Data: Time series (ts) object. It can be multivariate or univariate.
## Significance Level: Significance level for all test throughout the function.
## Diff: Takes the first difference of all time series.
## Reshape: Output is reshaped with results only.
## Subset: Subsets some of the test results with specific lag selection method.
## To run the function without any argument matching problem, make sure to specify Diff, Reshape and Subset arguments with their names always (if non-default values are selected).

# Examples:
#
## UR.Stationary.Tests(data.ts, 1)
## UR.Stationary.Tests(data.ts, 5)
## UR.Stationary.Tests(data.ts, 10, Reshape = TRUE)
## UR.Stationary.Tests(data.ts, 10, Reshape = TRUE, Subset = c("BIC", "AIC"))
## UR.Stationary.Tests(data.ts, 1, Diff = TRUE, Reshape = TRUE)
## UR.Stationary.Tests(data.ts, 1, Diff = TRUE, Reshape = TRUE, Subset = c("Long", "Short"))

UR.Stationary.Tests <- function(Data, Significance.Level, Diff = FALSE, Reshape = FALSE, Subset = NULL) {
    # Type of the test and lag selection procedure.
    tests <- list(test1 = c("PP", "Constant", "BIC"), test2 = c("PP", "Trend", "BIC"), test3 = c("PP", "Constant", "AIC"), test4 = c("PP", "Trend", "AIC"), test5 = c("PP", "Constant", "AIC*"), test6 = c("PP", "Trend", "AIC*"), test7 = c("PP", "Constant", "Short"), test8 = c("PP", "Trend", "Short"), test9 = c("PP", "Constant", "Long"), test10 = c("PP", "Trend", "Long"), test11 = c("KPSS", "Constant", "BIC"), test12 = c("KPSS", "Trend", "BIC"), test13 = c("KPSS", "Constant", "AIC"), test14 = c("KPSS", "Trend", "AIC"), test15 = c("KPSS", "Constant", "AIC*"), test16 = c("KPSS", "Trend", "AIC*"), test17 = c("KPSS", "Constant", "Short"), test18 = c("KPSS", "Trend", "Short"), test19 = c("KPSS", "Constant", "Long"), test20 = c("KPSS", "Trend", "Long"), test21 = c("ERS", "Constant", "BIC"), test22 = c("ERS", "Trend", "BIC"), test23 = c("ERS", "Constant", "AIC"), test24 = c("ERS", "Trend", "AIC"), test25 = c("ERS", "Constant", "AIC*"), test26 = c("ERS", "Trend", "AIC*"), test27 = c("ERS", "Constant", "Short"), test28 = c("ERS", "Trend", "Short"), test29 = c("ERS", "Constant", "Long"), test30 = c("ERS", "Trend", "Long"))
    utest <- length(tests) ## Number of unique tests.

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

    # Checks Significance.Level argument.
    if (length(Significance.Level) != 1)
        stop("Invalid Significance.Level. Please choose only one Significance.Level.\n")
    if (!is.numeric(Significance.Level))
        stop("Invalid Significance.Level. Please choose a numeric Significance.Level value.\n")
    if (!(Significance.Level %in% c(1, 5, 10)))
        stop("Invalid Significance.Level. Please choose one of the followings: 1) For %0.01: 1. 2) For %0.05: 5. 3) For %0.1: 10.\n")
    s.level.col <- ifelse(Significance.Level == 1, 1, ifelse(Significance.Level == 5, 2, 3)) ## Significance level column number in PP and ERS tests.
    s.level.col.kpss <- ifelse(Significance.Level == 1, 4, ifelse(Significance.Level == 5, 2, 1)) ## Significance level column number in KPSS tests

    # Checks Diff argument.
    if (length(Diff) != 1)
        stop("Invalid Diff. Please choose only one Diff.\n")
    if (!(Diff %in% c("TRUE", "FALSE")))
        stop("Invalid Diff. Please choose one of the followings: 1) To take the first difference of input data: TRUE. 2) To use the input data as is: FALSE.\n")

    # Checks Reshape argument.
    if (length(Reshape) != 1)
        stop("Invalid Reshape. Please choose only one Reshape.\n")
    if (!(Reshape %in% c("TRUE", "FALSE")))
        stop("Invalid Reshape. Please choose one of the followings: 1) To reshape output data frame only with results: TRUE. 2) Without reshaping: FALSE.\n")

    # Checks Subset argument.
    if (!is.null(Subset)) {
        if (!(sum(!(Subset %in% c("BIC", "AIC", "AIC*", "Long", "Short"))) == 0))
            stop("Invalid Subset. Please choose one or more of the followings: 1) To subset results with BIC lag selection from SelectModel function in FitAR package: BIC. 2) To subset results with AIC lag selection from SelectModel function in FitAR package: AIC. 3) To subset results with AIC lag selection from AR model: AIC*. 4) To subset results with Long lag selection, commonly used in the literature, across all tests: Long. 5) To subset results with Short lag selection, commonly used in the literature, across all tests: Short.\n")
    }

    # Runing the unit root and stationarity tests with selected arguments for all univarite time series in input data.
    for (i in 1:ncol) {
        temp <- data.frame(Variable = rep(NA, utest), Test = rep(NA, utest), Type = rep(NA, utest), Lag.Selection = rep(NA, utest), Result = rep(NA, utest), stringsAsFactors = FALSE) ## Data frame containing the results.

        if (is.null(ncol(Data))) { ## Data is in ts vector format.
            ts <- Data ## Univariate time series.
        }
        if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
            ts <- Data[, colname[i]] ## Univariate time series.
        }
        if (Diff == TRUE) { ## Taking the first difference.
            ts <- diff(ts, lag = 1, diff = 1)
        }

        # Running the Phillips-Perron, Elliott--Rothenberg--Stock and KPSS tests with possible specifications.
        for (j in 1:utest) {
            # Lag length selection.
            if (tests[[j]][3] %in% c("BIC", "AIC")) { ## Lag selection with SelectModel function in FitAR package. Note that lag.max is defined with the 2 times of frequency of the univariate series.
                lags <- SelectModel(ts, lag.max = frequency(ts), ARModel = c("AR"), Criterion = tests[[j]][3], Best = 1, Candidates = 12)
            }
            if (tests[[j]][3] %in% c("AIC*")) { ## Lag selection with an Autoregressive Model.
                lags <- ar(ts, aic = TRUE, method = c("yule-walker"))$order
            }

            ## To coincide with the literature, some common used lags are also applied.
            if (tests[[j]][3] == "Short") {
                if (tests[[j]][1] == "PP" | tests[[j]][1] == "ERS") {
                    lags <- trunc((4 * (length(ts) / 100) ^ 0.25))
                }
                if (tests[[j]][1] == "KPSS") {
                    lags <- trunc(3 * (length(ts) ^ 0.5) / 13)
                }
            }
            if (tests[[j]][3] == "Long") {
                if (tests[[j]][1] == "PP" | tests[[j]][1] == "ERS") {
                    lags <- trunc((12 * (length(ts) / 100) ^ 0.25))
                }
                if (tests[[j]][1] == "KPSS") {
                    lags <- trunc(10 * (length(ts) ^ 0.5) / 14)
                }
            }
            if (tests[[j]][3] %in% c("BIC", "AIC", "AIC*")) {
                lags <- lags - 1 ## Decreasing the lag length by 1 since AIC, BIC and AIC* chooses lags in level with either SelectModel or ar function because unit root tests uses series in first difference.
            }
            if (lags < 1) { ## Sometimes SelectModel or AR functions choose the best model without any lag. However, we want to control for serial correlation so the lag is specified as 1. This step is necessary since some part of the code does not work when the lag length is 0.
                message(paste0("For series ", colname[i], " with ", tests[[j]][3], " lag selection method, SelectModel function chooses ", lags, " lag which causes errors in some functions. Thus for this series, the selected lag is 1 which is the minimum lag accepted.\n"))
                lags <- 1
            }

            # Filling same variables.
            temp$Variable[j] <- colname[i]
            temp$Test[j] <- tests[[j]][1]
            temp$Type[j] <- tests[[j]][2]
            temp$Lag.Selection[j] <- paste0(tests[[j]][3], "(", lags, ")")

            # Phillips-Perron Tests:
            if (tests[[j]][1] == "PP") {
                test <- ur.pp(ts, type = "Z-tau", model = tolower(tests[[j]][2]), use.lag = lags)
                if (round(abs(test@teststat[1]) - abs(test@cval[1, s.level.col]), 4) < 0) {
                    temp$Result[j] <- "Unit Root"
                } else {
                    temp$Result[j] <- "Stationary"
                }
            }

            # Elliott-Rothenberg-Stock Tests:
            if (tests[[j]][1] == "ERS") {
                test <- ur.ers(ts, type = c("DF-GLS"), model = tolower(tests[[j]][2]), lag.max = lags)
                if (round(abs(test@teststat[1]) - abs(test@cval[1, s.level.col]), 4) < 0) {
                    temp$Result[j] <- "Unit Root"
                } else {
                    temp$Result[j] <- "Stationary"
                }
            }

            # Kwiatkowski–Phillips–Schmidt–Shin Tests:
            if (tests[[j]][1] == "KPSS") {
                test <- ur.kpss(ts, type = ifelse(tests[[j]][2] == "Constant", "mu", "tau"), use.lag = lags)
                if (round(abs(test@teststat[1]) - abs(test@cval[1, s.level.col.kpss]), 4) > 0) {
                    temp$Result[j] <- "Unit Root"
                } else {
                    temp$Result[j] <- "Stationary"
                }
            }
        }
        if (i == 1) {
            main <- temp
        } else {
            main <- rbind(main, temp)
        }
    }

    # Subsetting the results with specified lag selection methods.
    if (!is.null(Subset)) {
        for (i in 1:length(Subset)) {
            if (i == 1) {
                subset <- paste0("(^", Subset[i], "\\()")
            } else {
                temp <- paste0("(^", Subset[i], "\\()")
                subset <- paste(subset, temp, sep = "|")
            }
        }
        main <- main[c(grep(subset, main$Lag.Selection)), ]
        rownames(main) <- 1:nrow(main)
    }

    # Reshaping results for final use.
    if (Reshape == TRUE) {
        main$Lag.Selection <- gsub("\\([0-9]{1,2}\\)", "", main$Lag.Selection)
        main$Test <- paste0(main$Test, ".", main$Type, ".", main$Lag.Selection)
        main <- main[, c(1:2, 5)]
        main <- dcast(main, Variable ~ Test, value.var = "Result")
        if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
            main <- main[match(c(colnames(data.ts)), main$Variable), ]
        }
        main <- main[, c(grep("(^Variable$)", names(main)), grep("(^PP.)", names(main)), grep("(^ERS.)", names(main)), grep("(^KPSS.)", names(main)), grep("(^Variable$)|(^PP.)|(^ERS.)|(^KPSS.)", names(main), invert = TRUE))]
        rownames(main) <- 1:nrow(main)
    }
    assign("ur.stationary.tests", main, envir = globalenv()) ## Output data will be named as "ur.stationary.tests".
}

#==================================== END ======================================
