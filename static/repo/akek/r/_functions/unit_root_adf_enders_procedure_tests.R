#============================ ADF.Enders.Procedure =============================
#=========================== Enders' ADF Procedure =============================

# Notes:
#
## This procedure is a sensible way to test for unit roots when the form of the data-generating process is completely unknown. The whole procedure is taken from "Supplementary Manual to Accompany, Applied Econometric Time Series (4th Edition) by Walter Enders" at pdf page 63-66 and from "Applied Econometric Time Series (4th Edition) by Walter Enders" at pdf page 206-210. The manual can be found at the following site <http://time-series.net/yahoo_site_admin/assets/docs/SupplementaryManual_all.117125921.pdf>. Function takes the input multivariate/univariate time series object and runs the Enders' ADF procedure on each univariate time series with the selected significance level.
## Note that as default in SelectModel function lag.max and Candidates arguments is defined as lag.short = trunc((4 * (length(ts) / 100)^0.25)).
## Throughout the procedure α0 indicates coefficient of drift; α2 indicates coefficient of trend; and γ indicates coefficient of first lag of univariate time series.
## Function requires a time series (ts) object.
## Significance.Level is the significance level which is used in all tests in this function. Values are 1, 5, and 10.
## Function optionally takes the first difference of the input data and applies the same procedure mentioned above. The default for Diff argument is False.
## The default for Reshape argument is FALSE which produces a desired output without reshaping the results. If Reshape = TRUE, then the desired output is reshaped with results only.
## The output of the function is named as "adf.tests.results" data frame for the raw output and "adf.tests.results.conv" data frame for the reshaped output.

# Usage:
#
# ADF.Enders.Procedure(Data, Significance.Level = 5, Diff = FALSE, Reshape = FALSE)
#
## Data: Time series (ts) object. It can be multivariate or univariate.
## Significance Level: Single Numeric (1, 5, 10). Significance level for all test throughout the Enders' ADF Procedure.
## Diff: Logical. Takes the first difference of all time series.
## Reshape: Logical. Output is reshaped with results only.
## To run the function without any argument matching problem, make sure to always specify all arguments with their names (if non-default values are selected).

# Examples:
#
## ADF.Enders.Procedure(Data = data.ts, Significance.Level = 1)
## ADF.Enders.Procedure(Data = data.ts, Significance.Level = 5)
## ADF.Enders.Procedure(Data = data.ts, Significance.Level = 10, Reshape = TRUE)
## ADF.Enders.Procedure(Data = data.ts, Significance.Level = 1, Diff = TRUE, Reshape = TRUE)

ADF.Enders.Procedure <- function(Data, Significance.Level = 5, Diff = FALSE, Reshape = FALSE) {
    # Saves the current working directory for further use.
    WD.temp <- getwd()

    # Type of the test and lag selection procedure.
    tests <- list(test1 = c("ADF", "AIC"), test2 = c("ADF", "BIC"))
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
    s.level <- Significance.Level/100 ## Significance level for manually written regressions.
    s.level.col <- ifelse(Significance.Level == 1, 1, ifelse(Significance.Level == 5, 2, 3)) ## Significance level column number for R built-in function.

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

    # Running the ADF tests with selected arguments for all univariate time series in input data.
    for (i in 1:ncol) {
        temp <- data.frame(Variable = rep(NA, utest), Test = rep(NA, utest), Lag = rep(NA, utest), Result = rep(NA, utest), Model.Specified = rep(NA, utest), Decided = rep(NA, utest), stringsAsFactors = FALSE) ## Data frame containing the results.

        if (is.null(ncol(Data))) { ## Data is in ts vector format.
            ts <- Data ## Univariate time series.
        }
        if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
            ts <- Data[, colname[i]] ## Univariate time series.
        }
        if (Diff == TRUE) { ## Taking the first difference.
            ts <- diff(ts, lag = 1, diff = 1)
        }

        ## Some common used lags can be applied which is taken directly from the literature. These are used for lag.max and Candidates arguments in SelectModel function.
        lag.short <- trunc((4 * (length(ts) / 100)^0.25)) ## Short specified lags generally used for PP and KPSS tests.
        lag.long <- trunc((12 * (length(ts) / 100)^0.25)) ## Long specified lags generally used for PP and KPSS tests.

        # Running the Enders' procedure for ADF by each type of ADF test specified at the beginning of this function.
        for (j in 1:utest) {
            # Lag length selection with 3 different ways.
            if (tests[[j]][2] == "AIC" | tests[[j]][2] == "BIC") {  ## Lag selection with SelectModel function in FitAR package.
                lag <- SelectModel(ts, lag.max = lag.short, ARModel = c("AR"), Criterion = tests[[j]][2], Best = 1, Candidates = lag.short) ## Note that as default in SelectModel function lag.max and Candidates arguments is defined as lag.short = trunc((4 * (length(ts) / 100)^0.25)).
                lag <- lag - 1 ## Decreasing the lag length by 1 since AIC and BIC choose lags in level with SelectModel function because unit root tests uses series in first difference.
            }
            if (lag < 1) { ## Sometimes SelectModel function chooses the best model without any lag. However, we want to control for serial correlation so the lag is specified as 1. This step is necessary since some part of the code does not work when the lag length is 0.
                message(paste0("For series ", colname[i], " with ", tests[[j]][2], " lag selection method, SelectModel function chooses ", lag, " lag which causes errors in manually written functions. Thus for this series, the selected lag is 1 which is the minimum lag accepted.\n"))
                lag <- 1
            }

            # Variable preparation for specific regressions throughout the Enders' ADF procedure.
            ## At some point in the below procedure, it is necessary to run specific regressions (which cannot be done with built-in R functions). Therefore, the following code prepares the necessary variables. The code (note that lag length is defined in the previous section) is taken from the following web address: <http://www.r-bloggers.com/unit-root-tests/>
            z <- diff(ts)
            n <- length(z)
            z.diff <- embed(z, lag + 1)[ ,1]
            z.lag.1 <- ts[(lag + 1):n]
            k <- lag + 1
            z.diff.lag <- embed(z, lag + 1)[ ,2:k]
            trend <- (lag + 1):n

            # Filling same variables.
            temp$Variable[j] <- colname[i]
            temp$Test[j] <- tests[[j]][1]
            temp$Lag[j] <- paste0(tests[[j]][2], " (", lag, ")")

            # Step 1
            #-------------------------------------------------------------------
            ## Started with the least restrictive model (with drift and trend).
            test.trend <- ur.df(ts, type = c("trend"), lags = lag, selectlags = "Fixed")

            # Tau3 (γ = 0) is rejected. Conclusion: Data is stationary.
            if (round(abs(test.trend@teststat[1]) - abs(test.trend@cval[1, s.level.col]), 4) > 0) {
                ## Unit root tests have low power to reject the null hypothesis; hence, if the null hypothesis of a unit root is rejected, there is no need to proceed. Conclude that the data does not contain a unit root.
                temp$Result[j] <- "Stationary"
                temp$Model.Specified[j] <- "Not Specified Any"
                temp$Decided[j] <- "After Test with Trend (Step 1)"
            }

            # Step 2
            #-------------------------------------------------------------------
            ## Tau3 (γ = 0) cannot be rejected.
            if (round(abs(test.trend@teststat[1]) - abs(test.trend@cval[1, s.level.col]), 4) < 0) {
                ## If tau3 (γ = 0) cannot be rejected, then the next step is to determine whether trend (α2) belongs in the estimating equation. To determine that Phi3 (α2 = γ = 0) will be used.

                # Phi3 (α2 = γ = 0) is rejected.
                if (round(abs(test.trend@teststat[3]) - abs(test.trend@cval[3, s.level.col]), 4) > 0) {
                    ## Assuming there is a unit root (γ = 0), we can construct the following test to find out if trend (α2) is statistically significant. Note, that since unit root is assumed and imposed in the model, we can test the hypothesis with t.test.

                    # α2 = 0 (No-Trend) is rejected in the selected significance level. Conclusion: There is a trend.
                    if (summary(lm(z.diff ~ 1 + trend + z.diff.lag))$coefficients[2, 4] < s.level) {
                        ## Given that the trend belongs in the regression equation, the test for γ = 0 can be conducted using a t-distribution from ADF with trend specification.

                        ## Unit root (γ = 0) is rejected. Conclusion: Data is trend stationary.
                        if (summary(lm(z.diff ~ 1 + z.lag.1 + trend + z.diff.lag))$coefficients[2, 4] < s.level) {
                            temp$Result[j] <- "Stationary"
                            temp$Model.Specified[j] <- "Trend Stationary"
                            temp$Decided[j] <- "After Test with Trend (Step 2)"
                        }

                        ## Unit root (γ = 0) cannot be rejected. Conclusion: There is a unit root and data contains a quadratic trend.
                        if (summary(lm(z.diff ~ 1 + z.lag.1 + trend + z.diff.lag))$coefficients[2, 4] > s.level) {
                            temp$Result[j] <- "Unit Root"
                            temp$Model.Specified[j] <- "Unit Root and Quadratic Trend"
                            temp$Decided[j] <- "After Test with Trend (Step 2)"
                        }
                    }

                    # α2 = 0 (No-Trend) cannot be rejected in the selected significance level. Conclusion: There is not a trend.
                    if (summary(lm(z.diff ~ 1 + trend + z.diff.lag))$coefficients[2, 4] > s.level) {
                        ## α2 = 0 (No-Trend) cannot be rejected in the selected significance level, then assume there is no trend and proceed to Step 3. Therefore the last part of Step 2 is actually Step 3 (which covers Step 4 also).
                        # Step 2 - Step 3
                        #-------------------------------------------------------
                        ## More restrictive model (with drift only).
                        test.drift <- ur.df(ts, type = c("drift"), lags = lag, selectlags = "Fixed")

                        # Tau2 (γ = 0) is rejected. Conclusion: Data is stationary.
                        if (round(abs(test.drift@teststat[1]) - abs(test.drift@cval[1, s.level.col]), 4) > 0) {
                            temp$Result[j] <- "Stationary"
                            temp$Model.Specified[j] <- "No Unit Root, No Trend"
                            temp$Decided[j] <- "After Test with Drift (Step2 - Step 3)"
                        }

                        # Tau2 (γ = 0) cannot be rejected.
                        if (round(abs(test.drift@teststat[1]) - abs(test.drift@cval[1, s.level.col]), 4) < 0) {
                            ## If tau2 (γ = 0) cannot be rejected, then the next step is to determine whether drift (α0) belongs in the estimating equation. To determine that Phi1 (α0 = γ = 0) will be used.

                            # Phi1 (α0 = γ = 0) is rejected.
                            if (round(abs(test.drift@teststat[2]) - abs(test.drift@cval[2, s.level.col]), 4) > 0) {
                                ## Assuming there is a unit root (γ = 0) and no trend (α2 = 0), we can construct the following test to find out whether drift (α1) is statistically significant. Note, that since unit root and no trend is assumed and imposed in the model, we can test the hypothesis with t.test.

                                # α0 = 0 (No-Drift) is rejected in the selected significance level. Conclusion: There is a drift.
                                if (summary(lm(z.diff ~ 1 + z.diff.lag))$coefficients[1, 4] < s.level) {
                                    ## Given that the drift belongs in the regression equation, the test for γ = 0 can be conducted using a t-distribution from ADF with drift specification.

                                    ## Unit root (γ = 0) is rejected. Conclusion: Data is stationary around a non-zero mean (drift).
                                    if (summary(lm(z.diff ~ 1 + z.lag.1 + z.diff.lag))$coefficients[2, 4] < s.level) {
                                        temp$Result[j] <- "Stationary"
                                        temp$Model.Specified[j] <- "Stationary with Drift"
                                        temp$Decided[j] <- "After Test with Drift (Step 2 - Step 3)"
                                    }

                                    ## Unit root (γ = 0) cannot be rejected. Conclusion: There is a unit root and a drift.
                                    if (summary(lm(z.diff ~ 1 + z.lag.1 + z.diff.lag))$coefficients[2, 4] > s.level) {
                                        temp$Result[j] <- "Unit Root"
                                        temp$Model.Specified[j] <- "Unit Root and Drift"
                                        temp$Decided[j] <- "After Test with Drift (Step 2 - Step 3)"
                                    }
                                }

                                # α0 = 0 (No-Drift) cannot be rejected in the selected significance level. Conclusion: There is not a drift.
                                if (summary(lm(z.diff ~ 1 + z.diff.lag))$coefficients[1, 4] > s.level) {
                                    ## α0 = 0 (No-Drift) cannot be rejected in the selected significance level, then assume there is no drift and proceed to Step 4. There fore the last part of Step 3 is actually Step 4.

                                    # Step 2 - Step 3 - Step 4
                                    #-------------------------------------------
                                    ## The most restrictive model (without drift and trend).
                                    test.none <- ur.df(ts, type = c("none"), lags = lag, selectlags = "Fixed")

                                    # Tau1 (γ = 0) is rejected. Conclusion: Data is stationary.
                                    if (round(abs(test.none@teststat[1]) - abs(test.none@cval[1, s.level.col]), 4) > 0) {
                                        temp$Result[j] <- "Stationary"
                                        temp$Model.Specified[j] <- "No Unit Root, No Drift, No Trend"
                                        temp$Decided[j] <- "After Test with Drift (Step 2 - Step 3 - Step 4)"
                                    }
                                    # Tau1 (γ = 0) cannot be rejected. Conclusion: There is a unit root only.
                                    if (round(abs(test.none@teststat[1]) - abs(test.none@cval[1, s.level.col]), 4) < 0) {
                                        temp$Result[j] <- "Unit Root"
                                        temp$Model.Specified[j] <- "Unit Root Only"
                                        temp$Decided[j] <- "After Test with Drift (tep 2 - Step 3 - Step 4)"
                                    }
                                }
                            }

                            # Step 2 - Step 4
                            #---------------------------------------------------
                            # Phi1 (α0 = γ = 0) cannot be rejected. No-drift is assumed thereafter.
                            if (round(abs(test.drift@teststat[2]) - abs(test.drift@cval[2, s.level.col]), 4) < 0) {
                                ## The most restrictive model (without drift and trend).
                                test.none <- ur.df(ts, type = c("none"), lags = lag, selectlags = "Fixed")

                                # Tau1 (γ = 0) is rejected. Conclusion: Data is stationary.
                                if (round(abs(test.none@teststat[1]) - abs(test.none@cval[1, s.level.col]), 4) > 0) {
                                    temp$Result[j] <- "Stationary"
                                    temp$Model.Specified[j] <- "No Unit Root, No Drift, No Trend"
                                    temp$Decided[j] <- "After Test with None (Step 2 - Step 4)"
                                }
                                # Tau1 (γ = 0) cannot be rejected. Conclusion: There is a unit root only.
                                if (round(abs(test.none@teststat[1]) - abs(test.none@cval[1, s.level.col]), 4) < 0) {
                                    temp$Result[j] <- "Unit Root"
                                    temp$Model.Specified[j] <- "Unit Root Only"
                                    temp$Decided[j] <- "After Test with None (Step 2 - Step 4)"
                                }
                            }
                        }
                    }
                }

                # Step 3
                #---------------------------------------------------------------
                # Phi3 (α2 = γ = 0) cannot be rejected. No-trend is assumed thereafter.
                if (round(abs(test.trend@teststat[3]) - abs(test.trend@cval[3, s.level.col]), 4) < 0) {
                    ## More restrictive model (with drift only).
                    test.drift <- ur.df(ts, type = c("drift"), lags = lag, selectlags = "Fixed")

                    # Tau2 (γ = 0) is rejected. Conclusion: Data is stationary.
                    if (round(abs(test.drift@teststat[1]) - abs(test.drift@cval[1, s.level.col]), 4) > 0) {
                        temp$Result[j] <- "Stationary"
                        temp$Model.Specified[j] <- "No Unit Root, No Trend"
                        temp$Decided[j] <- "After Test with Drift (Step 3)"
                    }

                    # Tau2 (γ = 0) cannot be rejected.
                    if (round(abs(test.drift@teststat[1]) - abs(test.drift@cval[1, s.level.col]), 4) < 0) {
                        ## If tau2 (γ = 0) cannot be rejected, then the next step is to determine whether drift (α0) belongs in the estimating equation. To determine that Phi1 (α0 = γ = 0) will be used.

                        # Phi1 (α0 = γ = 0) is rejected.
                        if (round(abs(test.drift@teststat[2]) - abs(test.drift@cval[2, s.level.col]), 4) > 0) {
                            ## Assuming there is a unit root (γ = 0) and no trend (α2 = 0), we can construct the following test to find out whether drift (α1) is statistically significant. Note, that since unit root and no trend is assumed and imposed in the model, we can test the hypothesis with t.test.

                            # α0 = 0 (No-Drift) is rejected in the selected significance level. Conclusion: There is a drift.
                            if (summary(lm(z.diff ~ 1 + z.diff.lag))$coefficients[1, 4] < s.level) {
                                ## Given that the drift belongs in the regression equation, the test for γ = 0 can be conducted using a t-distribution from ADF with drift specification.

                                ## Unit root (γ = 0) is rejected. Conclusion: Data is stationary around a non-zero mean (drift).
                                if (summary(lm(z.diff ~ 1 + z.lag.1 + z.diff.lag))$coefficients[2, 4] < s.level) {
                                    temp$Result[j] <- "Stationary"
                                    temp$Model.Specified[j] <- "Stationary with Drift"
                                    temp$Decided[j] <- "After Test with Drift (Step 3)"
                                }

                                ## Unit root (γ = 0) cannot be rejected. Conclusion: There is a unit root and a drift.
                                if (summary(lm(z.diff ~ 1 + z.lag.1 + z.diff.lag))$coefficients[2, 4] > s.level) {
                                    temp$Result[j] <- "Unit Root"
                                    temp$Model.Specified[j] <- "Unit Root and Drift"
                                    temp$Decided[j] <- "After Test with Drift (Step 3)"
                                }
                            }

                            # α0 = 0 (No-Drift) cannot be rejected in the selected significance level. Conclusion: There is not a drift.
                            if (summary(lm(z.diff ~ 1 + z.diff.lag))$coefficients[1, 4] > s.level) {
                                ## α0 = 0 (No-Drift) cannot be rejected in the selected significance level, then assume there is no drift and proceed to Step 4. There fore the last part of Step 3 is actually Step 4.

                                # Step 3 - Step 4
                                #-----------------------------------------------
                                ## The most restrictive model (without drift and trend).
                                test.none <- ur.df(ts, type = c("none"), lags = lag, selectlags = "Fixed")

                                # Tau1 (γ = 0) is rejected. Conclusion: Data is stationary.
                                if (round(abs(test.none@teststat[1]) - abs(test.none@cval[1, s.level.col]), 4) > 0) {
                                    temp$Result[j] <- "Stationary"
                                    temp$Model.Specified[j] <- "No Unit Root, No Drift, No Trend"
                                    temp$Decided[j] <- "After Test with Drift (Step 3 - Step 4)"
                                }
                                # Tau1 (γ = 0) cannot be rejected. Conclusion: There is a unit root only.
                                if (round(abs(test.none@teststat[1]) - abs(test.none@cval[1, s.level.col]), 4) < 0) {
                                    temp$Result[j] <- "Unit Root"
                                    temp$Model.Specified[j] <- "Unit Root Only"
                                    temp$Decided[j] <- "After Test with Drift (Step 3 - Step 4)"
                                }
                            }
                        }

                        # Step 4
                        #-------------------------------------------------------
                        # Phi1 (α0 = γ = 0) cannot be rejected. No-drift is assumed thereafter.
                        if (round(abs(test.drift@teststat[2]) - abs(test.drift@cval[2, s.level.col]), 4) < 0) {
                            ## The most restrictive model (without drift and trend).
                            test.none <- ur.df(ts, type = c("none"), lags = lag, selectlags = "Fixed")

                            # Tau1 (γ = 0) is rejected. Conclusion: Data is stationary.
                            if (round(abs(test.none@teststat[1]) - abs(test.none@cval[1, s.level.col]), 4) > 0) {
                                temp$Result[j] <- "Stationary"
                                temp$Model.Specified[j] <- "No Unit Root, No Drift, No Trend"
                                temp$Decided[j] <- "After Test with None (Step 4)"
                            }
                            # Tau1 (γ = 0) cannot be rejected. Conclusion: There is a unit root only.
                            if (round(abs(test.none@teststat[1]) - abs(test.none@cval[1, s.level.col]), 4) < 0) {
                                temp$Result[j] <- "Unit Root"
                                temp$Model.Specified[j] <- "Unit Root Only"
                                temp$Decided[j] <- "After Test with None (Step 4)"
                            }
                        }
                    }
                }
            }
        }
        if (i == 1) {
            main <- temp
        } else {
            main <- rbind(main, temp)
        }
    }

    # Output data will be named as "adf.tests.results".
    assign("adf.tests.results", main, envir = globalenv())

    # Reshaping results for final use.
    if (Reshape == TRUE) {
        # main <- main[, -c(grep("(Test)", colnames(main)))] ## Deleting the Test column.
        main$Lag <- gsub("\\([0-9]{1,2}\\)", "", main$Lag)
        main <- reshape2::dcast(main, Test + Lag ~ Variable, value.var = "Result")
        main <- main[, c(1:2, match(colname, colnames(main)))] ## Arranging the column order.
        rownames(main) <- 1:nrow(main)
    }

    # Output data will be named as "adf.tests.results.conv".
    assign("adf.tests.results.conv", main, envir = globalenv())

    # Revert the working directory to initial path.
    setwd(WD.temp)
}

#==================================== END ======================================
