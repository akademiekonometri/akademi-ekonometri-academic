#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#============================ ADF.Enders.Procedure =============================
#=========================== Enders' ADF Procedure =============================

# Notes:
#
## This procedure is a sensible way to test for unit roots when the form of the data-generating process is completely unknown. The whole procedure is taken from "Supplementary Manual to Accompany, Applied Econometric Time Series (4th Edition) by Walter Enders" at pdf page 63-66 and from "Applied Econometric Time Series (4th Edition) by Walter Enders" at pdf page 206-210. The manual can be found at the following site <http://time-series.net/yahoo_site_admin/assets/docs/SupplementaryManual_all.117125921.pdf>. Function takes the input multivariate/univariate time series object and runs the Enders' ADF procedure on each univariate time series with the selected significance level.
## Throughout the procedure α0 indicates coefficient of drift; α2 indicates coefficient of trend; and γ indicates coefficient of first lag of univariate time series.
## Function requires a time series (ts) object.
## Significance.Level is the significance level which is used in all tests in this function.
## Function optionally takes the first difference of the input data and applies the same procedure mentioned above. The default for Diff argument is False.
## The default for Reshape argument is FALSE which produces a desired output without reshaping the results. If Reshape = TRUE, then the desired output is reshaped with results only.
## Lag length selection: An important practical issue for the implementation of the ADF test is the specification of the lag length p. If p is too small, then the remaining serial correlation in the errors will bias the test. If p is too large, then the power of the test will suffer. Therfore, while runing the Enders' ADF procedure 4 types of lag length selection criteria will be used: BIC, AIC, AIC* and the suggestions of Ng and Perron (1995) and Schwert (1989). For lag selection with BIC and AIC: Lag length is selected with SelectModel function in FitAR. The function selects the best order(lag) for AR model with BIC or AIC criterion by utilizing from "McLeod, A.I. and Zhang, Y. (2006) Partial Autocorrelation Parameterization for Subset Autoregression. Journal of Time Series Analysis, 27, 599-612". Note that as default in SelectModel function lag.max argument is defined as frequency of univariate series (e.g., for daily it is 365, for weekly it is 52, and for monthly it is 12). For lag selection with AIC*: Lag length is selected with AIC in AR model estimated by yule-walker method. Note that for AIC, BIC, and AIC*, the selected lag length is decreased by 1 since AIC, BIC and AIC* chooses lags in levels with either SelectModel or ar function but unit root tests uses series in first difference. For lag selection with Ng-Perron/Schwert: Lag length is selected by using the suggestions of Ng-Perron (1995) and Schwert (1989). Ng and Perron (1995) suggest the following data dependent lag length selection procedure that results in stable size of the test and minimal power loss: First, set an upper bound pmax for p. Next, estimate the ADF test regression with p = pmax. If the absolute value of the t-statistic for testing the significance of the last lagged difference is greater than 1.6, then set p = pmax and perform the unit root test. Otherwise, reduce the lag length by one and repeat the process. A useful rule of thumb for determining pmax, suggested by Schwert (1989), is pmax = trunc((12 * (length(data) / 100) ^ 0.25)). This choice allows pmax to grow with the sample so that the ADF test regressions are valid if the errors follow an ARMA process with unknown order.
## The default for Freq.Correction is 1. Note that in SelectModel function lag.max and Candidates arguments is defined as frequency of univariate series (e.g., for daily it is 365, for weekly it is 52, and for monthly it is 12). So while using daily or weekly data, the test procedure might take very long time due to defining the lag.max and Candidate arguments with the frequency of univariate series in SelectModel function. To mitigate this problem, the user can use the Freq.Correction argument which divides the frequency of the univariate series by the specified number.
## The output of the function is named as "adf.results" data frame.

# Usage:
#
# ADF.Enders.Procedure(Data, Significance.Level, Freq.Correction = 1, Diff = FALSE, Reshape = FALSE)
#
## Data: Time series (ts) object. It can be multivariate or univariate.
## Significance Level: Significance level for all test throughout the Enders' ADF Procedure.
## Freq.Correction: Divides the frequency of the univariate series by the specified number for using in SelectModel function.
## Diff: Takes the first difference of all time series.
## Reshape: Output is reshaped with results only.
## To run the function without any argument matching problem, make sure to specifiy Significance.Level, Freq.Correction, Diff and Reshape arguments with their names always (if non-default values are selected).

# Examples:
#
## ADF.Enders.Procedure(data.ts, Significance.Level = 1)
## ADF.Enders.Procedure(data.ts, Significance.Level = 5)
## ADF.Enders.Procedure(data.ts, Significance.Level = 10, Reshape = TRUE)
## ADF.Enders.Procedure(data.ts, Significance.Level = 1, Diff = TRUE, Reshape = TRUE)
## ADF.Enders.Procedure(data.ts, Significance.Level = 1, Freq.Correction = 1, Diff = TRUE, Reshape = TRUE)

ADF.Enders.Procedure <- function(Data, Significance.Level, Freq.Correction = 1, Diff = FALSE, Reshape = FALSE) {
    # Type of the test and lag selection procedure.
    tests <- list(test1 = c("ADF", "BIC"), test2 = c("ADF", "AIC"), test3 = c("ADF", "AIC*"), test4 = c("ADF", "NPS"))
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

    # Checks Freq.Correction argument.
    if (length(Freq.Correction) != 1)
        stop("Invalid Diff. Please choose only one Freq.Correction.\n")
    if (Freq.Correction < 1)
        stop("Invalid Freq.Correction. Please choose an value which is equal or larger than 1 for Freq.Correction.\n")
    if (!(Freq.Correction%%1 == 0))
        stop("Invalid Freq.Correction. Please choose an integer value for Freq.Correction.\n")

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

    # Runing the ADF tests with selected arguments for all univarite time series in input data.
    for (i in 1:ncol) {
        temp <- data.frame(Variable = rep(NA, utest), Test = rep(NA, utest), Lag.Selection = rep(NA, utest), Result = rep(NA, utest), Model.Specified = rep(NA, utest), Decided = rep(NA, utest), stringsAsFactors = FALSE) ## Data frame containing the results.

        if (is.null(ncol(Data))) { ## Data is in ts vector format.
            ts <- Data ## Univariate time series.
        }
        if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
            ts <- Data[, colname[i]] ## Univariate time series.
        }
        if (Diff == TRUE) { ## Taking the first difference.
            ts <- diff(ts, lag = 1, diff = 1)
        }

        # Running the Enders' procedure for ADF by each type of ADF test specified at the beginning of this function.
        for (j in 1:utest) {
            # Lag length selection with 3 different ways.
            if (tests[[j]][2] == "AIC" | tests[[j]][2] == "BIC") {  ## Lag selection with SelectModel function in FitAR package.
                lags <- SelectModel(ts, lag.max = round(frequency(ts)/Freq.Correction), ARModel = c("AR"), Criterion = tests[[j]][2], Best = 1, Candidates = round(frequency(ts)/Freq.Correction)) ## Note that as default in SelectModel function lag.max and Candidates arguments is defined as frequency of univariate series (e.g., for daily it is 365, for weekly it is 52, and for monthly it is 12).
            }
            if (tests[[j]][2] == "AIC*") { ## Lag selection with an Autoregressive Model.
                lags <- ar(ts, aic = TRUE, method = c("yule-walker"))$order
            }
            if (tests[[j]][2] == "NPS") {  ## Lag selection with Ng-Perron/Schwert method.
                pmax <- trunc((12 * (length(ts) / 100) ^ 0.25))
                test.pmax <- ur.df(ts, type = c("trend"), lags = pmax)
                while (abs(test.pmax@testreg$coefficients[nrow(test.pmax@testreg$coefficients), 3]) <= 1.6 & 1 <= pmax) {
                    pmax <- pmax - 1
                    test.pmax <- ur.df(ts, type = c("trend"), lags = pmax)
                }
                lags <- pmax
            }
            if (tests[[j]][2] %in% c("AIC", "BIC", "AIC*")) {
                lags <- lags - 1 ## Decreasing the lag length by 1 since AIC, BIC and AIC* chooses lags in level with either SelectModel or ar function because unit root tests uses series in first difference.
            }
            if (lags < 1) { ## Sometimes SelectModel or AR functions choose the best model without any lag. However, we want to control for serial correlation so the lag is specified as 1. This step is necessary since some part of the code does not work when the lag length is 0.
                message(paste0("For series ", colname[i], " with ", tests[[j]][2], " lag selection method, SelectModel function chooses ", lags, " lag which causes errors in manually written functions. Thus for this series, the selected lag is 1 which is the minimum lag accepted.\n"))
                lags <- 1
            }

            # Variable preperation for specific regressions throughout the Enders' ADF procedure.
            ## At some point in the below procedure, it is necesarry to run specific regressions (which cannot be done with built-in R functions). Therefore, the following code prepares the necessary variables. The code (note that lag length is defined in the previous section) is taken from the following web address: <http://www.r-bloggers.com/unit-root-tests/>
            z <- diff(ts)
            n <- length(z)
            z.diff <- embed(z, lags + 1)[ ,1]
            z.lag.1 <- ts[(lags + 1):n]
            k <- lags + 1
            z.diff.lag <- embed(z, lags + 1)[ ,2:k]
            trend <- (lags + 1):n

            # Filling same variables.
            temp$Variable[j] <- colname[i]
            temp$Test[j] <- tests[[j]][1]
            temp$Lag.Selection[j] <- paste0(tests[[j]][2], "(", lags, ")")

            # Step 1
            #-------------------------------------------------------------------
            ## Started with the least restrictive model (with drift and trend).
            test.trend <- ur.df(ts, type = c("trend"), lags = lags)


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
                        test.drift <- ur.df(ts, type = c("drift"), lags = lags)

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
                                    test.none <- ur.df(ts, type = c("none"), lags = lags)

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
                                test.none <- ur.df(ts, type = c("none"), lags = lags)

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
                    test.drift <- ur.df(ts, type = c("drift"), lags = lags)

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
                                test.none <- ur.df(ts, type = c("none"), lags = lags)

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
                            test.none <- ur.df(ts, type = c("none"), lags = lags)

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

    # Reshaping results for final use.
    if (Reshape == TRUE) {
        main$Lag.Selection <- gsub("\\([0-9]{1,2}\\)", "", main$Lag.Selection)
        main$Test <- paste0(main$Test, ".", main$Lag.Selection)
        main <- main[, c(1:2, 4)]
        main <- dcast(main, Variable ~ Test, value.var = "Result")
        if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
            main <- main[match(c(colnames(data.ts)), main$Variable), ]
        }
        rownames(main) <- 1:nrow(main)
    }
    assign("adf.tests", main, envir = globalenv()) ## Output data will be named as "adf.tests".
}

#==================================== END ======================================
