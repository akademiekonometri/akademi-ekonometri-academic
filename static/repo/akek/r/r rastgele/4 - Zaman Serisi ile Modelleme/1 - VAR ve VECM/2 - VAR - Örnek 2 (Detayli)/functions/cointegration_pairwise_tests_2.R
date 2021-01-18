#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#=========================== Coint.Pairwise.Tests.2 ============================
#========= Cointegration Pairwise Tests - with Statistics and Results ==========

# Notes:
#
## Function runs the pairwise cointegration tests: 1) Phillips-Ouliaris (PO) cointegration test: This is a 2-step method proposed by Engle and Granger (1987) where direction matters. Insted of using Augmented Dickey-Fuller (ADF) unit root test in the second stage, Phillips-Perron (PP) unit root test is used. Constant is added to cointegration regression (1st step) as default, and no constant added to auxiliary regression (2nd step). 2) Johansen Cointegration test: This is a systematic method where direction does not matter. User can use either Trace or Maximum Eigenvalue Tests (according to Lutkepohl (2000), Trace test has more power in small samples hence preferred). Only constant is added to model as deafult.
## Function requires a time series (ts) object, either bivariate or multivariate.
## Lag Length Selection: Lag length in PO tests (for 2nd step) is selected with PO.Lag argument with default as Short. Lag specification in PO tests are either Short = trunc((4 * (nrow(data) / 100) ^ 0.25)) or Long = trunc((12 * (nrow(data) / 100) ^ 0.25)). Lag length in Johansen tests is selected with Johansen.Lag argument by AIC (Akaike Information Criterion) as default since Kilian (2001) stated that even in finite-order VAR models in many cases of practical interest the AIC is likely to be preferable to the BIC or HQ because MSE is lower and impulse response confidence intervals are more accurate then others. Lag length in Johansen tests is determined in VARselect function from vars package by using several different information criterias. Note that in VARselect function lag.max argument is defined as frequency of bivariate series (e.g., for weekly it is 52, and for monthly it is 12). User can select on of the followings for lag selection: AIC, BIC, HQ (Hannan and Quinn Information Criterion) and FPE (Final Prediction Error).
## The default for Model.Type is Constant which adds only intercept to Johansen tests (cointegration) and to 1st regression in PO tests. User can also select none and trend. See source code for more information.
## The default for Johansen.Type is Trace which is suggested by Lütkepohl (2000) to have more power in small sample sizes. User can also seleect Eigen.
## The default for Season argument is NULL which does not add any seasonal dummies in Johansen cointegration tests and does not use seasonal dummies in Johansen test lag procedure. If Season is TRUE, then the frequency of the input time series object will be used in lag selection and in Johansen cointegration test.
## Note that in PO tests direction matters so the results can be asymmetric. So, PO test results will be tabulated with two values in each cell. The first value indicates test result when the row variable is on the right hand side and the second value indicates when the colomn variable is on the right hand side of the auxiliary regression (2nd step). All the PO results will be on the lower triangle of the output table. Since the Johansen tests are symmetric, only one value in each cell will be used. All the Johansen test results will be on the upper triangle of the output table. The diagonal of the table will be empty.
## The outputs of the function are named as "coint.pairwise.po.results" and "coint.pairwise.jo.results" data frames.

# Usage:
#
# Coint.Pairwise.Tests.2(Data, PO.Lag = "Short", Johansen.Lag = "AIC", Model.Type = "Constant", Johansen.Type = "Trace", Season = NULL)
#
## Data: Time series (ts) object. It can be bivariate or multivariate. Cannot be univariate.
## PO.Lag: Lag length selection in PO tests.
## Johansen.Lag: Lag length selection criteria in Johansen tests.
## Model.Type: Deterministic parts for the cointegration relationship.
## Johansen.Type: Type of the Johansen cointegration test.
## Season: Add seasonal dummies in Johansen test lag selection procedure and in cointegration regression.
## To run the function without any argument matching problem, make sure to specifiy Johansen.Lag, Model.Type, Johansen.Type, Season arguments with their names always (if non-default values are selected).

# Examples:
#
## Coint.Pairwise.Tests.2(data.ts, PO.Lag = "Short", Johansen.Lag = "BIC")
## Coint.Pairwise.Tests.2(data.ts, PO.Lag = "Long", Model.Type = "Trend", Johansen.Type = "Eigen")
## Coint.Pairwise.Tests.2(data.ts, PO.Lag = "Long", Johansen.Lag = "HQ", Model.Type = "Trend", Season = TRUE)
## Coint.Pairwise.Tests.2(data.ts, PO.Lag = "Long", Johansen.Lag = "FPE", Model.Type = "None", Johansen.Type = "Eigen", Season = TRUE)

Coint.Pairwise.Tests.2 <- function(Data, PO.Lag = "Short", Johansen.Lag = "AIC", Model.Type = "Constant", Johansen.Type = "Trace", Season = NULL) {
    # Checks Data argument.
    if (!is.ts(Data))
        stop("Invalid Data. Please choose a time series (ts) data as input.\n")
    if (is.null(ncol(Data))) ## Data is in vector format and thus univariate.
        stop("Invalid Data. Please choose either a bivariate or multivariate time series (ts) data as input.\n")
    if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
        if (ncol(Data) == 1) ## Data is univariate
            stop("Invalid Data. Please choose either a bivariate or multivariate time series (ts) data as input.\n")
        ncol <- ncol(Data)
        colname <- colnames(Data)
    }

    # Checks Model.Type argument.
    if (length(Model.Type) != 1)
        stop("Invalid Model.Type. Please choose only one Model.Type.\n")
    if (!(Model.Type %in% c("None", "Constant", "Trend")))
        stop("Invalid Model.Type. Please choose one of the followings: 1) None: For model without intercept and trend. 2) Constant: For a model with intercept only. 3) Trend: For a model with intercept and trend in PO tests and for a model with trend only in Johansen tests.\n")
    po.model.type <- tolower(Model.Type)
    johansen.model.type <- ifelse(Model.Type == "None", "none", ifelse(Model.Type == "Constant", "const", "trend"))

    # Checks PO.Lag argument.
    if (length(PO.Lag) != 1)
        stop("Invalid PO.Lag. Please choose only one PO.Lag.\n")
    if (!(PO.Lag %in% c("Long", "Short")))
        stop("Invalid PO.Lag. Please choose one of the followings: 1) Short: To select short lag with predefined formula. 2) Long: To select long lag with predefined formula.\n")
    po.lag <- tolower(PO.Lag)

    # Checks Johansen.Lag argument.
    if (length(Johansen.Lag) != 1)
        stop("Invalid Johansen.Lag. Please choose only one Johansen.Lag.\n")
    if (!(Johansen.Lag %in% c("AIC", "BIC", "HQ", "FPE")))
        stop("Invalid Johansen.Lag. Please choose one of the followings: 1) AIC: For lag selection with AIC. 2) BIC: For lag selection with BIC. 3) HQ: For lag selection with HQ. 4) FPE: For lag selection with FPE.\n")
    johansen.lag <- ifelse(Johansen.Lag == "AIC", 1, ifelse(Johansen.Lag == "HQ", 2, ifelse(Johansen.Lag == "BIC", 3, 4)))

    # Checks Johansen.Type argument.
    if (length(Johansen.Type) != 1)
        stop("Invalid Johansen.Type. Please choose only one Johansen.Type.\n")
    if (!(Johansen.Type %in% c("Trace", "Eigen")))
        stop("Invalid Johansen.Type. Please choose one of the followings: 1) Trace: For Johansen trace test. 2) Eigen: For Johansen maximum eigenvalue test.\n")
    johansen.type <- tolower(Johansen.Type)

    # Checks Season argument.
    if (!is.null(Season)) {
        if (length(Season) != 1)
            stop("Invalid Season. Please choose only one Season.\n")
        if (!(Season %in% c("TRUE")))
            stop("Invalid Season. Season can only be NULL or TRUE.\n")
        freq <- frequency(Data)
    }
    if (is.null(Season)) {
        freq <- NULL
    }

    # Generating the data frame containing the results.
    temp <- as.data.frame(matrix(NA, nrow = ncol, ncol = ncol)) ## Data frame containing the results.
    colnames(temp) <- colname
    rownames(temp) <- colname

    # PO tests.
    for (i in 1:ncol) {
        for (j in 1:ncol) {
            if (i <= j) {
                temp[i, j] <- ""
            }
            if (i > j) {
                # From i to j: the variable on row i is on the right hand side and the variable on column j is on left hand side of the cointegration regression (1st step).
                ts <- Data[, c(j, i)]
                test <- ca.po(ts, demean = po.model.type, lag = po.lag, type = "Pu")
                test.stat <- test@teststat[1]
                test.decision <- ifelse(test.stat > test@cval[1, 3], "***", ifelse(test.stat > test@cval[1, 2], "**", ifelse(test.stat > test@cval[1, 1], "*", "")))
                test.stat <- format(round(test@teststat[1], 2), nsmall = 2)
                po.result1 <- paste0(test.stat, test.decision)
                temp[i, j] <- paste0(po.result1)

                # From j to i: the variable on column j is on the right hand side and the variable on row i is on left hand side of the cointegration regression (1st step).
                ts <- Data[, c(i, j)]
                test <- ca.po(ts, demean = po.model.type, lag = po.lag, type = "Pu")
                test.stat <- test@teststat[1]
                test.decision <- ifelse(test.stat > test@cval[1, 3], "***", ifelse(test.stat > test@cval[1, 2], "**", ifelse(test.stat > test@cval[1, 1], "*", "")))
                test.stat <- format(round(test@teststat[1], 2), nsmall = 2)
                po.result2 <- paste0(test.stat, test.decision)
                temp[j, i] <- paste0(po.result2)
            }
        }
    }
    assign("coint.pairwise.po.results", temp, envir = globalenv()) ## Output data will be named as "coint.pairwise.po.results"

    # Johansen tests.
    for (i in 1:ncol) {
        for (j in 1:ncol) {
            if (j <= i) {
                temp[i, j] <- ""
            }
            if (i < j) {
                ts <- Data[, c(i, j)]
                jo.lag <- VARselect(ts, lag.max = frequency(ts), type = johansen.model.type, season = freq)$selection[[johansen.lag]] ## Lag selection with VARselect function. Note that lag.max is defined as frequency of bivariate series.
                if (jo.lag < 2) {
                    message(paste0("For pairs ", colname[i], " and ", colname[j], " with ", Johansen.Lag, " lag selection method, VARselect function chooses ", jo.lag, " lag which is not accepted in ca.jo function. Thus for this pair, the selected lag is 2 which is the minimum lag accepted.\n"))
                    jo.lag <- 2
                }
                test <- ca.jo(ts, type = johansen.type, ecdet = johansen.model.type, K = jo.lag, spec = "transitory", season = freq) ## Since the ca.jo test take lag argument (K) from the VAR which is specified in VARselect function, there is no need to decrease the lag by 1.

                # Trace test for Rank = 0 versus Rank = 1.
                ## DS: If not rejected then Rank = 0 which means that the error correction term disappears and the system is stationary in first differences assuming that the variables are unit root.
                ## C: If rejected then then Rank = 1 (there is 1 independent cointegrating relation among the bivariate variables) which means that there is cointegration assuming that the variables are unit root as long as the Rank <= 1 test is not rejected.
                test.stat.r0 <- test@teststat[[2]]
                test.decision.r0 <- ifelse(test.stat.r0 > test@cval[2, 3], "***", ifelse(test.stat.r0 > test@cval[2, 2], "^{**}", ifelse(test.stat.r0 > test@cval[2, 1], "*", "")))
                test.stat.r0 <- format(round(test@teststat[[2]], 2), nsmall = 2)
                johansen.result.r0 <- paste0(test.stat.r0, test.decision.r0)

                # Trace test for Rank <= 1 versus Rank = 2.
                ## C: If the first test is rejected and this test is not rejected then C applies.
                ## DS: If rejected then Rank = 2 (full rank) which is an impossible result and the decision is that the system is stationary in levels.
                test.stat.r1 <- test@teststat[[1]]
                test.decision.r1 <- ifelse(test.stat.r1 > test@cval[1, 3], "***", ifelse(test.stat.r1 > test@cval[1, 2], "**", ifelse(test.stat.r1 > test@cval[1, 1], "*", "")))
                test.stat.r1 <- format(round(test@teststat[[1]], 2), nsmall = 2)
                johansen.result.r1 <- paste0(test.stat.r1, test.decision.r1)

                # Writing the results
                temp[i, j] <- paste0(johansen.result.r0, " | ", johansen.result.r1)
            }
        }
    }
    assign("coint.pairwise.jo.results", temp, envir = globalenv()) ## Output data will be named as "coint.pairwise.jo.results".
}

#==================================== END ======================================
