#=========================== JO.Coint.Pairwise.Tests ===========================
#================= Johansen (JO) Cointegration Pairwise Tests ==================

# Notes:
#
## Function runs the Joahnsen (JO) cointegration pairwise tests.
## Johansen Cointegration test is a systematic method where direction does not matter, so the it is a symmetric test. User can use either Trace or Maximum Eigenvalue Tests (according to Lutkepohl (2000), Trace test has more power in small samples hence preferred). Only constant is added to model as deafult.
## In cointegration test the transitory form of VECM is used as default and can only be change in function source code.
## Function requires a time series (ts) object, either bivariate or multivariate.
## Lag length is selected with Lag argument by AIC (Akaike Information Criterion) as default since Kilian (2001) stated that even in finite-order VAR models in many cases of practical interest the AIC is likely to be preferable to the BIC or HQ because MSE is lower and impulse response confidence intervals are more accurate then others. Lag length is determined in VARselect function from vars package by using several different information criteria. User can select on of the followings for lag selection: AIC, BIC, HQ (Hannan and Quinn Information Criterion) and FPE (Final Prediction Error). Note that as default in VARselect function lag.max is defined as lag.short = trunc((4 * (length(ts) / 100)^0.25)).
## The default for Deterministic is "Constant" which adds only intercept to 1st regression. User can also select "None" and "Trend". Using Deterministic = "Trend" adds only trend the regression.
## The default for Type is Trace which is suggested by Lütkepohl (2000) to have more power in small sample sizes. User can also select Eigen for JO test.
## The default for Season argument is NULL which does not add any seasonal dummies in Johansen cointegration tests and does not use seasonal dummies in Johansen test lag procedure. If Season is TRUE, then the frequency of the input time series object will be used in lag selection and in Johansen cointegration test.
## The default for LaTeX is FALSE which produces output only for printing.
## All the test results will be on the upper triangle of the output table. The diagonal of the table will be empty.
## The outputs of the function is named as "jo.coint.pairwise.results" data frames.

# Usage:
#
# JO.Coint.Pairwise.Test(Data, Lag = "AIC", Deterministic = "Constant", Type = "Trace", Season = NULL, LaTeX = FALSE)

#
## Data: Time series (ts) object. It can be bivariate or multivariate. Cannot be univariate.
## Lag: Character. Lag length selection in JO tests. Options are AIC, BIC, HQ, and FPE.
## Deterministic: Character. Deterministic parts for the cointegration relationship. Options are None, Constant, Trend.
## Type: Type of the Johansen cointegration test. Options are Trace and Eigen.
## Season: Logical. Add seasonal dummies in Johansen test lag selection procedure and in cointegration regression.
## LaTeX: Logical. Output is corrected for LaTeX.
## To run the function without any argument matching problem, make sure to specify all arguments with their names always (if non-default values are selected).

# Examples:
#
## JO.Coint.Pairwise.Test(Data = data.ts)
## JO.Coint.Pairwise.Test(Data = data.ts, Lag = "BIC", Deterministic = "Trend", Type = "Eigen", LaTeX = TRUE)
## JO.Coint.Pairwise.Test(Data = data.ts, Lag = "HQ", Deterministic = "None", Type = "Eigen", LaTeX = TRUE)

JO.Coint.Pairwise.Test <- function(Data, Lag = "AIC", Deterministic = "Constant", Type = "Trace", Season = NULL, LaTeX = FALSE) {
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
    if (!(Lag %in% c("AIC", "BIC", "HQ", "FPE")))
        stop("Invalid Lag. Please choose one of the followings: 1) AIC: For lag selection with AIC. 2) BIC: For lag selection with BIC. 3) HQ: For lag selection with HQ. 4) FPE: For lag selection with FPE.\n")
    lag <- ifelse(Lag == "AIC", 1, ifelse(Lag == "HQ", 2, ifelse(Lag == "BIC", 3, 4)))

    # Checks Deterministic argument.
    if (length(Deterministic) != 1)
        stop("Invalid Deterministic. Please choose only one Deterministic.\n")
    if (!(Deterministic %in% c("None", "Constant", "Trend")))
        stop("Invalid Deterministic. Please choose one of the followings: 1) None: For model without intercept and trend. 2) Constant: For a model with intercept only. 3) Trend: For a model with trend only.\n")
    deterministic <- tolower(Deterministic)
    deterministic <- ifelse(Deterministic == "None", "none", ifelse(Deterministic == "Constant", "const", "trend"))

    # Checks Type argument.
    if (length(Type) != 1)
        stop("Invalid Type. Please choose only one Type.\n")
    if (!(Type %in% c("Trace", "Eigen")))
        stop("Invalid Type. Please choose one of the followings: 1) Trace: For Johansen trace test. 2) Eigen: For Johansen maximum eigenvalue test.\n")
    type <- tolower(Type)

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

    # Checks LaTeX argument.
    if (length(LaTeX) != 1)
        stop("Invalid LaTeX. Please choose only one LaTeX.\n")
    if (!(LaTeX %in% c("TRUE", "FALSE")))
        stop("Invalid LaTeX. Please choose one of the followings: 1) To generate output data frame for LaTeX: TRUE. 2) To generate output data frame for printing: FALSE.\n")

    # Generating the data frame containing the results.
    temp <- as.data.frame(matrix(NA, nrow = ncol, ncol = ncol)) ## Data frame containing the results.
    colnames(temp) <- colname
    rownames(temp) <- colname

    # Johansen Cointegration tests.
    for (i in 1:ncol) {
        for (j in 1:ncol) {
            if (j <= i) {
                temp[i, j] <- ""
            }
            if (i < j) {
                ts <- Data[, c(i, j)]

                ## Some common used lags can be applied which is taken directly from the literature. These are used for lag.max and Candidates arguments in SelectModel function.
                lag.short <- trunc((4 * (length(ts) / 100)^0.25)) ## Short specified lags generally used for PP and KPSS tests.
                lag.long <- trunc((12 * (length(ts) / 100)^0.25)) ## Long specified lags generally used for PP and KPSS tests.

                jo.lag <- VARselect(ts, lag.max = lag.short, type = deterministic, season = freq)$selection[[lag]] ## Lag selection with VARselect function. Note that as default in VARselect function lag.max is defined as lag.short = trunc((4 * (length(ts) / 100)^0.25)).
                if (jo.lag < 2) {
                    message(paste0("For pairs ", colname[i], " and ", colname[j], " with ", Lag, " lag selection method, VARselect function chooses ", jo.lag, " lag which is not accepted in ca.jo function. Thus for this pair, the selected lag is 2 which is the minimum lag accepted.\n"))
                    jo.lag <- 2
                }
                test <- ca.jo(ts, type = type, ecdet = deterministic, K = jo.lag, spec = "transitory", season = freq) ## Since the ca.jo test take lag argument (K) from the VAR which is specified in VARselect function, there is no need to decrease the lag by 1.

                # Trace test for Rank = 0 versus Rank = 1.
                ## DS: If not rejected then Rank = 0 which means that the error correction term disappears and the system is stationary in first differences assuming that the variables are unit root.
                ## C: If rejected then then Rank = 1 (there is 1 independent cointegrating relation among the bivariate variables) which means that there is cointegration assuming that the variables are unit root as long as the Rank <= 1 test is not rejected.
                test.stat.r0 <- test@teststat[[2]]
                test.decision.r0 <- ifelse(test.stat.r0 > test@cval[2, 3], "***", ifelse(test.stat.r0 > test@cval[2, 2], "^{**}", ifelse(test.stat.r0 > test@cval[2, 1], "*", "")))
                test.stat.r0 <- format(round(test@teststat[[2]], 2), nsmall = 2)
                johansen.result.r0 <- paste0("$", test.stat.r0, test.decision.r0, "$")

                # Trace test for Rank <= 1 versus Rank = 2.
                ## C: If the first test is rejected and this test is not rejected then C applies.
                ## DS: If rejected then Rank = 2 (full rank) which is an impossible result and the decision is that the system is stationary in levels.
                test.stat.r1 <- test@teststat[[1]]
                test.decision.r1 <- ifelse(test.stat.r1 > test@cval[1, 3], "***", ifelse(test.stat.r1 > test@cval[1, 2], "**", ifelse(test.stat.r1 > test@cval[1, 1], "*", "")))
                test.stat.r1 <- format(round(test@teststat[[1]], 2), nsmall = 2)
                johansen.result.r1 <- paste0("$", test.stat.r1, test.decision.r1, "$")

                # Writing the results
                # temp[i, j] <- paste0(johansen.result.r0, " | ", johansen.result.r1)
                temp[i, j] <- paste0(johansen.result.r0, "   $\\mid$   ", johansen.result.r1)
            }
        }
    }

    # Saving the results as main data frame.
    main <- temp

    # Correcting the results if not used for LaTeX.
    if (LaTeX == FALSE) {
        for (i in 1:ncol(main)) {
            main[, i] <- gsub("(\\$)|(\\^\\{)|(\\})", "", main[, i])
            main[, i] <- gsub("(   \\\\mid   )", " | ", main[, i])
        }
    }

    # Output data will be named as "jo.coint.pairwise.results"
    assign("jo.coint.pairwise.results", main, envir = globalenv())

    # Revert the working directory to initial path.
    setwd(WD.temp)
}

#==================================== END ======================================
