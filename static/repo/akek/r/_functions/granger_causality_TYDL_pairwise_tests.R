#======================= Granger.Causality.TYDL.Pairwise =======================
#==================== Granger Causality TYDL Pairwise Tests ====================

# Notes:
#
## Function runs the pairwise TYDL version of Granger Causality tests.
## If any of the time series are non–stationary (whether or not they are cointegrated), then the Wald test statistic does not follow its usual asymptotic 𝜒2 distribution under the null hypothesis. In that case the usual asymptotic distribution of the test statistic may not be valid under the null hypothesis. To mitigate these problems, Toda and Yamamoto (1995) and Dolado and Lutkepohl (1996) (TYDL henceforth) based on augmented VAR modeling introduced a modified Wald test statistic (MWALD). For more information about TYDL method please see the following links: http://davegiles.blogspot.de/2011/04/testing-for-granger-causality.html, and http://www.christophpfeiffer.org/2012/11/07/toda-yamamoto-implementation-in-r/.
## Function requires a time series (ts) object, either bivariate or multivariate.
## Int.Numbers should be numeric vector with a length same as the column number of the input data.
## Lag length is selected with Lag argument by AIC (Akaike Information Criterion) as default since Kilian (2001) stated that even in finite-order VAR models in many cases of practical interest the AIC is likely to be preferable to the BIC or HQ because MSE is lower and impulse response confidence intervals are more accurate then others. Lag length is determined in VARselect function from vars package by using several different information criteria. User can select on of the followings for lag selection: AIC, BIC, HQ (Hannan and Quinn Information Criterion) and FPE (Final Prediction Error). Note that as default in VARselect function lag.max is defined as lag.short = trunc((4 * (length(ts) / 100)^0.25)).
## The default for Deterministic is Constant which adds only intercept to VAR analysis. User can also select "None", "Trend", and "Both".
## The default for Season argument is NULL which does not add any seasonal dummies in VAR lag selection procedure and in VAR analysis. If Season is TRUE, then the frequency of the input time series object will be used in lag selection and in VAR analysis.
## The default for Exogen argument is NULL which does not add any exogenous series in VAR lag selection procedure and in VAR analysis. If Exogen is TRUE, then the supplied exogenous series will be used in lag selection and in VAR analysis. Note that Exogen is a time series (ts) object, either univariate, bivariate or multivariate, with the specified column and the row number same as the main data set.
## The default for LaTeX is FALSE which produces output only for printing.
## The outputs of the function are named as "granger.causality.TYDL.pairwise.results" data frames.

# Usage:
#
# Granger.Causality.TYDL.Pairwise(Data, Int.Numbers, Lag = "AIC", Deterministic = "Constant", Season = NULL, Exogen = NULL, LaTeX = FALSE)
#
## Data: Time series (ts) object. It can be bivariate or multivariate. Cannot be univariate.
## Int.Numbers: Numeric Vector. Numeric vector with a length same as the column number of the input data.
## Lag: Character. Lag length selection in JO tests. Options are AIC, BIC, HQ, and FPE.
## Deterministic: Character. Deterministic parts for the cointegration relationship. Options are None, Constant, Trend, Both.
## Season: Logical. Add seasonal dummies in VAR lag selection procedure and in VAR analysis.
## Exogen: Logical. Add exogenous series in VAR lag selection procedure and in VAR analysis.
## LaTeX: Logical. Output is corrected for LaTeX.
## To run the function without any argument matching problem, make sure to specify all arguments with their names always (if non-default values are selected).

# Examples:
#
## Granger.Causality.TYDL.Pairwise(Data = data.ts, Int.Numbers = int.numbers)
## Granger.Causality.TYDL.Pairwise(Data = data.ts, Int.Numbers = int.numbers, Lag = "BIC")
## Granger.Causality.TYDL.Pairwise(Data = data.ts, Int.Numbers = int.numbers, Lag = "HQ", Deterministic = "Trend", LaTeX = TRUE)
## Granger.Causality.TYDL.Pairwise(Data = data.ts, Int.Numbers = int.numbers, Lag = "FPE", Season = TRUE, Exogen = exogenous.ts, LaTeX = TRUE)

Granger.Causality.TYDL.Pairwise <- function(Data, Int.Numbers, Lag = "AIC", Deterministic = "Constant", Season = NULL, Exogen = NULL, LaTeX = FALSE) {
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

    # Checks Int.Numbers argument.
    if (is.null(Int.Numbers))
        stop("Invalid Int.Numbers. Int.Numbers cannot be NULL.\n")
    if (!is.null(Int.Numbers)) {
        if (length(Int.Numbers) != ncol | sum(!is.numeric(Int.Numbers)) != 0)
            stop("Invalid Int.Numbers. Int.Numbers should be a numeric vector with a length same as the column number of the input data.\n")
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
    if (!(Deterministic %in% c("None", "Constant", "Trend", "Both")))
        stop("Invalid Deterministic. Please choose one of the followings: 1) None: For model without intercept and trend. 2) Constant: For a model with intercept only. 3) Trend: For a model with trend only. 4) Both: For a model with intercept and trend.\n")
    deterministic <- ifelse(tolower(Deterministic) == "constant", "const", tolower(Deterministic))

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

    # Checks Exogen argument.
    if (!is.null(Exogen)) {
        if (!is.ts(Exogen))
            stop("Invalid Exogen. Please choose a time series (ts) data as Exogen.\n")
        if (frequency(Data) != frequency(Exogen))
            stop("Invalid Exogen. Please choose Exogen data set with frequency equal to main data set.\n")
        if (nrow(Data) != nrow(Exogen))
            stop("Invalid Exogen. Please choose Exogen data set with length equal to main data set.\n")
        if (is.null(ncol(Exogen)))
            stop("Invalid Exogen. Please choose Exogen data set with column names even if there is only one exogenous series.\n")
        exo <- Exogen
    }
    if (is.null(Exogen)) {
        exo <- NULL
    }

    # Checks LaTeX argument.
    if (length(LaTeX) != 1)
        stop("Invalid LaTeX. Please choose only one LaTeX.\n")
    if (!(LaTeX %in% c("TRUE", "FALSE")))
        stop("Invalid LaTeX. Please choose one of the followings: 1) To generate output data frame for LaTeX: TRUE. 2) To generate output data frame for printing: FALSE.\n")

    # Creating the Int.Numbers data set for identifying the integration number of each series.
    Int.Numbers <- data.frame(Int.Order = Int.Numbers, stringsAsFactors = FALSE)
    row.names(Int.Numbers) <- colnames(Data)

    # Generating the data frame containing the results.
    temp <- as.data.frame(matrix(NA, nrow = ncol, ncol = ncol)) ## Data frame containing the results.
    colnames(temp) <- colname
    rownames(temp) <- colname
    temp[1:ncol, 1:ncol] <- ""

    # Pairwise VAR Analysis: Granger Causality TYDL Pairwise tests.
    for (i in 1:ncol) {
        for (j in 1:ncol) {
            if (i > j) {
                ts <- Data[, c(i, j)] ## Since the coding system is dynamic it does not matter which series is first.

                ## Some common used lags can be applied which is taken directly from the literature. These are used for lag.max in VARselect function.
                lag.short <- trunc((4 * (length(ts) / 100)^0.25)) ## Short specified lags generally used for PP and KPSS tests.
                lag.long <- trunc((12 * (length(ts) / 100)^0.25)) ## Long specified lags generally used for PP and KPSS tests.

                dmax <- max(Int.Numbers[c(rownames(Int.Numbers) %in% colnames(ts)), , drop = FALSE]$Int.Order) ## Maximum number of integration.
                var.lag <- suppressWarnings(VARselect(ts, lag.max = lag.short, type = deterministic, season = freq, exogen = exo)$selection[[lag]]) ## Lag selection with VARselect function. Note that as default in VARselect function lag.max is defined as lag.short = trunc((4 * (length(ts) / 100)^0.25)).
                var <- vars::VAR(ts, p = var.lag + dmax, type = deterministic, season = freq, exogen = exo) ## Sum the information criterion selected lag and maximum number of integration in the system.

                # From i to j: the variable on row i Granger-Cause the variable on column j.
                eq.name <- c(grep(paste0("(*", colnames(Data)[j], "$)"), names(var$datamat), value = TRUE))[1]
                wald <- wald.test(b = coef(var$varresult[[eq.name]]), Sigma = vcov(var$varresult[[eq.name]]), Terms = c(grep(colnames(Data)[i], names(var$varresult[[eq.name]]$coefficients)))[1:var.lag]) ## Wald-test (H0: Series in row i does not Granger-Cause series in column j.)
                wald.stat <- wald$result$chi2[[1]] ## Test statistics.
                wald.pvalue <- wald$result$chi2[[3]] ## P-value.
                wald.decision <- ifelse(wald.pvalue < 0.01, "^{***}", ifelse(wald.pvalue < 0.05, "^{**}", ifelse(wald.pvalue < 0.1, "^{*}", ""))) ## Decision.
                wald.stat <- format(round(wald.stat, 2), nsmall = 2)
                wald.result <- paste0("$", wald.stat, wald.decision, "$") ## Result of the test.
                temp[i, j] <- paste0(wald.result)

                # From j to i: the variable on column j Granger-cause the variable on row i.
                eq.name <- c(grep(paste0("(*", colnames(Data)[i], "$)"), names(var$datamat), value = TRUE))[1]
                wald <- wald.test(b = coef(var$varresult[[eq.name]]), Sigma = vcov(var$varresult[[eq.name]]), Terms = c(grep(colnames(Data)[j], names(var$varresult[[eq.name]]$coefficients)))[1:var.lag]) ## Wald-test (H0: Series in column j does not Granger-cause series in row i.)
                wald.stat <- wald$result$chi2[[1]] ## Test statistics.
                wald.pvalue <- wald$result$chi2[[3]] ## P-value.
                wald.decision <- ifelse(wald.pvalue < 0.01, "^{***}", ifelse(wald.pvalue < 0.05, "^{**}", ifelse(wald.pvalue < 0.1, "^{*}", ""))) ## Decision.
                wald.stat <- format(round(wald.stat, 2), nsmall = 2)
                wald.result <- paste0("$", wald.stat, wald.decision, "$") ## Result of the test.
                temp[j, i] <- paste0(wald.result)
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

    # Output data will be named as "granger.causality.TYDL.pairwise.results".
    assign("granger.causality.TYDL.pairwise.results", main, envir = globalenv())

    # Revert the working directory to initial path.
    setwd(WD.temp)
}

#==================================== END ======================================
