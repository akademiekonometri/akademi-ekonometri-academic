#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#========================== Granger.Causality.TYDL.2 ===========================
#======== Granger Causality Tests (TYDL) - with Statistics and Results =========

# Notes:
#
## Function runs the pairwise VAR and multivariate VAR Granger-causality tests.
## If any of the time series are non–stationary (whether or not they are cointe- grated), then the Wald test statistic does not follow its usual asymptotic 𝜒2 distribution under the null hypothesis. In that case the usual asymptotic distribution of the test statistic may not be valid under the null hypothesis. To mitigate these problems, Toda and Yamamoto (1995) and Dolado and Lutkepohl (1996) (TYDL henceforth) based on augmented VAR modeling introduced a modified Wald test statistic (MWALD). For more information about TYDL method please see the following links: http://davegiles.blogspot.de/2011/04/testing-for-granger-causality.html, and http://www.christophpfeiffer.org/2012/11/07/toda-yamamoto-implementation-in-r/.
## Function requires a time series (ts) object, either bivariate or multivariate.
## Int.Numbers should be numeric vector with a length same as the column number of the input data.
## Lag Length Selection: Lag length in VAR analysis is selected with Lag.Selection argument by AIC (Akaike Information Criterion) as default since Kilian (2001) stated that even in finite-order VAR models in many cases of practical interest the AIC is likely to be preferable to the BIC or HQ because MSE is lower and impulse response confidence intervals are more accurate then others. Lag length in VAR analysis is determined in VARselect function from vars package by using several different information criteria. Note that in VARselect function lag.max argument is defined as frequency of bivariate or multivariate series (e.g., for weekly it is 52, and for monthly it is 12). User can select on of the followings for lag selection: AIC, BIC, HQ (Hannan and Quinn Information Criterion) and FPE (Final Prediction Error).
## The default for Divide.Lag.Max is 1. Divide.Lag.Max divides the frequency of data by the selected value in lag.max argument in lag selection function for only multivariate VAR.
## The default for Model.Type is Constant which adds only intercept to VAR analysis. User can also select None, Trend, and Both.
## The default for Season argument is NULL which does not add any seasonal dummies in VAR lag selection procedure and in VAR analysis. If Season is TRUE, then the frequency of the input time series object will be used in lag selection and in VAR analysis.
## The default for Exogen argument is NULL which does not add any exogenous series in VAR lag selection procedure and in VAR analysis. If Exogen is TRUE, then the supplied exogenous series will be used in lag selection and in VAR analysis. Note that Exogen is a time series (ts) object, either univariate, bivariate or multivariate, with the specified column and the row number same as the main data set.
## The default for Result.Type argument is Pairwise which only runs the pairwise VAR analysis. User can choose multivariate to run only multivariate VAR analysis and both to run both the pairwise and multivariate VAR analysis.
## Note that in both of the pairwise and multivariate VAR analysis direction matters so the results can be asymmetric. So, both the pairwise and multivariate VAR analysis results will be tabulated with two values in each cell. The first value indicates TYDL Granger-causality test result for the row variable granger causing the column variable and the second value indicates for the column variable granger causing the row variable. All the pairwise VAR analysis results will be on the lower triangle of the output table and all the multivariate VAR analysis results will be on the upper triangle of the output table. The diagonal of the table will be empty.
## The outputs of the function are named as "granger.TYDL.pairwise.results" and "granger.TYDL.multivariate.results" data frames.

# Usage:
#
# Granger.Causality.TYDL.2(Data, Int.Numbers, Lag.Selection = "AIC", Divide.Max.Lag = 1, Model.Type = "Constant", Season = NULL, Exogen = NULL, Result.Type = "Pairwise")
#
## Data: Time series (ts) object. It can be bivariate or multivariate. Cannot be univariate.
## Int.Numbers: Numeric vector with a length same as the column number of the input data.
## Lag.Selection: Lag length selection criteria in for VAR analysis.
## Divide.Lag.Max: Divides the frequency of data by the selected value in lag.max argument in lag selection function for only multivariate VAR.
## Model.Type: Deterministic parts for the cointegration relationship.
## Model.Type: Type of the determinitic part in VAR analysis.
## Season: Add seasonal dummies in VAR lag selection procedure and in VAR analysis.
## Exogen: Add exogenous series in VAR lag selection procedure and in VAR analysis.
## Result.Type: Runs either/both pairwise VAR and multivariate VAR analysis.
## To run the function without any argument matching problem, make sure to specifiy Lag.Selection, Model.Type, Season, Exogen and Result.Type arguments with their names always (if non-default values are selected).

# Examples:
#
## Granger.Causality.TYDL.2(data.ts)
## Granger.Causality.TYDL.2(data.ts, Lag.Selection = "BIC")
## Granger.Causality.TYDL.2(data.ts, Int.Numbers = int.numbers, Lag.Selection = "HQ", Model.Type = "Trend")
## Granger.Causality.TYDL.2(data.ts, Lag.Selection = "FPE", Model.Type = "Both", Season = NULL, Exogen = exogenous.ts, Result.Type = "Multivariate")
## Granger.Causality.TYDL.2(data.ts, Lag.Selection = "AIC", Divide.Lag.Max = 2, Model.Type = "Constant", Season = TRUE, Exogen = exogenous.ts, Result.Type = "Both")

Granger.Causality.TYDL.2 <- function(Data, Int.Numbers, Lag.Selection = "AIC", Divide.Lag.Max = 1, Model.Type = "Constant", Season = NULL, Exogen = NULL, Result.Type = "Pairwise") {
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

    # Checks Int.Numbers argument.
    if (is.null(Int.Numbers))
        stop("Invalid Int.Numbers. Int.Numbers cannot be NULL.\n")
    if (!is.null(Int.Numbers)) {
        if (length(Int.Numbers) != ncol | sum(!is.numeric(Int.Numbers)) != 0)
            stop("Invalid Int.Numbers. Int.Numbers should be a numeric vector with a length same as the column number of the input data.\n")
    }

    # Checks Model.Type argument.
    if (length(Model.Type) != 1)
        stop("Invalid Model.Type. Please choose only one Model.Type.\n")
    if (!(Model.Type %in% c("None", "Constant", "Trend", "Both")))
        stop("Invalid Model.Type. Please choose one of the followings: 1) None: For model without intercept and trend. 2) Constant: For a model with intercept only. 3) Trend: For a model with trend only. 3) Both: For a model with intercept and trend.\n")
    model.type <- tolower(Model.Type)
    model.type <- ifelse(Model.Type == "Constant", "const", model.type)

    # Checks Lag.Selection argument.
    if (length(Lag.Selection) != 1)
        stop("Invalid Lag.Selection. Please choose only one Lag.Selection.\n")
    if (!(Lag.Selection %in% c("AIC", "BIC", "HQ", "FPE")))
        stop("Invalid Lag.Selection. Please choose one of the followings: 1) AIC: For lag selection with AIC. 2) BIC: For lag selection with BIC. 3) HQ: For lag selection with HQ. 4) FPE: For lag selection with FPE.\n")
    lag.selection <- ifelse(Lag.Selection == "AIC", 1, ifelse(Lag.Selection == "HQ", 2, ifelse(Lag.Selection == "BIC", 3, 4)))

    # Checks Divide.Lag.Max argument.
    if (length(Divide.Lag.Max) != 1)
        stop("Invalid Divide.Lag.Max. Please choose only one Divide.Lag.Max.\n")
    if (!is.numeric(Divide.Lag.Max))
        stop("Invalid Divide.Lag.Max. Please choose a numeric Divide.Lag.Max value.\n")

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

    # Checks Result.Type argument.
    if (length(Result.Type) != 1)
        stop("Invalid Result.Type. Please choose only one Result.Type.\n")
    if (!(Result.Type %in% c("Pairwise", "Multivariate", "Both")))
        stop("Invalid Result.Type. Please choose one of the followings: 1) Pairwise: To load results data with only TYDL Granger-causality from pairwise VAR analysis. 2) Multivariate: To load results data with only TYDL Granger-causality from multivariate VAR analysis. 3) Both: To load results data with TYDL Granger-causality from pairwise analysis and from VAR.\n")

    # Creating the Int.Numbers data set for identifying the integration number of each series.
    Int.Numbers <- data.frame(Int.Order = int.numbers, stringsAsFactors = FALSE)
    row.names(Int.Numbers) <- colnames(Data)

    # Generating the data frame containing the results.
    temp <- as.data.frame(matrix(NA, nrow = ncol, ncol = ncol)) ## Data frame containing the results.
    colnames(temp) <- colname
    rownames(temp) <- colname
    temp[1:ncol, 1:ncol] <- ""

    # Pairwise VAR Analysis: TYDL Granger-causality tests.
    if (Result.Type == "Pairwise" | Result.Type == "Both") {
        for (i in 1:ncol) {
            for (j in 1:ncol) {
                if (i > j) {
                    ts <- Data[, c(i, j)] ## Since the system is dynamic it does not matter which series is first.
                    dmax <- max(Int.Numbers[c(rownames(Int.Numbers) %in% colnames(ts)), , drop = FALSE]$Int.Order) ## Maximum number of integration.
                    lag <- VARselect(ts, lag.max = frequency(ts), type = model.type, season = freq, exogen = exo)$selection[[lag.selection]] ## Note that lag.max is defined as frequency of series.
                    var <- vars::VAR(ts, p = lag + dmax, type = model.type, season = freq, exogen = exo) ## Sum the information criterion selected lag and maximum number of integration in the system.

                    # From i to j: the variable on row i Granger-cause the variable on column j.
                    eq.name <- c(grep(paste0("(*", colnames(Data)[j], "$)"), names(var$datamat), value = TRUE))[1]
                    wald <- wald.test(b = coef(var$varresult[[eq.name]]), Sigma = vcov(var$varresult[[eq.name]]), Terms = c(grep(colnames(Data)[i], names(var$varresult[[eq.name]]$coefficients)))[1:lag]) ## Wald-test (H0: Series in row i does not Granger-cause series in column j.)
                    wald.stat <- wald$result$chi2[[1]] ## Test statistics.
                    wald.pvalue <- wald$result$chi2[[3]] ## P-value.
                    wald.decision <- ifelse(wald.pvalue < 0.01, "***", ifelse(wald.pvalue < 0.05, "**", ifelse(wald.pvalue < 0.1, "*", ""))) ## Decision.
                    wald.result <- paste0(format(round(wald.stat, 2), nsmall = 2), wald.decision) ## Result of the test.
                    temp[i, j] <- paste0(wald.result)

                    # From j to i: the variable on column j Granger-cause the variable on row i.
                    eq.name <- c(grep(paste0("(*", colnames(Data)[i], "$)"), names(var$datamat), value = TRUE))[1]
                    wald <- wald.test(b = coef(var$varresult[[eq.name]]), Sigma = vcov(var$varresult[[eq.name]]), Terms = c(grep(colnames(Data)[j], names(var$varresult[[eq.name]]$coefficients)))[1:lag]) ## Wald-test (H0: Series in column j does not Granger-cause series in row i.)
                    wald.stat <- wald$result$chi2[[1]] ## Test statistics.
                    wald.pvalue <- wald$result$chi2[[3]] ## P-valu.
                    wald.decision <- ifelse(wald.pvalue < 0.01, "***", ifelse(wald.pvalue < 0.05, "**", ifelse(wald.pvalue < 0.1, "*", ""))) ## Decision.
                    wald.result <- paste0(format(round(wald.stat, 2), nsmall = 2), wald.decision) ## Result of the test.
                    temp[j, i] <- paste0(wald.result)
                }
            }
        }
        assign("granger.TYDL.pairwise.results", temp, envir = globalenv()) ## Output data will be named as "granger.TYDL.pairwise.results".
    }

    # multivariate VAR Analysis: TYDL Granger-causality tests.
    if (Result.Type == "Multivariate" | Result.Type == "Both") {
        ts <- Data
        dmax <- max(Int.Numbers[c(rownames(Int.Numbers) %in% colnames(ts)), , drop = FALSE]$Int.Order) ## Maximum number of integration.
        lag <- VARselect(ts, lag.max = frequency(ts) / Divide.Lag.Max, type = model.type, season = freq, exogen = exo)$selection[[lag.selection]] ## Note that lag.max is defined as frequency of series.
        var <- vars::VAR(ts, p = lag + dmax, type = model.type, season = freq, exogen = exo) ## Sum the information criterion selected lag and maximum number of integration in the system.
        for (i in 1:ncol) {
            for (j in 1:ncol) {
                if (i < j) {
                    # From i to j: the variable on row i Granger-cause the variable on column j.
                    eq.name <- c(grep(paste0("(*", colnames(Data)[j], "$)"), names(var$datamat), value = TRUE))[1]
                    wald <- wald.test(b = coef(var$varresult[[eq.name]]), Sigma = vcov(var$varresult[[eq.name]]), Terms = c(grep(colnames(Data)[i], names(var$varresult[[eq.name]]$coefficients)))[1:lag]) ## Wald-test (H0: Series in row i does not Granger-cause series in column j.)
                    wald.stat <- wald$result$chi2[[1]] ## Test statistics.
                    wald.pvalue <- wald$result$chi2[[3]] ## P-valu.
                    wald.decision <- ifelse(wald.pvalue < 0.01, "***", ifelse(wald.pvalue < 0.05, "**", ifelse(wald.pvalue < 0.1, "*", ""))) ## Decision.
                    wald.result <- paste0(format(round(wald.stat, 2), nsmall = 2), wald.decision) ## Result of the test.
                    temp[i, j] <- paste0(wald.result)

                    # From j to i: the variable on column j Granger-cause the variable on row i.
                    eq.name <- c(grep(paste0("(*", colnames(Data)[i], "$)"), names(var$datamat), value = TRUE))[1]
                    wald <- wald.test(b = coef(var$varresult[[eq.name]]), Sigma = vcov(var$varresult[[eq.name]]), Terms = c(grep(colnames(Data)[j], names(var$varresult[[eq.name]]$coefficients)))[1:lag]) ## Wald-test (H0: Series in column j does not Granger-cause series in row i.)
                    wald.stat <- wald$result$chi2[[1]] ## Test statistics.
                    wald.pvalue <- wald$result$chi2[[3]] ## P-valu.
                    wald.decision <- ifelse(wald.pvalue < 0.01, "***", ifelse(wald.pvalue < 0.05, "**", ifelse(wald.pvalue < 0.1, "*", ""))) ## Decision.
                    wald.result <- paste0(format(round(wald.stat, 2), nsmall = 2), wald.decision) ## Result of the test.
                    temp[j, i] <- paste0(wald.result)
                }
            }
        }
        assign("granger.TYDL.multivariate.results", temp, envir = globalenv()) ## Output data will be named as "granger.TYDL.multivariate.results".
    }
}

#==================================== END ======================================
