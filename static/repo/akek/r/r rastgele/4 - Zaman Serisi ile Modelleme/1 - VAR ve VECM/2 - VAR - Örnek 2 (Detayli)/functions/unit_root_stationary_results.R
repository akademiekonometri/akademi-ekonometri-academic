#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#=========================== UR.Stationary.Results =============================
#====================== Unit Root and Stationary Results =======================

# Notes:
#
## Function simply merges all the reshaped results of "UR.Stationary.Tests" and "ADF.Enders.Procedure" functions with some visualization benefits.
## Function requires a time series (ts) object.
## Significance.Level is the significance level which is used in all tests in this function.
## Function optionally takes the first difference of the input data and applies the same procedure in above mentioned functions. The default for Diff argument is False.
## Function optionally simplify (shorten) the results to present as table (in Rmarkdown files). The default for Table argument is TRUE.
## Function optionally subsets the results of "UR.Stationary.Tests" function with sprecified lag selections methods.
## The output of the function is named as "ur.stationary.results" data frame.

# Usage:
#
# UR.Stationary.Results(Data, Significance.Level, Diff = FALSE, Table = TRUE, Subset = NULL)
#
## Data: Time series (ts) object. It can be multivariate or univariate.
## Significance Level: Significance level for all test throughout the function.
## Diff: Takes the first difference of all time series.
## Table: Shrinks the table size in output files for printing purposes.
## Subset: Subsets some of the test results with specific lag selection method.
## To run the function without any argument matching problem, make sure to specify Diff, Table and Subset arguments with their names always (if non-default values are selected).

# Examples:
#
## UR.Stationary.Results(data.ts, 1)
## UR.Stationary.Results(data.ts, 5)
## UR.Stationary.Results(data.ts, 10, Subset = c("BIC", "AIC"))
## UR.Stationary.Results(data.ts, 1, Diff = TRUE, Subset = c("Long", "Short"))

UR.Stationary.Results <- function(Data, Significance.Level, Diff = FALSE, Table = TRUE, Subset = NULL) {
    # Checks Data argument.
    if (!is.ts(Data))
        stop("Invalid Data. Please choose a time series (ts) data as input.\n")

    # Checks Significance.Level argument.
    if (length(Significance.Level) != 1)
        stop("Invalid Significance.Level. Please choose only one Significance.Level.\n")
    if (!is.numeric(Significance.Level))
        stop("Invalid Significance.Level. Please choose a numeric Significance.Level value.\n")
    if (!(Significance.Level %in% c(1, 5, 10)))
        stop("Invalid Significance.Level. Please choose one of the followings: 1) For %0.01: 1. 2) For %0.05: 5. 3) For %0.1: 10.\n")

    # Checks Diff argument.
    if (length(Diff) != 1)
        stop("Invalid Diff. Please choose only one Diff.\n")
    if (!(Diff %in% c("TRUE", "FALSE")))
        stop("Invalid Diff. Please choose one of the followings: 1) To take the first difference of input data: TRUE. 2) To use the input data as is: FALSE.\n")

    # Checks Table argument.
    if (length(Table) != 1)
        stop("Invalid Table. Please choose only one Table.\n")
    if (!(Table %in% c("TRUE", "FALSE")))
        stop("Invalid Table. Please choose one of the followings: 1) To shrink the size of data table in output files: TRUE. 2) Otherwise: FALSE.\n")

    # Checks Subset argument.
    if (!is.null(Subset)) {
        if (!(sum(!(Subset %in% c("BIC", "AIC", "AIC*", "Long", "Short"))) == 0))
            stop("Invalid Subset. Please choose one or more of the followings: 1) To subset results with BIC lag selection from SelectModel function in FitAR package: BIC. 2) To subset results with AIC lag selection from SelectModel function in FitAR package: AIC. 3) To subset results with AIC lag selection from AR model: AIC*. 4) To subset results with Long lag selection, commonly used in the literature, across all tests: Long. 5) To subset results with Short lag selection, commonly used in the literature, across all tests: Short.\n")
    }

    # Runing the ADF.Enders.Procedure with selected arguments
    ADF.Enders.Procedure(Data, Significance.Level = Significance.Level, Diff = Diff, Reshape = TRUE) ## Output of this function is adf.results
    UR.Stationary.Tests(Data, Significance.Level = Significance.Level, Diff, Reshape = TRUE, Subset = Subset) ## Output of this function is ur.stationary.tests.
    temp <- dplyr::full_join(adf.tests, ur.stationary.tests, by = "Variable")
    main <- data.frame(t(temp[, 2:ncol(temp)]), stringsAsFactors = FALSE)
    colnames(main) <- temp[, 1]
    if (!is.null(ncol(Data))) { ## Data is in ts matrix format.
        main <- main[, c(colnames(Data))]
    }

    ur <- suppressWarnings(subset(main, rownames(main) %in% rownames(main)[c(grep("(^KPSS.)", rownames(main), invert = TRUE))])) ## For unit root tests.
    st <- suppressWarnings(subset(main, rownames(main) %in% rownames(main)[c(grep("(^KPSS.)", rownames(main)))])) ## For stationarity tests

    for (i in 1:ncol(ur)) {
        if (i == 1) {
            ur.percent <- as.data.frame(paste(round(100 * sum(ur[, i][1:nrow(ur)] == "Unit Root") / nrow(ur), 0), "%"))
        } else {
            temp <- as.data.frame(paste(round(100 * sum(ur[, i][1:nrow(ur)] == "Unit Root") / nrow(ur), 0), "%"))
            ur.percent <- cbind(ur.percent, temp)
        }
    }
    rownames(ur.percent) <- "URoot Percent"
    names(ur.percent) <- names(ur)

    for (i in 1:ncol(st)) {
        if (i == 1) {
            st.percent <- as.data.frame(paste(round(100 * sum(st[, i][1:nrow(st)] == "Unit Root") / nrow(st), 0), "%"))
        } else {
            temp <- as.data.frame(paste(round(100 * sum(st[, i][1:nrow(st)] == "Unit Root") / nrow(st), 0), "%"))
            st.percent <- cbind(st.percent, temp)
        }
    }
    rownames(st.percent) <- "URoot Percent (KPSS)"
    names(st.percent) <- names(st)

    main <- rbind(st, st.percent, ur, ur.percent)

    # Simplifying the results data frame to use in Rmarkdown reports.
    if (Table == TRUE) {
        for (i in 1:ncol(main)) {
            main[, i] <- gsub("Unit Root", "U", main[, i], fixed = TRUE)
            main[, i] <- gsub("Stationary", "S", main[, i], fixed = TRUE)
        }
    }

    assign("ur.stationary.results", main, envir = globalenv()) ## Output data will be named as "ur.stationary.results".
}

#==================================== END ======================================
