#============================== Other Functions ================================
#====================== All Other Functions in Collection ======================

# Notes:
#
## This script contains some functions which are not directly related to data and estimation stages but might be used in either one starting with the data stage.
## Some of these functions have specific purposes and should not be used separately.
## Information for some functions is given in its dedicated space.

#============================ Ask.User.YN.Question =============================
#======================== Interactive Yes/No Questions =========================

# Notes:
#
## Function for interactive yes/no question.

# Usage:
#
# Ask.User.YN.Question(question, GUI = TRUE, add.lines.before = TRUE)
#
## question: Your question as character
## GUI: TRUE or FALSE. Use GUI or console.
## add.lines.before: Simply add some line after your questions.

# Examples:
#
## if (Ask.User.YN.Question("Your question goes here?", GUI = FALSE, add.lines.before = TRUE)) {
##     ## Code
## } else {
##     ## More Code
## }

Ask.User.YN.Question <- function(question, GUI = TRUE, add.lines.before = TRUE) {
    choices <- c("yes", "no")
    if (add.lines.before & !GUI) cat("------------------------\n")
    answer <- menu(choices, graphics = GUI, title = question)
    ifelse(answer == 1L, TRUE, FALSE) ## Returns TRUE or FALSE.
}

#================================ DigitsByRows =================================
#========================= Significant Digits By Row ===========================

# Notes:
#
## Function for significant digits by row.

# Usage:
#
# DigitsByRows(df, digits)
#
## df: Data frame.
## digits: requested digits in order of rows.

# Examples:
#
## DigitsByRows(df, digits = c(0, 2))

DigitsByRows <- function(df, digits) {
    tmp0 <- data.frame(t(df))
    tmp1 <- mapply(
        function(df0, digits0) {
            formatC(df0, format = "f", digits = digits0)
        },
        df0 = tmp0, digits0 = digits
    )
    tmp1 <- data.frame(t(tmp1))
    names(tmp1) <- names(df)
    return(tmp1)
}

#============================== Perm.No.Replace ================================
#====================== Permutation with No Replacement ========================

# Notes:
#
## Permutation without replacement.

# Usage:
#
# Perm.No.Replace(v)
#
## v: vector.

# Examples:
#
## Perm.No.Replace(v = c(1:3))

Perm.No.Replace <- function(v) {
    n <- length(v)
    if (n == 1) v
    else {
        X <- NULL
        for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
        X
    }
}

#================================== sampled ====================================
#====================== Bug free Redefinition of Sample ========================
# Notes:
#
## If only one value is given sample() function chooses a value between 1 and the number you specified. The redefined sample function solves this problem.

sampled <- function(x, size, replace = FALSE, prob = NULL) {
    if (length(x) == 1) return(x)
    base::sample(x, size = size, replace = replace, prob = prob)
}

#=============== round.0, round.1, round.2, round.3, and round.4 ===============
#===================== Functions for Rounding with round2 ======================
# Notes:
#
## Simplifying and specializing the round2 function which is taken from the "exams" package.
## The function round2 does what is known in German as kaufmaennisches Runden (rounding away from zero for trailing 5s).

round.0 <- function(x) {
    round(x + sign(x) * 1e-10, digits = 0)
}

round.1 <- function(x) {
    round(x + sign(x) * 1e-10, digits = 1)
}

round.2 <- function(x) {
    round(x + sign(x) * 1e-10, digits = 2)
}

round.3 <- function(x) {
    round(x + sign(x) * 1e-10, digits = 3)
}

round.4 <- function(x) {
    round(x + sign(x) * 1e-10, digits = 4)
}

#============== trunc.0, trunc.1, trunc.2, trunc.3, and trunc.4 ================
#========================= Truncate but Do not Round ===========================
# Notes:
#
## Truncating the decimal number without rounding.

trunc.0 <- function(x) {
    trunc(x * 1)/1
}

trunc.1 <- function(x) {
    trunc(x * 10)/10
}

trunc.2 <- function(x) {
    trunc(x * 100)/100
}

trunc.3 <- function(x) {
    trunc(x * 1000)/1000
}

trunc.4 <- function(x) {
    trunc(x * 10000)/10000
}

#================================ sign.print ===================================
#========================= Prints The Sign of a Value ==========================
# Notes:
#
## Printing the sing of a value.

sign.print <- function(x) {
    ifelse(x > 0, "+", "-")
}

#==================================== END ======================================
