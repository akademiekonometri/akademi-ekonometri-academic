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

#==================================== END ======================================
