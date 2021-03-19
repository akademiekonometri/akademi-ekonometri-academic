#================================= Coint.Select ================================
#============ Cointegration Rank Calculation from Johansen Object ==============

# Notes:
#
## Function takes the johansen object and number of equations, and assignes the cointegration rank to general environment named as coint.number.
## Johansen.Object is the johansen object calculated with ca.jo function from vars package.
## Equation.Number is the number of equations or the number of the total series used in the johansen object. Note that function is written only for at max 6 equations (6 endogenous variables). For more than 6 equations, additional code should be appended to the bottom. See the codes of equation 6 for help.
## After calculation, function prints the critial values, test statistics and the selected cointegration rank. Also, the selected cointegration rank is assigned to coint.number object in the general environment.
## The outputs of the function is named as "coint.number" data frames.

# Usage:
#
# Coint.Select(Johansen.Object, Equation.Number)
#
## Johansen.Object: Johansen object calcualted with the ca.jo. function in vars package.
## Equation.Number: Numeric. Number of the equations in numeric form.
## To run the function without any argument matching problem, make sure to specify all arguments with their names always (if non-default values are selected).

# Examples:
#
## Coint.Select(Johansen.Object = johansen, Equation.Number = 3)
## Coint.Select(Johansen.Object = johansen, Equation.Number = 6)

Coint.Select <- function(Johansen.Object, Equation.Number) {
    # Checks Johansen.Object argument.
    if (length(Johansen.Object) != 1)
        stop("Invalid Johansen.Object. Please choose only one Johansen.Object.\n")
    if (class(Johansen.Object)[1] != "ca.jo")
        stop("Invalid Johansen.Object. Please choose a Johansen.Object which has a class of ca.jo.\n")

    # Checks Equation.Number argument.
    if (length(Equation.Number) != 1)
        stop("Invalid Equation.Number. Please choose only one Equation.Number.\n")
    if (Equation.Number >= 7)
        stop("Invalid Equation.Number. Please choose at max 6 for Equation.Number.\n")

    # Cointegration results.
    coint.results <- cbind(t(matrix(Johansen.Object@teststat, nrow = 1, ncol = Equation.Number, dimnames = list(c("test")))), Johansen.Object@cval)
    temp <- coint.results
    print(temp)

    # For 2 equation version.
    if (Equation.Number == 2) {
        # %1.
        rank.check <- ifelse(temp[2, 4] < temp[2, 1], ifelse(temp[1, 4] < temp[1, 1], 2, 1), 0)
        if (rank.check %in% c(2)) {
            coint <- 2
            message(paste0("Cointegration Rank in %1: ", coint, ", which means system is level stationary."))
        }
        if (rank.check %in% c(1)) {
            coint <- rank.check
            message(paste0("Cointegration Rank in %1: ", coint))
        }
        if (rank.check %in% c(0)) {

            # %5.
            rank.check <- ifelse(temp[2, 3] < temp[2, 1], ifelse(temp[1, 3] < temp[1, 1], 2, 1), 0)
            if (rank.check %in% c(2)) {
                coint <- 2
                message(paste0("Cointegration Rank in %5: ", coint, ", which means system is level stationary."))
            }
            if (rank.check %in% c(1)) {
                coint <- rank.check
                message(paste0("Cointegration Rank in %5: ", coint))
            }
            if (rank.check %in% c(0)) {

                # %10.
                rank.check <- ifelse(temp[2, 2] < temp[2, 1], ifelse(temp[1, 2] < temp[1, 1], 2, 1), 0)
                if (rank.check %in% c(2)) {
                    coint <- 2
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is level stationary."))
                }
                if (rank.check %in% c(1)) {
                    coint <- rank.check
                    message(paste0("Cointegration Rank in %10: ", coint))
                }
                if (rank.check %in% c(0)) {
                    coint <- 0
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is difference stationary."))
                }
            }
        }
    }

    # For 3 equation version.
    if (Equation.Number == 3) {
        # %1.
        rank.check <- ifelse(temp[3, 4] < temp[3, 1], ifelse(temp[2, 4] < temp[2, 1], ifelse(temp[1, 4] < temp[1, 1], 3, 2), 1), 0)
        if (rank.check %in% c(3)) {
            coint <- 3
            message(paste0("Cointegration Rank in %1: ", coint, ", which means system is level stationary."))
        }
        if (rank.check %in% c(1, 2)) {
            coint <- rank.check
            message(paste0("Cointegration Rank in %1: ", coint))
        }
        if (rank.check %in% c(0)) {

            # %5.
            rank.check <- ifelse(temp[3, 3] < temp[3, 1], ifelse(temp[2, 3] < temp[2, 1], ifelse(temp[1, 3] < temp[1, 1], 3, 2), 1), 0)
            if (rank.check %in% c(3)) {
                coint <- 3
                message(paste0("Cointegration Rank in %5: ", coint, ", which means system is level stationary."))
            }
            if (rank.check %in% c(1, 2)) {
                coint <- rank.check
                message(paste0("Cointegration Rank in %5: ", coint))
            }
            if (rank.check %in% c(0)) {

                # %10.
                rank.check <- ifelse(temp[3, 2] < temp[3, 1], ifelse(temp[2, 2] < temp[2, 1], ifelse(temp[1, 2] < temp[1, 1], 3, 2), 1), 0)
                if (rank.check %in% c(3)) {
                    coint <- 3
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is level stationary."))
                }
                if (rank.check %in% c(1, 2)) {
                    coint <- rank.check
                    message(paste0("Cointegration Rank in %10: ", coint))
                }
                if (rank.check %in% c(0)) {
                    coint <- 0
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is difference stationary."))
                }
            }
        }
    }

    # For 4 equation version.
    if (Equation.Number == 4) {
        # %1.
        rank.check <- ifelse(temp[4, 4] < temp[4, 1], ifelse(temp[3, 4] < temp[3, 1], ifelse(temp[2, 4] < temp[2, 1], ifelse(temp[1, 4] < temp[1, 1], 4, 3), 2), 1), 0)
        if (rank.check %in% c(4)) {
            coint <- 4
            message(paste0("Cointegration Rank in %1: ", coint, ", which means system is level stationary."))
        }
        if (rank.check %in% c(1, 2, 3)) {
            coint <- rank.check
            message(paste0("Cointegration Rank in %1: ", coint))
        }
        if (rank.check %in% c(0)) {

            # %5.
            rank.check <- ifelse(temp[4, 3] < temp[4, 1], ifelse(temp[3, 3] < temp[3, 1], ifelse(temp[2, 3] < temp[2, 1], ifelse(temp[1, 3] < temp[1, 1], 4, 3), 2), 1), 0)
            if (rank.check %in% c(4)) {
                coint <- 4
                message(paste0("Cointegration Rank in %5: ", coint, ", which means system is level stationary."))
            }
            if (rank.check %in% c(1, 2, 3)) {
                coint <- rank.check
                message(paste0("Cointegration Rank in %5: ", coint))
            }
            if (rank.check %in% c(0)) {

                # %10.
                rank.check <- ifelse(temp[4, 2] < temp[4, 1], ifelse(temp[3, 2] < temp[3, 1], ifelse(temp[2, 2] < temp[2, 1], ifelse(temp[1, 2] < temp[1, 1], 4, 3), 2), 1), 0)
                if (rank.check %in% c(4)) {
                    coint <- 4
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is level stationary."))
                }
                if (rank.check %in% c(1, 2, 3)) {
                    coint <- rank.check
                    message(paste0("Cointegration Rank in %10: ", coint))
                }
                if (rank.check %in% c(0)) {
                    coint <- 0
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is difference stationary."))
                }
            }
        }
    }

    # For 5 equation version.
    if (Equation.Number == 5) {
        # %1.
        rank.check <- ifelse(temp[5, 4] < temp[5, 1], ifelse(temp[4, 4] < temp[4, 1], ifelse(temp[3, 4] < temp[3, 1], ifelse(temp[2, 4] < temp[2, 1], ifelse(temp[1, 4] < temp[1, 1], 5, 4), 3), 2), 1), 0)
        if (rank.check %in% c(5)) {
            coint <- 5
            message(paste0("Cointegration Rank in %1: ", coint, ", which means system is level stationary."))
        }
        if (rank.check %in% c(1, 2, 3, 4)) {
            coint <- rank.check
            message(paste0("Cointegration Rank in %1: ", coint))
        }
        if (rank.check %in% c(0)) {

            # %5.
            rank.check <- ifelse(temp[5, 3] < temp[5, 1], ifelse(temp[4, 3] < temp[4, 1], ifelse(temp[3, 3] < temp[3, 1], ifelse(temp[2, 3] < temp[2, 1], ifelse(temp[1, 3] < temp[1, 1], 5, 4), 3), 2), 1), 0)
            if (rank.check %in% c(5)) {
                coint <- 5
                message(paste0("Cointegration Rank in %5: ", coint, ", which means system is level stationary."))
            }
            if (rank.check %in% c(1, 2, 3, 4)) {
                coint <- rank.check
                message(paste0("Cointegration Rank in %5: ", coint))
            }
            if (rank.check %in% c(0)) {

                # %10.
                rank.check <- ifelse(temp[5, 2] < temp[5, 1], ifelse(temp[4, 2] < temp[4, 1], ifelse(temp[3, 2] < temp[3, 1], ifelse(temp[2, 2] < temp[2, 1], ifelse(temp[1, 2] < temp[1, 1], 5, 4), 3), 2), 1), 0)
                if (rank.check %in% c(5)) {
                    coint <- 5
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is level stationary."))
                }
                if (rank.check %in% c(1, 2, 3, 4)) {
                    coint <- rank.check
                    message(paste0("Cointegration Rank in %10: ", coint))
                }
                if (rank.check %in% c(0)) {
                    coint <- 0
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is difference stationary."))
                }
            }
        }
    }

    # For 6 equation version.
    if (Equation.Number == 6) {
        # %1.
        rank.check <- ifelse(temp[6, 4] < temp[6, 1], ifelse(temp[5, 4] < temp[5, 1], ifelse(temp[4, 4] < temp[4, 1], ifelse(temp[3, 4] < temp[3, 1], ifelse(temp[2, 4] < temp[2, 1], ifelse(temp[1, 4] < temp[1, 1], 6, 5), 4), 3), 2), 1), 0)
        if (rank.check %in% c(6)) {
            coint <- 6
            message(paste0("Cointegration Rank in %1: ", coint, ", which means system is level stationary."))
        }
        if (rank.check %in% c(1, 2, 3, 4, 5)) {
            coint <- rank.check
            message(paste0("Cointegration Rank in %1: ", coint))
        }
        if (rank.check %in% c(0)) {

            # %5.
            rank.check <- ifelse(temp[6, 3] < temp[6, 1], ifelse(temp[5, 3] < temp[5, 1], ifelse(temp[4, 3] < temp[4, 1], ifelse(temp[3, 3] < temp[3, 1], ifelse(temp[2, 3] < temp[2, 1], ifelse(temp[1, 3] < temp[1, 1], 6, 5), 4), 3), 2), 1), 0)
            if (rank.check %in% c(6)) {
                coint <- 6
                message(paste0("Cointegration Rank in %5: ", coint, ", which means system is level stationary."))
            }
            if (rank.check %in% c(1, 2, 3, 4, 5)) {
                coint <- rank.check
                message(paste0("Cointegration Rank in %5: ", coint))
            }
            if (rank.check %in% c(0)) {

                # %10.
                rank.check <- ifelse(temp[6, 2] < temp[6, 1], ifelse(temp[5, 2] < temp[5, 1], ifelse(temp[4, 2] < temp[4, 1], ifelse(temp[3, 2] < temp[3, 1], ifelse(temp[2, 2] < temp[2, 1], ifelse(temp[1, 2] < temp[1, 1], 6, 5), 4), 3), 2), 1), 0)
                if (rank.check %in% c(6)) {
                    coint <- 6
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is level stationary."))
                }
                if (rank.check  %in% c(1, 2, 3, 4, 5)) {
                    coint <- rank.check
                    message(paste0("Cointegration Rank in %10: ", coint))
                }
                if (rank.check %in% c(0)) {
                    coint <- 0
                    message(paste0("Cointegration Rank in %10: ", coint, ", which means system is difference stationary."))
                }
            }
        }
    }

    assign("coint.number", coint, envir = globalenv()) ## Output data will be named as "coint.number".
}

#==================================== END ======================================
