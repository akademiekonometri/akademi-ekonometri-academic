#================================ Load.Install =================================
#======================== Load or Install/Load Packages ========================

# Notes:
#
## Either load or install/load the specified packages.

# Usage:
#
# Load.Install(Package.Names)
#
## Package.Names: The package names to be installed/loaded. It needs to be vector of strings.

# Examples:
#
## Load.Install(Package.Names = "plyr")
## Load.Install(Package.Names = c("plyr", "dplyr"))
## Load.Install(c("plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc"))

Load.Install <- function(Package.Names) {
    #update.packages()
    is_installed <- function(mypkg) is.element(mypkg, utils::installed.packages()[ ,1])
    for (Package.Names in Package.Names) {
        if (!is_installed(Package.Names)) {
            utils::install.packages(Package.Names, dependencies = TRUE)
        }
        suppressMessages(library(Package.Names, character.only = TRUE, quietly = TRUE, verbose = FALSE))
    }
}

#==================================== END ======================================
