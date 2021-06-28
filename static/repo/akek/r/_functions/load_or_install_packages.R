#================================ Load.Install =================================
#======================== Load or Install/Load Packages ========================

# Notes:
#
## Either load or install/load the specified packages.

# Usage:
#
# Load.Install(Package.Names, Quiet = FALSE, Update.All = FALSE)
#
## Package.Names: The package names to be installed/loaded. It needs to be vector of strings.

# Examples:
#
## Load.Install(Package.Names = "plyr")
## Load.Install(Package.Names = c("plyr", "dplyr"))
## Load.Install(c("plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc"))
## Load.Install(c("plyr", "dplyr"), Quiet = TRUE, Update.All = TRUE)

Load.Install <- function(Package.Names, Quiet = FALSE, Update.All = FALSE) {
    is_installed <- function(my.pkgs) base::is.element(my.pkgs, utils::installed.packages()[ ,1])
    github.pkgs <- base::grep("^.*?/.*?$", Package.Names, value = TRUE)
    github.bare.pkgs <- base::sub(".*?/", "", github.pkgs)
    cran.pkgs <- Package.Names[!(Package.Names %in% github.pkgs)]
    all.pkgs <- c(cran.pkgs, github.bare.pkgs)
    cran.missing <- cran.pkgs[which(!is_installed(cran.pkgs))]
    github.missing <- github.pkgs[which(!is_installed(github.bare.pkgs))]
    if (Update.All == TRUE) {
        cran.missing <- cran.pkgs
        github.missing <- github.pkgs
    } else {
        cran.missing <- cran.pkgs[which(!is_installed(cran.pkgs))]
        github.missing <- github.pkgs[which(!is_installed(github.bare.pkgs))]
    }
    if (length(cran.missing) > 0) {
        suppressWarnings(utils::install.packages(cran.missing, quiet = Quiet, dependencies = TRUE))
    }
    if (length(github.missing) > 0) {
        suppressWarnings(devtools::install_github(github.missing, quiet = Quiet, dependencies = TRUE))
    }
    failed.install <- all.pkgs[which(!is_installed(all.pkgs))]
    if (length(failed.install) > 0) {
        warning(paste0("Some packages failed to install: ", paste(failed.install, collapse = ", "), "."))
    }
    install.pkgs <- all.pkgs[which(is_installed(all.pkgs) == TRUE)]
    for (install.pkgs in install.pkgs) {
        suppressPackageStartupMessages(library(install.pkgs, character.only = TRUE, quietly = Quiet, verbose = FALSE))
    }
}

#==================================== END ======================================
