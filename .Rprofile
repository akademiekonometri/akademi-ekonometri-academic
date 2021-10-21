source("renv/activate.R")
#============================= Akademi Ekonometri ==============================
#================================== .Rprofile ==================================
#======================= Initial Settings for The Project ======================

#=============================== First Function ================================
.First <- function() {
  cat("\n********************************************************\n")
  cat("\nWelcome to 'Akademi Ekonometri' Project.\nRemember to edit .Rprofile accordingly!\n")
  cat("\nAll comments, suggestions, and other correspondences should be sent to Akamdemi Ekonometri, akademiekonometri@gmail.com.\n")
  cat("\n********************************************************\n")
  options(digits.secs = 3) ## show sub-second time stamps
  options(showWarnCalls = TRUE, showErrorCalls = TRUE)
  options(repos = c(CRAN = "https://cran.rstudio.com/"), browserNLdisabled = TRUE, deparse.max.lines = 2)
  options(digits = 8, show.signif.stars = TRUE, scipen = 6)
  options(replace.assign = TRUE, width = 90)
  # options("pdfviewer" = "skim")
}

#================================ Last Function ================================
.Last <- function()  cat("\nGoodbye!\n")

#================================ R Libraries =================================
.libPaths("/Volumes/Omer/Google Drive/Apps/R/R Libraries")

#============================== Blogdown Options ===============================
# Blogdown options.
options(
  blogdown.author = "Akademi Ekonometri", ## Default author for posts.
  blogdown.ext = ".Rmd", ## Default file type for posts.
  blogdown.subdir = "post", ## Default directory for posts.
  blogdown.yaml.empty = TRUE,
  blogdown.method = "html", ## Might crash the Rstudio.
  blogdown.new_bundle = TRUE, ## See: https://bookdown.org/yihui/blogdown/more-global-options.html
  blogdown.hugo.version = "0.82.0",
  blogdown.knit.on_save = TRUE,
  blogdown.serve_site.startup = FALSE,
  blogdown.generator.server = TRUE, ## Sometimes RStudio crashes after the site is served. Loading these function solves the issue.
  # blogdown.publishDir = "../public_site", ## See: https://bookdown.org/yihui/blogdown/more-global-options.html
  # blogdown.widgetsID = TRUE, ## See: https://bookdown.org/yihui/blogdown/more-global-options.html
  blogdown.hugo.server = c('-D', '-F', '--navigateToChanged'), ## See: https://bookdown.org/yihui/blogdown/livereload.html
  blogdown.title_case = TRUE
)

#================================== Settings ===================================
# The general settings of the entire project is defined in "_settings.R" script.
# As long as you don’t alter the file/folder structure of the main project folder, which is downloaded from the remote git repository to any path in your computer, the below codes will specify all the paths correctly. Then loads all the necessary packages and user-written R functions, and sets the seeds.
#
## Paths: for the important folder paths of this project.
## Functions: for the user-written R functions.
## Packages: for the necessary R packages.
## Seeds: for setting the seeds.
source("./_settings.R")
message("Your project working directory is:\n'", getwd(), "'\n") ## This is the main working directory of the project.

#==================================== END ======================================
