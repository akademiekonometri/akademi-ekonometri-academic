#============================= Akademi Ekonometri ==============================
#=================== Paths, Functions, Packaging, and Seeds ====================
#================================== Settings ===================================

#=================================== Paths =====================================
# ---- Settings.Paths.1 ----
# Main path of the "—AkademiEkonometri.io" folder.
base.path <- paste0(regmatches(getwd(), regexpr("(^.*Akademi Ekonometri-Academic)", getwd(), perl = TRUE)), "/") ## "base.path" is the unix path of "./Akademi Ekonometri-Academic.Rproj" folder.
# base.path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/") ## Note that this works when Running or Sourcing your file. The code automatically find out the location of the current file.

# Path for the ".static/repo" folder
repo.path <- paste0(base.path, "static/repo/")

# Paths for "./akek" folder.
akek.path <- paste0(repo.path, "akek/")

# Paths for "./akek/courses" folder.
courses.path <- paste0(repo.path, "akek/courses/")

# Paths for "./akek/r" folder.
r.path <- paste0(repo.path, "akek/r/")

# Path for the "./apps" folder.
apps.path <- paste0(repo.path, "apps/")

# Path for the "./data/raw" folder.
raw.data.path <- paste0(repo.path, "data/raw/")

# Path for the "./data/metadata" folder.
metadata.data.path <- paste0(repo.path, "data/metadata/")

# Path for the "./data/processed" folder.
processed.data.path <- paste0(repo.path, "data/processed/")

# Path for the "./DCL" folder.
DCL.path <- paste0(repo.path, "DCL/")

# Path for the "./docs" folder.
docs.path <- paste0(repo.path, "docs/")

# Path for the "./images" folder.
images.path <- paste0(repo.path, "images/")

# Path for the "./latex" folder.
latex.path <- paste0(repo.path, "latex/")

# Path for "./pdf" folder.
pdf.path <- paste0(repo.path, "pdf/")

# Path for the "./scripts" folder.
scripts.path <- paste0(repo.path, "scripts/")

# Path for "./scripts/functions" folder.
func.path <- paste0(repo.path, "scripts/functions/")

# Path for "./slides" folder.
slides.path <- paste0(repo.path, "slides/")

#=============================== Relative Paths ================================
# ---- Settings.Relative.Paths.1 ----

# Relative path extension which carries the folder path from /courses/CourseName/FileName to /repo/
courses.to.repo <- "../../../"

# Relative path for "./akek/courses/economics" folder.
economics.path <- paste0(courses.to.repo, "repo/akek/courses/economics/")

# Relative path for "./akek/courses/ekonometri-ars" folder.
ekonometri.ars.path <- paste0(courses.to.repo, "repo/akek/courses/ekonometri-ars/")

# Relative path for "./akek/courses/ekonometri-i" folder.
ekonometri.i.path <- paste0(courses.to.repo, "repo/akek/courses/ekonometri-i/")

# Relative path for "./akek/courses/ekonometri-ii" folder.
ekonometri.ii.path <- paste0(courses.to.repo, "repo/akek/courses/ekonometri-ii/")

# Relative path for "./akek/courses/ekonometrik-modelleme" folder.
ekonometrik.modelleme.path <- paste0(courses.to.repo, "repo/akek/courses/ekonometrik-modelleme/")

# Relative path for "./akek/courses/ekonomi" folder.
ekonomi.path <- paste0(courses.to.repo, "repo/akek/courses/ekonomi/")

# Relative path for "./akek/courses/ekonomi-i" folder.
ekonomi.i.path <- paste0(courses.to.repo, "repo/akek/courses/ekonomi-i/")

# Relative path for "./akek/courses/ekonomi-ii" folder.
ekonomi.ii.path <- paste0(courses.to.repo, "repo/akek/courses/ekonomi-ii/")

#================================= Functions ===================================
# ---- Settings.Functions.1 ----
# Load.Install
source(paste0(func.path, "load_or_install_packages.R")) ## Loads and/or installs packages.

# Proceed.or.Stop
source(paste0(func.path, "proceed_or_stop.R")) ## Checks whether to proceed or stop.

# Data Functions
source(paste0(func.path, "data_functions.R")) ## Data Functions in collection.

# Other Functions
source(paste0(func.path, "other_functions.R")) ## Other Functions in collection.

# Estimation Functions
source(paste0(func.path, "estimation_functions.R")) ## Estimation Functions in collection.

# Graphic Functions
source(paste0(func.path, "graphic_functions.R")) ## Graphic Functions in collection.

# Spatial Functions
source(paste0(func.path, "spatial_functions.R")) ## Spatial Functions in collection.

#================================= Packages ====================================
# ---- Settings.Packages.1 ----
#=================================
# Package that MUST be installed.
Load.Install(c("devtools", "png", "proto"))

#=================================
# Package for package control during collaboration.
Load.Install(c("renv")) ## Note that to activate the "renv" package you need to use "renv::init()" once.

#=================================
# Packages for emojis and icons.
if("fontawesome" %in% rownames(utils::installed.packages()) == FALSE) {devtools::install_github("rstudio/fontawesome")}
library("fontawesome")

if("emo" %in% rownames(utils::installed.packages()) == FALSE) {devtools::install_github("hadley/emo")}
library("emo")

#=================================
# Packages for reproducible research.
Load.Install(c("knitr", "rmarkdown", "tinytex", "formatR")) ## Use the following code for all the options of knitr: "str(knitr::opts_chunk$get())"
Load.Install("blogdown")
# Packages for YAML options.
Load.Install(c("ymlthis")) ## https://ymlthis.r-lib.org/index.html

# Packages for general miscellaneous actions.
Load.Install(c("plyr", "dplyr", "magrittr", "stringr", "stringi", "Hmisc", "tm", "lubridate", "NCmisc", "classInt"))

#=================================
# Packages for data loading, manipulation, and data reshaping.
Load.Install(c("reshape", "reshape2", "tidyr", "data.table"))
# Load.Install(c("XLConnect")) ## Loading XLConnect package before RStudio opens, makes Java unstable and causes R to crash. Therefore, load the XLConnect package after RStudio opens preferably just before the loading function.

#=================================
# Packages for table generation and LaTeX.
Load.Install(c("stargazer", "xtable", "tikzDevice", "latex2exp"))

#=================================
# Packages for graphics and related packages.
Load.Install(c("ggplot2", "ggthemes", "ggThemeAssist", "cowplot", "grid", "gridExtra", "gtable", "lattice", "latticeExtra", "dygraphs", "RColorBrewer"))

#=================================
# Packages for web scraping.
# Load.Install(c("rvest", "RCurl", "downloader"))

#=================================
# Packages for general econometric analyses.
Load.Install(c("lmtest", "sandwich", "car", "wooldridge", "nortest", "normtest", "gvlma", "moments"))

# Extra packages for the car package.
if("carData" %in% rownames(installed.packages()) == FALSE) {install.packages("carData")}
library("carData")

#=================================
# Packages for specific econometric analyses.
# Load.Install(c("lme4", "aod", "pbkrtest", "strucchange"))

#=================================
# Packages for time series analysis.
# Load.Install(c("zoo", "xts", "vars", "tsDyn", "FitAR", "lgarch", "seasonal", "x13binary", "urca", "fUnitRoots", "CADFtest", "forecast"))
# Load.Install(c("MTS", "tseries", "timeSeries"))

#=================================
# Packages for spatial and spatio-temporal analysis.
# Load.Install(c("sp", "spdep", "spatialreg", "rgdal", "maptools", "raster", "geoR", "fields", "pgirmess", "RANN", "pastecs", "spgwr", "maps", "mapproj", "mapdata", "choroplethr", "choroplethrMaps", "tmap"))

#=================================== Seeds =====================================
# ---- Settings.Seeds.1 ----
selected.seed <- 1234
set.seed(selected.seed)

#==================================== END ======================================
