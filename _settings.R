#============================= Akademi Ekonometri ==============================
#=================== Paths, Functions, Packaging, and Seeds ====================
#================================== Settings ===================================

#=================================== Paths =====================================
# ---- Settings.Paths.1 ----
# Main path of the "—AkademiEkonometri.io" folder.
base.path <- paste0(regmatches(getwd(), regexpr("(^.*Akademi Ekonometri-Academic)", getwd(), perl = TRUE)), "/") ## "base.path" is the unix path of "./Akademi Ekonometri-Academic.Rproj" folder.
# base.path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/") ## Note that this works when Running or Sourcing your file. The code automatically find out the location of the current file.

# Path for the ".static/repo/" folder
repo.path <- paste0(base.path, "static/repo/")

# Paths for "./akek/" folder.
akek.path <- paste0(repo.path, "akek/")

# Paths for "./akek/courses/" folder.
courses.path <- paste0(repo.path, "akek/courses/")

# Path for "./scripts/functions/" folder.
func.path <- paste0(repo.path, "akek/r/_functions/")

# Paths for "./akek/r/" folder.
r.path <- paste0(repo.path, "akek/r/")

# Path for the "./apps/" folder.
apps.path <- paste0(repo.path, "apps/")

# Path for the "./data/raw/" folder.
raw.data.path <- paste0(repo.path, "data/raw/")

# Path for the "./data/metadata/" folder.
metadata.data.path <- paste0(repo.path, "data/metadata/")

# Path for the "./data/processed/" folder.
processed.data.path <- paste0(repo.path, "data/processed/")

# Path for the "./DCL"/ folder.
DCL.path <- paste0(repo.path, "DCL/")

# Path for the "./docs/" folder.
docs.path <- paste0(repo.path, "docs/")

# Path for the "./images/" folder.
images.path <- paste0(repo.path, "images/")

# Path for the "./latex/" folder.
latex.path <- paste0(repo.path, "latex/")

# Path for "./pdf/" folder.
pdf.path <- paste0(repo.path, "pdf/")

# Path for the "./scripts/" folder.
scripts.path <- paste0(repo.path, "scripts/")

# Path for "./slides/" folder.
slides.path <- paste0(repo.path, "slides/")

#================================ Folder Names =================================
# ---- Folder.Names.1 ----
ekonometri.ars <- "ekonometri-ars/"
ekonometri.i <- "ekonometri-i/"
ekonometri.ii <- "ekonometri-ii/"
zaman.serileri.analizi <- "zaman-serileri-analizi/"
ekonometrik.modelleme <- "ekonometrik-modelleme/"
economics <- "economics/"
ekonomi <- "ekonomi/"
ekonomi.i <- "ekonomi-i/"
ekonomi.ii <- "ekonomi-ii/"
exams <- "exams/"

#=============================== Relative Paths ================================
# ---- Settings.Relative.Paths.1 ----

# Relative path extension which carries the folder path from ./courses/CourseName/FileName to ./static/
courses.to.static <- "../../../"
# Relative path extension which carries the folder path from ./courses/CourseName/FileName to ./static/repo/akek/courses/
repo.akek.courses <- paste0(courses.to.static, "repo/akek/courses/")
# Relative path extension which carries the folder path from ./courses/CourseName/FileName to ./static/repo/akek/r/_courses/
repo.akek.r.courses <- paste0(courses.to.static, "repo/akek/r/courses/")

#---

# Relative path for "./static/repo/akek/courses/ekonometri-ars/" folder.
ekonometri.ars.path <- paste0(repo.akek.courses, ekonometri.ars)

# Relative path for "./static/repo/akek/courses/ekonometri-i/" folder.
ekonometri.i.path <- paste0(repo.akek.courses, ekonometri.i)

# Relative path for "./static/repo/akek/courses/ekonometri-ii/" folder.
ekonometri.ii.path <- paste0(repo.akek.courses, ekonometri.ii)

# Relative path for "./static/repo/akek/courses/zaman-serileri-analizi/" folder.
zaman.serileri.analizi.path <- paste0(repo.akek.courses, zaman.serileri.analizi)

# Relative path for "./static/repo/akek/courses/ekonometrik-modelleme/" folder.
ekonometrik.modelleme.path <- paste0(repo.akek.courses, ekonometrik.modelleme)

# Relative path for "./static/repo/akek/courses/economics/" folder.
economics.path <- paste0(repo.akek.courses, economics)

# Relative path for "./static/repo/akek/courses/ekonomi/" folder.
ekonomi.path <- paste0(repo.akek.courses, ekonomi)

# Relative path for "./static/repo/akek/courses/ekonomi-i/" folder.
ekonomi.i.path <- paste0(repo.akek.courses, ekonomi.i)

# Relative path for "./static/repo/akek/courses/ekonomi-ii/" folder.
ekonomi.ii.path <- paste0(repo.akek.courses, ekonomi.ii)

#---

# Relative path for "./static/repo/akek/r/courses/ekonometri-ars/" folder.
ekonometri.ars.r.path <- paste0(repo.akek.r.courses, ekonometri.ars)

# Relative path for "./static/repo/akek/r/courses/ekonometri-i/" folder.
ekonometri.i.r.path <- paste0(repo.akek.r.courses, ekonometri.i)

# Relative path for "./static/repo/akek/r/courses/ekonometri-ii/" folder.
ekonometri.ii.r.path <- paste0(repo.akek.r.courses, ekonometri.ii)

# Relative path for "./static/repo/akek/r/courses/zaman-serileri-analizi/" folder.
zaman.serileri.analizi.r.path <- paste0(repo.akek.r.courses, zaman.serileri.analizi)

# Relative path for "./static/repo/akek/r/courses/ekonometrik-modelleme/" folder.
ekonometrik.modelleme.r.path <- paste0(repo.akek.r.courses, ekonometrik.modelleme)

#=================================== Links =====================================
# ---- Settings.Links.1 ----
# Base link for "akademiekonometri.bitbucket.io/"
bitbucket.io.link <- "https://akademiekonometri.bitbucket.io/"

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/".
akek.courses.link <- paste0(bitbucket.io.link, "akek/courses/")

# Link for "https://akademiekonometri.bitbucket.io/akek/r/courses/".
akek.r.courses.link <- paste0(bitbucket.io.link, "akek/r/courses/")

#---

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonometri-ars/".
ekonometri.ars.link <- paste0(akek.courses.link, ekonometri.ars)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonometri-i/".
ekonometri.i.link <- paste0(akek.courses.link, ekonometri.i)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonometri-ii/".
ekonometri.ii.link <- paste0(akek.courses.link, ekonometri.ii)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/zaman-serileri-analizi/".
zaman.serileri.analizi.link <- paste0(akek.courses.link, zaman.serileri.analizi)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonometrik-modelleme/".
ekonometrik.modelleme.link <- paste0(akek.courses.link, ekonometrik.modelleme)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/economics/".
economics.link <- paste0(akek.courses.link, economics)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonomi/".
ekonomi.link <- paste0(akek.courses.link, ekonomi)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonomi-i/".
ekonomi.i.link <- paste0(akek.courses.link, ekonomi.i)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonomi-ii/".
ekonomi.ii.link <- paste0(akek.courses.link, ekonomi.ii)

#---

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonometri-ars/exams/".
ekonometri.ars.exams.link <- paste0(akek.courses.link, ekonometri.ars, exams)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonometri-i/exams/".
ekonometri.i.exams.link <- paste0(akek.courses.link, ekonometri.i, exams)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonometri-ii/exams/".
ekonometri.ii.exams.link <- paste0(akek.courses.link, ekonometri.ii, exams)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/zaman-serileri-analizi/exams/".
zaman.serileri.analizi.exams.link <- paste0(akek.courses.link, zaman.serileri.analizi, exams)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonometrik-modelleme/exams/".
ekonometrik.modelleme.exams.link <- paste0(akek.courses.link, ekonometrik.modelleme, exams)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/economics/exams/".
economics.exams.link <- paste0(akek.courses.link, economics, exams)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonomi/exams/".
ekonomi.exams.link <- paste0(akek.courses.link, ekonomi, exams)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonomi-i/exams/".
ekonomi.i.exams.link <- paste0(akek.courses.link, ekonomi.i, exams)

# Link for "https://akademiekonometri.bitbucket.io/akek/courses/ekonomi-ii/exams/".
ekonomi.ii.exams.link <- paste0(akek.courses.link, ekonomi.ii, exams)

#---

# Link for "https://akademiekonometri.bitbucket.io/akek/r/courses/ekonometri-ars/".
ekonometri.ars.r.link <- paste0(akek.r.courses.link, ekonometri.ars)

# Link for "https://akademiekonometri.bitbucket.io/akek/r/courses/ekonometri-i/".
ekonometri.i.r.link <- paste0(akek.r.courses.link, ekonometri.i)

# Link for "https://akademiekonometri.bitbucket.io/akek/r/courses/ekonometri-ii/".
ekonometri.ii.r.link <- paste0(akek.r.courses.link, ekonometri.ii)

# Link for "https://akademiekonometri.bitbucket.io/akek/r/courses/zaman-serileri-analizi/".
zaman.serileri.analizi.r.link <- paste0(akek.r.courses.link, zaman.serileri.analizi)

# Link for "https://akademiekonometri.bitbucket.io/akek/r/courses/ekonometrik-modelleme/".
ekonometrik.modelleme.r.link <- paste0(akek.r.courses.link, ekonometrik.modelleme)

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
# update.packages(ask = FALSE, checkBuilt = TRUE)

# Package that MUST be installed.
Load.Install(c("devtools", "png", "proto"))

#=================================
# Package for package control during collaboration.
Load.Install(c("renv")) ## Note that to activate the "renv" package you need to use "renv::init()" once. Sometimes loading the renv package in the .Rprofile might cause not printing the data frames in Rmarkdown files; therefore, it would be better to load the renv package later.

#=================================
# Packages for emojis and icons.
Load.Install(c("rstudio/fontawesome", "hadley/emo"))

#=================================
# Packages for reproducible research.
Load.Install(c("knitr", "rmarkdown", "tinytex", "formatR")) ## Use the following code for all the options of knitr: "str(knitr::opts_chunk$get())"

# Packages for YAML options.
Load.Install(c("ymlthis")) ## https://ymlthis.r-lib.org/index.html

# Packages for general miscellaneous actions.
Load.Install(c("plyr", "dplyr", "magrittr", "stringr", "stringi", "Hmisc", "tm", "lubridate", "NCmisc", "classInt"))

#=================================
# Packages for various data sets.
Load.Install(c("datasets", "FinYang/tsdl", "WDI", "quantmod", "wooldridge"))

#=================================
# Packages for data loading, manipulation, and data reshaping.
Load.Install(c("readxl", "reshape", "reshape2", "tidyr", "data.table"))
# Load.Install(c("XLConnect")) ## Loading XLConnect package before RStudio opens, makes Java unstable and causes R to crash. Therefore, load the XLConnect package after RStudio opens preferably just before the loading function.
# options(java.parameters = "-Xmx8000m") ## Use this code if loading excel file with XLConnect package fails.

#=================================
# Packages for table generation and LaTeX.
Load.Install(c("stargazer", "xtable", "tikzDevice", "latex2exp", "latexpdf"))

#=================================
# Packages for graphics and related packages.
Load.Install(c("ggplot2", "ggthemes", "plotly", "ggThemeAssist", "cowplot", "grid", "gridExtra", "gtable", "lattice", "latticeExtra", "dygraphs", "RColorBrewer", "scales"))

#=================================
# Packages for web scraping.
# Load.Install(c("rvest", "RCurl", "downloader"))

#=================================
# Packages for general econometric analyses.
Load.Install(c("lmtest", "sandwich", "openxlsx", "car", "FSA", "nortest", "normtest", "gvlma", "moments", "pastecs", "aod"))

# Extra packages for the car package.
if("carData" %in% rownames(installed.packages()) == FALSE) {install.packages("carData")}
library("carData")

#=================================
# Packages for specific econometric analyses.
# Load.Install(c("lme4", "pbkrtest", "strucchange"))

#=================================
# Packages for time series analysis.
Load.Install(c("seasonal", "x13binary", "forecast", "aTSA", "urca", "FitAR", "vars", "tsDyn", "lgarch", "ggseas", "slider", "ecm", "dynlm"))
Load.Install(c("fpp2", "fpp3"))
# Load.Install(c("zoo", "xts", "fUnitRoots", "CADFtest"))
# Load.Install(c("MTS", "tseries", "timeSeries"))

#=================================
# Packages for spatial and spatio-temporal analysis.
# Load.Install(c("sp", "spdep", "spatialreg", "rgdal", "maptools", "raster", "geoR", "fields", "pgirmess", "RANN", "pastecs", "spgwr", "maps", "mapproj", "mapdata", "choroplethr", "choroplethrMaps", "tmap"))

#=================================== Seeds =====================================
# ---- Settings.Seeds.1 ----
selected.seed <- 1234
set.seed(selected.seed)

#==================================== END ======================================
