#============================= Akademi Ekonometri ==============================
#=================== Paths, Functions, Packaging, and Seeds ====================
#================================== Settings ===================================

#====================#
# 1. Paths =====
#====================#
# Main path of the "Akademi Ekonometri - Main" folder.
base.path <- paste0(regmatches(getwd(), regexpr("(^.*Akademi Ekonometri - Main)", getwd(), perl = TRUE)), "/") ## "base.path" is the unix path of "./Akademi Ekonometri - Main.Rproj" folder.
# base.path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/") ## Note that this works when Running or Sourcing your file. The code automatically find out the location of the current file.

# Path for the ".static/repo/" folder
repo.path <- paste0(base.path, "static/repo/")

# Paths for "./static/repo/akek/" folder.
akek.path <- paste0(repo.path, "akek/")

# Paths for "./static/repo/akek/courses/" folder.
courses.path <- paste0(repo.path, "akek/courses/")

# Paths for "./static/repo/akek/r/" folder.
r.path <- paste0(repo.path, "akek/r/")

# Path for "./static/repo/akek/r/_functions/" folder.
func.path <- paste0(repo.path, "akek/r/_functions/")

# Path for the "./static/repo/apps/" folder.
apps.path <- paste0(repo.path, "apps/")

# Path for the "./static/repo/css/" folder.
css.path <- paste0(repo.path, "css/")

# Path for the "./static/repo/data/raw/" folder.
raw.data.path <- paste0(repo.path, "data/raw/")

# Path for the "./static/repo/data/processed/" folder.
processed.data.path <- paste0(repo.path, "data/processed/")

# Path for the "./static/repo/data/metadata/" folder.
metadata.data.path <- paste0(repo.path, "data/metadata/")

# Path for the "./static/repo/DCL"/ folder.
DCL.path <- paste0(repo.path, "DCL/")

# Path for the "./static/repo/docs/" folder.
docs.path <- paste0(repo.path, "docs/")

# Path for the "./static/repo/images/" folder.
images.path <- paste0(repo.path, "images/")

# Path for the "./static/repo/latex/" folder.
latex.path <- paste0(repo.path, "latex/")

# Path for "./static/repo/pdfs/" folder.
pdfs.path <- paste0(repo.path, "pdfs/")

# Path for the "./static/repo/scripts/" folder.
scripts.path <- paste0(repo.path, "scripts/")

# Path for "./static/repo/slides/" folder.
slides.path <- paste0(repo.path, "slides/")

#====================#
## 1.1. Folder Names =====
#====================#
ekonometri.ars <- "ekonometri-ars/"
ekonometri.i <- "ekonometri-i/"
ekonometri.ii <- "ekonometri-ii/"
ekonometri <- "ekonometri/"
zaman.serileri.analizi <- "zaman-serileri-analizi/"
ekonometrik.modelleme <- "ekonometrik-modelleme/"
finansal.ekonometri <- "finansal-ekonometri/"
economics <- "economics/"
ekonomi <- "ekonomi/"
ekonomi.i <- "ekonomi-i/"
ekonomi.ii <- "ekonomi-ii/"
exams <- "exams/"

#====================#
## 1.2. Relative Paths =====
#====================#
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

# Relative path for "./static/repo/akek/courses/ekonometri/" folder.
ekonometri.path <- paste0(repo.akek.courses, ekonometri)

# Relative path for "./static/repo/akek/courses/zaman-serileri-analizi/" folder.
zaman.serileri.analizi.path <- paste0(repo.akek.courses, zaman.serileri.analizi)

# Relative path for "./static/repo/akek/courses/ekonometrik-modelleme/" folder.
ekonometrik.modelleme.path <- paste0(repo.akek.courses, ekonometrik.modelleme)

# Relative path for "./static/repo/akek/courses/finansal-ekonometri/" folder.
finansal.ekonometri.path <- paste0(repo.akek.courses, finansal.ekonometri)

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

# Relative path for "./static/repo/akek/r/courses/ekonometri/" folder.
ekonometri.r.path <- paste0(repo.akek.r.courses, ekonometri)

# Relative path for "./static/repo/akek/r/courses/zaman-serileri-analizi/" folder.
zaman.serileri.analizi.r.path <- paste0(repo.akek.r.courses, zaman.serileri.analizi)

# Relative path for "./static/repo/akek/r/courses/ekonometrik-modelleme/" folder.
ekonometrik.modelleme.r.path <- paste0(repo.akek.r.courses, ekonometrik.modelleme)

# Relative path for "./static/repo/akek/r/courses/finansal-ekonometri/" folder.
finansal.ekonometri.r.path <- paste0(repo.akek.r.courses, finansal.ekonometri)

#====================#
## 1.3. Links =====
#====================#
# Base link for "akademiekonometri.github.io/" or akademiekonometri.bitbucket.io/"
io.link <- "https://akademiekonometri.github.io/"
# io.link <- "https://akademiekonometri.bitbucket.io/"

# Link for "./akek/courses/".
akek.courses.link <- paste0(io.link, "akek/courses/")

# Link for "./akek/r/courses/".
akek.r.courses.link <- paste0(io.link, "akek/r/courses/")

#---

# Link for "./akek/courses/ekonometri-ars/".
ekonometri.ars.link <- paste0(akek.courses.link, ekonometri.ars)

# Link for "./akek/courses/ekonometri-i/".
ekonometri.i.link <- paste0(akek.courses.link, ekonometri.i)

# Link for "./akek/courses/ekonometri-ii/".
ekonometri.ii.link <- paste0(akek.courses.link, ekonometri.ii)

# Link for "./akek/courses/ekonometri/".
ekonometri.link <- paste0(akek.courses.link, ekonometri)

# Link for "./akek/courses/zaman-serileri-analizi/".
zaman.serileri.analizi.link <- paste0(akek.courses.link, zaman.serileri.analizi)

# Link for "./akek/courses/ekonometrik-modelleme/".
ekonometrik.modelleme.link <- paste0(akek.courses.link, ekonometrik.modelleme)

# Link for "./akek/courses/finansal-ekonometri/".
finansal.ekonometri.link <- paste0(akek.courses.link, finansal.ekonometri)

# Link for "./akek/courses/economics/".
economics.link <- paste0(akek.courses.link, economics)

# Link for "./akek/courses/ekonomi/".
ekonomi.link <- paste0(akek.courses.link, ekonomi)

# Link for "./akek/courses/ekonomi-i/".
ekonomi.i.link <- paste0(akek.courses.link, ekonomi.i)

# Link for "./akek/courses/ekonomi-ii/".
ekonomi.ii.link <- paste0(akek.courses.link, ekonomi.ii)

#---

# Link for "./akek/courses/ekonometri-ars/exams/".
ekonometri.ars.exams.link <- paste0(akek.courses.link, ekonometri.ars, exams)

# Link for "./akek/courses/ekonometri-i/exams/".
ekonometri.i.exams.link <- paste0(akek.courses.link, ekonometri.i, exams)

# Link for "./akek/courses/ekonometri-ii/exams/".
ekonometri.ii.exams.link <- paste0(akek.courses.link, ekonometri.ii, exams)

# Link for "./akek/courses/ekonometri/exams/".
ekonometri.exams.link <- paste0(akek.courses.link, ekonometri, exams)

# Link for "./akek/courses/zaman-serileri-analizi/exams/".
zaman.serileri.analizi.exams.link <- paste0(akek.courses.link, zaman.serileri.analizi, exams)

# Link for "./akek/courses/ekonometrik-modelleme/exams/".
ekonometrik.modelleme.exams.link <- paste0(akek.courses.link, ekonometrik.modelleme, exams)

# Link for "./akek/courses/finansal-ekonometri/exams/".
finansal.ekonometri.exams.link <- paste0(akek.courses.link, finansal.ekonometri, exams)

# Link for "./akek/courses/economics/exams/".
economics.exams.link <- paste0(akek.courses.link, economics, exams)

# Link for "./akek/courses/ekonomi/exams/".
ekonomi.exams.link <- paste0(akek.courses.link, ekonomi, exams)

# Link for "./akek/courses/ekonomi-i/exams/".
ekonomi.i.exams.link <- paste0(akek.courses.link, ekonomi.i, exams)

# Link for "./akek/courses/ekonomi-ii/exams/".
ekonomi.ii.exams.link <- paste0(akek.courses.link, ekonomi.ii, exams)

#---

# Link for "./akek/r/courses/ekonometri-ars/".
ekonometri.ars.r.link <- paste0(akek.r.courses.link, ekonometri.ars)

# Link for "./akek/r/courses/ekonometri-i/".
ekonometri.i.r.link <- paste0(akek.r.courses.link, ekonometri.i)

# Link for "./akek/r/courses/ekonometri-ii/".
ekonometri.ii.r.link <- paste0(akek.r.courses.link, ekonometri.ii)

# Link for "./akek/r/courses/ekonometri/".
ekonometri.r.link <- paste0(akek.r.courses.link, ekonometri)

# Link for "./akek/r/courses/zaman-serileri-analizi/".
zaman.serileri.analizi.r.link <- paste0(akek.r.courses.link, zaman.serileri.analizi)

# Link for "./akek/r/courses/ekonometrik-modelleme/".
ekonometrik.modelleme.r.link <- paste0(akek.r.courses.link, ekonometrik.modelleme)

# Link for "./akek/r/courses/finansal-ekonometri/".
finansal.ekonometri.r.link <- paste0(akek.r.courses.link, finansal.ekonometri)

#====================#
# 2. Functions =====
#====================#
# Seasonal Adjust Function
source(paste0(func.path, "seasonal_adjust.R")) ## Seasonal Adjust Function.

# ADF Enders' Procedure Function
source(paste0(func.path, "unit_root_adf_enders_procedure_tests.R")) ## ADF Enders' Procedure Function.

# Unit Root and Stationary Tests Function
source(paste0(func.path, "unit_root_tests.R")) ## Unit Root and Stationary Tests Function.

# Phillips-Ouliaris (PO) Cointegration Pairwise Tests
source(paste0(func.path, "cointegration_pairwise_po_tests.R")) ## Phillips-Ouliaris (PO) Cointegration Pairwise Tests.

# Joahnsen (JO) Cointegration Pairwise Tests
source(paste0(func.path, "cointegration_pairwise_jo_tests.R")) ## Joahnsen (JO) Cointegration Pairwise Tests.

# Cointegration Rank Select Function
source(paste0(func.path, "cointegration_select.R")) ## Cointegration Rank Select Function.

# Granger Causality TYDL Pairwise Tests
source(paste0(func.path, "granger_causality_TYDL_pairwise_tests.R")) ## Granger Causality TYDL Pairwise Tests.

# Granger Causality TYDL Multivariate Tests
source(paste0(func.path, "granger_causality_TYDL_multivariate_tests.R")) ## Granger Causality TYDL Multivariate Tests.

# IRF for VAR
# source(paste0(func.path, "irf_var.R")) ## Impulse Response Functions for VAR.

# IRF for VECM
# source(paste0(func.path, "irf_vecm2var.R")) ## Impulse Response Functions for VECM.

# FEVD for VAR
# source(paste0(func.path, "fevd_var.R")) ## Forecast Error Variance Decomposition for VAR.

# FEVD for VECM
# source(paste0(func.path, "fevd_vecm2var.R")) ## Forecast Error Variance Decomposition for VECM.

#====================#
# 3. Packages =====
#====================#

#====================#
## 3.1. Update =====
#====================#
# update.packages(ask = FALSE, checkBuilt = TRUE)

#====================#
## 3.2. Devtools/Personal =====
#====================#
# Packages that MUST be installed.
if("devtools" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(install.packages("devtools")))}
suppressWarnings(suppressMessages(library("devtools"))) ## devtools package is necessary for installing okara package.
if("okara" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(devtools::install_github("omerkara/okara", force = FALSE)))}
suppressWarnings(suppressMessages(library("okara"))) ## okara package.
Load.Install(c("png", "proto"))

Load.Install(c("blogdown")) ## Loads blogdown.
Load.Install(c("usethis", "remotes", "distill", "postcards")) ## Some necessary packages for this web site.

#====================#
## 3.3. renv =====
#====================#
# Package for package control during collaboration.
Load.Install(c("renv")) ## Note that to activate the "renv" package you need to use "renv::init()" once. Sometimes loading the renv package in the .Rprofile might cause not printing the data frames in Rmarkdown files; therefore, it would be better to load the renv package later.

#====================#
## 3.4. Shiny/Knitr =====
#====================#


#====================#
#### 3.4.1. Emoji and Icons =====
#====================#
# Packages for emojis and icons.
Load.Install(c("rstudio/fontawesome", "hadley/emo"))

#====================#
### 3.4.2. Shiny =====
#====================#
# Shiny related packages.
Load.Install(c("shiny", "shinythemes", "shinydashboard", "shinydashboardPlus", "shinyjs", "rintrojs", "shinyBS", "shinyWidgets", "waiter", "shinycssloaders"))

#====================#
### 3.4.3. Knitr =====
#====================#
# Packages for knitr.
Load.Install(c("knitr", "rmarkdown", "tinytex")) ## Use the following code for all the options of knitr: "str(knitr::opts_chunk$get())"

#====================#
#### 3.4.3.1. YAML =====
#====================#
# Packages for YAML options.
Load.Install(c("ymlthis")) ## https://ymlthis.r-lib.org/index.html

#====================#
#### 3.4.3.2. Slides =====
#====================#
# Packages related to slide presentation.
if("xaringan" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(devtools::install_github("yihui/xaringan", force = FALSE)))}

#====================#
## 3.5. Data =====
#====================#

#====================#
### 3.5.1. Data Sets =====
#====================#
# Packages for various data sets.
Load.Install(c("datasets", "FinYang/tsdl", "WDI", "quantmod", "wooldridge", "AER"))

#====================#
### 3.5.2. ETL =====
#====================#
# ETL (extract, transform, and load)
# Packages for extract.
Load.Install(c("readxl", "openxlsx"))
# Load.Install(c("XLConnect")) ## Loading XLConnect package before RStudio opens, makes Java unstable and causes R to crash. Therefore, load the XLConnect package after RStudio opens preferably just before the loading function.
# options(java.parameters = "-Xmx8000m") ## Use this code if loading excel file with XLConnect package fails.

# Packages for transform and load.
Load.Install(c("tidyr", "data.table", "reshape", "reshape2", "plyr", "dplyr", "magrittr", "stringr", "stringi", "Hmisc", "tm", "lubridate", "NCmisc", "classInt", "formatR"))

#====================#
## 3.6. Graphics =====
#====================#
# Packages for graphics and related packages.
Load.Install(c("ggplot2", "ggthemes", "plotly", "ggThemeAssist", "cowplot", "grid", "gridExtra", "gtable", "lattice", "latticeExtra", "dygraphs", "RColorBrewer", "scales"))

#====================#
## 3.7. Table and LaTeX =====
#====================#
# Packages for table generation and LaTeX.
Load.Install(c("stargazer", "xtable", "tikzDevice", "latex2exp", "latexpdf", "DT"))

#====================#
## 3.8. Econometrics =====
#====================#

#====================#
### 3.8.1. General =====
#====================#
# Packages for general econometric analyses.
Load.Install(c("lmtest", "sandwich", "car", "nortest", "normtest", "gvlma", "moments", "pastecs", "aod"))

# Extra packages for the car package.
if("carData" %in% rownames(installed.packages()) == FALSE) {install.packages("carData")}
library("carData")

#====================#
### 3.8.2. Time Series =====
#====================#
# Packages for time series analysis.
Load.Install(c("seasonal", "x13binary", "forecast", "aTSA", "urca", "vars", "tsDyn", "lgarch", "ggseas", "slider", "ecm", "dynlm"))
# Load.Install(c("FitAR"))
Load.Install(c("fpp2", "fpp3"))
# Load.Install(c("zoo", "xts", "fUnitRoots", "CADFtest"))
# Load.Install(c("MTS", "tseries", "timeSeries"))

#====================#
### 3.8.3. Spatial =====
#====================#
# Packages for spatial and spatio-temporal analysis.
# Load.Install(c("sp", "spdep", "spatialreg", "rgdal", "maptools", "raster", "geoR", "fields", "pgirmess", "RANN", "spgwr", "maps", "mapproj", "mapdata", "choroplethr", "choroplethrMaps", "tmap"))

#====================#
### 3.8.4. Econometrics Misc. =====
#====================#
# Packages for specific econometric analyses.
# Load.Install(c("lme4", "pbkrtest", "strucchange"))

#====================#
## 3.9. Misc. =====
#====================#

#====================#
### 3.9.1. Web Scraping =====
#====================#
# Packages for web scraping.
# Load.Install(c("rvest", "RCurl", "downloader"))

#====================#
# 4. Seeds =====
#====================#
selected.seed <- 1234
set.seed(selected.seed)

#==================================== END ======================================
