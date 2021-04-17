#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#=============================== Bizi Takip Edin ===============================

#================================= Render Rmd ==================================
# Notlar:
## Aşağıdaki R kodu, bulunduğu klasördeki Rmd dosyalarını tarar ve onların içindeki R kodlarını aynı dosya ismi ile R dosyası olarak kaydeder.

#=============================== Gerekli Paketler ==============================
# Tek bir adımda gerekli paketlerin yüklenmesi ve kurulması.
# Bu adimi daha kolay hale getirmek için öncelikle "Load.Install" fonksiyonunu tanımlayalım.
#===
Load.Install <- function(Package.Names, Quiet = FALSE, Update.All = FALSE) {
    is_installed <- function(my.pkgs) is.element(my.pkgs, utils::installed.packages()[ ,1])
    github.pkgs <- grep("^.*?/.*?$", Package.Names, value = TRUE)
    github.bare.pkgs <- sub(".*?/", "", github.pkgs)
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
#===
# Devtools paketinin yüklenmesi
## Load.Install fonksiyonunun çalışması için devtools paketi gereklidir.
if("devtools" %in% rownames(installed.packages()) == FALSE) {suppressWarnings(install.packages("devtools"))}
suppressWarnings(library("devtools"))

Load.Install(c("rstudioapi", "rmarkdown"))
#===
## Load.Install(Package.Names = "plyr")
## Load.Install(Package.Names = c("plyr", "dplyr"))
## Load.Install(c("plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc"))
## Load.Install(c("plyr", "dplyr"), Quiet = TRUE, Update.All = TRUE)
#===

#======================== Working Directory'yi Belirlemek ======================
# Working directory'nin bu kaynak dosyasının olduğu lokasyonda belirlenmesi.
#===
initial.wd <- getwd() ## Şimdiki working directory.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının uzantısını buluyor.
setwd(paste0(main.path, "/")) ## Yeni working directory bu kaynak dosyasının lokasyonunda belirleniyor.

#==================== Rmd Dosyalarından Kod Çıkartılması =======================
# Rmd uzantisina sahip dosya isimleri uzantisiz olarak aliniyor.
file.names <- gsub("(.Rmd)", "", list.files(pattern = ".*.Rmd"))

# Dongu icinde Rmd dosyalarindan R kodunun cikartilmasi.
for (i in 1:length(file.names)) {
    ## Dosya isimlerinin belirlenmesi.
    rmd.file <- paste0(file.names[i], ".Rmd") ## .Rmd dosyasi.
    rmd.r.file <- paste0(file.names[i], ".R") ## .R dosyasi.

    ## Kaydedilmemis olasi degisiklikler icin .Rmd dosyasi yeniden render ediliyor.
    rmarkdown::render(rmd.file)

    ## Daha once eger render edilmis .Rmd dosyasi (R uzantili) olma ihtimaline karsi, bu olasi dosya siliniyor. Asagidaki purl kodunda eger degisiklik yapilirsa, o degisiklerin calismasi icin bu adim gerekli.
    unlink(rmd.r.file)

    ## Ayni isimli bir .R dosyasi olusturuyor. Biz burada tip 1'i kullaniyoruz cunku hem chunk isimlerini hem de R kodunu istiyoruz.
    purl(rmd.file, documentation = 1, output = rmd.r.file) ## purl fonksiyonu icin notlar: 0 sadece kodun dahil edilmesi (tum text chunklari atiliyor); 1 (default) chunk basliklari koda ekleniyor; 2 roxygen yorum halinde tum textler chunka ekleniyor.
}

# Working directory ilk bastaki haline donduruluyor.
setwd(initial.wd)

#==================================== SON ======================================
