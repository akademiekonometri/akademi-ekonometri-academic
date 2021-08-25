#================================= Render Rmd ==================================
# Notlar:
## Aşağıdaki R kodu, bulunduğu klasördeki Rmd dosyalarını tarar ve onların içindeki R kodlarını aynı dosya ismi ile R dosyası olarak kaydeder.

#=============================== Gerekli Paketler ==============================
# Tek bir adımda gerekli paketlerin yüklenmesi ve kurulması.
#===
# Devtools ve okara paketlerinin yüklenmesi
if("devtools" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(install.packages("devtools")))}
suppressWarnings(suppressMessages(library("devtools"))) ## devtools paketi, okara paketinin yüklenmesi için gereklidir.
if("okara" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(devtools::install_github("omerkara/okara", force = FALSE)))}
suppressWarnings(suppressMessages(library("okara"))) ## okara paketi.
#===
Load.Install(c("rstudioapi", "rmarkdown"))
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
    knitr::purl(rmd.file, documentation = 1, output = rmd.r.file) ## purl fonksiyonu icin notlar: 0 sadece kodun dahil edilmesi (tum text chunklari atiliyor); 1 (default) chunk basliklari koda ekleniyor; 2 roxygen yorum halinde tum textler chunka ekleniyor.
}

# Working directory ilk bastaki haline donduruluyor.
setwd(initial.wd)

#==================================== SON ======================================
