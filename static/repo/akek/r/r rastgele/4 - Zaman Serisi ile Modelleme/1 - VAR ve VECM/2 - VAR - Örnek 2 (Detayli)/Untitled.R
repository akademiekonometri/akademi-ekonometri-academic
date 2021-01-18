ts1 <- data1.ts[, "RCAD"] ## Duzey
ts2 <- data2.ts[, "Gr.RCAD"] ## Buyume
ts1.diff <- diff(ts1) ## Duzey ilk farklar
ts2.diff <- diff(ts2) ## Buyume ilk farklar (Ikinci farklar olarak dusunulebilir)
adf.test(ts1, nlag = NULL, output = TRUE) ## Duzey: unit root (duragan degil).
adf.test(ts2, nlag = NULL, output = TRUE) ## Buyume: buyuk ihtimalle stationary (duragan).
adf.test(ts1.diff, nlag = NULL, output = TRUE) ## Duzey ilk farklar: buyuk ihtimalle stationary (duragan).
adf.test(ts2.diff, nlag = NULL, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

pp.test(ts1, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Duzey: unit root (duragan degil).
pp.test(ts2, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Buyume: buyuk ihtimalle stationary (duragan).
pp.test(ts1.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Duzey ilk farklar: buyuk ihtimalle stationary (duragan).
pp.test(ts2.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

kpss.test(ts1, lag.short = TRUE, output = TRUE) ## stationary (duragan).
kpss.test(ts2, lag.short = TRUE, output = TRUE) ## stationary (duragan).
kpss.test(ts1.diff, lag.short = TRUE, output = TRUE) ## stationary (duragan).

# RGDP, Gr.RGDP ve ayni zamanda RGDP'nin ilk farklari icin.
## Sonuc: Buzey duragan degil, buyume ve ilk farklar duragan.
ts1 <- data1.ts[, "RGDP"] ## Duzey
ts2 <- data2.ts[, "Gr.RGDP"] ## Buyume
ts1.diff <- diff(ts1) ## Duzey Ilk farklar
ts2.diff <- diff(ts2) ## Buyume ilk farklar (Ikinci farklar olarak dusunulebilir)
adf.test(ts1, nlag = NULL, output = TRUE) ## unit root (duragan degil).
adf.test(ts2, nlag = NULL, output = TRUE) ## stationary (duragan).
adf.test(ts1.diff, nlag = NULL, output = TRUE) ## unit root (duragan degil).

pp.test(ts1, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## unit root (duragan degil).
pp.test(ts2, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## stationary (duragan).
pp.test(ts1.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## unit root (duragan degil).

kpss.test(ts1, lag.short = TRUE, output = TRUE) ## stationary (duragan).
kpss.test(ts2, lag.short = TRUE, output = TRUE) ## stationary (duragan).
kpss.test(ts1.diff, lag.short = TRUE, output = TRUE) ## stationary (duragan).

# RER, Gr.RER ve ayni zamanda RER'nin ilk farklari icin.
## Sonuc: Buzey duragan degil, buyume ve ilk farklar duragan.
ts1 <- data1.ts[, "RER"] ## Duzey
ts2 <- data2.ts[, "Gr.RER"] ## Buyume
ts1.diff <- diff(ts1) ## Duzey Ilk farklar
ts2.diff <- diff(ts2) ## Buyume ilk farklar (Ikinci farklar olarak dusunulebilir)
adf.test(ts1, nlag = NULL, output = TRUE) ## unit root (duragan degil).
adf.test(ts2, nlag = NULL, output = TRUE) ## stationary (duragan).
adf.test(ts1.diff, nlag = NULL, output = TRUE) ## stationary (duragan).

pp.test(ts1, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## unit root (duragan degil).
pp.test(ts2, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## stationary (duragan).
pp.test(ts1.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## stationary (duragan).

kpss.test(ts1, lag.short = TRUE, output = TRUE) ## unit root (duragan degil).
kpss.test(ts2, lag.short = TRUE, output = TRUE) ## stationary (duragan).
kpss.test(ts1.diff, lag.short = TRUE, output = TRUE) ## stationary (duragan).
