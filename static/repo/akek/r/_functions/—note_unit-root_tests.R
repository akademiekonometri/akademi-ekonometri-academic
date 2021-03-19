#======================== Unit Root and Stationary Test ========================

# Notes:
#
## This script explains what the below functions do in general.
### 1. ADF.Enders.Procedure(ADF) ==> Check the detailed notes in the function document.
### 2. UR.Stationary.Tests(ADF, PP, ERS, and KPSS) ==> Check the detailed notes in the function document.

#============================== General Information ============================
## 1. Unit root testing of univariate series is often the first step in time series analysis. If the variables in a regression contain unit root (nonstationary), then there might be a spurious regression problem as was pointed out by Granger and Newbold (1974) and Phillips (1986). To avoid the spurious regression problem, we have to transform nonstationary time series to make them stationary.
## 2. Transformations such as logarithms can help to stabilize the variance of a time series. Thus, to stabilize the variance, univariate series can be transformed with natural logarithm.
## 3. Differencing can help stabilize the mean of a time series by removing changes in the level of a time series, and so eliminating trend and/or seasonality.
## 4. To stabilize the mean and get white noise series, transformation method depends on whether the time series are trend stationary (TS) or difference stationary (DS). A TS time series has a deterministic trend, whereas a DS time series has a stochastic trend.
## 5. Unit root tests can be used to determine if trending data should be first differenced or regressed on deterministic functions of time to yield the stationary data. Although the difference stationary and trend stationary series are both trending over time, the correct approach should be used in each case. Wrong method might cause over or under differencing depending on the true data generating process (DGP) and results specification error in the time series regressions.

#================================ Lag Selection ================================
# General information about lag selection.
## 1. An important practical issue for the unit root and stationarity tests is the specification of the lag length p.
## 2. If p is too small then the remaining serial correlation in the errors will bias the test.
## 3. If p is too large then the power of the test will suffer.
## 4. Thus, different lag specifications should be used in all tests.

# ADF tests: AIC, BIC, Short, Long, and user selected lags can be used.
## 1. AIC and BIC: Internally selected via ur.df function in UR.Stationary.Tests function. Selected via SelectModel in ADF.Enders.Procedure function.
## 2. Short and Long: for short = trunc[4(N/100)^0.25] and for long = trunc[12(N/100)^0.25] where N = Length of series.
## 3. User defined: user defines the lag.
## Note: In ADF.Enders.Procedure only AIC and BIC is considered.

# PP, ERS, and KPSS tests: Short, Long, and user selected lag can be used.
## 1. Short and Long: for short = trunc[4(N/100)^0.25] and for long = trunc[12(N/100)^0.25] where N = Length of series.
## 2. User defined: user defines the lag.

#================= Comparison of Unit Root and Stationary Tests ================
## 1. Conventional unit root tests such as Dickey-Fuller (DF), ADF and PP tests requires a prior knowledge about the DGP. For instance in ADF tests, inappropriate exclusion of the intercept or trend leads to biased coefficient estimates and causes size problems. On the other hand, inappropriate inclusion of the intercept or trend reduces power of the test. Thus the above tests work better when the researcher have some prior knowledge about the DGP. See https://en.wikipedia.org/wiki/Dickey%E2%80%93Fuller_test for more information.

## 2. First type of unit root test used is ADF. Since form of the DGP is completely unknown to us, we will first follow Enders’ ADF Procedure (EAP) to reveal whether the univariate series is TS or DS by using different deterministic parts (no-constant, constant and/or trend) in it while testing for unit root. In summary, Walter Enders uses ADF test several times (with different deterministic parts) for a univariate time series to identify possible intercept, trend and unit root. The whole procedure is taken from “Supplementary Manual to Accompany, Applied Econometric Time Series (4th Edition) by Walter Enders” at pdf page 63-66 and from “Applied Econometric Time Series (4th Edition) by Walter Enders” at pdf page 206-210. The supplementary manual can be found here (http://time-series.net/yahoo_site_admin/assets/docs/SupplementaryManual_all.117125921.pdf). The whole procedure is coded manually in R software with some little changes.

## 3. Second type of unit root test used is PP test. The ADF and PP tests are asymptotically equivalent but may differ substantially in finite samples due to the different ways in which they correct for serial correlation in the test regression. Also, PP tests tend to be more powerful than the ADF tests. But, they can have severe size distortions (when autocorrelations of εt are negative) and they are more sensitive to model misspecification. PP test is used with 2 different deterministic parts: Constant and Trend.

## 4. Third type of test used is KPSS test. Power of ADF and PP are low if the process is stationary but with a root close to the non-stationary boundary. One way to get around this is to use a stationarity test like KPSS test. Note that the null hypothesis is stationary. Thus, KPSS test is used with 2 different deterministic parts: Constant and Trend.

## 5. Forth type of unit root test is ERS test from Elliott, Rothenberg and Stock (1996). ERS test is also called DF-GLS test which first detrend the series with generalized least squares and then the DF test is conducted. This test is referred to as efficient unit root tests, and it can have substantially higher power than ADF or PP tests especially when root is close to unity. ERS uses generalized least squares. They demonstrate that this modified test has the best overall performance in terms of small-sample size and power, conclusively dominating the ordinary Dickey-Fuller test. In particular, Elliott et al. find that their “DF-GLS” test “has substantially improved power when an unknown mean or trend is present.” (1996, p.813). This test has significantly greater power than the previous versions of the augmented Dickey–Fuller test. Thus, ERS test is used with 2 different deterministic parts: Constant and Trend

#==================================== END ======================================
