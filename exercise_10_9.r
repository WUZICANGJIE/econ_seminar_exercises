# ARモデルによるインフレの推定とフィリプス曲線の推定

# データの読み込み
gdp_d <- ts(read.csv("GDPdefinflation_quarterlyNSA.csv", header = FALSE)[, 2], start = c(1980, 1), frequency = 4) # 四半期価格指数
gdp_o <- ts(read.csv("GDPO1980TABLE.csv", skip = 2, header = TRUE)$GDP, start = c(1980, 1), frequency = 4) # 実質原系列・1995暦年基準・単位兆円
gdp_a <- ts(read.csv("GDPA1980TABLE.csv", skip = 2, header = TRUE)$GDP, start = c(1980, 1), frequency = 4) # 実質季節調整系列・1995暦年基準・単位兆円（年率にしていない）

# データの確認
plot(gdp_o, type = "l", col = "blue", ylim = range(c(gdp_o, gdp_a)), ylab = "GDP", xlab = "Time")
lines(gdp_o, col = "blue")
lines(gdp_a, col = "red")
lines(gdp_d, col = "green")
legend("topright", legend = c("Original GDP", "Adjusted GDP", "Deflation GDP"), col = c("blue", "red", "green"), lty = 1)

# 前年同期比によるインフレ率
inflation_yoy <- diff(log(gdp_d), lag = 4) * 100

# 移動平均によるインフレ率
gdp_d_sma <- na.omit(filter(gdp_d, rep(1/4, 4), sides = 1))
inflation_sma <- diff(log(gdp_d_sma), lag = 4) * 100

# 中心移動平均によるインフレ率
weights <- c(1, 2, 2, 2, 1) / sum(c(1, 2, 2, 2, 1))
gdp_d_cma <- na.omit(filter(gdp_d, weights, sides = 2))
inflation_cma <- diff(log(gdp_d_cma), lag = 4) * 100

# プロット
plot(inflation_yoy, type = "l", col = "blue", ylim = range(c(inflation_yoy, inflation_sma, inflation_cma)), ylab = "Inflation Rate", xlab = "Time")
lines(inflation_yoy, col = "blue")
lines(inflation_sma, col = "red")
lines(inflation_cma, col = "green")
abline(h = 0, col = "black", lty = 2)
legend("topright", legend = c("YoY Inflation", "Simple Moving Average", "Centered Moving Average"), col = c("blue", "red", "green"), lty = 1)

# GDPギャップ
# 後で気づきましたが、ここはARMAを使うべきでした、時間があったら直します
d_gdp_o <- decompose(gdp_o, type = "multiplicative")
plot(d_gdp_o) # おかしいので季節調整系列を使用

d_gdp_a <- decompose(gdp_a, type = "multiplicative")
plot(d_gdp_a) # これも教科書の図と違い、ホワイトノイズぽい、additiveも試したが全部おかしい

gdp_a_cma <- na.omit(filter(gdp_a, weights, sides = 2))
plot(gdp_a_cmap) # これもおかしい

# GDPギャップのデータの読み込み
gdp_gap <- ts(read.csv("Fig_12_GDPgap_quarterly.csv", skip = 2)[, 4], start = c(1980, 1), frequency = 4) # 内閣府のGDPギャップ
plot(gdp_gap)

# Ljung-Box統計量の確認
Box.test(gdp_gap, lag = 20, type = "Ljung-Box") # 有意な自己相関がある

# コレログラムの確認
acf(gdp_gap, lag.max = 20)

# ARモデルの推定
gdp_gap_ar <- ar(gdp_gap)
gdp_gap_ar # AR(1)モデルが選ばれる

# inflation_yoyでADLモデルによるフィリプス曲線の推定
# (3回も使うから関数として定義したほうがいいかもしれないが、めんどくさいのでやっていない。)
inflation_yoy_list <- c(inflation_yoy)
inflation_yoy_lag <- c(NA, inflation_yoy[-length(inflation_yoy)])

min_l <- min(length(inflation_yoy_list), length(inflation_yoy_lag), length(gdp_gap))

data_yoy <- na.omit(data.frame(
    inflation_yoy_list = inflation_yoy[1:min_l],
    inflation_yoy_lag = inflation_yoy_lag[1:min_l],
    gdp_gap = gdp_gap[1:min_l]
))

adl_yoy <- lm(inflation_yoy_list ~ inflation_yoy_lag + gdp_gap, data = data_yoy)
summary(adl_yoy)

# inflation_smaでADLモデルによるフィリプス曲線の推定
inflation_sma_list <- c(inflation_sma)
inflation_sma_lag <- c(NA, inflation_sma[-length(inflation_sma)])

data_sma <- na.omit(data.frame(
    inflation_sma_list = inflation_sma[1:min_l],
    inflation_sma_lag = inflation_sma_lag[1:min_l],
    gdp_gap = gdp_gap[1:min_l]
))

adl_sma <- lm(inflation_sma_list ~ inflation_sma_lag + gdp_gap, data = data_sma)
summary(adl_sma)

# inflation_cmaでADLモデルによるフィリプス曲線の推定
inflation_cma_list <- c(inflation_cma)
inflation_cma_lag <- c(NA, inflation_cma[-length(inflation_cma)])

data_cma <- na.omit(data.frame(
    inflation_cma_list = inflation_cma[1:min_l],
    inflation_cma_lag = inflation_cma_lag[1:min_l],
    gdp_gap = gdp_gap[1:min_l]
))

adl_cma <- lm(inflation_cma_list ~ inflation_cma_lag + gdp_gap, data = data_cma)
summary(adl_cma) # 前期のインフレ率だけ有意