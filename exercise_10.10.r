# ARCHとGARCHモデルの推定

library(rugarch)

# データの読み込み
topix_data <- read.csv("Fig_16_TOPIXreturn_thinner.csv")

# topixの収益率を抽出
topix_return <- na.omit(ts(topix_data$return_topix, start = c(2000, 1), frequency = 4))

# データの確認
summary(topix_return)
plot(topix_return, type = "l", col = "blue", ylab = "TOPIX Return", xlab = "Time")

# 誤差項のARCH効果の確認
acf((topix_return - mean(topix_return))^2, lag.max = 20) # 非常に強い自己相関がある

# ARCH(1)モデルの推定
topix_arch1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
topix_arch1_fit <- ugarchfit(spec = topix_arch1, data = topix_return)
topix_arch1_fit

# GARCH(1, 1)モデルの推定
topix_garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
topix_garch11_fit <- ugarchfit(spec = topix_garch11, data = topix_return)
topix_garch11_fit

# AR(1)モデルによるTOPIXの収益率の推定
topix_ar <- arima(topix_return, order = c(1, 0, 0))
topix_ar
