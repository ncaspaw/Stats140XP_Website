K <- c(1:10, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
results_sum <- data.frame("Bid" = 0:99) 
results_sum[, paste0("ProbWin_", K)] <- integer(100)

for (k in K) {
  message(k)
  results <- data.frame("MyValue" = integer(100000), "TeamWin" = logical(100000))
  for (i in seq_len(nrow(results))) {
    values <- runif(2*k, 0, 100)
    results[i, ] <- c(values[1], sum(values[1:k]) > sum(values[(k+1):(2*k)]))
  }
  for (i in 1:100) {
    results_sum[i, paste0("ProbWin_", k)] <- sum(results[floor(results$MyValue) == (i - 1), "TeamWin"]) / 
      nrow(results[floor(results$MyValue) == (i - 1),])
  }
}



for (k in K) {
  plot(results_sum$Bid, results_sum[, paste0("ProbWin_", k)], ylab = "Probability of Winning", xlab = "Bid", 
       main = paste0("Probability of Winning Based on Bid \nwith ", k, " Players on Each Team"), ylim = c(0, 1))
  abline(lm(results_sum[, paste0("ProbWin_", k)] ~ results_sum$Bid), col = "red")
}

reg_coeffs <- data.frame("Players" = K, "Const" = integer(length(K)), "Bid" = integer(length(K)))

reg_coeffs <-read.csv("~/Documents/UCLA/2023-2024_ThirdYear/SpringQuarter/Econ106G_GameTheory/106G_RegressionCoeffs.csv", row.names = 1)

for (k in K) {
  reg_coeffs[reg_coeffs[,"Players"] == k, c("Const", "Bid")] <- c(coef(lm(results_sum[, paste0("ProbWin_", k)] ~ results_sum$Bid)))
}

library(car)

# Y-Int Fit

plot(reg_coeffs$Players, reg_coeffs$Const, main = "Y-Intercept Over Number of Players", xlab = "Number of Players", ylab = "Y-Intercept", pch = 16, col = "blue")

const_lm <- lm(reg_coeffs$Const ~ I((reg_coeffs$Players)^(-1/2)))

const_coefs <- coef(const_lm)

const_fit <- function(x) {
  x^(-1/2) * const_coefs[2] + const_coefs[1]
}

sigmoid <- function(s, t, u) {
  t / (t + exp(-u * s)) - 0.5
}

lines(c(seq(0.01, 1, by = 0.01), 1:1000), const_fit(c(seq(0.01, 1, by = 0.01), 1:1000)), col = "goldenrod", lwd = 2)
points(reg_coeffs$Players, reg_coeffs$Const, pch = 16, col = "blue")
legend("bottomright", legend = expression("Y-Int" == 0.5 -0.5 * "Players"^-0.5), col = "goldenrod", lwd = 2)

# Bid Coefficient Fit

plot(reg_coeffs$Players, reg_coeffs$Bid, main = "Slope Parameter Over Number of Players", xlab = "Number of Players", ylab = "Slope Coefficient", pch = 16, col = "blue")

bid_lambda <- as.numeric(powerTransform(reg_coeffs$Bid ~ reg_coeffs$Players)$lambda)

bid_lm <- lm(reg_coeffs$Bid ~ I((reg_coeffs$Players)^bid_lambda))

bid_coefs <- coef(bid_lm)

bid_fit <- function(x) {
  bid_coefs[1] + x^bid_lambda * bid_coefs[2]
}

lines(1:1000, bid_fit(1:1000), col = "goldenrod", lwd = 2)
points(reg_coeffs$Players, reg_coeffs$Bid, pch = 16, col = "blue")
legend("topright", legend = c(expression("Slope" ==  0.0003 + 0.01 * "Players"^-0.59)), col = "goldenrod", lwd = 2)

write.csv(results_sum, file = "106GFinalProjectWinProb.csv")
write.csv(reg_coeffs, file = "106G_RegressionCoeffs.csv")


library(MASS)


