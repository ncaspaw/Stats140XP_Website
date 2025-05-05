K <- c(1, 3, 5, 10, 30, 100, 500, 800, 1000)
results <- data.frame("Value" = integer(100000), "BidAmount"= integer(100000))
results_sum <- data.frame("Value" = 1:100)
results_sum[, paste0("WinProb_", K)] <- integer(100)
results_sum[, paste0("AvgMinBidAmt_", K)] <- integer(100)
free_riding_win_prob <- data.frame("Value" = 1:100)
free_riding_win_prob[, paste0("FreeRidingPercent_", K)] <- integer(100)

# We simulate the win probabilities for bidding the maximum amount you'd be 
# willing to bid in any situation (== value)
# 
# Essentially, with value V, should I bid at all? 
# 
# Also need to simulate the situation where my team wins without me bidding anything? 
# 
# 
# We see that as the number of players increases, the probability of winning by playing any bid goes to 50%


for (k in K) {
  message(k)
  for (i in 1:nrow(results)) {
    value <- runif(1, 0, 100)
    bids <- runif(2*k-1, 0, 100)
    min_bid <- max(ifelse(k > 1, sum(bids[k:(2*k-1)]) - sum(bids[1:(k-1)]), bids), 0)
    #                 Value  Min Bid Amount
    results[i, ] <- c(value, min_bid)
  }
  for (i in 1:100) {
    results_sum[i, paste0("WinProb_", k)] <- sum(i - results[, "BidAmount"] > 0) / 
      nrow(results)
    results_sum[i, paste0("AvgMinBidAmt_", k)] <- sum(results[ceiling(results$Value) == i, "BidAmount"] * (results[ceiling(results$Value) == i, "BidAmount"] < i)) / 
      nrow(results[ceiling(results$Value) == i, ])
  }
  for (i in 1:100) {
    free_riding_win_prob[i, paste0("FreeRidingPercent_", k)] <- sum(results[ceiling(results$Value) == i, "BidAmount"] == 0) / 
      nrow(results[ceiling(results$Value) == i, ])
  }
}

plot(results_sum$Value, results_sum$WinProb_1000, ylim = c(0,1))

plot(free_riding_win_prob$FreeRidingPercent_1000, ylim = c(0, 1))
