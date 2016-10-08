# rm(list = ls()) - uncomment to clean your workspace
# setwd("PATH") - uncomment to setup a custom path (must contain a dataset folder)
library(stargazer)

# Functions
getIndexPerformance = function(filename) {
  stock_index = read.csv(filename);
  stock_index_returns_asc = rev(stock_index$Adj.Close) # time is descending; make ascending
  stock_index_returns_log = sapply(stock_index_returns_asc, log) # apply log to all elements
  log_first_diff = diff(stock_index_returns_log) # returns a historical returns vector (log first difference)
  return(log_first_diff)
}

computeMinimumVariancePorfolio = function(data) {
  covmat = cov(data)
  stargazer(covmat, title = "Covariance Matrix", type = "text")
  one.vec = rep(1, nrow(covmat))
  num = solve(covmat) %*% one.vec
  den = as.numeric(t(one.vec) %*% num)
  mv_portfolio = num/den
  colnames(mv_portfolio) = "weight"
  return(mv_portfolio)
}

computeEqualWeightPortfolio = function(countries) {
  number_of_countries = length(countries)
  weight = matrix(1/number_of_countries, nrow = number_of_countries, ncol = 1)
  df = data.frame(weight)
  rownames(df) = countries
  colnames(df) = "weight"
  return(as.matrix(df))
}

computePortfolioReturn = function(data, portfolio) {
  return(as.numeric(colSums(colSums(data) * portfolio)))
}

computePortfolioRisk = function(data, portfolio) {
  return(as.numeric(t(portfolio) %*% cov(data) %*% portfolio))
}

# Load dataset as data frame
# If you have other indexes (other countries) you should provide their name and path to csv as below
dataset = cbind(name="Argentina", path="dataset/[ARG] MERVAL.csv")
dataset = rbind(dataset, cbind(name="Australia", path="dataset/[AUS] ASX 200.csv"))
dataset = rbind(dataset, cbind(name="Brazil", path="dataset/[BRA] IBOV.csv"))
dataset = rbind(dataset, cbind(name="Canada", path="dataset/[CAN] TSX Composite.csv"))
dataset = rbind(dataset, cbind(name="Chile", path="dataset/[CHI] IPSA.csv"))
dataset = rbind(dataset, cbind(name="China", path="dataset/[CHN] SSE.csv"))
dataset = rbind(dataset, cbind(name="European Union", path="dataset/[EUR] Euronext 100.csv"))
dataset = rbind(dataset, cbind(name="Hong Kong", path="dataset/[HKG] Hang seng.csv"))
dataset = rbind(dataset, cbind(name="India", path="dataset/[IND] S&P BSE Sensex.csv"))
dataset = rbind(dataset, cbind(name="Japan", path="dataset/[JAP] Nikkei 225.csv"))
dataset = rbind(dataset, cbind(name="Mexico", path="dataset/[MEX] IPC.csv"))
dataset = rbind(dataset, cbind(name="United States", path="dataset/[EUA] S&P 500.csv"))
dataset_df = as.data.frame(dataset)

# Out of sample and in sample returns
out_of_sample_start = 1 # change to customize out-of-sample window start
out_of_sample_end = 60 # change to customize out-of-sample window end
in_sample_start = out_of_sample_end + 1
indexes_performances = sapply(as.vector(dataset_df$path), getIndexPerformance)
colnames(indexes_performances) = dataset_df$name
out_of_sample_data = indexes_performances[out_of_sample_start:out_of_sample_end,]
in_sample_data = indexes_performances[in_sample_start:nrow(indexes_performances),]

# Minimum Variance Portfolio (mv)
mv_portfolio = computeMinimumVariancePorfolio(out_of_sample_data)
stargazer(mv_portfolio, title = "Minimum variance portfolio", type = "text")
mv_return = computePortfolioReturn(data = in_sample_data, portfolio = mv_portfolio)
mv_risk = computePortfolioRisk(data = in_sample_data, portfolio = mv_portfolio)
results = cbind(portfolio="Minimum variance", return=mv_return, variance=mv_risk, Sharpe=mv_return/sqrt(mv_risk))

# Equal-weight Portfolio (ew)
ew_portfolio = computeEqualWeightPortfolio(as.vector(dataset_df$name))
stargazer(ew_portfolio, title = "Equal-weight portfolio", type = "text")
ew_return = computePortfolioReturn(data = in_sample_data, portfolio = ew_portfolio)
ew_risk = computePortfolioRisk(data = in_sample_data, portfolio = ew_portfolio)
results = rbind(results, cbind(portfolio="Equal-weight", return=ew_return, variance=ew_risk, Sharpe=ew_return/sqrt(ew_risk)))

stargazer(results, title = "Results", type = "text")
