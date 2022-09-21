# MonthlyHHexpenditure

Statistics Canada publishes quarterly estimates of household final expenditure, value, volume and price. This is a component of gross domestic product and it accounts for over half of that aggregate. It is one of the most important macroeconomic quantities. For purposes of current economic analysis, it would be a big improvement if the household expenditure estimates could be made available in a more timely way. At present the estimates for a given quarter are available about 60 days after the quarter ends. 

The R code in this repository implements a system that produces monthly interpolations and extrapolations of the quarterly household expenditure time series, value, volume and price. It does this by using fitted models relating the quarterly published estimates to associated monthly indicators in the manner proposed by Gregory Chow and A-L Lin in 1971, and, in cases where suitable monthly indicators are not available, autoregressive integrated moving average models.

More information at: https://rpubs.com/PhilSmith26/941490
