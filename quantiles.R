quantiles.strategy <- function(days, students, k, q) {
	prices <- runif(k, 0, max.price)
	history <- sapply(prices, function(price) { 
		(max.price - price) * sum(simulate.a.day(price, students)) } )
		
	high.profit <- prices[history > quantile(history, q)]
		
	new.prices <- replicate(days - k, sample(high.profit, 1))
	new.history <- sapply(new.prices, function(price) {
		(max.price - price) * sum(simulate.a.day(price, students)) } )
		
	history <- c(history, new.history)
	
	sum(history)
}

create.quantiles.strategy <- function(k, q) {
	function(days, students, number.of.groups) {
		quantiles.strategy(days, students, k, q)
	}
}
