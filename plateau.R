plateau.strategy <- function(days, students, k, d1, d2) {
	prices <- runif(k, 0, max.price)
	bought <- sapply(prices, function(price) { sum(simulate.a.day(price, students)) } )
	
	left.ends <- NULL
	last.left.end <- 1000000
	
	pb <- cbind(prices, bought)
	pb <- pb[order(pb[,1]),]
	
	for (i in 1:length(prices)) {
		if (abs(last.left.end - pb[i, 2]) > d1) {
			last.left.end <- pb[i, 2]
			left.ends <- c(left.ends, i)
		}
	}
	
	# (price, profit)
	pp <- t(apply(pb, 1, function(row) { c(row[1], (max.price - row[1]) * row[2]) } ))
	pp <- pp[left.ends,]
	
	if (length(left.ends) > 1) {
		price <- pp[which.max(pp[,2]),1] + d2
	} else {
		price <- pp[1] + d2
	}
	
	new.prices <- replicate(days - k, price)
	new.bought <- sapply(new.prices, function(price) { sum(simulate.a.day(price, students)) } )
	
	prices <- c(prices, new.prices)
	bought <- c(bought, new.bought)
	
	pb <- cbind(prices, bought)
	
	sum(apply(pb, 1, function(row) { (max.price - row[1]) * row[2] } ))
}

create.plateau.strategy <- function(k, d1, d2) {
	function(days, students, number.of.groups) {
		plateau.strategy(days, students, k, d1, d2)
	}
}
