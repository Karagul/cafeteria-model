log.likelihood <- function(group.size, group, prices, price.group, parameters) {
	price.vector <- price.group[,group]
	
	alpha <- parameters[1]
	pminus <- parameters[2]
	pplus <- parameters[3]
		
	a <- price.vector[which(prices < alpha)]
	b <- price.vector[which(prices >= alpha)]
	
	if (length(a) > 0) {
		c <- replicate(length(a), group.size) - a
	} else {
		c <- 0
	}
	
	if (length(b) > 0) {
		d <- replicate(length(b), group.size) - b
	} else {
		d <- 0
	}
	
	sum(a) * log(pminus) + sum(b) * log(pplus) + sum(c) * log(1 - pminus) + sum(d) * log(1 - pplus)
}

estimate.parameters <- function(group.size, group, prices, price.group) {
	left.ends <- c(0, sort(prices))
	right.ends <- c(sort(prices), max.price)
	intervals <- cbind(left.ends, right.ends)
	
	res <- t(apply(intervals, 1, function(interval) {
		r <- optim(c(mean(interval), 0.25, 0.75),
               function(s) log.likelihood(group.size, group, prices, price.group, s),
               control=list(fnscale=-1),
               lower=c(interval[1] + 0.01, 0.01, 0.51),
               upper=c(interval[2] - 0.01, 0.49, 0.99),
               method = "L-BFGS-B")
               
        if (r$convergence == 0) {
        	c(r$value, r$par)
        } else {
        	c(-Inf, 0, 0, 0)
        }
	}))
	
	res[which.max(res[,1]), 2:4]
}

mle.strategy <- function(days, students, number.of.groups, k) {
	students.count <- length(students[,1])

	prices <- runif(k, 0, max.price)
	history <- t(sapply(prices, function(x) { simulate.a.day(x, students) } ))
	bought <- apply(history, 1, function(row) { sum(row) } )
	
	# (student, bought)
	sb <- cbind(matrix(1:students.count), matrix(colSums(history)))
	sb <- sb[order(sb[,2]),]
	
	# (student, bought, group)
	sbg <- cbind(sb, matrix(sort(c(replicate(students.count/number.of.groups, 1:number.of.groups)))))
	sbg <- sbg[order(sbg[,1]),]
	
	# (group)
	g <- sbg[,3]
	
	# price.group[which(prices == cena), grupa] ile kupiono w tej grupie po cenie cena
	price.group <- sapply(1:number.of.groups, 
			function(group) { apply(history, 1, 
				function(row) { sum(row[which(g == group)]) }) } )
	
	# estymowane parametry grup
	estimated.groups <- t(sapply(1:number.of.groups,
		function(group) { estimate.parameters(240 / number.of.groups, group, prices, price.group) } ))
		
	# (price, profit)
	pp <- t(sapply(c(0, groups[,1]), function(price) { c(price, expected.profit(groups, price)) } ))
		
	# wyznaczona cena
	price <- pp[which.max(pp[,2]), 1]
	
	new.prices <- replicate(days - k, price)
	new.bought <- sapply(new.prices, function(price) { sum(simulate.a.day(price, students)) })
	
	pb <- cbind(c(prices, new.prices), c(bought, new.bought))
	
	sum(apply(pb, 1, function(row) { (max.price - row[1]) * row[2] } ))

}
	
create.mle.strategy <- function(k) {
	function(days, students, number.of.groups) {
		mle.strategy(days, students, number.of.groups, k)
	}
}
