# Symulacja strategii bisekcji - zwraca łączny zysk.
bisect.strategy <- function(days, students, k) {
	left <- 0
	right <- max.price
	middle <- mean(c(left, right))
	
	price <- 0
	
	shots <- 0
	shooting.left = TRUE
	
	profit.left <- 0
	profit.right <- 0
	
	overall.profit <- 0
	
	for (i in 1:days) {
		if (left < middle && middle < right) {
			# wystrzelano już jedną połowę przedziału
			if (shots == k) {
				if (!shooting.left) {
					if (profit.left < profit.right) {
						left <- middle
					} else {
						right <- middle
					}
					profit.left <- 0
					profit.right <- 0
					middle <- mean(c(left, right))
				}
				shooting.left = !shooting.left;
				shots <- 0
			}
		
			# wybór ceny
			if (shooting.left) {
				price <- runif(1, left, middle)
			} else {
				price <- runif(1, middle, right)
			}
			
			# zysk
			profit <- (max.price - price) * sum(simulate.a.day(price, students))

			if (shooting.left) {
				profit.left <- profit.left + profit
			} else {
				profit.right <- profit.right + profit
			}
		
			overall.profit <- overall.profit + profit
	
			shots <- shots + 1;
		} else {
			# z za bardzo zawęziliśmy i !(left < middle && middle < right)
			overall.profit <- overall.profit + ((max.price - price) * 
				sum(simulate.a.day(price, students)))
		}
	}
	
	overall.profit
}

# "Konstruktor"
create.bisect.strategy <- function(k) {
	function(days, students, number.of.groups) {
		bisect.strategy(days, students, k)
	}
}
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
fraction.of.max.expected <- NULL
n <- NULL
number.of.groups <- NULL
strategy <- NULL

bisect.4 <- create.bisect.strategy(4)
mle.5 <- create.mle.strategy(5)
quantiles.5 <- create.quantiles.strategy(5, 0.85)
plateau.10.20.0.5 <- create.plateau.strategy(10, 20, 0.5)
mle.15 <- create.mle.strategy(15)

functions <- c(bisect.4, mle.5, quantiles.5, plateau.10.20.0.5, mle.15)
names <- c("bisect k=4", "mle k=5", "quantiles k=5, q=0.85", "plateau k=10, d1=20, d2=0.5", "mle k=15")

for (i in 1:5) {
	print(paste("i =", i))
	flush.console()
	
	for (j in 1:10) {
		print(paste("j =", j))
		flush.console()
		
		groups <- generate.groups(i)
		mep <- max.expected.profit(groups)
		
		for (k in c(20, 50, 200, 400)) {
			print(paste("k =", k))
			flush.console()
			
			students = generate.students(groups)
			
			for (l in 1:5) {
				print(paste("l =", l))
				flush.console()
			
				profit <- functions[[l]](k, students, i)
				fraction.of.max.expected <- c(fraction.of.max.expected, profit/(mep * k))
				n <- c(n, paste(k))
				number.of.groups <- c(number.of.groups, paste(i))
				strategy <- c(strategy, names[l])
			}				
		}
	}
}

coplot(fraction.of.max.expected ~ n | number.of.groups * strategy)
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
number.of.students <- 240
max.price <- 10

# generuje grupy
generate.groups <- function(count) {
    groups = t(replicate(
                count, 
                c(runif(1, 0, max.price), 
                runif(1, 0, 0.5), 
                runif(1, 0.5, 1))))
          
    colnames(groups) <- c("alpha", "pminus", "pplus")     
                
    groups
}

# generuje studentów
generate.students <- function(groups) {
    times <- number.of.students / length(groups[, 1])
	groups[sample(replicate(times, 1:length(groups[, 1]))), ]
}

# Symuluje działanie jednego studenta dla wiersza z macierzy students i ceny
simulate.a.student <- function(row, price) {
    probability = runif(1, 0, 1)

    if (price >= row["alpha"]) {
        probability <= row["pplus"]
    } else {
        probability <= row["pminus"]
    }
}

# Symuluje dzień dla danej ceny i macierzy studentów

simulate.a.day <- function(price, students) {
    apply(students, 1, function(row) { simulate.a.student(row, price) })
}

# Losowa strategia
random.strategy <- function(days, students) {
	prices <- runif(days, 0, max.price)
    sum(sapply(prices, function(price) { (max.price - price) * sum(simulate.a.day(price, students)) }))
}

probability.of.buying <- function(row, beta) {
    if (beta >= row["alpha"]) {
        row["pplus"]
    } else {
        row["pminus"]
    }
}

# Wartość oczekiwana liczby kupujących dla danej ceny
expected.buyers <- function(groups, beta) {
	(number.of.students / length(groups[, 1])) * 
        sum(apply(groups, 1, function(row) { probability.of.buying(row, beta) }))
}

# Wartość oczekiwana zysku dla danej ceny
expected.profit <- function(groups, beta) {
    (max.price - beta) * expected.buyers(groups, beta)
}

# Maksymalna wartość oczekiwana zysku dla jakiegoś podziału na grupy
max.expected.profit <- function(groups) {
    max(sapply(c(0, groups[,1]), function(x) { expected.profit(groups, x) } ))    
}
