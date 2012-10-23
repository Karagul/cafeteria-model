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
