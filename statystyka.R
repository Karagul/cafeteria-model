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
