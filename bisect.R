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
