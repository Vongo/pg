testVector <- function(t) {
	t[1]+13*t[2]/t[3]+t[4]+12*t[5]-t[6]-11+t[7]*t[8]/t[9]-10 == 66
}

## Random approaches

# Stop at first solution
system.time({
	nbs <- 1:9
	t <- NULL
	go <- T
	while(go) {
		t <- sample(nbs, 9, replace=F)
		go <- !testVector(t)
	}
	print(t)
})

## Systematic approaches
library(gtools)
perm <- permutations(9,9)

# Stop at first solution
system.time({
	t <- NULL
	i <- 1
	go <- T
	while(go) {
		t <- perm[i,]
		i <- i+1
		go <- !testVector(t)
	}
	print(t)
})

# All solutions
system.time({
	res <- as.data.frame(matrix(NA,0,9))
	nbs <- 1:9
	t <- NULL
	mute <- sapply(seq(nrow(perm)), function(x) {
		t <- perm[x,]
		if (testVector(t)) res <<- rbind(res,t)
	})
	print(res)
})
