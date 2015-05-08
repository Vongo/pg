require(microbenchmark)

fibs <- c(1,1)

# Naive Fibs
fib <- function(x) {
	if(x==1 || x==2)
		1
	else
		(fib(x-1) + fib(x-2))
}

# with storage
fib1 <- function(x) {
	if (x>length(fibs)) {
		ret <- (fib1(x-1) + fib1(x-2))
		fibs <<- c(fibs, ret)
		ret
	} else 
		fibs[x]
}

# No recursion
fib2 <- function(x) {
	sapply(3:(x), function(y) {
		if (y > length(fibs))
			fibs <<- c(fibs, fibs[y-1]+fibs[y-2])
	})
	fibs[x]
}

fibz <- c(1,1)
fib3 <- function(x) {
	for (i in 3:x) {
		fibz[i] <<- fibz[i-1]+fibz[i-2]
	}
	fibz[x]
}

fubz <- c(1,1)
fib4 <- function(x) {
	sapply(3:x,function(i) {
		fubz[i] <<- fubz[i-1]+fubz[i-2]
	})
	fubz[x]
}

# no storage, no recursion
fib5 <- function(x) {
	a <- 1
	b <- 1
	a<-sapply(3:x,function(i) {
		c<-a+b
		a<<-b
		b<<-c
	})
	b
}

# no storage, no recursion
fib6 <- function(x) {
	a <- 1
	b <- 1
	a<-sapply(3:x,function(i) {
		b<<-a+b
		a<<-b-a
	})
	b
}

# Test process
TEST <- 600

# TOO LONG
# print(microbenchmark({
# 	fib(TEST)
# }))

cat("\n test 1\n")
print(length(fibs))
print(microbenchmark({
	fibs <<- c(1,1)
	fib1(TEST)
}))
print(length(fibs))

cat("\n test 2\n")
fibs <- c(1,1)
print(length(fibs))
print(microbenchmark({
	fibs <<- c(1,1)
	fib2(TEST)
}))
print(length(fibs))

cat("\n test 3\n")
fibz <- c(1,1)
print(length(fibz))
print(microbenchmark({
	fibz <<- c(1,1)
	fib3(TEST)
}))
print(length(fibz))

cat("\n test 4\n")
fubz <- c(1,1)
print(length(fubz))
print(microbenchmark({
	fubz <<- c(1,1)
	fib4(TEST)
}))
print(length(fubz))

cat("\n test 5\n")
print(microbenchmark({
	fib5(TEST)
}))

cat("\n test 6\n")
print(microbenchmark({
	fib6(TEST)
}))

cat("\n")
print(fib1(1000))
print(fib2(1000))
print(fib3(1000))
print(fib4(1000))
print(fib5(1000))
print(fib6(1000))