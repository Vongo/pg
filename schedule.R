ad.days <- c("Thursday", "Saturday", "Sunday")
ad.nb <- 3
ad.cities <- c("Rothenburg","Würzburg","Bamberg","Bayreuth","Linz","Innsbruck")
ad <- data.frame(days=ad.days, nb=ad.nb, cities=ad.cities, stringsAsFactors=F)

mael.days <- c("Thursday", "Saturday", "Sunday")
mael.nb <- 2
mael.cities <- c("Rothenburg","Würzburg","Bamberg","Bayreuth","Linz","Innsbruck")
mael <- data.frame(days=mael.days, nb=mael.nb, cities=mael.cities, stringsAsFactors=F)

manon.days <- c("Thursday", "Saturday", "Sunday")
manon.nb <- 3
manon.cities <- c("Rothenburg","Würzburg","Bamberg","Bayreuth","Linz","Innsbruck")
manon <- data.frame(days=manon.days, nb=manon.nb, cities=manon.cities, stringsAsFactors=F)

nastia.days <- "Thursday"
nastia.nb <- 1
nastia.cities <- c("Rothenburg","Würzburg","Bamberg","Bayreuth")
nastia <- data.frame(days=nastia.days, nb=nastia.nb, cities=nastia.cities, stringsAsFactors=F)

people.name <- data.frame(name=c("Mael", "Adrian", "Manon", "Anastasia"), i=1:4, stringsAsFactors=F)
people <- list(mael, ad, manon, nastia)

schedule <- data.frame(matrix(NA,length(unique(ad.days)),(2+length(people.name$name)),dimnames=list(c(), c("Day", "City", people.name$name))), stringsAsFactors=F)
schedule$Day <- unique(ad$days) # Because of the car <3

schedule$City <- sapply(schedule$Day, function(x) {
	cities <- unique(ad.cities)
	day.x.people <- people.name[sapply(people.name$i,function(j) x%in%people[[j]]$days),"name"]
	for (p in day.x.people) {
		cities <- intersect(cities, people[[people.name[people.name$name==p,]$i]]$cities)
	}
	repeat {
		ret <- sample(cities,1)
		if (!(ret %in% schedule$City)) break
	}
	ret
})

sapply(people.name$name, function(x) {
	days.ok <- vector()
	mute <- sapply(schedule$Day, function(y) {
		if (y%in%people[[people.name[people.name$name==x,]$i]]$days)
			days.ok <<- c(days.ok, y)
	})
	days.go <- sample(days.ok, unique(people[[people.name[people.name$name==x,]$i]]$nb))
	schedule[,x] <<- sapply(schedule$Day, function(y) {
		y %in% days.go
	})
})

print(schedule)