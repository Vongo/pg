max_depart <- c("AUT","BB","BIM","GCU","GE","GEA","GEN","GI","GMC","GMCIP","GMD","GMPP","GMPPA","INFO","SGM","TC","TCA","TCOM")
years <- 1999:2014
effectif_depart <- as.data.frame(matrix(NA,length(years),length(max_depart),dimnames=list(years,max_depart)))
meilleur_classement_depart <- as.data.frame(matrix(NA,length(years),length(max_depart),dimnames=list(years,max_depart)))
pire_classement_depart <- as.data.frame(matrix(NA,length(years),length(max_depart),dimnames=list(year=years,departement=max_depart)))
moyenne_classement_depart <- as.data.frame(matrix(NA,length(years),length(max_depart),dimnames=list(years,max_depart)))
mediane_classement_depart <- as.data.frame(matrix(NA,length(years),length(max_depart),dimnames=list(years,max_depart)))
ecart_type_classement_depart <- as.data.frame(matrix(NA,length(years),length(max_depart),dimnames=list(years,max_depart)))
pc_filles_depart <- as.data.frame(matrix(NA,length(years),length(max_depart),dimnames=list(years,max_depart)))
pc_redoublants_depart <- as.data.frame(matrix(NA,length(years),length(max_depart),dimnames=list(years,max_depart)))

iYear <- 1
effectif <- vector()
effectifPCC <- vector()
nDepart <- vector()

for (year in years) {
    cat(paste("Processing data for year ",year,"…\n",sep=""))
    fyear <- paste("../data/ALL-",year,".csv",sep="")
    aa <- read.csv(fyear, sep="\t", stringsAsFactors=F)
    colnames(aa) <- c("annee","depart","numero","civ","nom","prenom","login","email","datenais","bac","redoublant","nat","groupe","promo","rang")
    aa$depart <- toupper(aa$depart)
    bb <- aa[aa$rang>0,]

    effectif <- c(effectif, nrow(aa))
    effectifPCC <- c(effectifPCC, nrow(bb))

    moy <- tapply(bb$rang,bb$depart,mean)
    et <- tapply(bb$rang,bb$depart,sd)
    medi <- tapply(bb$rang,bb$depart,median)
    len <- tapply(bb$rang,bb$depart,length)
    lRank <- tapply(bb$rang,bb$depart,min)
    fRank <- tapply(bb$rang,bb$depart,max)

    oo <- cbind(effectif = len, moyRang = moy, etRang = et, medRang = medi,  pireRang = fRank, meilleurRang = lRank)
    oo <- as.data.frame(oo)

    oo$departement = rownames(oo)

    void <- rep(NA,nrow(oo))
    oo <- cbind(oo,void,void,void,void,void)
    colnames(oo) <- c(colnames(oo)[1:7],"pcFilles","pcRedoublants","moyAvance","pcAvance","moyAvanceDesGensEnAvance")

    departs <- unique(oo$departement)
    #print(departs)
    nDepart <- c(nDepart,length(departs))

    for (depart in departs) {
        #cat(paste("\nProcessing department ",depart,"…\n",sep=""))
        tbb <- bb[bb$depart==depart,]
        #tbb <- tbb[complete.cases(tbb),]
        pcf <- 100*nrow(tbb[tbb$civ=="Mll",])/nrow(tbb)
        pcr <- 100*(nrow(tbb[!is.na(tbb$redoublant),])/nrow(tbb))
        sur <- sapply(tbb$datenais, function(x) (as.numeric(strsplit(as.character(x),"/")[[1]][3])-1995))
        sur <- sur[complete.cases(sur)]
        msur <- mean(sur)
        hmsur <- mean(sur[sur>0])
        psur <- 100*(length(sur[sur>0])/length(sur))

        oo[oo$departement==depart,8:12] <- c(pcf,pcr,msur,psur,hmsur)

        #cat(paste("\tPourcentage de redoublants :",pcr,"%\n"))
        #cat(paste("\tMoyenne d'avance :",msur,"ans\n"))
        #cat(paste("\tPourcentage de gens en avance :",psur,"%\n"))
        #cat(paste("\tPourcentage de filles :",pcf,"%\n"))
        #cat(paste("\tMoyenne d'avance des gens en avance :",hmsur,"ans\n"))


        effectif_depart[iYear,depart] <- oo[oo$departement==depart,]$effectif
        meilleur_classement_depart[iYear,depart] <- oo[oo$departement==depart,]$meilleurRang
        pire_classement_depart[iYear,depart] <- oo[oo$departement==depart,]$pireRang
        moyenne_classement_depart[iYear,depart] <- oo[oo$departement==depart,]$moyRang
        mediane_classement_depart[iYear,depart] <- oo[oo$departement==depart,]$medRang
        ecart_type_classement_depart[iYear,depart] <- oo[oo$departement==depart,]$etRang
        pc_filles_depart[iYear,depart] <- oo[oo$departement==depart,]$pcFilles
        pc_redoublants_depart[iYear,depart] <- oo[oo$departement==depart,]$pcRedoublants
    }

    oo <- oo[order(oo$moyRang),]
    write.csv(oo,paste("../results/stats",year,".csv",sep=""),row.names=T)


    iYear <- iYear + 1
}

global <- as.data.frame(rbind(effectif,effectifPCC,nDepart))
names(global) <- years

print("global")
print(global)

cat(paste("\nBuilding image outputs...\n"))


print_global <- function(datak,name) {
    cat(paste("\tProcessing ",name,"…\n",sep=""))

    xrange <- range(as.numeric(colnames(datak)))
    yrange <- range(datak[complete.cases(datak),])

    #departs <- colnames(datak)
    metrics <- c("effectif","effectifPCC")
    nDepart <- length(metrics)
    years <- colnames(datak)

    png(paste("../results/img/",name,".png",sep=""))

    plot(xrange, yrange, type="n", xlab="Année", ylab=name)
    colors <- rainbow(nDepart)
    linetype <- 1:nDepart
    plotchar <- seq(18,18+nDepart,1)

    i <- 1
    for (m in metrics) {
        lines(years, datak[m,], type="b", lwd=1.5,
            lty=linetype[i], col=colors[i], pch=plotchar[i])
        i + 1 -> i
    }

    title(paste(name,sep=""))

    # add a legend
    legend("topleft", metrics, metrics, cex=0.8, col=colors,
        pch=plotchar, lty=linetype, title="Légende")

    dev.off()
}

print_by_depart <- function(datak,name) {
    cat(paste("\tProcessing ",name,"…\n",sep=""))

    xrange <- range(as.numeric(rownames(datak)))
    #yrange <- range(datak[complete.cases(datak),])
    departs <- c("INFO","GMC","GMD","GMPP","TCOM","BB","SGM","GE","GI","BIM","GEN")
    dd <- lapply(departs,function(c) {
        d <- datak[,c]
        d[!is.na(d)]
        })
    yrange <- range(unlist(dd))

    #departs <- colnames(datak)
    nDepart <- length(departs)
    years <- rownames(datak)

    png(paste("../results/img/",name,".png",sep=""))

    plot(xrange, yrange, type="n", xlab="Année", ylab=name)
    colors <- rainbow(nDepart)
    linetype <- 1:nDepart
    plotchar <- seq(18,18+nDepart,1)

    i <- 1
    for (depart in departs) {
        lines(years, datak[,depart], type="b", lwd=1.5,
            lty=linetype[i], col=colors[i], pch=plotchar[i])
        i + 1 -> i
    }

    title(paste(name,sep=""))

    # add a legend
    legend("topleft", departs, departs, cex=0.8, col=colors,
        pch=plotchar, lty=linetype, title="Département")

    dev.off()
}

print_global(global,"Effectifs généraux")
print_by_depart(effectif_depart,"Effectif département")
print_by_depart(meilleur_classement_depart,"Meilleur classement département")
print_by_depart(pire_classement_depart,"Pire classement département")
print_by_depart(moyenne_classement_depart,"Moyenne classement département")
print_by_depart(mediane_classement_depart,"Mediane classement département")
#print_by_depart(ecart_type_classement_depart,"Ecart-type classement département")
print_by_depart(pc_filles_depart,"Part filles département")
print_by_depart(pc_redoublants_depart,"Part redoublants département")

cat(paste("Done.\n"))
