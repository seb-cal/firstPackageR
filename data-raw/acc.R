library(sqldf)

caracteristics=read.csv(file = "data-raw/caracteristics.csv")
places=read.csv("data-raw/places.csv")

# ajout d'un "0" pour les années à un seul chiffre
caracteristics$an=sapply(caracteristics$an, function(x) {
  if (x<10) {
    return (paste("0",x,sep = ""))
  } else {
    return(x)
  }
})

# Creation d'une nouvelle colonne en format date
caracteristics$date=as.Date(paste("20",caracteristics$an,"-",caracteristics$mois,"-",caracteristics$jour,sep=""))

# Drop les anciennes colonnes codant la date
caracteristics=caracteristics[,c(-2,-3,-4)]

q1 = "Select *  FROM caracteristics inner JOIN places ON caracteristics.Num_Acc = places.Num_Acc;"
acc=sqldf(q1)
acc=acc[,-15]

usethis::use_data(acc,overwrite = TRUE)
