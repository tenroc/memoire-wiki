#### Mesures de base sur les réseaux: ####

page <- names(ledits_graphe)
page_attributes <- as.data.frame(page)

a <- character()
b <- character()
c <- character()
d <- character()


for (i in page_attributes$page){
  a[i] <- edge_density(ledits_graphe[[i]])
  b[i] <- vcount(ledits_graphe[[i]])
  temp <- append(as.character(ledits_edgelists[[i]][,"V1"]),as.character(ledits_edgelists[[i]][,"V2"]))
  c[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes$contributeurs[attributes$isanon == "1"]])
  d[i] <- ecount(ledits_graphe[[i]])
}

page_attributes$density <- a
page_attributes$ncontributors <- b
page_attributes$nanon <- c
page_attributes$nedits <- d