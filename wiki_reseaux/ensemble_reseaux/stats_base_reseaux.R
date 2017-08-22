#### Mesures de base sur les réseaux: ####

page <- names(ledits_graphe)
page_attributes <- as.data.frame(page)

a <- character()
b <- character()
c <- character()
d <- character()
e <- character()
f <- character()
g <- character()


for (i in page_attributes$page){
  a[i] <- edge_density(ledits_graphe[[i]])
  b[i] <- vcount(ledits_graphe[[i]])
  temp <- append(as.character(ledits_edgelists[[i]][,"V1"]),as.character(ledits_edgelists[[i]][,"V2"]))
  c[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes$contributeurs[attributes$isanon == "1"]])
  d[i] <- ecount(ledits_graphe[[i]])
  e[i] <- transitivity(ledits_graphe[[i]])
}

page_attributes$density <- a
page_attributes$ncontributors <- b
page_attributes$nanon <- c
page_attributes$nedits <- d
page_attributes$transitivity <- e

# Is the page controversial or featured?

temp <- gsub( ".csv.*$", "", page_attributes$page)

for (i in page_attributes$page){
  if (as.character(page_attributes$page[i]) %in% controversy_data){
    f[i] <- 1
  } else {
    f[i] <- 0
  }
}

for (i in page_attributes$page){
  if (as.character(page_attributes$page[i]) %in% featured_data){
    g[i] <- 1
  } else {
    g[i] <- 0
  }
}

page_attributes$iscontroversial <- f
page_attributes$isfeatured <- g