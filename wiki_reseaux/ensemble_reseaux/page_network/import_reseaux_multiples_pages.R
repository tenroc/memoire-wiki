#### Automatisation de la réccupération des donées contrib sur l'ensemble des réseaux: ####

# Test 2: toujours featured/edits, mais autre import:

list_edits <- list.files(pattern="*.csv")

ledits <- list()

for (i in list_edits){
  ledits[[paste(i,"edits",sep="_")]] <- read.csv2(i, fileEncoding = "UTF8")
}

# Quels réseaux sont vides ou ne présentent qu'une seule édition?

temp <- character()
for (i in names(ledits)){
  if (is.na(ledits[[i]][2,1]) == T){
    temp[i] <- paste(i,"edits",sep="_")
    ledits[[i]] <- NULL
  }
}

temp <- levels(as.factor(temp))

## retrieve original page name

temp <- gsub( ".csv.*$", "", temp )
write(temp, file="../temp_edit.txt", sep="/n")

# Récuppérer les réseaux de talk


list_talk <- list.files(pattern="*.csv")

ltalks <- list()

for (i in list_talk){
  ltalks[[paste(i,"talk",sep="_")]] <- read.csv2(i, fileEncoding = "UTF8")
}

# Quels réseaux sont nuls?

temp <- character()
for (i in names(ltalks)){
  if (is.na(ltalks[[i]][1,1]) == T){
    temp[i] <- paste(i,"talk",sep="_")
    ltalks[[i]] <- NULL
  }
}

temp <- levels(as.factor(temp))

temp <- gsub( ".csv.*$", "", temp )
write(temp, file="../temp_talk.txt", sep="/n")

# Réccupérer tous les contributeurs dans une grande liste? (pas forcément une bonne idée)
contributeurs <- character()
isanon <- character()

for (i in names(ledits)){
  contributeurs <- append(contributeurs,as.character(ledits[[i]][,"ActiveUser"]))
  isanon <- append(isanon,as.character(ledits[[i]][,"IsAnonymous"]))
}

contributeurs_talk <- character()

for (i in names(ltalks)){
  contributeurs_talk <- append(contributeurs_talk,as.character(ltalks[[i]][,"ActiveUser"]))
}

isanon_talk <- ifelse(grepl("[0-9+]\\.[0-9+].*", contributeurs_talk) ==T, 1, 0)

contributeurs <- append(contributeurs, contributeurs_talk)
isanon <- append(isanon, isanon_talk)

attributes <- data.frame(contributeurs,isanon)
attributes <- unique(attributes)

# temp

attributes <- data.frame(contributeurs_talk,isanon_talk)
attributes <- unique(attributes)
colnames(attributes) <- c("contributeurs","isanon")

attributes2 <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/attributes.csv")

attributes <- merge(attributes, attributes2, by="contributeurs", all=T)
attributes[,2] <- cbind(attributes$isanon.x,attributes$isanon.y)
attributes <- attributes[,c(1,2,5:7)]
colnames(attributes) <- c("contributeurs","isanon","status_contrib","total_rev_count","registration_year")

attributes <- attributes[is.na(attributes$status_contrib) == T,]

attributes <- attributes[-1,c(-3,-4,-5)]

attributes$isanon <- attributes$isanon[,-2]

# Recodages anonymes

attributes$status_contrib[attributes$isanon == 0] <- "inscrit"
attributes$status_contrib[attributes$isanon == 1] <- "anonyme"

# Remplacer les " " par des "_" dans les noms de contributeurs (sinon bug XML)

attributes$contributeurs <- gsub(" ","_",attributes$contributeurs)

# Récuppérer les attributs de la meta_contributor list

for (i in 1:length(attributes$contributeurs)){
  tryCatch({
    if(attributes[i,"status_contrib"] != "anonyme"){
      temp <- userInfo(attributes[i,"contributeurs"],"en")
      attributes[i,"total_rev_count"] <- paste(temp[[5]][1,"editcount"])
      attributes[i, "registration_year"] <- substring(temp[[5]][1,"registration"],1,4)
      if ("sysop" %in% temp[[3]] == T | as.character(attributes[i,"contributeurs"]) %in% admin_list_definitive){
        attributes[i,"status_contrib"] <- "admin"
      } else {
        if ("bot" %in% temp[[3]] == T){
          attributes[i,"status_contrib"] <- "bot"
        }
      }
    } else {
      temp <- contrib_list(attributes[i,"contributeurs"], domain ="en")
      attributes[i,"total_rev_count"] <- length(temp[,"V1"])
      attributes[i,"registration_year"] <- as.character(min(as.numeric(substring(temp$V2,1,4))))
    }
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


write.csv2(attributes, file="nodes_attributes_page_network.csv", fileEncoding = "UTF8")

## Rattacher les attributs des noeuds a la base originale:

contributeurs <- unique(contributeurs)

attributes$contributeurs <- contributeurs

for(i in names(ledits)){
  for(j in 1:nrow(ledits[[i]])){
  ledits[[i]][j,"status_contrib"] <-  attributes2[as.character(attributes2$contributeurs) == as.character(ledits[[i]][j,"ActiveUser"]),
                                                  "status_contrib"]
  ledits[[i]][j,"total_rev_count"] <-  attributes2[as.character(attributes2$contributeurs) == as.character(ledits[[i]][j,"ActiveUser"]),
                                                  "total_rev_count"]
  ledits[[i]][j,"registration_year"] <-  attributes2[as.character(attributes2$contributeurs) == as.character(ledits[[i]][j,"ActiveUser"]),
                                                  "registration_year"]
  }
}

#### Pour chaque élément de la liste des réseaux, obtenir un graphe ####

# Creer les edgelists

ledits_edgelists <- list()

for (i in names(ledits)){
  ledits_edgelists[[paste(i,"_edgelist",sep="")]] <- subset(ledits[[i]], select= c(ActiveUser, TargetAuthor, InteractionType, WordCount, status_contrib, total_rev_count, registration_year))
  colnames(ledits_edgelists[[paste(i,"_edgelist",sep="")]]) <- c("V1","V2", "InteractionType","Wordcount", "status_contrib", "total_rev_count", "registration_year")
  ledits_edgelists[[paste(i,"_edgelist",sep="")]][,"InteractionType_num"] <- as.character(ledits_edgelists[[paste(i,"_edgelist",sep="")]][,"InteractionType"])
  ledits_edgelists[[paste(i,"_edgelist",sep="")]][ledits_edgelists[[paste(i,"_edgelist",sep="")]][,"InteractionType"] == "ADDED"
                                                    ,"InteractionType_num"] <- 1
  ledits_edgelists[[paste(i,"_edgelist",sep="")]][ledits_edgelists[[paste(i,"_edgelist",sep="")]][,"InteractionType"] == "DELETED"
                                                  ,"InteractionType_num"] <- 2
  ledits_edgelists[[paste(i,"_edgelist",sep="")]][ledits_edgelists[[paste(i,"_edgelist",sep="")]][,"InteractionType"] == "RESTORED"
                                                  ,"InteractionType_num"] <- 3
}


# Puis les objets graphe

ledits_graphe <- list()

for (i in names(ledits_edgelists)){
  ledits_edgelists[[i]][,"V2"] <- as.character(ledits_edgelists[[i]][,"V2"])
  ledits_edgelists[[i]][ledits_edgelists[[i]][,"V2"] == "" | is.na(ledits_edgelists[[i]][,"V2"]) == T,"V2"] <- "page"
  ledits_graphe[[i]] <- graph.data.frame(ledits_edgelists[[i]], directed = T)
}

#### constitution de la base: page_attributes ####

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
