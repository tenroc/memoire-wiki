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

# isanon <- ifelse(grepl("[0-9+]\\.[0-9+]\\.[0-9+].*", contributeurs) ==T | grepl("[0-9+]\\:[0-9+]\\:[0-9+].*", contributeurs) == T, 1, 0)

contributeurs_talk <- character()

for (i in names(ltalks)){
  contributeurs_talk <- append(contributeurs_talk,as.character(ltalks[[i]][,"ActiveUser"]))
}

isanon_talk <- ifelse(grepl("^*\\d+\\.\\d+\\.\\d+", contributeurs_talk) ==T | grepl("^*.*\\:.*\\:.*\\:.*\\:", contributeurs_talk) == T, 1, 0)

contributeurs <- append(contributeurs, contributeurs_talk)
isanon <- append(isanon, isanon_talk)

attributes_page <- data.frame(contributeurs,isanon)
attributes_page <- unique(attributes_page)

# tests

a <-c("Bugboy52.40", "trala67.89","seim","eiufhuze","168.365.13","222:333:444:5","64.3oulala","2607:F470:6:5002:A1FC:502:7F3D:5655")

grepl("^*\\d+\\.\\d+\\.\\d+", a)

grepl("^*.*\\:.*\\:.*\\:.*\\:",a)

grepl("^*.*\\:.*\\:.*\\:.*\\:",a)

attributes_page[grepl("^*.*\\:.*\\:.*\\:.*\\:",attributes_page$contributeurs) == T & attributes_page$isanon == 0,"contributeurs"]

grepl("*[a-z]",a)


attributes_page[as.character(attributes_page$contributeurs) %in% as.character(ledits[[i]][j,"ActiveUser"]),
                "contributeurs"]

attributes_page[attributes_page$contributeurs == "Bugboy52.40",]

# Recodages anonymes

attributes_page$status_contrib[attributes_page$isanon == 0] <- "inscrit"
attributes_page$status_contrib[attributes_page$isanon == 1] <- "anonyme"

# Remplacer les " " par des "_" dans les noms de contributeurs (sinon bug XML)

attributes_page$contributeurs <- gsub(" ","_",attributes_page$contributeurs)

# Récuppérer les attributs de la meta_contributor list

for (i in 1:length(attributes_page$contributeurs)){
  tryCatch({
    if(attributes_page[i,"status_contrib"] != "anonyme"){
      temp <- userInfo(attributes_page[i,"contributeurs"],"en")
      attributes_page[i,"total_rev_count"] <- paste(temp[[5]][1,"editcount"])
      attributes_page[i, "registration_year"] <- substring(temp[[5]][1,"registration"],1,4)
      if ("sysop" %in% temp[[3]] == T | as.character(attributes_page[i,"contributeurs"]) %in% admin_list_definitive){
        attributes_page[i,"status_contrib"] <- "admin"
      } else {
        if ("bot" %in% temp[[3]] == T){
          attributes_page[i,"status_contrib"] <- "bot"
        }
      }
    } else {
      temp <- contrib_list(attributes_page[i,"contributeurs"], domain ="en")
      attributes_page[i,"total_rev_count"] <- length(temp[,"V1"])
      attributes_page[i,"registration_year"] <- as.character(min(as.numeric(substring(temp$V2,1,4))))
    }
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


write.csv2(attributes_page, file="../nodes_attributes_page_network.csv", fileEncoding = "UTF8")

## Rattacher les attributs des noeuds a la base originale:

# edits:

for(i in names(ledits)){
  ledits[[i]][,"ActiveUser"] <- gsub(" ","_",ledits[[i]][,"ActiveUser"])
  ledits[[i]][,"TargetAuthor"] <- gsub(" ","_",ledits[[i]][,"TargetAuthor"])
  ledits[[i]][,"UndoTarget"] <- gsub(" ","_",ledits[[i]][,"UndoTarget"])
  ledits[[i]][,"RedoTarget"] <- gsub(" ","_",ledits[[i]][,"RedoTarget"])
}

View(ledits[[2]])

for(i in names(ledits)){
  for(j in 1:nrow(ledits[[i]])){
  ledits[[i]][j,"status_contrib"] <-  attributes_page[as.character(attributes_page$contributeurs) %in% as.character(ledits[[i]][j,"ActiveUser"]),
                                                  "status_contrib"]
  ledits[[i]][j,"total_rev_count"] <-  attributes_page[as.character(attributes_page$contributeurs) == as.character(ledits[[i]][j,"ActiveUser"]),
                                                  "total_rev_count"]
  ledits[[i]][j,"registration_year"] <-  attributes_page[as.character(attributes_page$contributeurs) == as.character(ledits[[i]][j,"ActiveUser"]),
                                                  "registration_year"]
  }
}


# talk:

for(i in names(ltalkss)){
  ltalks[[i]][,"ActiveUser"] <- gsub(" ","_",ltalks[[i]][,"ActiveUser"])
  ltalks[[i]][,"TargetAuthor"] <- gsub(" ","_",ltalks[[i]][,"TargetAuthor"])
  ltalks[[i]][,"UndoTarget"] <- gsub(" ","_",ltalks[[i]][,"UndoTarget"])
  ltalks[[i]][,"RedoTarget"] <- gsub(" ","_",ltalks[[i]][,"RedoTarget"])
}

View(ltalks[[2]])

for(i in names(ltalks)){
  for(j in 1:nrow(ltalks[[i]])){
    ltalks[[i]][j,"status_contrib"] <-  attributes_page[as.character(attributes_page$contributeurs) %in% as.character(ltalks[[i]][j,"ActiveUser"]),
                                                        "status_contrib"]
    ltalks[[i]][j,"total_rev_count"] <-  attributes_page[as.character(attributes_page$contributeurs) == as.character(ltalks[[i]][j,"ActiveUser"]),
                                                         "total_rev_count"]
    ltalks[[i]][j,"registration_year"] <-  attributes_page[as.character(attributes_page$contributeurs) == as.character(ltalks[[i]][j,"ActiveUser"]),
                                                           "registration_year"]
  }
}

#### Pour chaque élément de la liste des réseaux, obtenir un graphe ####

# Creer les edgelists

ledits_edgelists <- list()

for (i in names(ledits)){
  ledits_edgelists[[paste(i,"_edgelist",sep="")]] <- subset(ledits[[i]], select= c(ActiveUser, TargetAuthor, InteractionType, WordCount))
  colnames(ledits_edgelists[[paste(i,"_edgelist",sep="")]]) <- c("V1","V2", "InteractionType","Wordcount")
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

#### constitution de la base: page_attributes_edit ####

page <- names(ledits_graphe)
page_attributes_edit <- as.data.frame(page)

a <- character()
b <- character()
c <- character()
d <- character()
e <- character()
f <- character()
g <- character()


for (i in page_attributes_edit$page){
  a[i] <- edge_density(ledits_graphe[[i]])
  b[i] <- vcount(ledits_graphe[[i]])
  temp <- append(as.character(ledits_edgelists[[i]][,"V1"]),as.character(ledits_edgelists[[i]][,"V2"]))
  c[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$isanon == "1"]])
  d[i] <- ecount(ledits_graphe[[i]])
  e[i] <- transitivity(ledits_graphe[[i]])
}

page_attributes_edit$density <- a
page_attributes_edit$ncontributors <- b
page_attributes_edit$nanon <- c
page_attributes_edit$nedits <- d
page_attributes_edit$transitivity <- e

# Is the page controversial or featured?

temp <- gsub( ".csv.*$", "", page_attributes_edit$page)

for (i in page_attributes_edit$page){
  if (as.character(page_attributes_edit$page[i]) %in% controversy_data){
    f[i] <- 1
  } else {
    f[i] <- 0
  }
}

for (i in page_attributes_edit$page){
  if (as.character(page_attributes_edit$page[i]) %in% featured_data){
    g[i] <- 1
  } else {
    g[i] <- 0
  }
}

page_attributes_edit$iscontroversial <- f
page_attributes_edit$isfeatured <- g
