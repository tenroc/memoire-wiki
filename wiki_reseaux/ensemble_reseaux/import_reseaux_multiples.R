#### Automatisation de la réccupération des donées contrib sur l'ensemble des réseaux: ####

# Test 2: toujours featured/edits, mais autre import:

list_edits <- list.files(pattern="*.csv")

ledits <- list()

for (i in list_edits){
  ledits[[paste(i,"edits",sep="_")]] <- read.csv2(i)
}

temp <- character()
for (i in names(ledits)){
  if (is.na(ledits[[i]][1,1]) == T){
    temp[i] <- paste(i,"edits",sep="_")
    ledits[[i]] <- NULL
  }
}

temp <- levels(as.factor(temp))

## retrieve original page name

temp <- gsub( ".csv.*$", "", temp )
write(temp, file="../temp.txt", sep="/n")

# Récuppérer les réseaux de talk


list_talk <- list.files(pattern="*.csv")

ltalks <- list()

for (i in list_talk){
  ltalks[[paste(i,"talk",sep="_")]] <- read.csv2(i)
}

# Réccupérer tous les contributeurs dans une grande liste? (pas forcément une bonne idée)
contributeurs <- character()
isanon <- character()
for (i in names(ledits)){
  contributeurs <- append(contributeurs,as.character(ledits[[i]][,"ActiveUser"]))
  isanon <- append(isanon,as.character(ledits[[i]][,"IsAnonymous"]))
}

for (i in names(ledits)){
  contributeurs <- append(contributeurs,as.character(ltalks[[i]][,"ActiveUser"]))
  isanon <- append(isanon,as.character(ltalks[[i]][,"IsAnonymous"]))
}

attributes <- data.frame(contributeurs,isanon)
attributes <- unique(attributes)

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


write.csv2(attributes, file="attributes.csv", fileEncoding = "UTF8")

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
