#### Automatisation de la réccupération des donées contrib sur l'ensemble des réseaux: ####

setwd('~/Documents/EHESS/SocStat-M2/memoire/networks/pages/edits/')

# Test 1 sur les réseaux d'édition.

list_edits <- lapply(Sys.glob("*.csv"), read.csv2)

# Test 2: toujours featured/edits, mais autre import:

list_edits <- list.files(pattern="*.csv")

ledits <- list()

for (i in list_edits){
  ledits[[paste(i,"edits",sep="_")]] <- read.csv2(i)
}

View(ledits[[1]])

# Récuppérer les réseaux de talk

setwd('~/Documents/EHESS/SocStat-M2/memoire/networks/pages/talk/')

list_talk <- list.files(pattern="*.csv")

ltalks <- list()

for (i in list_talk){
  ltalk[[paste(i,"talk",sep="_")]] <- read.csv2(i)
}

# Réccupérer tous les contributeurs dans une grande liste? (pas forcément une bonne idée)
contributeurs <- character()
isanon <- character()
for (i in names(ledits)){
  contributeurs <- append(contributeurs,as.character(ledits[[i]][,"ActiveUser"]))
  isanon <- append(isanon,as.character(ledits[[i]][,"IsAnonymous"]))
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
      if ("sysop" %in% temp[[3]] == T){
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