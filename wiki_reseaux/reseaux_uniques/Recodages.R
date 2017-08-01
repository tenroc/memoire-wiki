## Script Test (import, recodage): à automatiser (+ modulariser)

#### Import des donnees d'edit, recodages et transformation en edgelist ####

cultwar_edit <- read.csv2('Culture_war.csv', fileEncoding = 'UTF-8')
str(cultwar_edit)


cultwar_edit_edgelist <- subset(cultwar_edit, select= c(ActiveUser, TargetAuthor, InteractionType, WordCount, IsAnonymous))

for (i in colnames(cultwar_edit_edgelist)){
  cultwar_edit_edgelist[,i] <- as.character(cultwar_edit_edgelist[,i])
}

# Normalisation (0 - 1) du WordCount? Pas forcément utile:

range01 <- function(x){(x - min(x))/(max(x)-min(x))}

cultwar_edit_edgelist$WordCount_norm <- range01(as.numeric(cultwar_edit_edgelist$WordCount))

# Recodage d'Interaction Type en numerique:

table(cultwar_edit_edgelist$InteractionType)
cultwar_edit_edgelist$InteractionType_num <- cultwar_edit_edgelist$InteractionType
cultwar_edit_edgelist$InteractionType_num[cultwar_edit_edgelist$InteractionType_num == "ADDED"] <- 1
cultwar_edit_edgelist$InteractionType_num[cultwar_edit_edgelist$InteractionType_num == "DELETED"] <- 2
cultwar_edit_edgelist$InteractionType_num[cultwar_edit_edgelist$InteractionType_num == "RESTORED"] <- 3
table(cultwar_edit_edgelist$InteractionType_num)


# Statut du contributeur:

contributeurs <- c(levels(cultwar_edit$ActiveUser),"cultwar_page")
status_anon <- 1:338
attributes <- data.frame(contributeurs, status_anon)

for (i in colnames(attributes)){
  attributes[,i] <- as.character(attributes[,i])
}


# Creation d'une variable anon status:

test <-cultwar_edit$ActiveUser[cultwar_edit$IsAnonymous == 1]

attributes$status_anon <- ifelse(attributes$contributeurs %in% test, 1, 0)

# Creation de l'objet graphe

cultwar_edit_edgelist$TargetAuthor[cultwar_edit_edgelist$TargetAuthor == ""] <- "cultwar_page"

colnames(cultwar_edit_edgelist) <- c("V1","V2", "InteractionType","Wordcount", "IsAnonymous", "WordCount_normalized", "InteractionType_num")

cultwar_edit_graphe <- graph.data.frame(cultwar_edit_edgelist, directed=T)
str(cultwar_edit_graphe)

# Remettre les attributes dans l'ordre du graphe:

temp <- labels(V(cultwar_edit_graphe))

temp <- temp[is.na(temp) == F]

attributes <- attributes[match(temp, attributes$contributeurs),]

# attribution d'ID numériques

attributes$id <- c(1:length(attributes$contributeurs))
attributes <- attributes[,c(3,1,2)]

# Recodage du status

attributes$status_contrib[attributes$status_anon == 0] <- "inscrit"
attributes$status_contrib[attributes$status_anon == 1] <- "anonyme"
attributes$status_contrib[attributes$contributeurs == "cultwar_page"] <- "page_edits"

# Remplacer les " " par des "_" dans les noms de contributeurs (sinon bug XML)

attributes$contributeurs <- gsub(" ","_",attributes$contributeurs)

# Reccuperer, et ajouter aux attributs des contributeurs (pour les contributeurs inscrits): le nombre total de revisions effectuees, 
#le statut (admin/?) et la date d'inscription sur le site. (saute les anonymes, et les comptes supprimés)
# A ajouter: bureaucrats, admin comittee.
# Recupere pour les anonymes l'annee de la premiere contribution recensee

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
