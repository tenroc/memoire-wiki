#### Automatisation de la réccupération des donées contrib sur l'ensemble des réseaux: ####

# Import featured/edits:

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

# Supprimer les erreurs d'édition (ActiveUser == "")

for(i in names(ledits)){
  ledits[[i]] <- ledits[[i]][!ledits[[i]][,"ActiveUser"]=="",]
  ledits[[i]] <- ledits[[i]][!ledits[[i]][,"TargetAuthor"]=="",]
  ledits[[i]] <- ledits[[i]][is.na(ledits[[i]][,"ActiveUser"])==F,]
  ledits[[i]] <- ledits[[i]][is.na(ledits[[i]][,"TargetAuthor"])==F,]
}

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

# Supprimer les erreurs d'édition (ActiveUser == "")

for(i in names(ltalks)){
  ltalks[[i]] <- ltalks[[i]][!ltalks[[i]][,"ActiveUser"]=="",]
  ltalks[[i]] <- ltalks[[i]][!ltalks[[i]][,"Target"]=="",]
  ltalks[[i]] <- ltalks[[i]][is.na(ltalks[[i]][,"ActiveUser"])==F,]
  ltalks[[i]] <- ltalks[[i]][is.na(ltalks[[i]][,"Target"])==F,]
}

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

isanon_talk <- ifelse(grepl("^*\\d+\\.\\d+\\.\\d+\\.\\d+", contributeurs_talk) ==T | grepl("^*.*\\:.*\\:.*\\:.*\\:", contributeurs_talk) == T, 1, 0)

contributeurs <- append(contributeurs, contributeurs_talk)
isanon <- append(isanon, isanon_talk)

attributes_featured <- data.frame(contributeurs,isanon)
attributes_featured <- unique(attributes_featured)

# Recodages anonymes

attributes_featured$status_contrib[attributes_featured$isanon == 0] <- "inscrit"
attributes_featured$status_contrib[attributes_featured$isanon == 1] <- "anonyme"

# Remplacer les " " par des "_" dans les noms de contributeurs (sinon bug XML)

attributes_featured$contributeurs <- gsub(" ","_",attributes_featured$contributeurs)

# Récuppérer les attributs de la meta_contributor list

for (i in 1:length(attributes_featured$contributeurs)){
  tryCatch({
    if(attributes_featured[i,"status_contrib"] != "anonyme"){
      temp <- userInfo(attributes_featured[i,"contributeurs"],"en")
      attributes_featured[i,"total_rev_count"] <- paste(temp[[5]][1,"editcount"])
      attributes_featured[i, "registration_year"] <- substring(temp[[5]][1,"registration"],1,4)
      if ("sysop" %in% temp[[3]] == T | as.character(attributes_featured[i,"contributeurs"]) %in% admin_list_definitive){
        attributes_featured[i,"status_contrib"] <- "admin"
      } else {
        if ("bot" %in% temp[[3]] == T){
          attributes_featured[i,"status_contrib"] <- "bot"
        }
      }
    } else {
      temp <- contrib_list(attributes_featured[i,"contributeurs"], domain ="en")
      attributes_featured[i,"total_rev_count"] <- length(temp[,"V1"])
      attributes_featured[i,"registration_year"] <- as.character(min(as.numeric(substring(temp$V2,1,4))))
    }
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

attributes_featured$contributeurs <- as.character(attributes_featured$contributeurs)
attributes_featured$status_contrib <- as.character(attributes_featured$status_contrib)
attributes_featured[attributes_featured$contributeurs == "", "status_contrib"] <- "page"
attributes_featured[attributes_featured$contributeurs == "", "contributeurs"] <- "page"

write.csv2(attributes_featured, file="../nodes_attributes_featured_network.csv", fileEncoding = "UTF8")

## Rattacher les attributs des noeuds a la base originale:

# edits:

for(i in names(ledits)){
  ledits[[i]][,"ActiveUser"] <- gsub(" ","_",ledits[[i]][,"ActiveUser"])
  ledits[[i]][,"TargetAuthor"] <- gsub(" ","_",ledits[[i]][,"TargetAuthor"])
  ledits[[i]][,"UndoTarget"] <- gsub(" ","_",ledits[[i]][,"UndoTarget"])
  ledits[[i]][,"RedoTarget"] <- gsub(" ","_",ledits[[i]][,"RedoTarget"])
}

## Pas forcément trés utile, mieux vaut indexer directement sur la base d'attributs

#for(i in names(ledits)){
#  for(j in 1:nrow(ledits[[i]])){
#    ledits[[i]][j,"status_contrib"] <-  attributes_featured[as.character(attributes_featured$contributeurs) %in% as.character(ledits[[i]][j,"ActiveUser"]),
#                                                          "status_contrib"]
#    ledits[[i]][j,"total_rev_count"] <-  attributes_featured[as.character(attributes_featured$contributeurs) == as.character(ledits[[i]][j,"ActiveUser"]),
#                                                           "total_rev_count"]
#    ledits[[i]][j,"registration_year"] <-  attributes_featured[as.character(attributes_featured$contributeurs) == as.character(ledits[[i]][j,"ActiveUser"]),
#                                                             "registration_year"]
#  }
#}


# talk:

for(i in names(ltalks)){
  ltalks[[i]][,"ActiveUser"] <- gsub(" ","_",ltalks[[i]][,"ActiveUser"])
  ltalks[[i]][,"TargetAuthor"] <- gsub(" ","_",ltalks[[i]][,"Target"])
}

## Pas forcément trés utile, mieux vaut indexer directement sur la base d'attributs

#for(i in names(ltalks)){
#  for(j in 1:nrow(ltalks[[i]])){
#    ltalks[[i]][j,"status_contrib"] <-  attributes_featured[as.character(attributes_featured$contributeurs) %in% as.character(ltalks[[i]][j,"ActiveUser"]),
#                                                          "status_contrib"]
#    ltalks[[i]][j,"total_rev_count"] <-  attributes_featured[as.character(attributes_featured$contributeurs) == as.character(ltalks[[i]][j,"ActiveUser"]),
#                                                           "total_rev_count"]
#    ltalks[[i]][j,"registration_year"] <-  attributes_featured[as.character(attributes_featured$contributeurs) == as.character(ltalks[[i]][j,"ActiveUser"]),
#                                                             "registration_year"]
#  }
#}

#### Pour chaque élément de la liste des réseaux, obtenir un graphe ####

## edits:

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


## talk:

View(ltalks[[2]])

ltalks_edgelists <- list()

for (i in names(ltalks)){
  ltalks_edgelists[[paste(i,"_edgelist",sep="")]] <- subset(ltalks[[i]], select= c(ActiveUser, Target, DiscussionType,
                                                                                   CharacterCount, ThreadHeadline,
                                                                                   IndexInThread, PreceedingAuthors...))
  colnames(ltalks_edgelists[[paste(i,"_edgelist",sep="")]]) <- c("V1","V2", "DiscussionType","CharacterCount", "ThreadHeadline",
                                                                 "IndexInThread","PreceedingAuthors")
  ltalks_edgelists[[paste(i,"_edgelist",sep="")]][,"DiscussionType_num"] <- as.character(ltalks_edgelists[[paste(i,"_edgelist",sep="")]][,"DiscussionType"])
  ltalks_edgelists[[paste(i,"_edgelist",sep="")]][ltalks_edgelists[[paste(i,"_edgelist",sep="")]][,"DiscussionType"] == "replied_to"
                                                  ,"DiscussionType_num"] <- 1
  ltalks_edgelists[[paste(i,"_edgelist",sep="")]][ltalks_edgelists[[paste(i,"_edgelist",sep="")]][,"DiscussionType"] == "initialized_thread"
                                                  ,"DiscussionType_num"] <- 2
}


# Puis les objets graphe

ltalks_graphe <- list()

for (i in names(ltalks_edgelists)){
  ltalks_edgelists[[i]][,"V2"] <- as.character(ltalks_edgelists[[i]][,"V2"])
  ltalks_edgelists[[i]][ltalks_edgelists[[i]][,"V2"] == "" | is.na(ltalks_edgelists[[i]][,"V2"]) == T,"V2"] <- "page"
  ltalks_graphe[[i]] <- graph.data.frame(ltalks_edgelists[[i]], directed = T)
}

#### constitution de la base: featured_attributes ####

## Edits

page <- names(ledits_graphe)
featured_attributes_edits <- as.data.frame(page)

a <- character()
b <- character()
c <- character()
d <- character()
e <- character()
f <- character()
g <- character()
h <- character()
l <- character()
m <- character()
n <- character()
o <- character()
p <- character()
q <- character()
r <- character()
s <- character()
t <- character()
u <- character()
v <- character()
w <- character()
a2 <- character()

attributes_featured$total_rev_count[is.na(attributes_featured$total_rev_count) == T] <- 1
quart <- quantile(attributes_featured$total_rev_count[attributes_featured$status_contrib != "page" & attributes_featured$status_contrib != "structure_page"], seq(0,1,0.25))

attributes_featured$rev_count_quar[attributes_featured$total_rev_count <= as.numeric(quart[2])] <- 1
attributes_featured$rev_count_quar[attributes_featured$total_rev_count > as.numeric(quart[2]) & attributes_featured$total_rev_count <= as.numeric(quart[3])] <- 2
attributes_featured$rev_count_quar[attributes_featured$total_rev_count > as.numeric(quart[3]) & attributes_featured$total_rev_count <= as.numeric(quart[4])] <- 3
attributes_featured$rev_count_quar[attributes_featured$total_rev_count > as.numeric(quart[4])] <- 4
table(attributes_featured$rev_count_quar)

b2 <- character()
c2 <- character()
d2 <- character()
e2 <- character()
f2 <- character()
g2 <- character()
h2 <- character()
k2 <- character()
l2 <- character()
m2 <- character()
n2 <- character()
o2 <- character()
p2 <- character()
q2 <- character()
r2 <- character()
s2 <- character()
t2 <- character()
u2 <- character()
v2 <- character()
w2 <- character()
a3 <- character()
b3 <- character()
c3 <- character()
d3 <- character()
e3 <- character()
f3 <- character()
g3 <- character()

f22 <- character()
g22 <- character()
h22 <- character()
k22 <- character()
l22 <- character()
m22 <- character()
n22 <- character()
o22 <- character()
p22 <- character()
q22 <- character()
r22 <- character()
s22 <- character()
t22 <- character()
u22 <- character()
v22 <- character()
w22 <- character()
a32 <- character()
b32 <- character()
c32 <- character()
d32 <- character()


for (i in featured_attributes_edits$page){
  a[i] <- edge_density(ledits_graphe[[i]])
  b[i] <- vcount(ledits_graphe[[i]])
  temp <- append(as.character(ledits_edgelists[[i]][,"V1"]),as.character(ledits_edgelists[[i]][,"V2"]))
  c[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$isanon == "1"]])
  d[i] <- ecount(ledits_graphe[[i]])
  e[i] <- transitivity(ledits_graphe[[i]])
  h[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  l[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "bot"]])
  m[i] <- centralization.degree(ledits_graphe[[i]])[[2]]
  n[i] <- mean(betweenness(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  o[i] <- mean(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  p[i] <- mean(betweenness(ledits_graphe[[i]])[labels(degree(ledits_graphe[[i]])) != "page"])
  q[i] <- mean(degree(ledits_graphe[[i]])[labels(degree(ledits_graphe[[i]])) != "page"])
  r[i] <- sd(betweenness(ledits_graphe[[i]])[labels(degree(ledits_graphe[[i]])) != "page"])
  s[i] <- sd(degree(ledits_graphe[[i]])[labels(degree(ledits_graphe[[i]])) != "page"])
  t[i] <- sd(betweenness(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  u[i] <- sd(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  v[i] <- assortativity.degree(ledits_graphe[[i]])
  #w[i] <- assortativity(ledits_graphe[[i]], as.factor(ledits_graphe[[i]][,"status_contrib"]))
  #a2[i] <- assortativity(ledits_graphe[[i]], as.factor(ledits_graphe[[i]][,"rev_count_quar"]))
  b2[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  c2[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  d2[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  e2[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  f2[i] <- mean(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  g2[i] <- mean(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  h2[i] <- mean(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  k2[i] <- mean(degree(ledits_graphe[[i]],mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  l2[i] <- mean(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  m2[i] <- mean(degree(ledits_graphe[[i]],mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  n2[i] <- mean(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  o2[i] <- mean(degree(ledits_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  p2[i] <- mean(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  q2[i] <- mean(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  r2[i] <- mean(degree(ledits_graphe[[i]],mode= "out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  s2[i] <- mean(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  t2[i] <- mean(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  u2[i] <- mean(degree(ledits_graphe[[i]], mode = "out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  v2[i] <- mean(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  w2[i] <- mean(degree(ledits_graphe[[i]],mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  a3[i] <- mean(degree(ledits_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  b3[i] <- mean(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  c3[i] <- mean(degree(ledits_graphe[[i]], mode = "in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  d3[i] <- mean(degree(ledits_graphe[[i]],mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  e3[i] <- length(ledits_edgelists[[i]][ledits_edgelists[[i]][,"InteractionType"] == "ADDED","V1"])
  f3[i] <- length(ledits_edgelists[[i]][ledits_edgelists[[i]][,"InteractionType"] == "DELETED","V1"])
  g3[i] <- length(ledits_edgelists[[i]][ledits_edgelists[[i]][,"InteractionType"] == "RESTORED","V1"])
  f22[i] <- sd(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  g22[i] <- sd(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  h22[i] <- sd(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  k22[i] <- sd(degree(ledits_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  l22[i] <- sd(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  m22[i] <- sd(degree(ledits_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  n22[i] <- sd(degree(ledits_graphe[[i]],mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  o22[i] <- sd(degree(ledits_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  p22[i] <- sd(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  q22[i] <- sd(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  r22[i] <- sd(degree(ledits_graphe[[i]], mode="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  s22[i] <- sd(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  t22[i] <- sd(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  u22[i] <- sd(degree(ledits_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  v22[i] <- sd(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  w22[i] <- sd(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  a32[i] <- sd(degree(ledits_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  b32[i] <- sd(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  c32[i] <- sd(degree(ledits_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  d32[i] <- sd(degree(ledits_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  
}

featured_attributes_edits$density <- a
featured_attributes_edits$ncontributors <- b
featured_attributes_edits$nanon <- c
featured_attributes_edits$nedits <- d
featured_attributes_edits$transitivity <- e
featured_attributes_edits$nadmins <- h
featured_attributes_edits$nbots <- l
featured_attributes_edits$centralization <- m
featured_attributes_edits$mean_betweenness_admin <- n
featured_attributes_edits$mean_degree_centrality_admins <- o
featured_attributes_edits$mean_betweenness <- p
featured_attributes_edits$mean_degree_centrality <- q
featured_attributes_edits$sd_betweenness <- r
featured_attributes_edits$sd_degree_centrality <- s
featured_attributes_edits$sd_betweenness_admin <- t
featured_attributes_edits$sd_degree_centrality_admins <- u
featured_attributes_edits$degree_assortativity <- v
#featured_attributes_edits$assortativity_status <- w
#featured_attributes_edits$assortativity_quartiles <- a2
featured_attributes_edits$nquar1 <- b2
featured_attributes_edits$nquar2 <- c2
featured_attributes_edits$nquar3 <- d2
featured_attributes_edits$nquar4 <- e2
featured_attributes_edits$mean_degree_anon <- f2
featured_attributes_edits$mean_degree_inscrit <- g2
featured_attributes_edits$mean_indegree_admin <- h2
featured_attributes_edits$mean_outdegree_admin <- k2
featured_attributes_edits$mean_indegree_anon <- l2
featured_attributes_edits$mean_outdegree_anon <- m2
featured_attributes_edits$mean_indegree_inscrit <- n2
featured_attributes_edits$mean_outdegree_inscrit <- o2
featured_attributes_edits$mean_degree_quar1 <- p2
featured_attributes_edits$mean_indegree_quar1 <- q2
featured_attributes_edits$mean_outdegree_quar1 <- r2
featured_attributes_edits$mean_degree_quar2 <- s2
featured_attributes_edits$mean_indegree_quar2 <- t2
featured_attributes_edits$mean_outdegree_quar2 <- u2
featured_attributes_edits$mean_degree_quar3 <- v2
featured_attributes_edits$mean_indegree_quar3 <- w2
featured_attributes_edits$mean_outdegree_quar3 <- a3
featured_attributes_edits$mean_degree_quar4 <- b3
featured_attributes_edits$mean_indegree_quar4 <- c3
featured_attributes_edits$mean_outdegree_quar4 <- d3
featured_attributes_edits$nadded <- e3
featured_attributes_edits$ndeleted <- f3
featured_attributes_edits$nrestored <- g3

featured_attributes_edits$sd_degree_anon <- f22
featured_attributes_edits$sd_degree_inscrit <- g22
featured_attributes_edits$sd_indegree_admin <- h22
featured_attributes_edits$sd_outdegree_admin <- k22
featured_attributes_edits$sd_indegree_anon <- l22
featured_attributes_edits$sd_outdegree_anon <- m22
featured_attributes_edits$sd_indegree_inscrit <- n22
featured_attributes_edits$sd_outdegree_inscrit <- o22
featured_attributes_edits$sd_degree_quar1 <- p22
featured_attributes_edits$sd_indegree_quar1 <- q22
featured_attributes_edits$sd_outdegree_quar1 <- r22
featured_attributes_edits$sd_degree_quar2 <- s22
featured_attributes_edits$sd_indegree_quar2 <- t22
featured_attributes_edits$sd_outdegree_quar2 <- u22
featured_attributes_edits$sd_degree_quar3 <- v22
featured_attributes_edits$sd_indegree_quar3 <- w22
featured_attributes_edits$sd_outdegree_quar3 <- a32
featured_attributes_edits$sd_degree_quar4 <- b32
featured_attributes_edits$sd_indegree_quar4 <- c32
featured_attributes_edits$sd_outdegree_quar4 <- d32

# page controversial ou featured?

featured_attributes_edits$page <- as.character(featured_attributes_edits$page)
featured_attributes_edits$page <- gsub( "\\.csv.*$", "", featured_attributes_edits$page)

for (i in featured_attributes_edits$page){
  if (as.character(featured_attributes_edits$page[i]) %in% controversy_data){
    f[i] <- 1
  } else {
    f[i] <- 0
  }
}

for (i in featured_attributes_edits$page){
  if (as.character(featured_attributes_edits$page[i]) %in% featured_data){
    g[i] <- 1
  } else {
    g[i] <- 0
  }
}

featured_attributes_edits$iscontroversial <- f
featured_attributes_edits$isfeatured <- g

## Talk

page <- names(ltalks_graphe)
featured_attributes_talks <- as.data.frame(page)

a <- character()
b <- character()
c <- character()
d <- character()
e <- character()
f <- character()
g <- character()
h <- character()
l <- character()
m <- character()
n <- character()
o <- character()
p <- character()
q <- character()
r <- character()
s <- character()
t <- character()
u <- character()
v <- character()
w <- character()
z <- character()

v3 <- character()
w3 <- character()
a2 <- character()

b2 <- character()
c2 <- character()
d2 <- character()
e2 <- character()
f2 <- character()
g2 <- character()
h2 <- character()
k2 <- character()
l2 <- character()
m2 <- character()
n2 <- character()
o2 <- character()
p2 <- character()
q2 <- character()
r2 <- character()
s2 <- character()
t2 <- character()
u2 <- character()
v2 <- character()
w2 <- character()
a3 <- character()
b3 <- character()
c3 <- character()
d3 <- character()
e3 <- character()
f3 <- character()
g3 <- character()

f22 <- character()
g22 <- character()
h22 <- character()
k22 <- character()
l22 <- character()
m22 <- character()
n22 <- character()
o22 <- character()
p22 <- character()
q22 <- character()
r22 <- character()
s22 <- character()
t22 <- character()
u22 <- character()
v22 <- character()
w22 <- character()
a32 <- character()
b32 <- character()
c32 <- character()
d32 <- character()


# temp/test


for (i in featured_attributes_talks$page){
  a[i] <- edge_density(ltalks_graphe[[i]])
  b[i] <- vcount(ltalks_graphe[[i]])
  temp <- append(as.character(ltalks_edgelists[[i]][,"V1"]),as.character(ltalks_edgelists[[i]][,"V2"]))
  c[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$isanon == "1"]])
  d[i] <- ecount(ltalks_graphe[[i]])
  e[i] <- transitivity(ltalks_graphe[[i]])
  h[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  l[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "bot"]])
  m[i] <- centralization.degree(ltalks_graphe[[i]])[[2]]
  n[i] <- mean(betweenness(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  o[i] <- mean(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  p[i] <- mean(betweenness(ltalks_graphe[[i]])[grepl("^==.*==", labels(degree(ltalks_graphe[[i]]))) == F])
  q[i] <- mean(degree(ltalks_graphe[[i]])[grepl("^==.*==", labels(degree(ltalks_graphe[[i]]))) == F])
  r[i] <- sd(betweenness(ltalks_graphe[[i]])[grepl("^==.*==", labels(degree(ltalks_graphe[[i]]))) == F])
  s[i] <- sd(degree(ltalks_graphe[[i]])[grepl("^==.*==", labels(degree(ltalks_graphe[[i]]))) == F])
  t[i] <- sd(betweenness(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  u[i] <- sd(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  v[i] <- max(as.numeric(ltalks_edgelists[[i]][,"IndexInThread"]))
  w[i] <- mean(as.numeric(ltalks_edgelists[[i]][,"IndexInThread"]))
  z[i] <- sd(as.numeric(ltalks_edgelists[[i]][,"IndexInThread"]))
  v3[i] <- assortativity.degree(ltalks_graphe[[i]])
  #w3[i] <- assortativity(ltalks_graphe[[i]], as.factor(ltalks_graphe[[i]][,"status_contrib"]))
  #a2[i] <- assortativity(ltalks_graphe[[i]], as.factor(ltalks_graphe[[i]][,"rev_count_quar"]))
  b2[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  c2[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  d2[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  e2[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  f2[i] <- mean(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  g2[i] <- mean(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  h2[i] <- mean(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  k2[i] <- mean(degree(ltalks_graphe[[i]],mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  l2[i] <- mean(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  m2[i] <- mean(degree(ltalks_graphe[[i]],mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  n2[i] <- mean(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  o2[i] <- mean(degree(ltalks_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  p2[i] <- mean(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  q2[i] <- mean(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  r2[i] <- mean(degree(ltalks_graphe[[i]],mode= "out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  s2[i] <- mean(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  t2[i] <- mean(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  u2[i] <- mean(degree(ltalks_graphe[[i]], mode = "out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  v2[i] <- mean(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  w2[i] <- mean(degree(ltalks_graphe[[i]],mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  a3[i] <- mean(degree(ltalks_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  b3[i] <- mean(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  c3[i] <- mean(degree(ltalks_graphe[[i]], mode = "in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  d3[i] <- mean(degree(ltalks_graphe[[i]],mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  e3[i] <- length(ltalks_edgelists[[i]][ltalks_edgelists[[i]][,"DiscussionType"] == "initialized_thread","V1"])
  f3[i] <- length(ltalks_edgelists[[i]][ltalks_edgelists[[i]][,"DiscussionType"] == "replied_to","V1"])
  f22[i] <- sd(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  g22[i] <- sd(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  h22[i] <- sd(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  k22[i] <- sd(degree(ltalks_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "admin"]])
  l22[i] <- sd(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  m22[i] <- sd(degree(ltalks_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "anonyme"]])
  n22[i] <- sd(degree(ltalks_graphe[[i]],mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  o22[i] <- sd(degree(ltalks_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$status_contrib == "inscrit"]])
  p22[i] <- sd(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  q22[i] <- sd(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  r22[i] <- sd(degree(ltalks_graphe[[i]], mode="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 1]])
  s22[i] <- sd(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  t22[i] <- sd(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  u22[i] <- sd(degree(ltalks_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 2]])
  v22[i] <- sd(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  w22[i] <- sd(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  a32[i] <- sd(degree(ltalks_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 3]])
  b32[i] <- sd(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  c32[i] <- sd(degree(ltalks_graphe[[i]], mode ="in")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  d32[i] <- sd(degree(ltalks_graphe[[i]], mode ="out")[levels(as.factor(temp)) %in% attributes_featured$contributeurs[attributes_featured$rev_count_quar == 4]])
  
}

featured_attributes_talks$density <- a
featured_attributes_talks$ncontributors <- b
featured_attributes_talks$nanon <- c
featured_attributes_talks$nedits <- d
featured_attributes_talks$transitivity <- e
featured_attributes_talks$nadmins <- h
featured_attributes_talks$nbots <- l
featured_attributes_talks$centralization <- m
featured_attributes_talks$mean_betweenness_admin <- n
featured_attributes_talks$mean_degree_centrality_admins <- o
featured_attributes_talks$mean_betweenness <- p
featured_attributes_talks$mean_degree_centrality <- q
featured_attributes_talks$sd_betweenness <- r
featured_attributes_talks$sd_degree_centrality <- s
featured_attributes_talks$sd_betweenness_admin <- t
featured_attributes_talks$sd_degree_centrality_admins <- u
featured_attributes_talks$max_discussion_depth <- v
featured_attributes_talks$mean_discussion_depth <- w
featured_attributes_talks$sd_discussion_depth <- z

featured_attributes_talks$degree_assortativity <- v3
#featured_attributes_talks$assortativity_status <- w3
#featured_attributes_talks$assortativity_quartiles <- a2
featured_attributes_talks$nquar1 <- b2
featured_attributes_talks$nquar2 <- c2
featured_attributes_talks$nquar3 <- d2
featured_attributes_talks$nquar4 <- e2
featured_attributes_talks$mean_degree_anon <- f2
featured_attributes_talks$mean_degree_inscrit <- g2
featured_attributes_talks$mean_indegree_admin <- h2
featured_attributes_talks$mean_outdegree_admin <- k2
featured_attributes_talks$mean_indegree_anon <- l2
featured_attributes_talks$mean_outdegree_anon <- m2
featured_attributes_talks$mean_indegree_inscrit <- n2
featured_attributes_talks$mean_outdegree_inscrit <- o2
featured_attributes_talks$mean_degree_quar1 <- p2
featured_attributes_talks$mean_indegree_quar1 <- q2
featured_attributes_talks$mean_outdegree_quar1 <- r2
featured_attributes_talks$mean_degree_quar2 <- s2
featured_attributes_talks$mean_indegree_quar2 <- t2
featured_attributes_talks$mean_outdegree_quar2 <- u2
featured_attributes_talks$mean_degree_quar3 <- v2
featured_attributes_talks$mean_indegree_quar3 <- w2
featured_attributes_talks$mean_outdegree_quar3 <- a3
featured_attributes_talks$mean_degree_quar4 <- b3
featured_attributes_talks$mean_indegree_quar4 <- c3
featured_attributes_talks$mean_outdegree_quar4 <- d3
featured_attributes_talks$ninitialized <- e3
featured_attributes_talks$nresponded <- f3

featured_attributes_talks$sd_degree_anon <- f22
featured_attributes_talks$sd_degree_inscrit <- g22
featured_attributes_talks$sd_indegree_admin <- h22
featured_attributes_talks$sd_outdegree_admin <- k22
featured_attributes_talks$sd_indegree_anon <- l22
featured_attributes_talks$sd_outdegree_anon <- m22
featured_attributes_talks$sd_indegree_inscrit <- n22
featured_attributes_talks$sd_outdegree_inscrit <- o22
featured_attributes_talks$sd_degree_quar1 <- p22
featured_attributes_talks$sd_indegree_quar1 <- q22
featured_attributes_talks$sd_outdegree_quar1 <- r22
featured_attributes_talks$sd_degree_quar2 <- s22
featured_attributes_talks$sd_indegree_quar2 <- t22
featured_attributes_talks$sd_outdegree_quar2 <- u22
featured_attributes_talks$sd_degree_quar3 <- v22
featured_attributes_talks$sd_indegree_quar3 <- w22
featured_attributes_talks$sd_outdegree_quar3 <- a32
featured_attributes_talks$sd_degree_quar4 <- b32
featured_attributes_talks$sd_indegree_quar4 <- c32
featured_attributes_talks$sd_outdegree_quar4 <- d32

# page controversial ou featured?

featured_attributes_talks$page <- as.character(featured_attributes_talks$page)
featured_attributes_talks$page <- gsub( "\\.csv.*$", "", featured_attributes_talks$page)

for (i in featured_attributes_talks$page){
  if (as.character(featured_attributes_talks$page[i]) %in% controversy_data){
    f[i] <- 1
  } else {
    f[i] <- 0
  }
}

for (i in featured_attributes_talks$page){
  if (as.character(featured_attributes_talks$page[i]) %in% featured_data){
    g[i] <- 1
  } else {
    g[i] <- 0
  }
}

featured_attributes_talks$iscontroversial <- f
featured_attributes_talks$isfeatured <- g

## Lier les deux réseaux:

featured_attributes_edits$istalk_page[featured_attributes_edits$page %in% featured_attributes_talks$page] <- 1
featured_attributes_edits$istalk_page[(featured_attributes_edits$page %in% featured_attributes_talks$page) == F] <- 0


featured_attributes_cross <- data.frame(featured_attributes_edits$page[featured_attributes_edits$istalk_page == 1])
colnames(featured_attributes_cross) <- "page"

for (i in featured_attributes_edits$page[featured_attributes_edits$istalk_page == 1]){
  featured_attributes_cross[featured_attributes_cross$page == i,"nedits_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"nedits"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nedits_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "nedits"]
  featured_attributes_cross[featured_attributes_cross$page == i,"density_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"density"]
  featured_attributes_cross[featured_attributes_cross$page == i, "density_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "density"]
  featured_attributes_cross[featured_attributes_cross$page == i,"ncontributors_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"ncontributors"]
  featured_attributes_cross[featured_attributes_cross$page == i, "ncontributors_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "ncontributors"]
  featured_attributes_cross[featured_attributes_cross$page == i,"nadmins_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"nadmins"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nadmins_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "nadmins"]
  featured_attributes_cross[featured_attributes_cross$page == i,"nbots_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"nbots"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nbots_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "nbots"]
  featured_attributes_cross[featured_attributes_cross$page == i,"nanon_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"nanon"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nanon_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "nanon"]
  featured_attributes_cross[featured_attributes_cross$page == i,"density_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"density"]
  featured_attributes_cross[featured_attributes_cross$page == i, "density_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "density"]
  featured_attributes_cross[featured_attributes_cross$page == i,"transitivity_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"transitivity"]
  featured_attributes_cross[featured_attributes_cross$page == i, "transitivity_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "transitivity"]
  featured_attributes_cross[featured_attributes_cross$page == i,"centralization_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"centralization"]
  featured_attributes_cross[featured_attributes_cross$page == i, "centralization_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "centralization"]
  featured_attributes_cross[featured_attributes_cross$page == i,"mean_betweenness_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"mean_betweenness"]
  featured_attributes_cross[featured_attributes_cross$page == i, "mean_betweenness_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "mean_betweenness"]
  featured_attributes_cross[featured_attributes_cross$page == i,"mean_betweenness_admins_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"mean_betweenness_admin"]
  featured_attributes_cross[featured_attributes_cross$page == i, "mean_betweenness_admins_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "mean_betweenness_admin"]
  featured_attributes_cross[featured_attributes_cross$page == i,"mean_degree_centrality_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"mean_degree_centrality"]
  featured_attributes_cross[featured_attributes_cross$page == i, "mean_degree_centrality_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "mean_degree_centrality"]
  featured_attributes_cross[featured_attributes_cross$page == i,"mean_degree_centrality_admins_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"mean_degree_centrality_admins"]
  featured_attributes_cross[featured_attributes_cross$page == i, "mean_degree_centrality_admins_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "mean_degree_centrality_admins"]
  featured_attributes_cross[featured_attributes_cross$page == i,"sd_betweenness_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"sd_betweenness"]
  featured_attributes_cross[featured_attributes_cross$page == i, "sd_betweenness_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "sd_betweenness"]
  featured_attributes_cross[featured_attributes_cross$page == i,"sd_betweenness_admins_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"sd_betweenness_admin"]
  featured_attributes_cross[featured_attributes_cross$page == i, "sd_betweenness_admins_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "sd_betweenness_admin"]
  featured_attributes_cross[featured_attributes_cross$page == i,"sd_degree_centrality_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"sd_degree_centrality"]
  featured_attributes_cross[featured_attributes_cross$page == i, "sd_degree_centrality_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "sd_degree_centrality"]
  featured_attributes_cross[featured_attributes_cross$page == i,"sd_degree_centrality_admins_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i,"sd_degree_centrality_admins"]
  featured_attributes_cross[featured_attributes_cross$page == i, "sd_degree_centrality_admins_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "sd_degree_centrality_admins"]
  featured_attributes_cross[featured_attributes_cross$page == i, "max_discussion_depth"] <- featured_attributes_talks[featured_attributes_edits$page == i, "max_discussion_depth"]
  featured_attributes_cross[featured_attributes_cross$page == i, "mean_discussion_depth"] <- featured_attributes_talks[featured_attributes_talks$page == i, "mean_discussion_depth"]
  featured_attributes_cross[featured_attributes_cross$page == i, "sd_discussion_depth"] <- featured_attributes_talks[featured_attributes_talks$page == i, "sd_discussion_depth"]
  featured_attributes_cross[featured_attributes_cross$page == i, "iscontroversial"] <- featured_attributes_talks[featured_attributes_talks$page == i, "iscontroversial"]
  featured_attributes_cross[featured_attributes_cross$page == i, "isfeatured"] <- featured_attributes_talks[featured_attributes_talks$page == i, "isfeatured"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nadded"] <- featured_attributes_edits[featured_attributes_edits$page == i, "nadded"]
  featured_attributes_cross[featured_attributes_cross$page == i, "ndeleted"] <- featured_attributes_edits[featured_attributes_edits$page == i, "ndeleted"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nrestored"] <- featured_attributes_edits[featured_attributes_edits$page == i, "nrestored"]
  featured_attributes_cross[featured_attributes_cross$page == i, "ninitialized"] <- featured_attributes_talks[featured_attributes_talks$page == i, "ninitialized"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nresponded"] <- featured_attributes_talks[featured_attributes_talks$page == i, "nresponded"]
  featured_attributes_cross[featured_attributes_cross$page == i, "degree_assortativity_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i, "degree_assortativity"]
  featured_attributes_cross[featured_attributes_cross$page == i, "degree_assortativity_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "degree_assortativity"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nquar1_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i, "nquar1"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nquar2_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i, "nquar2"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nquar3_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i, "nquar3"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nquar4_edit"] <- featured_attributes_edits[featured_attributes_edits$page == i, "nquar4"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nquar1_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "nquar1"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nquar2_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "nquar2"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nquar3_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "nquar3"]
  featured_attributes_cross[featured_attributes_cross$page == i, "nquar4_talk"] <- featured_attributes_talks[featured_attributes_talks$page == i, "nquar4"]
}

## exporter les bases

write.csv2(featured_attributes_edits, file="../featured_page_attributes_edits.csv", fileEncoding = "UTF8")
write.csv2(featured_attributes_talks, file="../featured_page_attributes_talks.csv", fileEncoding = "UTF8")
write.csv2(featured_attributes_cross, file="../featured_page_attributes_cross.csv", fileEncoding = "UTF8")