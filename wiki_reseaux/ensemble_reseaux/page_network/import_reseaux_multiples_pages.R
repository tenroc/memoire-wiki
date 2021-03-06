#### Automatisation de la réccupération des donées contrib sur l'ensemble des réseaux: ####

# Test 2: toujours page/edits, mais autre import:

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

for(i in names(ledits)){
  ltalks[[i]] <- ltalks[[i]][!ltalks[[i]][,"ActiveUser"]=="",]
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

attributes_page <- data.frame(contributeurs,isanon)
attributes_page <- unique(attributes_page)

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

attributes_page$contributeurs <- as.character(attributes_page$contributeurs)
attributes_page$status_contrib <- as.character(attributes_page$status_contrib)
attributes_page[attributes_page$contributeurs == "", "status_contrib"] <- "page"
attributes_page[attributes_page$contributeurs == "", "contributeurs"] <- "page"

write.csv2(attributes_page, file="../nodes_attributes_page_network.csv", fileEncoding = "UTF8")

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
#  ledits[[i]][j,"status_contrib"] <-  attributes_page[as.character(attributes_page$contributeurs) %in% as.character(ledits[[i]][j,"ActiveUser"]),
#                                                  "status_contrib"]
#  ledits[[i]][j,"total_rev_count"] <-  attributes_page[as.character(attributes_page$contributeurs) == as.character(ledits[[i]][j,"ActiveUser"]),
#                                                  "total_rev_count"]
#  ledits[[i]][j,"registration_year"] <-  attributes_page[as.character(attributes_page$contributeurs) == as.character(ledits[[i]][j,"ActiveUser"]),
#                                                  "registration_year"]
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
#    ltalks[[i]][j,"status_contrib"] <-  attributes_page[as.character(attributes_page$contributeurs) %in% as.character(ltalks[[i]][j,"ActiveUser"]),
#                                                        "status_contrib"]
#    ltalks[[i]][j,"total_rev_count"] <-  attributes_page[as.character(attributes_page$contributeurs) == as.character(ltalks[[i]][j,"ActiveUser"]),
#                                                         "total_rev_count"]
#    ltalks[[i]][j,"registration_year"] <-  attributes_page[as.character(attributes_page$contributeurs) == as.character(ltalks[[i]][j,"ActiveUser"]),
#                                                           "registration_year"]
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

#### constitution de la base: page_attributes ####

## Edits

page <- names(ledits_graphe)
page_attributes_edits <- as.data.frame(page)

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


for (i in page_attributes_edits$page){
  a[i] <- edge_density(ledits_graphe[[i]])
  b[i] <- vcount(ledits_graphe[[i]])
  temp <- append(as.character(ledits_edgelists[[i]][,"V1"]),as.character(ledits_edgelists[[i]][,"V2"]))
  c[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$isanon == "1"]])
  d[i] <- ecount(ledits_graphe[[i]])
  e[i] <- transitivity(ledits_graphe[[i]])
  h[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
  l[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "bot"]])
  m[i] <- centralization.degree(ledits_graphe[[i]])[[2]]
  n[i] <- mean(betweenness(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
  o[i] <- mean(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
  p[i] <- mean(betweenness(ledits_graphe[[i]])[labels(degree(ledits_graphe[[i]])) != "page"])
  q[i] <- mean(degree(ledits_graphe[[i]])[labels(degree(ledits_graphe[[i]])) != "page"])
  r[i] <- sd(betweenness(ledits_graphe[[i]])[labels(degree(ledits_graphe[[i]])) != "page"])
  s[i] <- sd(degree(ledits_graphe[[i]])[labels(degree(ledits_graphe[[i]])) != "page"])
  t[i] <- sd(betweenness(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
  u[i] <- sd(degree(ledits_graphe[[i]])[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
}

page_attributes_edits$density <- a
page_attributes_edits$ncontributors <- b
page_attributes_edits$nanon <- c
page_attributes_edits$nedits <- d
page_attributes_edits$transitivity <- e
page_attributes_edits$nadmins <- h
page_attributes_edits$nbots <- l
page_attributes_edits$centralization <- m
page_attributes_edits$mean_betweenness_admin <- n
page_attributes_edits$mean_degree_centrality_admins <- o
page_attributes_edits$mean_betweenness <- p
page_attributes_edits$mean_degree_centrality <- q
page_attributes_edits$sd_betweenness <- r
page_attributes_edits$sd_degree_centrality <- s
page_attributes_edits$sd_betweenness_admin <- t
page_attributes_edits$sd_degree_centrality_admins <- u

# page controversial ou featured?

page_attributes_edits$page <- as.character(page_attributes_edits$page)
page_attributes_edits$page <- gsub( "\\.csv.*$", "", page_attributes_edits$page)

for (i in page_attributes_edits$page){
  if (as.character(page_attributes_edits$page[i]) %in% controversy_data){
    f[i] <- 1
  } else {
    f[i] <- 0
  }
}

for (i in page_attributes_edits$page){
  if (as.character(page_attributes_edits$page[i]) %in% featured_data){
    g[i] <- 1
  } else {
    g[i] <- 0
  }
}

page_attributes_edits$iscontroversial <- f
page_attributes_edits$isfeatured <- g

## Talk

page <- names(ltalks_graphe)
page_attributes_talks <- as.data.frame(page)

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



for (i in page_attributes_talks$page){
  a[i] <- edge_density(ltalks_graphe[[i]])
  b[i] <- vcount(ltalks_graphe[[i]])
  temp <- append(as.character(ltalks_edgelists[[i]][,"V1"]),as.character(ltalks_edgelists[[i]][,"V2"]))
  c[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$isanon == "1"]])
  d[i] <- ecount(ltalks_graphe[[i]])
  e[i] <- transitivity(ltalks_graphe[[i]])
  h[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
  l[i] <- length(levels(as.factor(temp))[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "bot"]])
  m[i] <- centralization.degree(ltalks_graphe[[i]])[[2]]
  n[i] <- mean(betweenness(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
  o[i] <- mean(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
  p[i] <- mean(betweenness(ltalks_graphe[[i]])[grepl("^==.*==", labels(degree(ltalks_graphe[[i]]))) == F])
  q[i] <- mean(degree(ltalks_graphe[[i]])[grepl("^==.*==", labels(degree(ltalks_graphe[[i]]))) == F])
  r[i] <- sd(betweenness(ltalks_graphe[[i]])[grepl("^==.*==", labels(degree(ltalks_graphe[[i]]))) == F])
  s[i] <- sd(degree(ltalks_graphe[[i]])[grepl("^==.*==", labels(degree(ltalks_graphe[[i]]))) == F])
  t[i] <- sd(betweenness(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
  u[i] <- sd(degree(ltalks_graphe[[i]])[levels(as.factor(temp)) %in% attributes_page$contributeurs[attributes_page$status_contrib == "admin"]])
  v[i] <- max(as.numeric(ltalks_edgelists[[i]][,"IndexInThread"]))
  w[i] <- mean(as.numeric(ltalks_edgelists[[i]][,"IndexInThread"]))
  z[i] <- sd(as.numeric(ltalks_edgelists[[i]][,"IndexInThread"]))
}

page_attributes_talks$density <- a
page_attributes_talks$ncontributors <- b
page_attributes_talks$nanon <- c
page_attributes_talks$nedits <- d
page_attributes_talks$transitivity <- e
page_attributes_talks$nadmins <- h
page_attributes_talks$nbots <- l
page_attributes_talks$centralization <- m
page_attributes_talks$mean_betweenness_admin <- n
page_attributes_talks$mean_degree_centrality_admins <- o
page_attributes_talks$mean_betweenness <- p
page_attributes_talks$mean_degree_centrality <- q
page_attributes_talks$sd_betweenness <- r
page_attributes_talks$sd_degree_centrality <- s
page_attributes_talks$sd_betweenness_admin <- t
page_attributes_talks$sd_degree_centrality_admins <- u
page_attributes_talks$max_discussion_depth <- v
page_attributes_talks$mean_discussion_depth <- w
page_attributes_talks$sd_discussion_depth <- z

# page controversial ou featured?

page_attributes_talks$page <- as.character(page_attributes_talks$page)
page_attributes_talks$page <- gsub( "\\.csv.*$", "", page_attributes_talks$page)

for (i in page_attributes_talks$page){
  if (as.character(page_attributes_talks$page[i]) %in% controversy_data){
    f[i] <- 1
  } else {
    f[i] <- 0
  }
}

for (i in page_attributes_talks$page){
  if (as.character(page_attributes_talks$page[i]) %in% featured_data){
    g[i] <- 1
  } else {
    g[i] <- 0
  }
}

page_attributes_talks$iscontroversial <- f
page_attributes_talks$isfeatured <- g

## Lier les deux réseaux:

page_attributes_edits$istalk_page[page_attributes_edits$page %in% page_attributes_talks$page] <- 1
page_attributes_edits$istalk_page[(page_attributes_edits$page %in% page_attributes_talks$page) == F] <- 0

page_attribute_cross <- data.frame(page_attributes_edits$page[page_attributes_edits$istalk_page == 1])
colnames(page_attribute_cross) <- "page"

for (i in page_attributes_edits$page[page_attributes_edits$istalk_page == 1]){
  page_attribute_cross[page_attribute_cross$page == i,"nedits_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"nedits"]
  page_attribute_cross[page_attribute_cross$page == i, "nedits_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "nedits"]
  page_attribute_cross[page_attribute_cross$page == i,"density_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"density"]
  page_attribute_cross[page_attribute_cross$page == i, "density_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "density"]
  page_attribute_cross[page_attribute_cross$page == i,"ncontributors_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"ncontributors"]
  page_attribute_cross[page_attribute_cross$page == i, "ncontributors_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "ncontributors"]
  page_attribute_cross[page_attribute_cross$page == i,"nadmins_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"nadmins"]
  page_attribute_cross[page_attribute_cross$page == i, "nadmins_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "nadmins"]
  page_attribute_cross[page_attribute_cross$page == i,"nbots_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"nbots"]
  page_attribute_cross[page_attribute_cross$page == i, "nbots_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "nbots"]
  page_attribute_cross[page_attribute_cross$page == i,"nanon_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"nanon"]
  page_attribute_cross[page_attribute_cross$page == i, "nanon_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "nanon"]
  page_attribute_cross[page_attribute_cross$page == i,"density_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"density"]
  page_attribute_cross[page_attribute_cross$page == i, "density_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "density"]
  page_attribute_cross[page_attribute_cross$page == i,"transitivity_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"transitivity"]
  page_attribute_cross[page_attribute_cross$page == i, "transitivity_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "transitivity"]
  page_attribute_cross[page_attribute_cross$page == i,"centralization_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"centralization"]
  page_attribute_cross[page_attribute_cross$page == i, "centralization_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "centralization"]
  page_attribute_cross[page_attribute_cross$page == i,"mean_betweenness_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"mean_betweenness"]
  page_attribute_cross[page_attribute_cross$page == i, "mean_betweenness_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "mean_betweenness"]
  page_attribute_cross[page_attribute_cross$page == i,"mean_betweenness_admins_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"mean_betweenness_admin"]
  page_attribute_cross[page_attribute_cross$page == i, "mean_betweenness_admins_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "mean_betweenness_admin"]
  page_attribute_cross[page_attribute_cross$page == i,"mean_degree_centrality_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"mean_degree_centrality"]
  page_attribute_cross[page_attribute_cross$page == i, "mean_degree_centrality_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "mean_degree_centrality"]
  page_attribute_cross[page_attribute_cross$page == i,"mean_degree_centrality_admins_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"mean_degree_centrality_admins"]
  page_attribute_cross[page_attribute_cross$page == i, "mean_degree_centrality_admins_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "mean_degree_centrality_admins"]
  page_attribute_cross[page_attribute_cross$page == i,"sd_betweenness_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"sd_betweenness"]
  page_attribute_cross[page_attribute_cross$page == i, "sd_betweenness_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "sd_betweenness"]
  page_attribute_cross[page_attribute_cross$page == i,"sd_betweenness_admins_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"sd_betweenness_admin"]
  page_attribute_cross[page_attribute_cross$page == i, "sd_betweenness_admins_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "sd_betweenness_admin"]
  page_attribute_cross[page_attribute_cross$page == i,"sd_degree_centrality_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"sd_degree_centrality"]
  page_attribute_cross[page_attribute_cross$page == i, "sd_degree_centrality_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "sd_degree_centrality"]
  page_attribute_cross[page_attribute_cross$page == i,"sd_degree_centrality_admins_edit"] <- page_attributes_edits[page_attributes_edits$page == i,"sd_degree_centrality_admins"]
  page_attribute_cross[page_attribute_cross$page == i, "sd_degree_centrality_admins_talk"] <- page_attributes_talks[page_attributes_talks$page == i, "sd_degree_centrality_admins"]
  page_attribute_cross[page_attribute_cross$page == i, "max_discussion_depth"] <- page_attributes_talks[page_attributes_edits$page == i, "max_discussion_depth"]
  page_attribute_cross[page_attribute_cross$page == i, "mean_discussion_depth"] <- page_attributes_talks[page_attributes_talks$page == i, "mean_discussion_depth"]
  page_attribute_cross[page_attribute_cross$page == i, "sd_discussion_depth"] <- page_attributes_talks[page_attributes_talks$page == i, "sd_discussion_depth"]
  page_attribute_cross[page_attribute_cross$page == i, "iscontroversial"] <- page_attributes_talks[page_attributes_talks$page == i, "iscontroversial"]
  page_attribute_cross[page_attribute_cross$page == i, "isfeatured"] <- page_attributes_talks[page_attributes_talks$page == i, "isfeatured"]
}

## exporter les bases

write.csv2(page_attributes_edits, file="../page_page_attributes_edits.csv", fileEncoding = "UTF8")
write.csv2(page_attributes_talks, file="../page_page_attributes_talks.csv", fileEncoding = "UTF8")
write.csv2(page_attribute_cross, file ="../page_page_attribute_cross.csv", fileEncoding = "UTF8")

