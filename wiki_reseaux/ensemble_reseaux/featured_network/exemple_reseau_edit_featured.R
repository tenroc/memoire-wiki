#### Visualisation contro_edit network ####

# Basique:

plot(ledits_graphe[["Death_Valley_National_Park.csv_edits_edgelist"]],
     main ="Reseau editions Death valley national park",
     layout=layout_nicely(ledits_graphe[["Death_Valley_National_Park.csv_edits_edgelist"]]),
     edge.arrow.size=.1,
     vertex.label= NA,
     vertex.size = 3,
     rescale=TRUE)


death_valley_graphe <- ledits_graphe[["Death_Valley_National_Park.csv_edits_edgelist"]]
death_valley_edgelist <- ledits_edgelists[["Death_Valley_National_Park.csv_edits_edgelist"]]
temp <- append(death_valley_edgelist[,"V1"], death_valley_edgelist[,"V2"])
death_valley_attributes <- attributes_featured[as.character(attributes_featured$contributeurs) %in% levels(as.factor(temp)),]
death_valley_attributes$contributeurs <- as.character(death_valley_attributes$contributeurs)

# Remettre les attributes dans l'ordre du graphe:

temp <- labels(V(death_valley_graphe))

temp <- temp[is.na(temp) == F]

death_valley_attributes <- death_valley_attributes[match(as.character(death_valley_attributes$contributeurs),temp),]
death_valley_attributes[death_valley_attributes$contributeurs == "page","total_rev_count"] <- 1
death_valley_attributes[death_valley_attributes$contributeurs == "page","registration_year"] <- 2018
death_valley_attributes[is.na(death_valley_attributes$total_rev_count) == T,"total_rev_count"] <- 1
death_valley_attributes[is.na(death_valley_attributes$registration_year) == T,"registration_year"] <- 2018
## Première visualisation:

# Couleurs:

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(death_valley_graphe)$color <- colors_nodes[as.factor(death_valley_attributes$status_contrib)]
V(death_valley_graphe)$size <- abs((as.numeric(death_valley_attributes$registration_year) - 2000) -20)/3
V(death_valley_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe)$width <- as.numeric(E(death_valley_graphe)$Wordcount)/40
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(death_valley_graphe, layout=layout_nicely(death_valley_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe)$InteractionType_num)],
     main= "Reseau editions Death valley national park")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","ancienneté -", "ancienneté +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)

## Stats de base:

contro_attributes_edits$nadmins[contro_attributes_edits$page == "Oil_spill"]
contro_attributes_edits$nanon[contro_attributes_edits$page == "Oil_spill"]
contro_attributes_edits$nbots[contro_attributes_edits$page == "Oil_spill"]


centralization.degree(death_valley_graphe)
transitivity(death_valley_graphe)
edge_density(death_valley_graphe)
vcount(death_valley_graphe)
ecount(death_valley_graphe)
table (death_valley_edgelist$InteractionType)
table(death_valley_attributes$status_contrib)

death_valley_attributes$degree_centrality <- degree(death_valley_graphe)
boxplot(death_valley_attributes$degree_centrality[death_valley_attributes$contributeurs != "page"])
mean(death_valley_attributes$degree_centrality[death_valley_attributes$degree_centrality!="page"], na.rm = T)
sd(death_valley_attributes$degree_centrality[death_valley_attributes$degree_centrality!="page"], na.rm = T)

death_valley_attributes$betweenness_centrality <- betweenness(death_valley_graphe)
boxplot(death_valley_attributes$betweenness_centrality[death_valley_attributes$contributeurs != "page"])

death_valley_attributes$in_degree <- degree(death_valley_graphe, mode = "in")
boxplot(death_valley_attributes$in_degree[death_valley_attributes$contributeurs != "page"])
mean(death_valley_attributes$in_degree[death_valley_attributes$in_degree != "page"], na.rm = T)
sd(death_valley_attributes$in_degree[death_valley_attributes$in_degree != "page"], na.rm = T)

death_valley_attributes$out_degree <- degree(death_valley_graphe, mode = "out")
boxplot(death_valley_attributes$out_degree[death_valley_attributes$contributeurs != "page"])
mean(death_valley_attributes$out_degree[death_valley_attributes$out_degree != "page"], na.rm = T)
sd(death_valley_attributes$out_degree[death_valley_attributes$out_degree !="page"], na.rm = T)


death_valley_attributes$total_rev_count <- as.numeric(death_valley_attributes$total_rev_count)
mean(as.numeric(death_valley_attributes$total_rev_count))
sd(as.numeric(death_valley_attributes$total_rev_count))
boxplot(death_valley_attributes$total_rev_count)
quint <- quantile(death_valley_attributes$total_rev_count, seq(0,1,0.10))
quint2 <- quantile(death_valley_attributes$total_rev_count, seq(0,1,0.25))

death_valley_attributes$total_rev_count_quar[death_valley_attributes$total_rev_count < as.numeric(quint2[2])] <- 1
death_valley_attributes$total_rev_count_quar[death_valley_attributes$total_rev_count >= as.numeric(quint2[2]) & death_valley_attributes$total_rev_count < as.numeric(quint2[3])] <- 2
death_valley_attributes$total_rev_count_quar[death_valley_attributes$total_rev_count >= as.numeric(quint2[3]) & death_valley_attributes$total_rev_count < as.numeric(quint2[4])] <- 3
death_valley_attributes$total_rev_count_quar[death_valley_attributes$total_rev_count >= as.numeric(quint2[4])] <- 4

table(death_valley_attributes$total_rev_count_quar)

#### Deuxième représentation graphique: 

#### Size = Degree centrality

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(death_valley_graphe)$color <- colors_nodes[as.factor(death_valley_attributes$status_contrib)]
V(death_valley_graphe)$size <- log(death_valley_attributes$degree_centrality)*3
V(death_valley_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe)$width <- as.numeric(E(death_valley_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(death_valley_graphe, layout=layout_nicely(death_valley_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe)$InteractionType_num)],
     main= "Reseau editions Death valley national park")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","degrés -", "degrés +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Indegree

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(death_valley_graphe)$color <- colors_nodes[as.factor(death_valley_attributes$status_contrib)]
V(death_valley_graphe)$size <- log(death_valley_attributes$in_degree)*3
V(death_valley_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe)$width <- as.numeric(E(death_valley_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(death_valley_graphe, layout=layout_nicely(death_valley_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe)$InteractionType_num)],
     main= "Reseau editions Death valley national park")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","degrés entrant -", "degrés entrant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Out degree

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(death_valley_graphe)$color <- colors_nodes[as.factor(death_valley_attributes$status_contrib)]
V(death_valley_graphe)$size <- log(death_valley_attributes$degree_centrality)*3
V(death_valley_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe)$width <- as.numeric(E(death_valley_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(death_valley_graphe, layout=layout.fruchterman.reingold, rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe)$InteractionType_num)],
     main= "Reseau editions Death valley national park")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","degrés sortant -", "degrès sortant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)



#### Size = total rev count

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(death_valley_graphe)$color <- colors_nodes[as.factor(death_valley_attributes$status_contrib)]
V(death_valley_graphe)$size <- as.numeric(death_valley_attributes$total_rev_count_quar)*2
V(death_valley_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe)$width <- as.numeric(E(death_valley_graphe)$Wordcount)/40
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(death_valley_graphe, layout=layout.fruchterman.reingold, rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe)$InteractionType_num)],
     main= "Reseau editions Death valley national park")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse", "nombre mots -", "nombre mots +","admin","anonyme","bot", "inscrit", "page","quartile -", "quartile +"), pch=c(24,24,24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","white", "white","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,1,3,2,2,2,2,2,1,3), cex=.8, bty="n", ncol=1)


#### Size = Indegree, col = total_rev_count

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- heat.colors(n = 4)
colors_nodes[5] <- "#333333"

V(death_valley_graphe)$color <- colors_nodes[as.factor(death_valley_attributes$total_rev_count_quar)]
V(death_valley_graphe)$size <- log(death_valley_attributes$in_degree) *3
V(death_valley_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe)$width <- as.numeric(E(death_valley_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(death_valley_graphe, layout=layout_nicely(death_valley_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe)$InteractionType_num)],
     main= "Reseau editions Death valley national park")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","Nombre total d'édition -","Nombre total d'éditions +", "page","degrés entrant -", "degrés entrant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d", colors_nodes[4], colors_nodes[1], "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Out degree col= total rev count

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- heat.colors(n = 4)
colors_nodes <- colors_nodes[c(4,3,2,1)]
colors_nodes[5] <- "#333333"

V(death_valley_graphe)$color <- colors_nodes[as.factor(death_valley_attributes$total_rev_count_quar)]
V(death_valley_graphe)$size <- death_valley_attributes$out_degree/10
V(death_valley_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe)$width <- as.numeric(E(death_valley_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(death_valley_graphe, layout=layout_nicely(death_valley_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe)$InteractionType_num)],
     main= "Reseau editions Death valley national park")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","degrés sortant -", "degrès sortant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = degree_centrality col= total rev count

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- heat.colors(n = 4)
colors_nodes <- colors_nodes[c(4,3,2,1)]
colors_nodes[5] <- "#333333"

V(death_valley_graphe)$color <- colors_nodes[as.factor(death_valley_attributes$total_rev_count_quar)]
V(death_valley_graphe)$size <- log(death_valley_attributes$degree_centrality)
V(death_valley_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe)$width <- as.numeric(E(death_valley_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(death_valley_graphe, layout=layout_nicely(death_valley_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe)$InteractionType_num)],
     main= "Reseau editions Death valley national park")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","total edits -","totals edits +", "page","degrés", "degrès"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#FFFF80FF","#FF0000FF", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


## Représentations bivariées

# Degrés par rapport au décompte total d'edits.

plot(death_valley_attributes$degree_centrality, death_valley_attributes$total_rev_count)

plot(death_valley_attributes$in_degree, death_valley_attributes$total_rev_count)

plot(death_valley_attributes$out_degree, death_valley_attributes$total_rev_count)

# Tableau croisement total rev count / status_contrib

table(death_valley_attributes$status_contrib, death_valley_attributes$total_rev_count_quar)[c(1:4),c(1:4)]

# Rattacher le nombre total d'édition efféctuées au sein de ce réseau par éditeur

for (i in death_valley_attributes$contributeurs){
  death_valley_attributes$total_rev_count_local[death_valley_attributes$contributeurs == i] <- length(death_valley_edgelist$V1[death_valley_edgelist$V1 == i])
  death_valley_attributes$added_rev_count_local[death_valley_attributes$contributeurs == i] <- length(death_valley_edgelist$V1[death_valley_edgelist$V1 == i & death_valley_edgelist$InteractionType == "ADDED"])
  death_valley_attributes$deleted_rev_count_local[death_valley_attributes$contributeurs == i] <- length(death_valley_edgelist$V1[death_valley_edgelist$V1 == i & death_valley_edgelist$InteractionType == "DELETED"])
  death_valley_attributes$reversed_rev_count_local[death_valley_attributes$contributeurs == i] <- length(death_valley_edgelist$V1[death_valley_edgelist$V1 == i & death_valley_edgelist$InteractionType == "RESTORED"])
}

boxplot(death_valley_attributes$total_rev_count_local)

# Nombre de contributions en fontion du statut + moyenne et sd

sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$status_contrib == "admin"])
sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

mean(death_valley_attributes$total_rev_count_local)
mean(death_valley_attributes$total_rev_count_local[death_valley_attributes$status_contrib == "admin"])
mean(death_valley_attributes$total_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
mean(death_valley_attributes$total_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

sd(death_valley_attributes$total_rev_count_local[death_valley_attributes$status_contrib == "admin"])
sd(death_valley_attributes$total_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
sd(death_valley_attributes$total_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

# Nombre d'ajouts en fontion du statut + moyenne et sd

sum(death_valley_attributes$added_rev_count_local[death_valley_attributes$status_contrib == "admin"])
sum(death_valley_attributes$added_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
sum(death_valley_attributes$added_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

mean(death_valley_attributes$added_rev_count_local)
mean(death_valley_attributes$added_rev_count_local[death_valley_attributes$status_contrib == "admin"])
mean(death_valley_attributes$added_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
mean(death_valley_attributes$added_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

sd(death_valley_attributes$added_rev_count_local[death_valley_attributes$status_contrib == "admin"])
sd(death_valley_attributes$added_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
sd(death_valley_attributes$added_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

# Nombre de suppressions en fontion du statut + moyenne et sd

sum(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$status_contrib == "admin"])
sum(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
sum(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

mean(death_valley_attributes$deleted_rev_count_local)
mean(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$status_contrib == "admin"])
mean(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
mean(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

sd(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$status_contrib == "admin"])
sd(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
sd(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

# Nombre de reverse en fontion du statut + moyenne et sd

sum(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$status_contrib == "admin"])
sum(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
sum(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

mean(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$status_contrib == "admin"])
mean(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
mean(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

sd(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$status_contrib == "admin"])
sd(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$status_contrib == "inscrit"])
sd(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$status_contrib == "anonyme"])

# centralité de dégrés en fontion du statut + moyenne et sd

mean(death_valley_attributes$degree_centrality[death_valley_attributes$status_contrib == "admin"])
mean(death_valley_attributes$degree_centrality[death_valley_attributes$status_contrib == "inscrit"])
mean(death_valley_attributes$degree_centrality[death_valley_attributes$status_contrib == "anonyme"])

sd(death_valley_attributes$degree_centrality[death_valley_attributes$status_contrib == "admin"])
sd(death_valley_attributes$degree_centrality[death_valley_attributes$status_contrib == "inscrit"])
sd(death_valley_attributes$degree_centrality[death_valley_attributes$status_contrib == "anonyme"])

# centralité indegree en fontion du statut + moyenne et sd

mean(death_valley_attributes$in_degree[death_valley_attributes$status_contrib == "admin"])
mean(death_valley_attributes$in_degree[death_valley_attributes$status_contrib == "inscrit"])
mean(death_valley_attributes$in_degree[death_valley_attributes$status_contrib == "anonyme"])

sd(death_valley_attributes$in_degree[death_valley_attributes$status_contrib == "admin"])
sd(death_valley_attributes$in_degree[death_valley_attributes$status_contrib == "inscrit"])
sd(death_valley_attributes$in_degree[death_valley_attributes$status_contrib == "anonyme"])

# centralité outdegree en fontion du statut + moyenne et sd

mean(death_valley_attributes$out_degree[death_valley_attributes$status_contrib == "admin"])
mean(death_valley_attributes$out_degree[death_valley_attributes$status_contrib == "inscrit"])
mean(death_valley_attributes$out_degree[death_valley_attributes$status_contrib == "anonyme"])

sd(death_valley_attributes$out_degree[death_valley_attributes$status_contrib == "admin"])
sd(death_valley_attributes$out_degree[death_valley_attributes$status_contrib == "inscrit"])
sd(death_valley_attributes$out_degree[death_valley_attributes$status_contrib == "anonyme"])



# Nombre de contributions en fonction du revcount + moyenne et sd

sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

mean(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
mean(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
mean(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
mean(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
sum(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

sd(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
sd(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
sd(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
sd(death_valley_attributes$total_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])


# Nombre d'ajouts en fonction du revcount + moyenne et sd

sum(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
sum(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
sum(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
sum(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

mean(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
mean(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
mean(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
mean(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

sd(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
sd(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
sd(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
sd(death_valley_attributes$added_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

# Nombre de suppressions en fonction du revcount + moyenne et sd

sum(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
sum(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
sum(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
sum(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

mean(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
mean(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
mean(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
mean(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

sd(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
sd(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
sd(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
sd(death_valley_attributes$deleted_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

# Nombre de reverse en fonction du revcount + moyenne et sd

sum(death_valley_attributes$reverse_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
sum(death_valley_attributes$reverse_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
sum(death_valley_attributes$reverse_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
sum(death_valley_attributes$reverse_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

mean(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
mean(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
mean(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
mean(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

sd(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$total_rev_count_quar == 1])
sd(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$total_rev_count_quar == 2])
sd(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$total_rev_count_quar == 3])
sd(death_valley_attributes$reversed_rev_count_local[death_valley_attributes$total_rev_count_quar == 4])

# centralité de degré en fonction du revcount + moyenne et sd

mean(death_valley_attributes$degree_centrality[death_valley_attributes$total_rev_count_quar == 1])
mean(death_valley_attributes$degree_centrality[death_valley_attributes$total_rev_count_quar == 2])
mean(death_valley_attributes$degree_centrality[death_valley_attributes$total_rev_count_quar == 3])
mean(death_valley_attributes$degree_centrality[death_valley_attributes$total_rev_count_quar == 4])

sd(death_valley_attributes$degree_centrality[death_valley_attributes$total_rev_count_quar == 1])
sd(death_valley_attributes$degree_centrality[death_valley_attributes$total_rev_count_quar == 2])
sd(death_valley_attributes$degree_centrality[death_valley_attributes$total_rev_count_quar == 3])
sd(death_valley_attributes$degree_centrality[death_valley_attributes$total_rev_count_quar == 4])

# demi degré entrant en fonction du revcount + moyenne et sd

mean(death_valley_attributes$in_degree[death_valley_attributes$total_rev_count_quar == 1])
mean(death_valley_attributes$in_degree[death_valley_attributes$total_rev_count_quar == 2])
mean(death_valley_attributes$in_degree[death_valley_attributes$total_rev_count_quar == 3])
mean(death_valley_attributes$in_degree[death_valley_attributes$total_rev_count_quar == 4])

sd(death_valley_attributes$in_degree[death_valley_attributes$total_rev_count_quar == 1])
sd(death_valley_attributes$in_degree[death_valley_attributes$total_rev_count_quar == 2])
sd(death_valley_attributes$in_degree[death_valley_attributes$total_rev_count_quar == 3])
sd(death_valley_attributes$in_degree[death_valley_attributes$total_rev_count_quar == 4])

# demi degré sortant en fonction du revcount + moyenne et sd

mean(death_valley_attributes$out_degree[death_valley_attributes$total_rev_count_quar == 1])
mean(death_valley_attributes$out_degree[death_valley_attributes$total_rev_count_quar == 2])
mean(death_valley_attributes$out_degree[death_valley_attributes$total_rev_count_quar == 3])
mean(death_valley_attributes$out_degree[death_valley_attributes$total_rev_count_quar == 4])

sd(death_valley_attributes$out_degree[death_valley_attributes$total_rev_count_quar == 1])
sd(death_valley_attributes$out_degree[death_valley_attributes$total_rev_count_quar == 2])
sd(death_valley_attributes$out_degree[death_valley_attributes$total_rev_count_quar == 3])
sd(death_valley_attributes$out_degree[death_valley_attributes$total_rev_count_quar == 4])


table(death_valley_attributes$total_rev_count_quar, death_valley_attributes$status_contrib)

## Regressions

reg1 <- glm(added_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes)
summary(reg1)
1 - (reg1$deviance / reg1$null.deviance)

reg2 <- glm(deleted_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes)
summary(reg2)
1 - (reg2$deviance / reg2$null.deviance)

reg3 <- glm(reversed_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes)
summary(reg3)
1 - (reg3$deviance / reg3$null.deviance)

reg4 <- glm(in_degree ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes)
summary(reg4)
1 - (reg4$deviance / reg4$null.deviance)

reg5 <- glm(out_degree ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes)
summary(reg5)
1 - (reg5$deviance / reg5$null.deviance)

reg6 <- glm(degree_centrality ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes)
summary(reg6)
1 - (reg6$deviance / reg6$null.deviance)

#### Assortativité

assortativity.degree(death_valley_graphe)

assortativity(death_valley_graphe, death_valley_attributes$status_contrib)

assortativity(death_valley_graphe, death_valley_attributes$total_rev_count_quar)