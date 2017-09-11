#### Visualisation contro_edit network ####

# Basique:

plot(ledits_graphe[["Oil_spill.csv_edits_edgelist"]],
     main ="Reseau editions Oil spill",
     layout=layout_nicely(ledits_graphe[["Oil_spill.csv_edits_edgelist"]]),
     edge.arrow.size=.1,
     vertex.label= NA,
     vertex.size = 3,
     rescale=TRUE)


oil_spill_graphe <- ledits_graphe[["Oil_spill.csv_edits_edgelist"]]
oil_spill_edgelist <- ledits_edgelists[["Oil_spill.csv_edits_edgelist"]]
temp <- append(oil_spill_edgelist[,"V1"], oil_spill_edgelist[,"V2"])
oil_spill_attributes <- attributes_contro[as.character(attributes_contro$contributeurs) %in% levels(as.factor(temp)),]
oil_spill_attributes$contributeurs <- as.character(oil_spill_attributes$contributeurs)
# oil_spill_attributes[length(oil_spill_attributes$contributeurs)+1,] <- c(NA,NA,"Hannes_R0",0,"inscrit",1,2018)

# Remettre les attributes dans l'ordre du graphe:

temp <- labels(V(oil_spill_graphe))

temp <- temp[is.na(temp) == F]

oil_spill_attributes <- oil_spill_attributes[match(as.character(oil_spill_attributes$contributeurs),temp),]
oil_spill_attributes[oil_spill_attributes$contributeurs == "page","total_rev_count"] <- 1
oil_spill_attributes[oil_spill_attributes$contributeurs == "page","registration_year"] <- 2018
oil_spill_attributes[is.na(oil_spill_attributes$total_rev_count) == T,"total_rev_count"] <- 1
oil_spill_attributes[is.na(oil_spill_attributes$registration_year) == T,"registration_year"] <- 2018
## Première visualisation:

# Couleurs:

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(oil_spill_graphe)$color <- colors_nodes[as.factor(oil_spill_attributes$status_contrib)]
V(oil_spill_graphe)$size <- abs((as.numeric(oil_spill_attributes$registration_year) - 2000) -20)/3
V(oil_spill_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe)$width <- as.numeric(E(oil_spill_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(oil_spill_graphe, layout=layout_nicely(oil_spill_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe)$InteractionType_num)],
     main= "Reseau editions Oil spill")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","ancienneté -", "ancienneté +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)

## Stats de base:

contro_attributes_edits$nadmins[contro_attributes_edits$page == "Oil_spill"]
contro_attributes_edits$nanon[contro_attributes_edits$page == "Oil_spill"]
contro_attributes_edits$nbots[contro_attributes_edits$page == "Oil_spill"]


centralization.degree(oil_spill_graphe)
transitivity(oil_spill_graphe)
edge_density(oil_spill_graphe)
vcount(oil_spill_graphe)
ecount(oil_spill_graphe)
table (oil_spill_edgelist$InteractionType)

oil_spill_attributes$degree_centrality <- degree(oil_spill_graphe)
boxplot(oil_spill_attributes$degree_centrality[oil_spill_attributes$contributeurs != "page"])
mean(oil_spill_attributes$degree_centrality[oil_spill_attributes$degree_centrality!="page"], na.rm = T)
sd(oil_spill_attributes$degree_centrality[oil_spill_attributes$degree_centrality!="page"], na.rm = T)

oil_spill_attributes$betweenness_centrality <- betweenness(oil_spill_graphe)
boxplot(oil_spill_attributes$betweenness_centrality[oil_spill_attributes$contributeurs != "page"])

oil_spill_attributes$in_degree <- degree(oil_spill_graphe, mode = "in")
boxplot(oil_spill_attributes$in_degree[oil_spill_attributes$contributeurs != "page"])
mean(oil_spill_attributes$in_degree[oil_spill_attributes$in_degree != "page"], na.rm = T)
sd(oil_spill_attributes$in_degree[oil_spill_attributes$in_degree != "page"], na.rm = T)

oil_spill_attributes$out_degree <- degree(oil_spill_graphe, mode = "out")
boxplot(oil_spill_attributes$out_degree[oil_spill_attributes$contributeurs != "page"])
mean(oil_spill_attributes$out_degree[oil_spill_attributes$out_degree != "page"], na.rm = T)
sd(oil_spill_attributes$out_degree[oil_spill_attributes$out_degree !="page"], na.rm = T)


oil_spill_attributes$total_rev_count <- as.numeric(oil_spill_attributes$total_rev_count)
mean(as.numeric(oil_spill_attributes$total_rev_count))
sd(as.numeric(oil_spill_attributes$total_rev_count))
boxplot(oil_spill_attributes$total_rev_count)
quint <- quantile(oil_spill_attributes$total_rev_count, seq(0,1,0.10))
quint2 <- quantile(oil_spill_attributes$total_rev_count, seq(0,1,0.25))

oil_spill_attributes$total_rev_count_quar[oil_spill_attributes$total_rev_count < as.numeric(quint2[2])] <- 1
oil_spill_attributes$total_rev_count_quar[oil_spill_attributes$total_rev_count >= as.numeric(quint2[2]) & oil_spill_attributes$total_rev_count < as.numeric(quint2[3])] <- 2
oil_spill_attributes$total_rev_count_quar[oil_spill_attributes$total_rev_count >= as.numeric(quint2[3]) & oil_spill_attributes$total_rev_count < as.numeric(quint2[4])] <- 3
oil_spill_attributes$total_rev_count_quar[oil_spill_attributes$total_rev_count >= as.numeric(quint2[4])] <- 4

table(oil_spill_attributes$total_rev_count_quar)

#### Deuxième représentation graphique: 

#### Size = Degree centrality

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(oil_spill_graphe)$color <- colors_nodes[as.factor(oil_spill_attributes$status_contrib)]
V(oil_spill_graphe)$size <- log(oil_spill_attributes$degree_centrality)*3
V(oil_spill_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe)$width <- as.numeric(E(oil_spill_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(oil_spill_graphe, layout=layout_nicely(oil_spill_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe)$InteractionType_num)],
     main= "Reseau editions Oil spill")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","degrés -", "degrés +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Indegree

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(oil_spill_graphe)$color <- colors_nodes[as.factor(oil_spill_attributes$status_contrib)]
V(oil_spill_graphe)$size <- log(oil_spill_attributes$in_degree)*3
V(oil_spill_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe)$width <- as.numeric(E(oil_spill_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(oil_spill_graphe, layout=layout_nicely(oil_spill_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe)$InteractionType_num)],
     main= "Reseau editions Oil spill")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","degrés entrant -", "degrés entrant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Out degree

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(oil_spill_graphe)$color <- colors_nodes[as.factor(oil_spill_attributes$status_contrib)]
V(oil_spill_graphe)$size <- log(oil_spill_attributes$degree_centrality)*3
V(oil_spill_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe)$width <- as.numeric(E(oil_spill_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(oil_spill_graphe, layout=layout.fruchterman.reingold, rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe)$InteractionType_num)],
     main= "Reseau editions Oil spill")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","degrés sortant -", "degrès sortant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Indegree, col = total_rev_count

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- heat.colors(n = 4)
colors_nodes[5] <- "#333333"

V(oil_spill_graphe)$color <- colors_nodes[as.factor(oil_spill_attributes$total_rev_count_quar)]
V(oil_spill_graphe)$size <- log(oil_spill_attributes$in_degree) *3
V(oil_spill_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe)$width <- as.numeric(E(oil_spill_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(oil_spill_graphe, layout=layout_nicely(oil_spill_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe)$InteractionType_num)],
     main= "Reseau editions Oil spill")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","Nombre total d'édition -","Nombre total d'éditions +", "page","degrés entrant -", "degrés entrant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d", colors_nodes[4], colors_nodes[1], "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Out degree col= total rev count

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- heat.colors(n = 4)
colors_nodes <- colors_nodes[c(4,3,2,1)]
colors_nodes[5] <- "#333333"

V(oil_spill_graphe)$color <- colors_nodes[as.factor(oil_spill_attributes$total_rev_count_quar)]
V(oil_spill_graphe)$size <- oil_spill_attributes$out_degree/10
V(oil_spill_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe)$width <- as.numeric(E(oil_spill_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(oil_spill_graphe, layout=layout_nicely(oil_spill_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe)$InteractionType_num)],
     main= "Reseau editions Oil spill")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","degrés sortant -", "degrès sortant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = degree_centrality col= total rev count

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- heat.colors(n = 4)
colors_nodes <- colors_nodes[c(4,3,2,1)]
colors_nodes[5] <- "#333333"

V(oil_spill_graphe)$color <- colors_nodes[as.factor(oil_spill_attributes$total_rev_count_quar)]
V(oil_spill_graphe)$size <- log(oil_spill_attributes$degree_centrality)
V(oil_spill_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe)$width <- as.numeric(E(oil_spill_graphe)$Wordcount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(oil_spill_graphe, layout=layout_nicely(oil_spill_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe)$InteractionType_num)],
     main= "Reseau edition Oil spill")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse", "nombre mots -", "nombre mots +","total edits -","totals edits +", "page","centralité degrés -", "centralité degrès +"), pch=c(24,24,24,24,24,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","white", "white","#FFFF80FF","#FF0000FF", "#333333", "white", "white"), pt.cex=c(2,2,2,1,3,2,2,2,1,3), cex=.8, bty="n", ncol=1)


## Représentations bivariées

# Degrés par rapport au décompte total d'edits.

plot(oil_spill_attributes$degree_centrality, oil_spill_attributes$total_rev_count)

plot(oil_spill_attributes$in_degree, oil_spill_attributes$total_rev_count)

plot(oil_spill_attributes$out_degree, oil_spill_attributes$total_rev_count)

# Tableau croisement total rev count / status_contrib

table(oil_spill_attributes$status_contrib, oil_spill_attributes$total_rev_count_quar)[c(1:4),c(1:4)]

# Rattacher le nombre total d'édition efféctuées au sein de ce réseau par éditeur

for (i in oil_spill_attributes$contributeurs){
  oil_spill_attributes$total_rev_count_local[oil_spill_attributes$contributeurs == i] <- length(oil_spill_edgelist$V1[oil_spill_edgelist$V1 == i])
  oil_spill_attributes$added_rev_count_local[oil_spill_attributes$contributeurs == i] <- length(oil_spill_edgelist$V1[oil_spill_edgelist$V1 == i & oil_spill_edgelist$InteractionType == "ADDED"])
  oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$contributeurs == i] <- length(oil_spill_edgelist$V1[oil_spill_edgelist$V1 == i & oil_spill_edgelist$InteractionType == "DELETED"])
  oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$contributeurs == i] <- length(oil_spill_edgelist$V1[oil_spill_edgelist$V1 == i & oil_spill_edgelist$InteractionType == "RESTORED"])
}

boxplot(oil_spill_attributes$total_rev_count_local)

# Nombre de contributions en fontion du statut + moyenne et sd

sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

mean(oil_spill_attributes$total_rev_count_local)
mean(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
mean(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
mean(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

sd(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
sd(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
sd(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

# Nombre d'ajouts en fontion du statut + moyenne et sd

sum(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
sum(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
sum(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

mean(oil_spill_attributes$added_rev_count_local)
mean(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
mean(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
mean(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

sd(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
sd(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
sd(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

# Nombre de suppressions en fontion du statut + moyenne et sd

sum(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
sum(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
sum(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

mean(oil_spill_attributes$deleted_rev_count_local)
mean(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
mean(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
mean(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

sd(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
sd(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
sd(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

# Nombre de reverse en fontion du statut + moyenne et sd

sum(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
sum(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
sum(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

mean(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
mean(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
mean(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

sd(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$status_contrib == "admin"])
sd(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$status_contrib == "inscrit"])
sd(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$status_contrib == "anonyme"])

# centralité de dégrés en fontion du statut + moyenne et sd

mean(oil_spill_attributes$degree_centrality[oil_spill_attributes$status_contrib == "admin"])
mean(oil_spill_attributes$degree_centrality[oil_spill_attributes$status_contrib == "inscrit"])
mean(oil_spill_attributes$degree_centrality[oil_spill_attributes$status_contrib == "anonyme"])

sd(oil_spill_attributes$degree_centrality[oil_spill_attributes$status_contrib == "admin"])
sd(oil_spill_attributes$degree_centrality[oil_spill_attributes$status_contrib == "inscrit"])
sd(oil_spill_attributes$degree_centrality[oil_spill_attributes$status_contrib == "anonyme"])

# centralité indegree en fontion du statut + moyenne et sd

mean(oil_spill_attributes$in_degree[oil_spill_attributes$status_contrib == "admin"])
mean(oil_spill_attributes$in_degree[oil_spill_attributes$status_contrib == "inscrit"])
mean(oil_spill_attributes$in_degree[oil_spill_attributes$status_contrib == "anonyme"])

sd(oil_spill_attributes$in_degree[oil_spill_attributes$status_contrib == "admin"])
sd(oil_spill_attributes$in_degree[oil_spill_attributes$status_contrib == "inscrit"])
sd(oil_spill_attributes$in_degree[oil_spill_attributes$status_contrib == "anonyme"])

# centralité outdegree en fontion du statut + moyenne et sd

mean(oil_spill_attributes$out_degree[oil_spill_attributes$status_contrib == "admin"])
mean(oil_spill_attributes$out_degree[oil_spill_attributes$status_contrib == "inscrit"])
mean(oil_spill_attributes$out_degree[oil_spill_attributes$status_contrib == "anonyme"])

sd(oil_spill_attributes$out_degree[oil_spill_attributes$status_contrib == "admin"])
sd(oil_spill_attributes$out_degree[oil_spill_attributes$status_contrib == "inscrit"])
sd(oil_spill_attributes$out_degree[oil_spill_attributes$status_contrib == "anonyme"])



# Nombre de contributions en fonction du revcount + moyenne et sd

sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

mean(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
mean(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
mean(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
mean(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
sum(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

sd(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
sd(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
sd(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
sd(oil_spill_attributes$total_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])


# Nombre d'ajouts en fonction du revcount + moyenne et sd

sum(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
sum(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
sum(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
sum(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

mean(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
mean(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
mean(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
mean(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

sd(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
sd(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
sd(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
sd(oil_spill_attributes$added_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

# Nombre de suppressions en fonction du revcount + moyenne et sd

sum(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
sum(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
sum(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
sum(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

mean(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
mean(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
mean(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
mean(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

sd(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
sd(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
sd(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
sd(oil_spill_attributes$deleted_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

# Nombre de reverse en fonction du revcount + moyenne et sd

sum(oil_spill_attributes$reverse_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
sum(oil_spill_attributes$reverse_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
sum(oil_spill_attributes$reverse_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
sum(oil_spill_attributes$reverse_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

mean(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
mean(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
mean(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
mean(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

sd(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$total_rev_count_quar == 1])
sd(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$total_rev_count_quar == 2])
sd(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$total_rev_count_quar == 3])
sd(oil_spill_attributes$reversed_rev_count_local[oil_spill_attributes$total_rev_count_quar == 4])

# centralité de degré en fonction du revcount + moyenne et sd

mean(oil_spill_attributes$degree_centrality[oil_spill_attributes$total_rev_count_quar == 1])
mean(oil_spill_attributes$degree_centrality[oil_spill_attributes$total_rev_count_quar == 2])
mean(oil_spill_attributes$degree_centrality[oil_spill_attributes$total_rev_count_quar == 3])
mean(oil_spill_attributes$degree_centrality[oil_spill_attributes$total_rev_count_quar == 4])

sd(oil_spill_attributes$degree_centrality[oil_spill_attributes$total_rev_count_quar == 1])
sd(oil_spill_attributes$degree_centrality[oil_spill_attributes$total_rev_count_quar == 2])
sd(oil_spill_attributes$degree_centrality[oil_spill_attributes$total_rev_count_quar == 3])
sd(oil_spill_attributes$degree_centrality[oil_spill_attributes$total_rev_count_quar == 4])

# demi degré entrant en fonction du revcount + moyenne et sd

mean(oil_spill_attributes$in_degree[oil_spill_attributes$total_rev_count_quar == 1])
mean(oil_spill_attributes$in_degree[oil_spill_attributes$total_rev_count_quar == 2])
mean(oil_spill_attributes$in_degree[oil_spill_attributes$total_rev_count_quar == 3])
mean(oil_spill_attributes$in_degree[oil_spill_attributes$total_rev_count_quar == 4])

sd(oil_spill_attributes$in_degree[oil_spill_attributes$total_rev_count_quar == 1])
sd(oil_spill_attributes$in_degree[oil_spill_attributes$total_rev_count_quar == 2])
sd(oil_spill_attributes$in_degree[oil_spill_attributes$total_rev_count_quar == 3])
sd(oil_spill_attributes$in_degree[oil_spill_attributes$total_rev_count_quar == 4])

# demi degré sortant en fonction du revcount + moyenne et sd

mean(oil_spill_attributes$out_degree[oil_spill_attributes$total_rev_count_quar == 1])
mean(oil_spill_attributes$out_degree[oil_spill_attributes$total_rev_count_quar == 2])
mean(oil_spill_attributes$out_degree[oil_spill_attributes$total_rev_count_quar == 3])
mean(oil_spill_attributes$out_degree[oil_spill_attributes$total_rev_count_quar == 4])

sd(oil_spill_attributes$out_degree[oil_spill_attributes$total_rev_count_quar == 1])
sd(oil_spill_attributes$out_degree[oil_spill_attributes$total_rev_count_quar == 2])
sd(oil_spill_attributes$out_degree[oil_spill_attributes$total_rev_count_quar == 3])
sd(oil_spill_attributes$out_degree[oil_spill_attributes$total_rev_count_quar == 4])

## Regressions

reg1 <- glm(added_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes)
summary(reg1)
1 - (reg1$deviance / reg1$null.deviance)

reg2 <- glm(deleted_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes)
summary(reg2)
1 - (reg2$deviance / reg2$null.deviance)

reg3 <- glm(reversed_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes)
summary(reg3)
1 - (reg3$deviance / reg3$null.deviance)

reg4 <- glm(in_degree ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes)
summary(reg4)
1 - (reg4$deviance / reg4$null.deviance)

reg5 <- glm(out_degree ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes)
summary(reg5)
1 - (reg5$deviance / reg5$null.deviance)

reg6 <- glm(degree_centrality ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes)
summary(reg6)
1 - (reg6$deviance / reg6$null.deviance)

#### Assortativité

assortativity.degree(oil_spill_graphe)

assortativity(oil_spill_graphe, oil_spill_attributes$status_contrib)

assortativity(oil_spill_graphe, oil_spill_attributes$total_rev_count_quar)