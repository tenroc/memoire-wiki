#### Visualisation contro_edit network ####

# Basique:

plot(ltalks_graphe[["Oil_spill.csv_talk_edgelist"]],
     main ="Reseau discussion Oil spill",
     layout=layout_nicely(ltalks_graphe[["Oil_spill.csv_talk_edgelist"]]),
     edge.arrow.size=.1,
     vertex.label= NA,
     vertex.size = 3,
     rescale=TRUE)


oil_spill_graphe_talk <- ltalks_graphe[["Oil_spill.csv_talk_edgelist"]]
oil_spill_edgelist_talk <- ltalks_edgelists[["Oil_spill.csv_talk_edgelist"]]
temp <- append(oil_spill_edgelist_talk[,"V1"], oil_spill_edgelist_talk[,"V2"])
oil_spill_attributes_talk <- attributes_contro[attributes_contro$contributeurs %in% levels(as.factor(temp)),]
a <- levels(as.factor(oil_spill_edgelist_talk$V2[grepl("^==" ,oil_spill_edgelist_talk[,"V2"])]))
a <- data.frame(a)
a[,2:7] <- NA
a <- subset(a[,c(2,3,1,4:7)])
colnames(a) <- colnames(oil_spill_attributes_talk)
oil_spill_attributes_talk <- rbind(oil_spill_attributes_talk, a)

# Remettre les attributes dans l'ordre du graphe:

temp <- labels(V(oil_spill_graphe_talk))

temp <- temp[is.na(temp) == F]

oil_spill_attributes_talk <- oil_spill_attributes_talk[match(temp, oil_spill_attributes_talk$contributeurs),]
oil_spill_attributes_talk[grepl("^==" ,oil_spill_attributes_talk$contributeurs) == T,"total_rev_count"] <- 1
oil_spill_attributes_talk[grepl("^==" ,oil_spill_attributes_talk$contributeurs) == T,"registration_year"] <- 2001
oil_spill_attributes_talk$status_contrib <- as.character(oil_spill_attributes_talk$status_contrib)
oil_spill_attributes_talk[grepl("^==" ,oil_spill_attributes_talk$contributeurs) == T,"status_contrib"] <- "structure_page"
oil_spill_attributes_talk[grepl("^==" ,oil_spill_attributes_talk$contributeurs) == T,"isanon"] <- 0
oil_spill_attributes_talk[is.na(oil_spill_attributes_talk$total_rev_count) == T,"total_rev_count"] <- 1
oil_spill_attributes_talk[is.na(oil_spill_attributes_talk$registration_year) == T,"registration_year"] <- 2018
oil_spill_attributes_talk <- unique(oil_spill_attributes_talk)
## Première visualisation:

# Couleurs:

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(oil_spill_graphe_talk)$color <- colors_nodes[as.factor(oil_spill_attributes_talk$status_contrib)]
V(oil_spill_graphe_talk)$size <- abs((as.numeric(oil_spill_attributes_talk$registration_year) - 2000) -20)/3
V(oil_spill_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe_talk)$width <- 1 #as.numeric(E(oil_spill_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(oil_spill_graphe_talk, layout=layout_nicely(oil_spill_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Oil spill")

# Légende:

legend(x="topleft", c("Initialisation conversation","Réponse","admin","anonyme","bot", "inscrit", "page","ancienneté -", "ancienneté +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)

## Stats de base:

centralization.degree(oil_spill_graphe_talk)
transitivity(oil_spill_graphe_talk)
edge_density(oil_spill_graphe_talk)
vcount(oil_spill_graphe_talk)
ecount(oil_spill_graphe_talk)
table (oil_spill_edgelist_talk$DiscussionType)
table(oil_spill_attributes_talk$status_contrib)

oil_spill_attributes_talk$degree_centrality <- degree(oil_spill_graphe_talk)
boxplot(oil_spill_attributes_talk$degree_centrality[grepl("^==" ,oil_spill_attributes_talk$contributeurs) == F])
mean(oil_spill_attributes_talk$degree_centrality)
sd(oil_spill_attributes_talk$degree_centrality)

oil_spill_attributes_talk$betweenness_centrality <- betweenness(oil_spill_graphe_talk)
boxplot(oil_spill_attributes_talk$betweenness_centrality[grepl("^==" ,oil_spill_attributes_talk$contributeurs) == F])


oil_spill_attributes_talk$in_degree <- degree(oil_spill_graphe_talk, mode = "in")
boxplot(oil_spill_attributes_talk$in_degree[grepl("^==" ,oil_spill_attributes_talk$contributeurs) == F])
mean(oil_spill_attributes_talk$in_degree)
sd(oil_spill_attributes_talk$in_degree)

oil_spill_attributes_talk$out_degree <- degree(oil_spill_graphe_talk, mode = "out")
boxplot(oil_spill_attributes_talk$out_degree[grepl("^==" ,oil_spill_attributes_talk$contributeurs) == F])
mean(oil_spill_attributes_talk$out_degree)
sd(oil_spill_attributes_talk$out_degree)

mean(oil_spill_attributes_talk$total_rev_count)
sd(oil_spill_attributes_talk$total_rev_count)
boxplot(oil_spill_attributes_talk$total_rev_count)
quint <- quantile(oil_spill_attributes_talk$total_rev_count, seq(0,1,0.25))

oil_spill_attributes_talk$total_rev_count_quar[oil_spill_attributes_talk$total_rev_count <= as.numeric(quint[2])] <- 1
oil_spill_attributes_talk$total_rev_count_quar[oil_spill_attributes_talk$total_rev_count > as.numeric(quint[2]) & oil_spill_attributes_talk$total_rev_count < as.numeric(quint[3])] <- 2
oil_spill_attributes_talk$total_rev_count_quar[oil_spill_attributes_talk$total_rev_count >= as.numeric(quint[3]) & oil_spill_attributes_talk$total_rev_count < as.numeric(quint[4])] <- 3
oil_spill_attributes_talk$total_rev_count_quar[oil_spill_attributes_talk$total_rev_count >= as.numeric(quint[4])] <- 4

table(oil_spill_attributes_talk$total_rev_count_quar)

#### Deuxième représentation graphique: 

#### Size = Degree centrality

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(oil_spill_graphe_talk)$color <- colors_nodes[as.factor(oil_spill_attributes_talk$status_contrib)]
V(oil_spill_graphe_talk)$size <- log(oil_spill_attributes_talk$degree_centrality)*3
V(oil_spill_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe_talk)$width <- 1 #as.numeric(E(oil_spill_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(oil_spill_graphe_talk, layout=layout_nicely(oil_spill_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Oil spill")

# Légende:

legend(x="topleft", c("Initialisation conversation","Réponse","admin","anonyme","bot", "inscrit", "page","degrés -", "degrés +"), pch=c(24,24,21,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Indegree

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(oil_spill_graphe_talk)$color <- colors_nodes[as.factor(oil_spill_attributes_talk$status_contrib)]
V(oil_spill_graphe_talk)$size <- log(oil_spill_attributes_talk$in_degree)*3
V(oil_spill_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe_talk)$width <- 1 #as.numeric(E(oil_spill_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(oil_spill_graphe_talk, layout=layout_nicely(oil_spill_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Oil spill")

# Légende:

legend(x="topleft", c("Initialisation conversation","Reverse","admin","anonyme","bot", "inscrit", "page","degrés entrant -", "degrés entrant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Out degree

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(oil_spill_graphe_talk)$color <- colors_nodes[as.factor(oil_spill_attributes_talk$status_contrib)]
V(oil_spill_graphe_talk)$size <- log(oil_spill_attributes_talk$degree_centrality)*3
V(oil_spill_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe_talk)$width <- 1 #as.numeric(E(oil_spill_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(oil_spill_graphe_talk, layout=layout.fruchterman.reingold, rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Oil spill")

# Légende:

legend(x="topleft", c("Initialisation conversation","Reverse","admin","anonyme","bot", "inscrit", "page","degrés sortant -", "degrès sortant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Indegree, col = total_rev_count

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- heat.colors(n = 4)
colors_nodes[5] <- "#333333"

V(oil_spill_graphe_talk)$color <- colors_nodes[as.factor(oil_spill_attributes_talk$total_rev_count_quar)]
V(oil_spill_graphe_talk)$size <- log(oil_spill_attributes_talk$in_degree) *3
V(oil_spill_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe_talk)$width <- 1 #as.numeric(E(oil_spill_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(oil_spill_graphe_talk, layout=layout_nicely(oil_spill_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Oil spill")

# Légende:

legend(x="topleft", c("Initialisation conversation","Reverse","Nombre total d'édition -","Nombre total d'éditions +", "page","degrés entrant -", "degrés entrant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d", colors_nodes[4], colors_nodes[1], "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Out degree col= total rev count

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- heat.colors(n = 4)
colors_nodes[5] <- "#333333"

V(oil_spill_graphe_talk)$color <- colors_nodes[as.factor(oil_spill_attributes_talk$total_rev_count_quar)]
V(oil_spill_graphe_talk)$size <- oil_spill_attributes_talk$out_degree/10
V(oil_spill_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe_talk)$width <- 1 #as.numeric(E(oil_spill_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(oil_spill_graphe_talk, layout=layout_nicely(oil_spill_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Oil spill")

# Légende:

legend(x="topleft", c("Initialisation conversation","Réponse","Reverse","admin","anonyme","bot", "inscrit", "page","degrés sortant -", "degrès sortant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = degree_centrality col= total rev count

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- heat.colors(n = 4)
colors_nodes[5] <- "#333333"

V(oil_spill_graphe_talk)$color <- colors_nodes[as.factor(oil_spill_attributes_talk$total_rev_count_quar)]
V(oil_spill_graphe_talk)$size <- log(oil_spill_attributes_talk$degree_centrality)
V(oil_spill_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(oil_spill_graphe_talk)$width <- 1 #as.numeric(E(oil_spill_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(oil_spill_graphe_talk, layout=layout.fruchterman.reingold, rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(oil_spill_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Oil spill")

# Légende:

legend(x="topleft", c("Initialisation conversation","Réponse","admin","anonyme","bot", "inscrit", "page","degrés", "degrès"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


## Représentations bivariées

# Degrés par rapport au décompte total d'edits.

plot(oil_spill_attributes_talk$degree_centrality, oil_spill_attributes_talk$total_rev_count)

plot(oil_spill_attributes_talk$in_degree, oil_spill_attributes_talk$total_rev_count)

plot(oil_spill_attributes_talk$out_degree, oil_spill_attributes_talk$total_rev_count)

# Tableau croisement total rev count / status_contrib

table(oil_spill_attributes_talk$status_contrib, oil_spill_attributes_talk$total_rev_count_quar)[c(1:4),c(1:4)]

# Rattacher le nombre total d'édition efféctuées au sein de ce réseau par éditeur

for (i in oil_spill_attributes_talk$contributeurs){
  oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$contributeurs == i] <- length(oil_spill_edgelist_talk$V1[oil_spill_edgelist_talk$V1 == i])
  oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$contributeurs == i] <- length(oil_spill_edgelist_talk$V1[oil_spill_edgelist_talk$V1 == i & oil_spill_edgelist_talk$DiscussionType == "initialized_thread"])
  oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$contributeurs == i] <- length(oil_spill_edgelist_talk$V1[oil_spill_edgelist_talk$V1 == i & oil_spill_edgelist_talk$DiscussionType == "replied_to"])
}

boxplot(oil_spill_attributes_talk$total_rev_count_local)

# Nombre de contributions en fontion du statut + moyenne et sd

sum(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$status_contrib == "admin"])
sum(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$status_contrib == "inscrit"])
sum(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$status_contrib == "anonyme"])

mean(oil_spill_attributes_talk$total_rev_count_local)
mean(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$status_contrib == "admin"])
mean(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$status_contrib == "inscrit"])
mean(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$status_contrib == "anonyme"])

sd(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$status_contrib == "admin"])
sd(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$status_contrib == "inscrit"])
sd(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$status_contrib == "anonyme"])

# Nombre d'initialisation en fontion du statut + moyenne et sd

sum(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$status_contrib == "admin"])
sum(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$status_contrib == "inscrit"])
sum(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$status_contrib == "anonyme"])

mean(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$status_contrib == "admin"])
mean(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$status_contrib == "inscrit"])
mean(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$status_contrib == "anonyme"])

sd(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$status_contrib == "admin"])
sd(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$status_contrib == "inscrit"])
sd(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$status_contrib == "anonyme"])

# Nombre de réponses en fontion du statut + moyenne et sd

sum(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$status_contrib == "admin"])
sum(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$status_contrib == "inscrit"])
sum(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$status_contrib == "anonyme"])

mean(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$status_contrib == "admin"])
mean(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$status_contrib == "inscrit"])
mean(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$status_contrib == "anonyme"])

sd(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$status_contrib == "admin"])
sd(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$status_contrib == "inscrit"])
sd(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$status_contrib == "anonyme"])

# Nombre de contributions en fonction du revcount + moyenne et sd

length(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
length(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
length(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
length(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])

mean(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
mean(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
mean(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
mean(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])

length(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
length(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
length(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
length(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])

sd(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
sd(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
sd(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
sd(oil_spill_attributes_talk$total_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])


# Nombre d'initialisation en fonction du revcount + moyenne et sd

length(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
length(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
length(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
length(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])

mean(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
mean(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
mean(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
mean(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])

sd(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
sd(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
sd(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
sd(oil_spill_attributes_talk$initialized_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])

# Nombre de réponses en fonction du revcount + moyenne et sd

length(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
length(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
length(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
length(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])

mean(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
mean(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
mean(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
mean(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])

sd(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 1])
sd(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 2])
sd(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 3])
sd(oil_spill_attributes_talk$responded_rev_count_local[oil_spill_attributes_talk$total_rev_count_quar == 4])

## Regression contrib_type = status_contrib + total_rev_count

oil_spill_attributes_talk$status_contrib <- relevel(as.factor(oil_spill_attributes_talk$status_contrib), ref="anonyme")

reg1 <- glm(initialized_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes_talk)
summary(reg1)

# pseudo R2:

1 - (reg1$deviance / reg1$null.deviance)

reg2 <- glm(responded_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes_talk)
summary(reg2)

# Pseudo R2

1 - (reg2$deviance / reg2$null.deviance)

reg4 <- glm(in_degree ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes_talk)
summary(reg4)

# Pseudo R2

1 - (reg4$deviance / reg4$null.deviance)

reg5 <- glm(out_degree ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes_talk)
summary(reg5)

# Pseudo R2

1 - (reg5$deviance / reg5$null.deviance)

reg6 <- glm(degree_centrality ~ status_contrib +  as.character(total_rev_count_quar), data=oil_spill_attributes_talk)
summary(reg6)

# Pseudo R2

1 - (reg6$deviance / reg6$null.deviance)

## chaîne maximale de conversation

max(oil_spill_edgelist_talk$IndexInThread)
mean(oil_spill_edgelist_talk$IndexInThread)
sd(oil_spill_edgelist_talk$IndexInThread)
boxplot(oil_spill_edgelist_talk$IndexInThread)


#### Assortativité

assortativity.degree(oil_spill_graphe_talk)

assortativity(oil_spill_graphe_talk, as.factor(oil_spill_attributes_talk$status_contrib))

assortativity(oil_spill_graphe_talk, as.factor(oil_spill_attributes_talk$total_rev_count_quar))