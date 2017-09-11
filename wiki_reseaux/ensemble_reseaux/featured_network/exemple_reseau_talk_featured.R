#### Visualisation contro_edit network ####

# Basique:

plot(ltalks_graphe[["Death_Valley_National_Park.csv_talk_edgelist"]],
     main ="Reseau discussion Death valley national park",
     layout=layout_nicely(ltalks_graphe[["Death_Valley_National_Park.csv_talk_edgelist"]]),
     edge.arrow.size=.1,
     vertex.label= NA,
     vertex.size = 3,
     rescale=TRUE)


death_valley_graphe_talk <- ltalks_graphe[["Death_Valley_National_Park.csv_talk_edgelist"]]
death_valley_edgelist_talk <- ltalks_edgelists[["Death_Valley_National_Park.csv_talk_edgelist"]]
temp <- append(death_valley_edgelist_talk[,"V1"], death_valley_edgelist_talk[,"V2"])
death_valley_attributes_talk <- attributes_featured[attributes_featured$contributeurs %in% levels(as.factor(temp)),]
a <- levels(as.factor(death_valley_edgelist_talk$V2[grepl("^==" ,death_valley_edgelist_talk[,"V2"])]))
a <- data.frame(a)
a[,2:6] <- NA
a <- subset(a[,c(2,1,3:6)])
colnames(a) <- colnames(death_valley_attributes_talk)
death_valley_attributes_talk <- rbind(death_valley_attributes_talk, a)

# Remettre les attributes dans l'ordre du graphe:

temp <- labels(V(death_valley_graphe_talk))

temp <- temp[is.na(temp) == F]

death_valley_attributes_talk <- death_valley_attributes_talk[match(temp, death_valley_attributes_talk$contributeurs),]
death_valley_attributes_talk[grepl("^==" ,death_valley_attributes_talk$contributeurs) == T,"total_rev_count"] <- 1
death_valley_attributes_talk[grepl("^==" ,death_valley_attributes_talk$contributeurs) == T,"registration_year"] <- 2001
death_valley_attributes_talk$status_contrib <- as.character(death_valley_attributes_talk$status_contrib)
death_valley_attributes_talk[grepl("^==" ,death_valley_attributes_talk$contributeurs) == T,"status_contrib"] <- "structure_page"
death_valley_attributes_talk[grepl("^==" ,death_valley_attributes_talk$contributeurs) == T,"isanon"] <- 0
death_valley_attributes_talk[is.na(death_valley_attributes_talk$total_rev_count) == T,"total_rev_count"] <- 1
death_valley_attributes_talk[is.na(death_valley_attributes_talk$registration_year) == T,"registration_year"] <- 2018
death_valley_attributes_talk <- unique(death_valley_attributes_talk)
## Première visualisation:

# Couleurs:

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(death_valley_graphe_talk)$color <- colors_nodes[as.factor(death_valley_attributes_talk$status_contrib)]
V(death_valley_graphe_talk)$size <- abs((as.numeric(death_valley_attributes_talk$registration_year) - 2000) -20)/3
V(death_valley_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe_talk)$width <- 1 #as.numeric(E(death_valley_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(death_valley_graphe_talk, layout=layout_nicely(death_valley_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Death valley national park")

# Légende:

legend(x="topleft", c("Initialisation conversation","Réponse","admin","anonyme","bot", "inscrit", "page","ancienneté -", "ancienneté +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)

## Stats de base:

centralization.degree(death_valley_graphe_talk)
transitivity(death_valley_graphe_talk)
edge_density(death_valley_graphe_talk)
vcount(death_valley_graphe_talk)
ecount(death_valley_graphe_talk)
table (death_valley_edgelist_talk$DiscussionType)
table(death_valley_attributes_talk$status_contrib)

death_valley_attributes_talk$degree_centrality <- degree(death_valley_graphe_talk)
boxplot(death_valley_attributes_talk$degree_centrality[grepl("^==" ,death_valley_attributes_talk$contributeurs) == F])
mean(death_valley_attributes_talk$degree_centrality)
sd(death_valley_attributes_talk$degree_centrality)

death_valley_attributes_talk$betweenness_centrality <- betweenness(death_valley_graphe_talk)
boxplot(death_valley_attributes_talk$betweenness_centrality[grepl("^==" ,death_valley_attributes_talk$contributeurs) == F])


death_valley_attributes_talk$in_degree <- degree(death_valley_graphe_talk, mode = "in")
boxplot(death_valley_attributes_talk$in_degree[grepl("^==" ,death_valley_attributes_talk$contributeurs) == F])
mean(death_valley_attributes_talk$in_degree)
sd(death_valley_attributes_talk$in_degree)

death_valley_attributes_talk$out_degree <- degree(death_valley_graphe_talk, mode = "out")
boxplot(death_valley_attributes_talk$out_degree[grepl("^==" ,death_valley_attributes_talk$contributeurs) == F])
mean(death_valley_attributes_talk$out_degree)
sd(death_valley_attributes_talk$out_degree)

mean(death_valley_attributes_talk$total_rev_count)
sd(death_valley_attributes_talk$total_rev_count)
boxplot(death_valley_attributes_talk$total_rev_count)
quint <- quantile(death_valley_attributes_talk$total_rev_count[death_valley_attributes_talk$status_contrib != "structure_page"], seq(0,1,0.25))

death_valley_attributes_talk$total_rev_count_quar[death_valley_attributes_talk$total_rev_count <= as.numeric(quint[2])] <- 1
death_valley_attributes_talk$total_rev_count_quar[death_valley_attributes_talk$total_rev_count > as.numeric(quint[2]) & death_valley_attributes_talk$total_rev_count < as.numeric(quint[3])] <- 2
death_valley_attributes_talk$total_rev_count_quar[death_valley_attributes_talk$total_rev_count >= as.numeric(quint[3]) & death_valley_attributes_talk$total_rev_count < as.numeric(quint[4])] <- 3
death_valley_attributes_talk$total_rev_count_quar[death_valley_attributes_talk$total_rev_count >= as.numeric(quint[4])] <- 4

table(death_valley_attributes_talk$total_rev_count_quar)

#### Deuxième représentation graphique: 

#### Size = Degree centrality

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- c("#8a4ebf", "white","#93d844","#333333","#333333")

V(death_valley_graphe_talk)$color <- colors_nodes[as.factor(death_valley_attributes_talk$status_contrib)]
V(death_valley_graphe_talk)$size <- log(death_valley_attributes_talk$degree_centrality)*6
V(death_valley_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe_talk)$width <- 1 #as.numeric(E(death_valley_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(death_valley_graphe_talk, layout=layout_nicely(death_valley_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Death valley national park")

# Légende:

legend(x="topleft", c("Initialisation conversation","Réponse","admin","anonyme","bot", "inscrit", "page","centralité degrés -", "centralité degrés +"), pch=c(24,24,21,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Indegree

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(death_valley_graphe_talk)$color <- colors_nodes[as.factor(death_valley_attributes_talk$status_contrib)]
V(death_valley_graphe_talk)$size <- log(death_valley_attributes_talk$in_degree)*3
V(death_valley_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe_talk)$width <- 1 #as.numeric(E(death_valley_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(death_valley_graphe_talk, layout=layout_nicely(death_valley_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Death valley national park")

# Légende:

legend(x="topleft", c("Initialisation conversation","Reverse","admin","anonyme","bot", "inscrit", "page","degrés entrant -", "degrés entrant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Out degree

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(death_valley_graphe_talk)$color <- colors_nodes[as.factor(death_valley_attributes_talk$status_contrib)]
V(death_valley_graphe_talk)$size <- log(death_valley_attributes_talk$degree_centrality)*3
V(death_valley_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe_talk)$width <- 1 #as.numeric(E(death_valley_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(death_valley_graphe_talk, layout=layout.fruchterman.reingold, rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Death valley national park")

# Légende:

legend(x="topleft", c("Initialisation conversation","Reverse","admin","anonyme","bot", "inscrit", "page","degrés sortant -", "degrès sortant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Indegree, col = total_rev_count

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- heat.colors(n = 4)
colors_nodes[5] <- "#333333"

V(death_valley_graphe_talk)$color <- colors_nodes[as.factor(death_valley_attributes_talk$total_rev_count_quar)]
V(death_valley_graphe_talk)$size <- log(death_valley_attributes_talk$in_degree) *3
V(death_valley_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe_talk)$width <- 1 #as.numeric(E(death_valley_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(death_valley_graphe_talk, layout=layout_nicely(death_valley_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Death valley national park")

# Légende:

legend(x="topleft", c("Initialisation conversation","Reverse","Nombre total d'édition -","Nombre total d'éditions +", "page","degrés entrant -", "degrés entrant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d", colors_nodes[4], colors_nodes[1], "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = Out degree col= total rev count

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- heat.colors(n = 4)
colors_nodes[5] <- "#333333"

V(death_valley_graphe_talk)$color <- colors_nodes[as.factor(death_valley_attributes_talk$total_rev_count_quar)]
V(death_valley_graphe_talk)$size <- death_valley_attributes_talk$out_degree/10
V(death_valley_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe_talk)$width <- 1 #as.numeric(E(death_valley_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(death_valley_graphe_talk, layout=layout_nicely(death_valley_graphe_talk), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Death valley national park")

# Légende:

legend(x="topleft", c("Initialisation conversation","Réponse","Reverse","admin","anonyme","bot", "inscrit", "page","degrés sortant -", "degrès sortant +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


#### Size = degree_centrality col= total rev count

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- heat.colors(n = 4)
colors_nodes[5] <- "#333333"

V(death_valley_graphe_talk)$color <- colors_nodes[as.factor(death_valley_attributes_talk$total_rev_count_quar)]
V(death_valley_graphe_talk)$size <- log(death_valley_attributes_talk$degree_centrality)
V(death_valley_graphe_talk)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(death_valley_graphe_talk)$width <- 1 #as.numeric(E(death_valley_graphe_talk)$CharacterCount)/20
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$DiscussionType_num)]

plot(death_valley_graphe_talk, layout=layout.fruchterman.reingold, rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(death_valley_graphe_talk)$DiscussionType_num)],
     main= "Reseau discussion Death valley national park")

# Légende:

legend(x="topleft", c("Initialisation conversation","Réponse","admin","anonyme","bot", "inscrit", "page","degrés", "degrès"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)


## Représentations bivariées

# Degrés par rapport au décompte total d'edits.

plot(death_valley_attributes_talk$degree_centrality, death_valley_attributes_talk$total_rev_count)

plot(death_valley_attributes_talk$in_degree, death_valley_attributes_talk$total_rev_count)

plot(death_valley_attributes_talk$out_degree, death_valley_attributes_talk$total_rev_count)

# Tableau croisement total rev count / status_contrib

table(death_valley_attributes_talk$status_contrib, death_valley_attributes_talk$total_rev_count_quar)[c(1:4),c(1:4)]

# Rattacher le nombre total d'édition efféctuées au sein de ce réseau par éditeur

for (i in death_valley_attributes_talk$contributeurs){
  death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$contributeurs == i] <- length(death_valley_edgelist_talk$V1[death_valley_edgelist_talk$V1 == i])
  death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$contributeurs == i] <- length(death_valley_edgelist_talk$V1[death_valley_edgelist_talk$V1 == i & death_valley_edgelist_talk$DiscussionType == "initialized_thread"])
  death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$contributeurs == i] <- length(death_valley_edgelist_talk$V1[death_valley_edgelist_talk$V1 == i & death_valley_edgelist_talk$DiscussionType == "replied_to"])
}

boxplot(death_valley_attributes_talk$total_rev_count_local)

# Nombre de contributions en fontion du statut + moyenne et sd

sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$status_contrib == "admin"])
sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$status_contrib == "inscrit"])
sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$status_contrib == "anonyme"])

mean(death_valley_attributes_talk$total_rev_count_local)
mean(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$status_contrib == "admin"])
mean(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$status_contrib == "inscrit"])
mean(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$status_contrib == "anonyme"])

sd(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$status_contrib == "admin"])
sd(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$status_contrib == "inscrit"])
sd(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$status_contrib == "anonyme"])

# Nombre d'initialisation en fontion du statut + moyenne et sd

sum(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$status_contrib == "admin"])
sum(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$status_contrib == "inscrit"])
sum(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$status_contrib == "anonyme"])

mean(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$status_contrib == "admin"])
mean(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$status_contrib == "inscrit"])
mean(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$status_contrib == "anonyme"])

sd(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$status_contrib == "admin"])
sd(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$status_contrib == "inscrit"])
sd(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$status_contrib == "anonyme"])

# Nombre de réponses en fontion du statut + moyenne et sd

sum(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$status_contrib == "admin"])
sum(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$status_contrib == "inscrit"])
sum(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$status_contrib == "anonyme"])

mean(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$status_contrib == "admin"])
mean(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$status_contrib == "inscrit"])
mean(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$status_contrib == "anonyme"])

sd(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$status_contrib == "admin"])
sd(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$status_contrib == "inscrit"])
sd(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$status_contrib == "anonyme"])

# centralité degré en fonction du statut: moyenne et sd

mean(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$status_contrib == "admin"])
mean(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$status_contrib == "inscrit"])
mean(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$status_contrib == "anonyme"])

sd(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$status_contrib == "admin"])
sd(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$status_contrib == "inscrit"])
sd(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$status_contrib == "anonyme"])

# indegree en fonction du statut: moyenne et sd

mean(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$status_contrib == "admin"])
mean(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$status_contrib == "inscrit"])
mean(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$status_contrib == "anonyme"])

sd(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$status_contrib == "admin"])
sd(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$status_contrib == "inscrit"])
sd(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$status_contrib == "anonyme"])

# out degree en fonction du statut: moyenne et sd

mean(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$status_contrib == "admin"])
mean(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$status_contrib == "inscrit"])
mean(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$status_contrib == "anonyme"])

sd(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$status_contrib == "admin"])
sd(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$status_contrib == "inscrit"])
sd(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$status_contrib == "anonyme"])


# Nombre de contributions en fonction du revcount + moyenne et sd

sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])

mean(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
mean(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
mean(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
mean(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])

sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
sum(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])

sd(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
sd(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
sd(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
sd(death_valley_attributes_talk$total_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])


# Nombre d'initialisation en fonction du revcount + moyenne et sd

sum(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
sum(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
sum(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
sum(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])

mean(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
mean(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
mean(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
mean(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])

sd(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
sd(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
sd(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
sd(death_valley_attributes_talk$initialized_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])

# Nombre de réponses en fonction du revcount + moyenne et sd

sum(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
sum(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
sum(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
sum(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])

mean(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
mean(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
mean(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
mean(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])

sd(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 1])
sd(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 2])
sd(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 3])
sd(death_valley_attributes_talk$responded_rev_count_local[death_valley_attributes_talk$total_rev_count_quar == 4])


# Centralité de degré en fonction du rev_count : moyenne + sd

mean(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$total_rev_count_quar == 1])
mean(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$total_rev_count_quar == 2])
mean(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$total_rev_count_quar == 3])
mean(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$total_rev_count_quar == 4])

sd(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$total_rev_count_quar == 1])
sd(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$total_rev_count_quar == 2])
sd(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$total_rev_count_quar == 3])
sd(death_valley_attributes_talk$degree_centrality[death_valley_attributes_talk$total_rev_count_quar == 4])

# indegree en fonction du rev_count : moyenne + sd

mean(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$total_rev_count_quar == 1])
mean(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$total_rev_count_quar == 2])
mean(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$total_rev_count_quar == 3])
mean(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$total_rev_count_quar == 4])

sd(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$total_rev_count_quar == 1])
sd(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$total_rev_count_quar == 2])
sd(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$total_rev_count_quar == 3])
sd(death_valley_attributes_talk$in_degree[death_valley_attributes_talk$total_rev_count_quar == 4])

# outdegree en fonction du rev_count : moyenne + sd

mean(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$total_rev_count_quar == 1])
mean(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$total_rev_count_quar == 2])
mean(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$total_rev_count_quar == 3])
mean(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$total_rev_count_quar == 4])

sd(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$total_rev_count_quar == 1])
sd(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$total_rev_count_quar == 2])
sd(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$total_rev_count_quar == 3])
sd(death_valley_attributes_talk$out_degree[death_valley_attributes_talk$total_rev_count_quar == 4])

table(death_valley_attributes_talk$total_rev_count_quar, death_valley_attributes_talk$status_contrib)

## Regression contrib_type = status_contrib + total_rev_count

death_valley_attributes_talk$status_contrib <- relevel(as.factor(death_valley_attributes_talk$status_contrib), ref="anonyme")

reg1 <- glm(initialized_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes_talk)
summary(reg1)

# pseudo R2:

1 - (reg1$deviance / reg1$null.deviance)

reg2 <- glm(responded_rev_count_local ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes_talk)
summary(reg2)

# Pseudo R2

1 - (reg2$deviance / reg2$null.deviance)

reg4 <- glm(in_degree ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes_talk)
summary(reg4)

# Pseudo R2

1 - (reg4$deviance / reg4$null.deviance)

reg5 <- glm(out_degree ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes_talk)
summary(reg5)

# Pseudo R2

1 - (reg5$deviance / reg5$null.deviance)

reg6 <- glm(degree_centrality ~ status_contrib +  as.character(total_rev_count_quar), data=death_valley_attributes_talk)
summary(reg6)

# Pseudo R2

1 - (reg6$deviance / reg6$null.deviance)

## chaîne maximale de conversation

max(as.numeric(death_valley_edgelist_talk$IndexInThread), na.rm =T)
mean(death_valley_edgelist_talk$IndexInThread, na.rm =T)
sd(death_valley_edgelist_talk$IndexInThread, na.rm =T)
boxplot(death_valley_edgelist_talk$IndexInThread)


#### Assortativité

assortativity.degree(death_valley_graphe_talk)

assortativity(death_valley_graphe_talk, as.factor(death_valley_attributes_talk$status_contrib))

assortativity(death_valley_graphe_talk, as.factor(death_valley_attributes_talk$total_rev_count_quar))