#### Visualisation ####

# Basique:

plot(cultwar_edit_graphe,
     main ="Reseau editions Culture_War",
     layout=layout_nicely(cultwar_edit_graphe),
     edge.arrow.size=.1,
     vertex.label= NA,
     vertex.size = 3,
     rescale=TRUE)


# Meilleure visualisation:

# Couleurs:

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("white","#f37329","#93d844","#333333")

V(cultwar_edit_graphe)$color <- colors_nodes[as.factor(attributes$status_contrib)]
V(cultwar_edit_graphe)$size <- as.numeric(attributes$registration_year) - 2000
V(cultwar_edit_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(cultwar_edit_graphe)$width <- as.numeric(E(cultwar_edit_graphe)$Wordcount)/40
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(cultwar_edit_graphe, layout=layout_nicely(cultwar_edit_graphe), rescale=TRUE,edge.arrow.size=.1,
     edge.color=colors_edge[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)],
     main= "Reseau editions Culture_War")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse", "anonyme", "inscrit", "page"), pch=c(24,24,24,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#f37329","#93d844", "#333333"), pt.cex=c(2,2,2,2,2,2), cex=.8, bty="n", ncol=1)

