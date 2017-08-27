#### Visualisation page_edit network ####

# Basique:

plot(ledits_graphe[["Calodendrum_capense.csv_edits_edgelist"]],
     main ="Reseau editions Culture_War",
     layout=layout_nicely(ledits_graphe[["Calodendrum_capense.csv_edits_edgelist"]]),
     edge.arrow.size=.1,
     vertex.label= NA,
     vertex.size = 3,
     rescale=TRUE)


calonendrum_capense_graphe <- ledits_graphe[["Calodendrum_capense.csv_edits_edgelist"]]
calonendrum_capense_edgelist <- ledits_edgelists[["Calodendrum_capense.csv_edits_edgelist"]]
temp <- append(calonendrum_capense_edgelist[,"V1"], calonendrum_capense_edgelist[,"V2"])
calonendrum_capense_attributes <- attributes_page[attributes_page$contributeurs %in% levels(as.factor(temp)),]

# Remettre les attributes dans l'ordre du graphe:

temp <- labels(V(calonendrum_capense_graphe))

temp <- temp[is.na(temp) == F]

calonendrum_capense_attributes <- calonendrum_capense_attributes[match(temp, calonendrum_capense_attributes$contributeurs),]
calonendrum_capense_attributes[calonendrum_capense_attributes$contributeurs == "page","total_rev_count"] <- 1
calonendrum_capense_attributes[calonendrum_capense_attributes$contributeurs == "page","registration_year"] <- 2001
# Meilleure visualisation:

# Couleurs:

colors_edge <- c("#3892e0","#da4d45", "#fbd25d")
colors_nodes <- c("#8a4ebf", "white","#f37329","#93d844","#333333")

V(calonendrum_capense_graphe)$color <- colors_nodes[as.factor(calonendrum_capense_attributes$status_contrib)]
V(calonendrum_capense_graphe)$size <- abs((as.numeric(calonendrum_capense_attributes$registration_year) - 2000) -20)
V(calonendrum_capense_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(calonendrum_capense_graphe)$width <- as.numeric(E(calonendrum_capense_graphe)$Wordcount)/5
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(calonendrum_capense_graphe, layout=layout_nicely(calonendrum_capense_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(calonendrum_capense_graphe)$InteractionType_num)],
     main= "Reseau editions Calodendrum Capense")

# Légende:

legend(x="topleft", c("Ajout","Suppression","Reverse","admin","anonyme","bot", "inscrit", "page","ancienneté -", "ancienneté +"), pch=c(24,24,24,21,21,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#fbd25d","#8a4ebf","white","#f37329","#93d844", "#333333", "white", "white"), pt.cex=c(2,2,2,2,2,2,2,2,1,4), cex=.8, bty="n", ncol=1)
