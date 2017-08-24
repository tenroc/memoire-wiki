#### Visualisation page_talk network ####

# Basique:

plot(ltalks_graphe[["Calodendrum_capense.csv_talk_edgelist"]],
     main ="Reseau editions Culture_War",
     layout=layout_nicely(ltalks_graphe[["Calodendrum_capense.csv_talk_edgelist"]]),
     edge.arrow.size=.1,
     vertex.label= NA,
     vertex.size = 3,
     rescale=TRUE)


calonendrum_capense_graphe <- ltalks_graphe[["Calodendrum_capense.csv_talk_edgelist"]]
calonendrum_capense_edgelist <- ltalks_edgelists[["Calodendrum_capense.csv_talk_edgelist"]]
temp <- append(as.character(calonendrum_capense_edgelist[,"V1"]),as.character(calonendrum_capense_edgelist[,"V2"]))
calonendrum_capense_attributes <- attributes_page[attributes_page$contributeurs %in% levels(as.factor(temp)),]

temp <- data.frame(as.character(labels(V(calonendrum_capense_graphe))))
colnames(temp) <- "contributeurs"
calonendrum_capense_attributes <- merge(temp,calonendrum_capense_attributes, by="contributeurs", all=T)

# Remettre les attributes dans l'ordre du graphe:

temp <- labels(V(calonendrum_capense_graphe))

temp <- temp[is.na(temp) == F]

calonendrum_capense_attributes <- calonendrum_capense_attributes[match(temp,calonendrum_capense_attributes$contributeurs),]
calonendrum_capense_attributes[is.na(calonendrum_capense_attributes$total_rev_count),"total_rev_count"] <- 1
calonendrum_capense_attributes[is.na(calonendrum_capense_attributes$registration_year),"registration_year"] <- 2018
calonendrum_capense_attributes[is.na(calonendrum_capense_attributes$status_contrib),"status_contrib"] <- "page_structure"

# Meilleure visualisation:

# Couleurs:

colors_edge <- c("#3892e0","#da4d45")
colors_nodes <- c("#8a4ebf","#f37329","#93d844","#333333")

V(calonendrum_capense_graphe)$color <- colors_nodes[as.factor(calonendrum_capense_attributes$status_contrib)]
V(calonendrum_capense_graphe)$size <- abs((as.numeric(calonendrum_capense_attributes$registration_year) - 2000) -20)
V(calonendrum_capense_graphe)$label <- NA

# Attention, grosse variation: peut être vaudrait mieux discrétiser la variable pour la rendre lisible?
E(calonendrum_capense_graphe)$width <- as.numeric(E(calonendrum_capense_graphe)$Charactercount)
# Marche pas, aucune idee de pourquoi:
# E(cultwar_edit_graphe)$edge.color <- colorsQ[as.numeric(E(cultwar_edit_graphe)$InteractionType_num)]

plot(calonendrum_capense_graphe, layout=layout_nicely(calonendrum_capense_graphe), rescale=TRUE,edge.arrow.size=.3,
     edge.color=colors_edge[as.numeric(E(calonendrum_capense_graphe)$DiscussionType_num)],
     main= "Reseau discussion Calodendrum Capense")

# Légende:

legend(x="topleft", c("Réponse","Nouveau fil de discussion","admin","anonyme","bot", "inscrit", "page_structure"), pch=c(24,24,21,21,21,21,21), col="#777777", 
       pt.bg= c("#3892e0","#da4d45","#8a4ebf","white","#f37329","#93d844", "#333333"), pt.cex=c(2,2,2,2,2,2,2), cex=.8, bty="n", ncol=1)
