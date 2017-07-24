## Script Test (import, recodage, visualisation): à automatiser (+ modulariser)

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

attributes$status_contrib[attributes$status_anon == 0] <- "anonyme"
attributes$status_contrib[attributes$status_anon == 1] <- "inscrit"
attributes$status_contrib[attributes$contributeurs == "cultwar_page"] <- "page_edits"

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
colors_nodes <- c("#f37329","#93d844","#333333")

V(cultwar_edit_graphe)$color <- colors_nodes[as.factor(attributes$status_contrib)]
V(cultwar_edit_graphe)$size <- 3
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

## Stats descriptives générales: ####

# Densité:

