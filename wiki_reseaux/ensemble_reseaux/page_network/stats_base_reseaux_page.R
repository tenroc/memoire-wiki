## Statistiques de base sur les attributs des réseaux talk_page et edits_page:

## Quelques visualisation générales de la structure des réseaux: edits

## Moyennes centralisation, centralité de dégrés, et centralité d'intermédiarité, en général, et entre administrateurs:

boxplot(as.numeric(page_attributes_edits$centralization))
boxplot(as.numeric(page_attributes_edits$mean_betweenness),as.numeric(page_attributes_edits$mean_betweenness_admin))
boxplot(as.numeric(page_attributes_edits$mean_degree_centrality), as.numeric(page_attributes_edits$mean_degree_centrality_admins))

boxplot(as.numeric(page_attributes_edits$mean_betweenness_admin))
boxplot(as.numeric(page_attributes_edits$mean_degree_centrality_admins))

mean(as.numeric(page_attributes_edits$mean_degree_centrality))
mean(as.numeric(page_attributes_edits$mean_degree_centrality_admins), na.rm = T)
sd(as.numeric(page_attributes_edits$mean_degree_centrality))
sd(as.numeric(page_attributes_edits$mean_degree_centrality_admins), na.rm = T)

mean(as.numeric(page_attributes_edits$mean_betweenness))
mean(as.numeric(page_attributes_edits$mean_betweenness_admin),na.rm = T)
sd(as.numeric(page_attributes_edits$mean_betweenness))
sd(as.numeric(page_attributes_edits$mean_betweenness_admin),na.rm = T)

## Pareil mais pour les déviations standards:

boxplot(as.numeric(page_attributes_edits$sd_betweenness),as.numeric(page_attributes_edits$sd_betweenness_admin))
boxplot(as.numeric(page_attributes_edits$sd_degree_centrality), as.numeric(page_attributes_edits$sd_degree_centrality_admins))

boxplot(as.numeric(page_attributes_edits$mean_betweenness_admin))
boxplot(as.numeric(page_attributes_edits$mean_degree_centrality_admins))

## stats distribution nombre de... dans les réseaux

# admins

boxplot(as.numeric(page_attributes_edits$nadmins))
mean(as.numeric(page_attributes_edits$nadmins))
sd(as.numeric(page_attributes_edits$nadmins))

# Anonymes

boxplot(as.numeric(page_attributes_edits$nanon))
mean(as.numeric(page_attributes_edits$nanon))
sd(as.numeric(page_attributes_edits$nanon))

# actes d'éditions:

boxplot(as.numeric(page_attributes_edits$nedits))
mean(as.numeric(page_attributes_edits$nedits))
sd(as.numeric(page_attributes_edits$nedits))

# Régression

reg_sub <- subset(page_attributes_edits[,c(1,2:5,7,8,13)])
for(i in 2:ncol(reg_sub)){
  reg_sub[,i] <- as.numeric(reg_sub[,i])
}

reg1 <- glm(mean_degree_centrality ~ nanon + nadmins + nedits + ncontributors + nbots ,data =reg_sub)
summary(reg1)

