## Statistiques de base sur les attributs des réseaux talk_page et edits_page:

#### Quelques visualisation générales de la structure des réseaux: edits ####

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

# bots:

boxplot(as.numeric(page_attributes_edits$nbots))
mean(as.numeric(page_attributes_edits$nbots))
sd(as.numeric(page_attributes_edits$nbots))

# Contributeurs

boxplot(as.numeric(page_attributes_edits$ncontributors))
mean(as.numeric(page_attributes_edits$contributors))
sd(as.numeric(page_attributes_edits$contributors))

#### Régressions ####

reg_sub <- subset(page_attributes_edits[,c(1,2:5,7,8,12:15,20)])
for(i in 2:ncol(reg_sub)){
  reg_sub[,i] <- as.numeric(reg_sub[,i])
}

reg1 <- lm(mean_degree_centrality ~ nanon + nadmins + nedits + ncontributors + nbots + istalk_page ,data =reg_sub)
summary(reg1)

reg2 <- lm(mean_betweenness ~ nanon + nadmins + nedits + ncontributors + nbots + istalk_page ,data =reg_sub)
summary(reg2)


#### Quelques visualisation générales de la structure des réseaux: talks ####

## Moyennes centralisation, centralité de dégrés, et centralité d'intermédiarité, en général, et entre administrateurs:

boxplot(as.numeric(page_attributes_talks$centralization))
boxplot(as.numeric(page_attributes_talks$mean_betweenness),as.numeric(page_attributes_talks$mean_betweenness_admin))
boxplot(as.numeric(page_attributes_talks$mean_degree_centrality), as.numeric(page_attributes_talks$mean_degree_centrality_admins))

boxplot(as.numeric(page_attributes_talks$mean_betweenness_admin))
boxplot(as.numeric(page_attributes_talks$mean_degree_centrality_admins))

mean(as.numeric(page_attributes_talks$mean_degree_centrality))
mean(as.numeric(page_attributes_talks$mean_degree_centrality_admins), na.rm = T)
sd(as.numeric(page_attributes_talks$mean_degree_centrality))
sd(as.numeric(page_attributes_talks$mean_degree_centrality_admins), na.rm = T)

mean(as.numeric(page_attributes_talks$mean_betweenness))
mean(as.numeric(page_attributes_talks$mean_betweenness_admin),na.rm = T)
sd(as.numeric(page_attributes_talks$mean_betweenness))
sd(as.numeric(page_attributes_talks$mean_betweenness_admin),na.rm = T)

## Pareil mais pour les déviations standards:

boxplot(as.numeric(page_attributes_talks$sd_betweenness),as.numeric(page_attributes_talks$sd_betweenness_admin))
boxplot(as.numeric(page_attributes_talks$sd_degree_centrality), as.numeric(page_attributes_talks$sd_degree_centrality_admins))

boxplot(as.numeric(page_attributes_talks$mean_betweenness_admin))
boxplot(as.numeric(page_attributes_talks$mean_degree_centrality_admins))

## stats distribution nombre de... dans les réseaux

# admins

boxplot(as.numeric(page_attributes_talks$nadmins))
mean(as.numeric(page_attributes_talks$nadmins))
sd(as.numeric(page_attributes_talks$nadmins))

# Anonymes

boxplot(as.numeric(page_attributes_talks$nanon))
mean(as.numeric(page_attributes_talks$nanon))
sd(as.numeric(page_attributes_talks$nanon))

# actes d'éditions:

boxplot(as.numeric(page_attributes_talks$nedits))
mean(as.numeric(page_attributes_talks$nedits))
sd(as.numeric(page_attributes_talks$nedits))

# bots:

boxplot(as.numeric(page_attributes_talks$nbots))
mean(as.numeric(page_attributes_talks$nbots))
sd(as.numeric(page_attributes_talks$nbots))

# Contributeurs

boxplot(as.numeric(page_attributes_talks$ncontributors))
mean(as.numeric(page_attributes_talks$contributors))
sd(as.numeric(page_attributes_talks$contributors))

#### Régressions: ####

reg_sub <- subset(page_attributes_talks[,c(1,2:6,8,9,13:16,20)])
for(i in 2:ncol(reg_sub)){
  reg_sub[,i] <- as.numeric(reg_sub[,i])
}

reg1 <- lm(mean_degree_centrality ~ nanon + nadmins + nedits + ncontributors + nbots ,data =reg_sub)
summary(reg1)

reg2 <- lm(mean_betweenness ~ nanon + nadmins + nedits + ncontributors + nbots ,data =reg_sub)
summary(reg2)