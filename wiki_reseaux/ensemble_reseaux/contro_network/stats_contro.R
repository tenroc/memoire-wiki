## Moyennes et sd pour toutes les mesures pertinentes: edition

contro_attributes_edits$ninscrit <- contro_attributes_edits$ncontributors - (contro_attributes_edits$nadmins + contro_attributes_edits$nanon + contro_attributes_edits$nbots)
contro_attributes_talks$ninscrit <- contro_attributes_talks$ncontributors - (contro_attributes_talks$nadmins + contro_attributes_talks$nanon + contro_attributes_talks$nbots)
contro_attributes_cross$ninscrit_edit <- contro_attributes_cross$ncontributors_edit - (contro_attributes_cross$nadmins_edit + contro_attributes_cross$nanon_edit + contro_attributes_cross$nbots_edit)
contro_attributes_cross$ninscrit_talk <- contro_attributes_cross$ncontributors_talk - (contro_attributes_cross$nadmins_talk + contro_attributes_cross$nanon_talk + contro_attributes_cross$nbots_talk)


## stats distribution nombre de... dans les réseaux

# admins

boxplot(as.numeric(contro_attributes_edits$nadmins))
mean(as.numeric(contro_attributes_edits$nadmins), na.rm = T)
sd(as.numeric(contro_attributes_edits$nadmins), na.rm = T)

# Anonymes

boxplot(as.numeric(contro_attributes_edits$nanon))
mean(as.numeric(contro_attributes_edits$nanon), na.rm = T)
sd(as.numeric(contro_attributes_edits$nanon), na.rm = T)

# actes d'éditions:

boxplot(as.numeric(contro_attributes_edits$nedits))
mean(as.numeric(contro_attributes_edits$nedits), na.rm = T)
sd(as.numeric(contro_attributes_edits$nedits), na.rm = T)

# bots:

boxplot(as.numeric(contro_attributes_edits$nbots))
mean(as.numeric(contro_attributes_edits$nbots), na.rm = T)
sd(as.numeric(contro_attributes_edits$nbots), na.rm = T)

# Contributeurs

boxplot(as.numeric(contro_attributes_edits$ncontributors))
mean(as.numeric(contro_attributes_edits$ncontributors), na.rm = T)
sd(as.numeric(contro_attributes_edits$ncontributors), na.rm = T)

# Reverse

boxplot(as.numeric(contro_attributes_edits$nrestored))
mean(as.numeric(contro_attributes_edits$nrestored), na.rm = T)
sd(as.numeric(contro_attributes_edits$nrestored), na.rm = T)

# Added

boxplot(as.numeric(contro_attributes_edits$nadded))
mean(as.numeric(contro_attributes_edits$nadded), na.rm =T)
sd(as.numeric(contro_attributes_edits$nadded), na.rm = T)

# Deleted

boxplot(as.numeric(contro_attributes_edits$ndeleted))
mean(as.numeric(contro_attributes_edits$ndeleted), na.rm = T)
sd(as.numeric(contro_attributes_edits$ndeleted), na.rm = T)

boxplot(as.numeric(contro_attributes_edits$ndeleted),as.numeric(contro_attributes_edits$nrestored),
                                                          main = "Moyennes du nombre d'actes de suppression et d'annulation",
                                                          names = c("Suppressions","annulations"))

boxplot()

# Quart1

mean(as.numeric(contro_attributes_edits$nquar1), na.rm = T)
sd(as.numeric(contro_attributes_edits$nquar1), na.rm = T)

# Quart2

mean(as.numeric(contro_attributes_edits$nquar2), na.rm = T)
sd(as.numeric(contro_attributes_edits$nquar2), na.rm = T)

# Quart3

mean(as.numeric(contro_attributes_edits$nquar3), na.rm = T)
sd(as.numeric(contro_attributes_edits$nquar3), na.rm = T)

# Quart4

mean(as.numeric(contro_attributes_edits$nquar4), na.rm = T)
sd(as.numeric(contro_attributes_edits$nquar4), na.rm = T)

## Assortativité

mean(as.numeric(paste(contro_attributes_edits$degree_assortativity)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$degree_assortativity)), na.rm = T)

## Centralization

mean(as.numeric(paste(contro_attributes_edits$centralization)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$centralization)), na.rm = T)

## Density

mean(as.numeric(paste(contro_attributes_edits$density)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$density)), na.rm = T)

## Transitivity

mean(as.numeric(paste(contro_attributes_edits$transitivity)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$transitivity)), na.rm = T)


## Centralité

# Admins

mean(as.numeric(paste(contro_attributes_edits$mean_degree_admins)), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_degree_admins), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_indegree_admin)), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_indegree_admin), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_outdegree_admin)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_outdegree_admin)), na.rm = T)

# Anons

mean(as.numeric(paste(contro_attributes_edits$mean_degree_anon)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_degree_anon)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_indegree_anon)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_indegree_anon)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_outdegree_anon)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_outdegree_anon)), na.rm = T)

# Inscrits

mean(as.numeric(paste(contro_attributes_edits$mean_degree_inscrit)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_degree_inscrit)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_indegree_inscrit)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_indegree_inscrit)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_outdegree_inscrit)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_outdegree_inscrit)), na.rm = T)

# Q1

mean(as.numeric(paste(contro_attributes_edits$mean_degree_quar1)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_degree_quar1)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_indegree_quar1)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_indegree_quar1)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_outdegree_quar1)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_outdegree_quar1)), na.rm = T)

# Q2

mean(as.numeric(paste(contro_attributes_edits$mean_degree_quar2)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_degree_quar2)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_indegree_quar2)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_indegree_quar2)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_outdegree_quar2)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_outdegree_quar2)), na.rm = T)

# Q3

mean(as.numeric(paste(contro_attributes_edits$mean_degree_quar3)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_degree_quar3)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_indegree_quar3)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_indegree_quar3)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_outdegree_quar3)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_outdegree_quar3)), na.rm = T)

# Q4

mean(as.numeric(paste(contro_attributes_edits$mean_degree_quar4)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_degree_quar4)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_indegree_quar4)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_indegree_quar4)), na.rm = T)

mean(as.numeric(paste(contro_attributes_edits$mean_outdegree_quar4)), na.rm = T)
sd(as.numeric(paste(contro_attributes_edits$mean_outdegree_quar4)), na.rm = T)



## Moyennes et sd pour toutes les mesures pertinentes: talk

## stats distribution nombre de... dans les réseaux

# admins

boxplot(as.numeric(contro_attributes_talks$nadmins))
mean(as.numeric(contro_attributes_talks$nadmins), na.rm = T)
sd(as.numeric(contro_attributes_talks$nadmins), na.rm = T)

# Anonymes

boxplot(as.numeric(contro_attributes_talks$nanon))
mean(as.numeric(contro_attributes_talks$nanon), na.rm = T)
sd(as.numeric(contro_attributes_talks$nanon), na.rm = T)

# actes d'éditions:

boxplot(as.numeric(contro_attributes_talks$nedits))
mean(as.numeric(contro_attributes_talks$nedits), na.rm = T)
sd(as.numeric(contro_attributes_talks$nedits), na.rm = T)

# bots:

boxplot(as.numeric(contro_attributes_talks$nbots))
mean(as.numeric(contro_attributes_talks$nbots), na.rm = T)
sd(as.numeric(contro_attributes_talks$nbots), na.rm = T)

# Contributeurs

boxplot(as.numeric(contro_attributes_talks$ncontributors))
mean(as.numeric(contro_attributes_talks$ncontributors), na.rm = T)
sd(as.numeric(contro_attributes_talks$ncontributors), na.rm = T)

# Initialized

mean(as.numeric(contro_attributes_talks$ninitialized, na.rm = T))
sd(as.numeric(contro_attributes_talks$ninitialized), na.rm = T)

# Responded

mean(as.numeric(contro_attributes_talks$nresponded), na.rm =T)
sd(as.numeric(contro_attributes_talks$nresponded), na.rm = T)

boxplot(as.numeric(contro_attributes_talks$ndeleted),as.numeric(contro_attributes_talks$nrestored),
        main = "Moyennes du nombre d'actes de suppression et d'annulation",
        names = c("Suppressions","annulations"))

boxplot()

# Max thread depth:

mean(as.numeric(paste(contro_attributes_talks$max_discussion_depth)), na.rm=T)
sd(as.numeric(contro_attributes_talks$max_discussion_depth), na.rm=T)

# Mean thread depth:

mean(as.numeric(paste(contro_attributes_talks$mean_discussion_depth)), na.rm=T)
sd(as.numeric(paste(contro_attributes_talks$mean_discussion_depth)), na.rm=T)


# Quart1

mean(as.numeric(contro_attributes_talks$nquar1), na.rm = T)
sd(as.numeric(contro_attributes_talks$nquar1), na.rm = T)

# Quart2

mean(as.numeric(contro_attributes_talks$nquar2), na.rm = T)
sd(as.numeric(contro_attributes_talks$nquar2), na.rm = T)

# Quart3

mean(as.numeric(contro_attributes_talks$nquar3), na.rm = T)
sd(as.numeric(contro_attributes_talks$nquar3), na.rm = T)

# Quart4

mean(as.numeric(contro_attributes_talks$nquar4), na.rm = T)
sd(as.numeric(contro_attributes_talks$nquar4), na.rm = T)

## Assortativité

mean(as.numeric(paste(contro_attributes_talks$degree_assortativity)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$degree_assortativity)), na.rm = T)

## Centralization

mean(as.numeric(paste(contro_attributes_talks$centralization)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$centralization)), na.rm = T)

## Density

mean(as.numeric(paste(contro_attributes_talks$density)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$density)), na.rm = T)

## Transitivity

mean(as.numeric(paste(contro_attributes_talks$transitivity)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$transitivity)), na.rm = T)


## Centralité

# Admins

mean(as.numeric(paste(contro_attributes_talks$mean_degree_centrality_admins)), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_degree_centrality_admins), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_indegree_admin)), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_indegree_admin), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_outdegree_admin)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_outdegree_admin)), na.rm = T)

# Anons

mean(as.numeric(paste(contro_attributes_talks$mean_degree_anon)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_degree_anon)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_indegree_anon)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_indegree_anon)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_outdegree_anon)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_outdegree_anon)), na.rm = T)

# Inscrits

mean(as.numeric(paste(contro_attributes_talks$mean_degree_inscrit)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_degree_inscrit)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_indegree_inscrit)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_indegree_inscrit)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_outdegree_inscrit)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_outdegree_inscrit)), na.rm = T)

# Q1

mean(as.numeric(paste(contro_attributes_talks$mean_degree_quar1)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_degree_quar1)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_indegree_quar1)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_indegree_quar1)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_outdegree_quar1)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_outdegree_quar1)), na.rm = T)

# Q2

mean(as.numeric(paste(contro_attributes_talks$mean_degree_quar2)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_degree_quar2)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_indegree_quar2)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_indegree_quar2)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_outdegree_quar2)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_outdegree_quar2)), na.rm = T)

# Q3

mean(as.numeric(paste(contro_attributes_talks$mean_degree_quar3)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_degree_quar3)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_indegree_quar3)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_indegree_quar3)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_outdegree_quar3)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_outdegree_quar3)), na.rm = T)

# Q4

mean(as.numeric(paste(contro_attributes_talks$mean_degree_quar4)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_degree_quar4)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_indegree_quar4)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_indegree_quar4)), na.rm = T)

mean(as.numeric(paste(contro_attributes_talks$mean_outdegree_quar4)), na.rm = T)
sd(as.numeric(paste(contro_attributes_talks$mean_outdegree_quar4)), na.rm = T)

#### Regressions: base cross

for (i in 3:ncol(contro_attributes_cross)){
  contro_attributes_cross[,i] <- as.numeric(paste(contro_attributes_cross[,i]))
}

# Reg 1 : nreverse 

reg1 <- lm(nrestored ~ ninscrit_talk + ninscrit_edit + nadmins_edit + nanon_edit + nadmins_talk + nanon_talk +  ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg1)

summary.aov(reg1)

plot(reg1)

gvlma(reg1)

ncvTest(reg1)

bptest(reg1)

spreadLevelPlot(reg1)

durbinWatsonTest(reg1)

reg1 <- lm(nrestored ~ nquar1_edit + nquar1_talk + nquar2_edit + nquar2_talk + nquar3_edit + nquar3_talk + nquar4_edit + nquar4_talk +  ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg1)

gvlma.lm(reg1)

bptest(reg1)

durbinWatsonTest(reg1)

## Normalisation du restoredcount

contro_attributes_cross$nrestored_re <- contro_attributes_cross$nrestored
contro_attributes_cross$nrestored_re[contro_attributes_cross$nrestored == 0] <- 1
distBCMod <- BoxCoxTrans(contro_attributes_cross$nrestored_re, na.rm = T)

contro_attributes_cross <- cbind(contro_attributes_cross, nrestored_box=predict(distBCMod, contro_attributes_cross$nrestored_re))

reg1 <- lm(nrestored_box ~ ninscrit_talk + ninscrit_edit + nadmins_edit + nanon_edit + nadmins_talk + nanon_talk +  ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg1)

summary.aov(reg1)

plot(reg1)

gvlma(reg1)

ncvTest(reg1)

bptest(reg1)

spreadLevelPlot(reg1)

durbinWatsonTest(reg1)

reg1 <- lm(nrestored_box ~ nquar1_edit + nquar1_talk + nquar2_edit + nquar2_talk + nquar3_edit + nquar3_talk + nquar4_edit + nquar4_talk +  ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg1)

gvlma.lm(reg1)

bptest(reg1)

durbinWatsonTest(reg1)

#Reg3: nadded

reg3 <- lm(nadded ~ ninscrit_edit + ninscrit_talk + nadmins_edit + nanon_edit + nadmins_talk + nanon_talk + ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg3)

gvlma(reg3)

bptest(reg3)

durbinWatsonTest(reg3)

reg3 <- lm(nadded ~ nquar1_edit + nquar1_talk + nquar2_edit + nquar2_talk + nquar3_edit + nquar3_talk + nquar4_edit + nquar4_talk + ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg3)

gvlma(reg3)

bptest(reg3)

durbinWatsonTest(reg3)

## Normalisation nadded

contro_attributes_cross$nadded_re <- contro_attributes_cross$nadded
contro_attributes_cross$nadded_re[contro_attributes_cross$nadded == 0] <- 1
distBCMod <- BoxCoxTrans(contro_attributes_cross$nadded_re, na.rm = T)

contro_attributes_cross <- cbind(contro_attributes_cross, nadded_box=predict(distBCMod, contro_attributes_cross$nadded_re))

reg3 <- lm(nadded_box ~ ninscrit_edit + ninscrit_talk + nadmins_edit + nanon_edit + nadmins_talk + nanon_talk + ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg3)

gvlma(reg3)

bptest(reg3)

durbinWatsonTest(reg3)

reg3 <- lm(nadded_box ~ nquar1_edit + nquar1_talk + nquar2_edit + nquar2_talk + nquar3_edit + nquar3_talk + nquar4_edit + nquar4_talk + ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg3)

gvlma(reg3)

bptest(reg3)

durbinWatsonTest(reg3)


#Reg4: ndeleted

reg4 <- lm(ndeleted ~ ninscrit_edit + ninscrit_talk + nadmins_edit + nanon_edit + nadmins_talk + nanon_talk +  ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg4)

gvlma(reg4)

bptest(reg4)

durbinWatsonTest(reg4)

reg4 <- lm(ndeleted ~ nquar1_edit + nquar1_talk + nquar2_edit + nquar2_talk + nquar3_edit + nquar3_talk + nquar4_edit + nquar4_talk + ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg4)

gvlma(reg4)

bptest(reg4)

durbinWatsonTest(reg4)

## Normalisation ndeleted

contro_attributes_cross$deleted_re <- contro_attributes_cross$ndeleted
contro_attributes_cross$deleted_re[contro_attributes_cross$ndeleted == 0] <- 1
distBCMod <- BoxCoxTrans(contro_attributes_cross$deleted_re, na.rm = T)

contro_attributes_cross <- cbind(contro_attributes_cross, ndeleted_box=predict(distBCMod, contro_attributes_cross$deleted_re))

reg4 <- lm(ndeleted_box ~ ninscrit_edit + ninscrit_talk + nadmins_edit + nanon_edit + nadmins_talk + nanon_talk +  ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg4)

gvlma(reg4)

bptest(reg4)

durbinWatsonTest(reg4)

reg4 <- lm(ndeleted_box ~ nquar1_edit + nquar1_talk + nquar2_edit + nquar2_talk + nquar3_edit + nquar3_talk + nquar4_edit + nquar4_talk + ninitialized + nresponded + mean_discussion_depth + max_discussion_depth, data = contro_attributes_cross)
summary(reg4)

gvlma(reg4)

bptest(reg4)

durbinWatsonTest(reg4)

## Boxplots distribution normalisees

boxplot(contro_attributes_cross$nrestored_box, )
