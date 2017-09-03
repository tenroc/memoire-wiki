## Moyennes et sd pour toutes les mesures pertinentes: edition

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

mean(as.numeric(contro_attributes_edits$nrestored), na.rm = T)
sd(as.numeric(contro_attributes_edits$nrestored), na.rm = T)

# Added

mean(as.numeric(contro_attributes_edits$nadded), na.rm =T)
sd(as.numeric(contro_attributes_edits$nadded), na.rm = T)

# Deleted

mean(as.numeric(contro_attributes_edits$ndeleted), na.rm = T)
sd(as.numeric(contro_attributes_edits$ndeleted), na.rm = T)

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

mean(as.numeric(contro_attributes_edits$degree_assortativity), na.rm = T)
sd(as.numeric(contro_attributes_edits$degree_assortativity), na.rm = T)

## Centralization

mean(as.numeric(contro_attributes_edits$centralization), na.rm = T)
sd(as.numeric(contro_attributes_edits$centralization), na.rm = T)

## Density

mean(as.numeric(contro_attributes_edits$density), na.rm = T)
sd(as.numeric(contro_attributes_edits$density), na.rm = T)

## Transitivity

mean(as.numeric(contro_attributes_edits$transitivity), na.rm = T)
sd(as.numeric(contro_attributes_edits$transitivity), na.rm = T)


## Centralité

# Admins

mean(as.numeric(contro_attributes_edits$mean_degree_admins), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_degree_admins), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_indegree_admin), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_indegree_admin), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_outdegree_admin), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_outdegree_admin), na.rm = T)

# Anons

mean(as.numeric(contro_attributes_edits$mean_degree_anon), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_degree_anon), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_indegree_anon), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_indegree_anon), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_outdegree_anon), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_outdegree_anon), na.rm = T)

# Inscrits

mean(as.numeric(contro_attributes_edits$mean_degree_inscrit), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_degree_inscrit), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_indegree_inscrit), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_indegree_inscrit), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_outdegree_inscrit), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_outdegree_inscrit), na.rm = T)

# Q1

mean(as.numeric(contro_attributes_edits$mean_degree_quar1), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_degree_quar1), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_indegree_quar1), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_indegree_quar1), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_outdegree_quar1), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_outdegree_quar1), na.rm = T)

# Q2

mean(as.numeric(contro_attributes_edits$mean_degree_quar2), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_degree_quar2), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_indegree_quar2), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_indegree_quar2), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_outdegree_quar2), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_outdegree_quar2), na.rm = T)

# Q3

mean(as.numeric(contro_attributes_edits$mean_degree_quar3), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_degree_quar3), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_indegree_quar3), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_indegree_quar3), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_outdegree_quar3), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_outdegree_quar3), na.rm = T)

# Q4

mean(as.numeric(contro_attributes_edits$mean_degree_quar4), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_degree_quar4), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_indegree_quar4), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_indegree_quar4), na.rm = T)

mean(as.numeric(contro_attributes_edits$mean_outdegree_quar4), na.rm = T)
sd(as.numeric(contro_attributes_edits$mean_outdegree_quar4), na.rm = T)



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

# Reverse

mean(as.numeric(contro_attributes_talks$nrestored), na.rm = T)
sd(as.numeric(contro_attributes_talks$nrestored), na.rm = T)

# Added

mean(as.numeric(contro_attributes_talks$nadded), na.rm =T)
sd(as.numeric(contro_attributes_talks$nadded), na.rm = T)

# Deleted

mean(as.numeric(contro_attributes_talks$ndeleted), na.rm = T)
sd(as.numeric(contro_attributes_talks$ndeleted), na.rm = T)

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

mean(as.numeric(contro_attributes_talks$degree_assortativity), na.rm = T)
sd(as.numeric(contro_attributes_talks$degree_assortativity), na.rm = T)

## Centralization

mean(as.numeric(contro_attributes_talks$centralization), na.rm = T)
sd(as.numeric(contro_attributes_talks$centralization), na.rm = T)

## Density

mean(as.numeric(contro_attributes_talks$density), na.rm = T)
sd(as.numeric(contro_attributes_talks$density), na.rm = T)

## Transitivity

mean(as.numeric(contro_attributes_talks$transitivity), na.rm = T)
sd(as.numeric(contro_attributes_talks$transitivity), na.rm = T)


## Centralité

# Admins

mean(as.numeric(contro_attributes_talks$mean_degree_admins), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_degree_admins), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_indegree_admin), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_indegree_admin), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_outdegree_admin), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_outdegree_admin), na.rm = T)

# Anons

mean(as.numeric(contro_attributes_talks$mean_degree_anon), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_degree_anon), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_indegree_anon), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_indegree_anon), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_outdegree_anon), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_outdegree_anon), na.rm = T)

# Inscrits

mean(as.numeric(contro_attributes_talks$mean_degree_inscrit), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_degree_inscrit), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_indegree_inscrit), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_indegree_inscrit), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_outdegree_inscrit), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_outdegree_inscrit), na.rm = T)

# Q1

mean(as.numeric(contro_attributes_talks$mean_degree_quar1), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_degree_quar1), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_indegree_quar1), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_indegree_quar1), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_outdegree_quar1), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_outdegree_quar1), na.rm = T)

# Q2

mean(as.numeric(contro_attributes_talks$mean_degree_quar2), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_degree_quar2), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_indegree_quar2), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_indegree_quar2), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_outdegree_quar2), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_outdegree_quar2), na.rm = T)

# Q3

mean(as.numeric(contro_attributes_talks$mean_degree_quar3), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_degree_quar3), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_indegree_quar3), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_indegree_quar3), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_outdegree_quar3), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_outdegree_quar3), na.rm = T)

# Q4

mean(as.numeric(contro_attributes_talks$mean_degree_quar4), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_degree_quar4), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_indegree_quar4), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_indegree_quar4), na.rm = T)

mean(as.numeric(contro_attributes_talks$mean_outdegree_quar4), na.rm = T)
sd(as.numeric(contro_attributes_talks$mean_outdegree_quar4), na.rm = T)

#### Regressions: base cross



