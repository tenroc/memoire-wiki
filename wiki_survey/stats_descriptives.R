#### Statistiques descriptives générales ####

## N'ont pas finis le questionnaire: 3490 personnes

table(wiki_survey_en$V10)

## Combien de wikipédiens interviennent dans plus d'une langue: (moyenne a 1,21, standard deviation a 0.98): données faussées par les NA

table(wiki_survey$Q5_langsum)
mean(wiki_survey$Q5_langsum)
sd(wiki_survey$Q5_langsum)
boxplot(wiki_survey$Q5_langsum, main="nombre de projet wikipédia dans des langues différentes ou les contributeurs interviennent")

# Sans les NA: moyenne a 1,4, sd a 0,7

wikitest <- subset(wiki_survey_en[wiki_survey_en$Q5_langsum != 0,])
table(wikitest$Q5_langsum)
mean(as.numeric(wikitest$Q5_langsum))
sd(as.numeric(wikitest$Q5_langsum))
median(as.numeric(wikitest$Q5_langsum))
boxplot(as.numeric(wikitest$Q5_langsum), main="nombre de projet wikipédia dans des langues différentes ou les contributeurs interviennent")

#### Stats bivariées ####




#### ACM ####

## Recodage des supplémentaires:

# Anciennetée:

table(wiki_survey_en$Q2, useNA = "ifany")
wiki_survey_en$Q2_anciennete[wiki_survey_en$Q2 %in% c("2001","2002","2003","2004")] <- "[2001-2004]"
wiki_survey_en$Q2_anciennete[wiki_survey_en$Q2 %in% c("2005","2006","2007","2008","2009")] <- "[2005-2009]"
wiki_survey_en$Q2_anciennete[wiki_survey_en$Q2 %in% c("2009","2010","2011","2012")] <- "[2009-2012]"
table(wiki_survey_en$Q2_anciennete, useNA = "ifany")

# Temps passé la dernière semaine a éditer wikipédia:

table(wiki_survey_en$Q20_1_TEXT, useNA = "ifany")
boxplot(as.numeric(wiki_survey_en$Q20_1_TEXT))

wiki_survey_en$Q20_tempseditheures[as.numeric(wiki_survey_en$Q20_1_TEXT) == 0] <- "0 hours"
wiki_survey_en$Q20_tempseditheures[as.numeric(wiki_survey_en$Q20_1_TEXT) %in% c(1:3)] <- "1-3 hours"
wiki_survey_en$Q20_tempseditheures[as.numeric(wiki_survey_en$Q20_1_TEXT) %in% c(4:15)] <- "4-15 hours"
wiki_survey_en$Q20_tempseditheures[as.numeric(wiki_survey_en$Q20_1_TEXT) > 15] <- ">15 hours"
table(wiki_survey_en$Q20_tempseditheures, useNA = "ifany")

# Age:

table(wiki_survey_en$Q21, useNA = "ifany")
wiki_survey_en$age_tr[wiki_survey_en$Q21 == "_ 18"] <- "less than 18 years old"
wiki_survey_en$age_tr[as.numeric(wiki_survey_en$Q21) %in% c(18:30)] <- "[18-30]"
wiki_survey_en$age_tr[as.numeric(wiki_survey_en$Q21) %in% c(31:50)] <- "[30-50]"
wiki_survey_en$age_tr[as.numeric(wiki_survey_en$Q21) >50 | wiki_survey_en$Q21 == "> 99"] <- "[>50]"
wiki_survey_en$age_tr[wiki_survey_en$Q21 == "Decline to state"] <- "Decline to state"
table(wiki_survey_en$age_tr, useNA = "ifany")

## ACM sur les premières Questions pour y voir un peu plus clair: (! beaucoup de NA, on les supprime, mais quand même...)

acm <- subset(wiki_survey_en, select=c(Q1_everedited,Q9_newarticles,Q9_content,Q9_spellcheck,Q9_translation,Q9_vandalism,Q9_readerscomplaint,
                                       Q9_mediation,Q9_technical, Q9_discussion, Q9_regulation, Q9_featuredreview, Q9_suppression,
                                       Q9_helpdesk, Q16_editorial, Q16_technical, Q16_article, Q16_references, Q16_content))
acm <- as.data.frame(lapply(acm, factor))

res.acm <- MCA(acm, quali.sup=1:8, graph=T, level.ventil=0.005)

# Axes retenus (3 axes)

valprop.acm <- res.acm$eig[1:10,]

barplot(res.acm$eig[1:10,2], main="Histogramme des valeurs propres", names.arg=1:10,        
        xlab="Axes", ylab="Pourcentage d'inertie", cex.axis=0.8, font.lab=3, ylim=c(0, 20),
        col="orange")

# Seuil de contrib (> contrib moyenne)

seuil <- 100/nrow(res.acm$var$contrib)

modatot <- which(res.acm$var$contrib[, 1]>seuil
                 | res.acm$var$contrib[, 2]>seuil
                 | res.acm$var$contrib[, 3]>seuil)

moda12 <- which(res.acm$var$contrib[, 1]>seuil 
                | res.acm$var$contrib[, 2]>seuil)

dim1 <- cbind(res.acm$var$contrib[,1], res.acm$var$coord[,1], res.acm$var$cos2[,1])
colnames(dim1) <- c("dim1_contrib","dim1_coord","dim1_cos2")

dim2 <- cbind(res.acm$var$contrib[,2], res.acm$var$coord[,2], res.acm$var$cos2[,2])
colnames(dim2) <- c("dim2_contrib","dim2_coord","dim2_cos2")

plot.MCA(res.acm, invisible=c("ind","quali.sup"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2), 
         autoLab="yes", unselect=1,
         selectMod=c(moda12))

# Beaucoup trop de NA: déforment l'ACM. Plus que 4354 observations

acm2 <- subset(acm, is.na(Q1_everedited) !=T & is.na(Q9_newarticles) != T & is.na(Q9_content) !=T & is.na(Q9_spellcheck) !=T & is.na(Q9_translation) != T &
                 is.na(Q9_vandalism) != T & is.na(Q9_readerscomplaint) != T & is.na(Q9_mediation) != T & is.na(Q9_technical) != T &
                 is.na(Q9_discussion) != T & is.na(Q9_regulation) !=T & is.na(Q9_featuredreview) !=T & is.na(Q9_suppression) !=T &
                  is.na(Q9_helpdesk) != T & is.na(Q9_helpdesk) != T & is.na(Q16_editorial) !=T & is.na(Q16_technical) !=T &
                 is.na(Q16_article) !=T & is.na(Q16_references) != T & is.na(Q16_content) !=T)

## Recodages pour regrouper les modalités extrêmes:

for (i in colnames(acm2)){
  acm2[,i] <- as.character(acm2[,i])
}

for (i in colnames(acm2)){
  acm2[acm2[,i] == "often" | acm2[,i] == "very often",i] <- "regularly"
  acm2[acm2[,i] == "seldom" | acm2[,i] == "sometimes",i] <- "occasionally"
}

acm2 <- as.data.frame(lapply(acm2, factor))

# 2eme ACM:

res.acm <- MCA(acm2, quali.sup=1:8, graph=T, level.ventil=0.005)

# Axes retenus

valprop.acm <- res.acm$eig[1:10,]

barplot(res.acm$eig[1:10,2], main="Histogramme des valeurs propres", names.arg=1:10,        
        xlab="Axes", ylab="Pourcentage d'inertie", cex.axis=0.8, font.lab=3, ylim=c(0, 30),
        col="orange")

# Seuil de contrib (> contrib moyenne)

seuil <- 100/nrow(res.acm$var$contrib)

modatot <- which(res.acm$var$contrib[, 1]>seuil
                 | res.acm$var$contrib[, 2]>seuil
                 | res.acm$var$contrib[, 3]>seuil
                 | res.acm$var$contrib[, 4]>seuil)

moda12 <- which(res.acm$var$contrib[, 1]>seuil 
                | res.acm$var$contrib[, 2]>seuil)

moda23 <- which(res.acm$var$contrib[, 2]>seuil 
                | res.acm$var$contrib[, 3]>seuil)


dim1 <- cbind(res.acm$var$contrib[,1], res.acm$var$coord[,1], res.acm$var$cos2[,1])
colnames(dim1) <- c("dim1_contrib","dim1_coord","dim1_cos2")

dim2 <- cbind(res.acm$var$contrib[,2], res.acm$var$coord[,2], res.acm$var$cos2[,2])
colnames(dim2) <- c("dim2_contrib","dim2_coord","dim2_cos2")

dim3 <- cbind(res.acm$var$contrib[,3], res.acm$var$coord[,3], res.acm$var$cos2[,3])
colnames(dim3) <- c("dim3_contrib","dim3_coord","dim3_cos2")

export.acm <- cbind(dim1, dim2, dim3)
write_ods(as.data.frame(export.acm), "tab-ACM.ods")

plot.MCA(res.acm, invisible=c("ind"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2), 
         autoLab="yes", unselect=1,
         selectMod=c(moda12))

plot.MCA(res.acm, invisible=c("ind"), 
         title="Nuage des modalites actives Plan 2-3", axes=c(2,3), 
         autoLab="yes", unselect=1,
         selectMod=c(moda23))

## Représentation graphique de l'ACM - axe 1-2

round(res.acm$var$coord[modatot, 1:2], 2)

# cadre

png(file='ACM1.png', width = 1024, height = 768, units = "px", pointsize = 14)

plot(res.acm$var$coord[modatot, 1:2]*1.4, type="n", 
     xlab=paste0("Axe 1 (", round(res.acm$eig[1,2], 1), "%)"),
     ylab=paste0("Axe 2 (", round(res.acm$eig[2,2], 1), "%)"),
     main="Premier plan factoriel",
     cex.main=1, cex.axis=0.8, cex.lab=0.7, font.lab=3, 
     asp=1)
abline(h=0, v=0, col="grey", lty=3, lwd=1)

# Representer les points et les etiquettes des modalites actives
points(res.acm$var$coord[modatot, 1:2], ## projection des points
       col="black", ## couleur des points
       pch=c(15))

etiquettes <- rownames(res.acm$var$coord)

# projection des etiquettes

text(res.acm$var$coord[modatot,1:2], labels=etiquettes[modatot],
     col="black", cex=1, pos=c(4))

## Représentation graphique de l'ACM - axe 2-3

round(res.acm$var$coord[modatot, 2:3], 2)

# cadre

png(file='ACM1.png', width = 1024, height = 768, units = "px", pointsize = 14)

plot(res.acm$var$coord[modatot, 2:3]*1.4, type="n", 
     xlab=paste0("Axe 2 (", round(res.acm$eig[1,2], 1), "%)"),
     ylab=paste0("Axe 3 (", round(res.acm$eig[2,2], 1), "%)"),
     main="Troisième plan factoriel",
     cex.main=1, cex.axis=0.8, cex.lab=0.7, font.lab=3, 
     asp=1) ## asp=1 on définis la meme échelle pour les axes x et y
abline(h=0, v=0, col="grey", lty=3, lwd=1)

# Representer les points et les etiquettes des modalites actives
points(res.acm$var$coord[modatot, 2:3],
       col="#da4d45",
       pch=c(15, 16, 17, 15, 16, 15, 16, 15, 16, 15, 16, 15, 16))

etiquettes <- rownames(res.acm$var$coord)


# projection des etiquettes

text(res.acm$var$coord[modatot,2:3], labels=etiquettes[modatot],
     col="#da4d45", cex=1, pos=c(3, 4, 3, 3, 3, 2, 4, 4, 1, 2, 
                               4, 2, 3))
