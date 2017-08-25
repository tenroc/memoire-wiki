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
wiki_survey_en$Q21_re <- as.numeric(paste(wiki_survey_en$Q21))
table(wiki_survey_en$Q21_re, useNA = "ifany")

wiki_survey_en$age_tr[wiki_survey_en$Q21 == "_ 18"] <- "<18"
wiki_survey_en$age_tr[wiki_survey_en$Q21_re %in% c(18:30)] <- "[18-30]"
wiki_survey_en$age_tr[wiki_survey_en$Q21_re %in% c(31:50)] <- "[30-50]"
wiki_survey_en$age_tr[wiki_survey_en$Q21_re >50 | wiki_survey_en$Q21 == "> 99"] <- "[>50]"
wiki_survey_en$age_tr[wiki_survey_en$Q21 == "Decline to state"] <- "Refuse"
table(wiki_survey_en$age_tr, useNA = "ifany")

# definition des subsets ACM et ACM2 (sans les NA): on en a besoin pour le recodage

acm <- subset(wiki_survey_en, select=c(Q17_toomuchrules, Q17_support, Q17_access, Q17_critiscism, Q17_notfun, Q17_ownership,
                                       Q18_mission, Q18_fellowquality, Q18_opinion, Q14_helpful, Q14_friendly, Q14_collaborative,
                                       Q14_rude, Q14_unfriendly, Q14_intelligent, Q14_dumb, Q14_arrogant,
                                       Q22_education, Q23_currentlyinschool, Q25_conjugal, Q26_child, Q27_gender, age_tr, Q2_anciennete,
                                       Q20_tempseditheures, Q24_employement, Q1_everedited,Q9_newarticles,Q9_content,Q9_spellcheck, 
                                       Q9_translation,Q9_vandalism,Q9_readerscomplaint,
                                       Q9_mediation,Q9_technical, Q9_discussion, Q9_regulation, Q9_featuredreview, Q9_suppression,
                                       Q9_helpdesk, Q16_editorial, Q16_technical, Q16_article, Q16_references, Q16_content))
colnames(acm)

acm <- as.data.frame(lapply(acm, factor))

acm2 <- subset(acm, is.na(Q1_everedited) !=T & is.na(Q9_newarticles) != T & is.na(Q9_content) !=T & is.na(Q9_spellcheck) !=T & is.na(Q9_translation) != T &
                 is.na(Q9_vandalism) != T & is.na(Q9_readerscomplaint) != T & is.na(Q9_mediation) != T & is.na(Q9_technical) != T &
                 is.na(Q9_discussion) != T & is.na(Q9_regulation) !=T & is.na(Q9_featuredreview) !=T & is.na(Q9_suppression) !=T &
                 is.na(Q9_helpdesk) != T & is.na(Q9_helpdesk) != T & is.na(Q16_editorial) !=T & is.na(Q16_technical) !=T &
                 is.na(Q16_article) !=T & is.na(Q16_references) != T & is.na(Q16_content) !=T)


# Recodage des catégories de fréquence pour les Q9 (fréquences de contribution à...)

for (i in 1:ncol(wiki_survey)){
  wiki_survey_en[,i] <- as.character(wiki_survey_en[,i])
}

for (i in c("Q9_newarticles","Q9_content","Q9_spellcheck", "Q9_translation","Q9_vandalism","Q9_readerscomplaint",
            "Q9_mediation","Q9_technical", "Q9_discussion", "Q9_regulation", "Q9_featuredreview", "Q9_suppression",
            "Q9_helpdesk", "Q16_editorial", "Q16_technical", "Q16_article", "Q16_references", "Q16_content")){
  wiki_survey_en[acm2[,i] == "not at all",paste(i,"_re",sep="")] <- "no"
  wiki_survey_en[acm2[,i] == "often" | acm2[,i] == "very often",paste(i,"_re",sep="")] <- "regularly"
  wiki_survey_en[acm2[,i] == "seldom" | acm2[,i] == "sometimes",paste(i,"_re",sep="")] <- "occasionally"
}

table(wiki_survey_en$Q9_newarticles_re)


#### Stats bivariées ####

## temps de contribution sur la semaine dernière / rien n'est significatif

# accueil des nouveaux contributeurs

round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_helpdesk_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_helpdesk_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q20_tempseditheures[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_helpdesk_re) !=T],
                      wiki_survey_en$Q9_helpdesk_re[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_helpdesk_re) !=T]))

# participation aux PAS

round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_suppression_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_suppression_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q20_tempseditheures[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_suppression_re) !=T],
                      wiki_survey_en$Q9_suppression_re[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_suppression_re) !=T]))

# Participation au développement et maintiens de recommendations, règles, etc.

round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q20_tempseditheures[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_regulation_re) !=T]
                      , wiki_survey_en$Q9_regulation_re[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_regulation_re) !=T]))

# Participations aux discussion

round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q20_tempseditheures[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_discussion_re) !=T]))

# Participation aux travaux techniques (admin)

round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q20_tempseditheures[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T]))

# Résolution de disputes utilisateurs

round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q20_tempseditheures, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q20_tempseditheures[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_mediation_re) !=T],
                      wiki_survey_en$Q9_mediation_re[is.na(wiki_survey_en$Q20_tempseditheures) !=T & is.na(wiki_survey_en$Q9_mediation_re) !=T]))

## ancienneté/ rien n'est significatif

# accueil des nouveaux contributeurs

round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_helpdesk_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_helpdesk_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q2_anciennete[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_helpdesk_re) !=T],
                      wiki_survey_en$Q9_helpdesk_re[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_helpdesk_re) !=T]))

# participation aux PAS

round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_suppression_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_suppression_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q2_anciennete[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_suppression_re) !=T],
                      wiki_survey_en$Q9_suppression_re[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_suppression_re) !=T]))

# Participation au développement et maintiens de recommendations, règles, etc.

round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q2_anciennete[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_regulation_re) !=T],
                      wiki_survey_en$Q9_regulation_re[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_regulation_re) !=T]))

# Participations aux discussion

round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q2_anciennete[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_discussion_re) !=T]))

# Participation aux travaux techniques (admin)

round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q2_anciennete[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T]))

# Résolution de disputes utilisateurs

round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q2_anciennete, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q2_anciennete[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_mediation_re) !=T],
                      wiki_survey_en$Q9_mediation_re[is.na(wiki_survey_en$Q2_anciennete) !=T & is.na(wiki_survey_en$Q9_mediation_re) !=T]))

# stats descriptive (prudence, elles portent sur les questions formulées de manière absolument abominables)

### mediation/too much rules: Non significatif

round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_toomuchrules[is.na(wiki_survey_en$Q9_mediation_re) !=T],
                      wiki_survey_en$Q9_mediation_re[is.na(wiki_survey_en$Q9_mediation_re) !=T]))

# mediation: critiscism: non significatif

round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_critiscism[is.na(wiki_survey_en$Q9_mediation_re) !=T],
                      wiki_survey_en$Q9_mediation_re[is.na(wiki_survey_en$Q9_mediation_re) !=T]))

# mediation notfun: non significatif

round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_notfun[is.na(wiki_survey_en$Q9_mediation_re) !=T],
                      wiki_survey_en$Q9_mediation_re[is.na(wiki_survey_en$Q9_mediation_re) !=T]))

# mediation / mission: non significatif

round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_mission[is.na(wiki_survey_en$Q9_mediation_re) !=T],
                      wiki_survey_en$Q9_mediation_re[is.na(wiki_survey_en$Q9_mediation_re) !=T]))

# mediation / fellow_quality: non significatif

round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_fellowquality[is.na(wiki_survey_en$Q9_mediation_re) !=T],
                      wiki_survey_en$Q9_mediation_re[is.na(wiki_survey_en$Q9_mediation_re) !=T]))

# mediation /opinion: non significatif

round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_mediation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_opinion[is.na(wiki_survey_en$Q9_mediation_re) !=T],
                      wiki_survey_en$Q9_mediation_re[is.na(wiki_survey_en$Q9_mediation_re) !=T]))


### technical /too much rules: Non significatif

round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_toomuchrules[is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_technical_re) !=T]))

# technical /criticism: Significatif au seuil de 10%

round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_critiscism[is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_technical_re) !=T]))

# technical /notfun: non significatif

round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_notfun[is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_technical_re) !=T]))

# technical access to research materials : non significatif

round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_access[is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_technical_re) !=T]))

# technical /mission: significatif au seuil de 5%

round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_mission[is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_technical_re) !=T]))

# technical / fellow_quality: non significatif

round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_fellowquality[is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_technical_re) !=T]))

# technical /newarticles : significatif au seuil de 1%

round(prop.table(table(wiki_survey_en$Q9_newarticles_re, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q9_newarticles_re, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q9_newarticles_re[is.na(wiki_survey_en$Q9_newarticles_re) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_newarticles_re) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T]))


# technical /opinion: non significatif

round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_opinion[is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_technical_re) !=T]))

### newarticles /too much rules: Non significatif

round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_toomuchrules[is.na(wiki_survey_en$Q9_newarticles_re) !=T],
                      wiki_survey_en$Q9_newarticles_re[is.na(wiki_survey_en$Q9_newarticles_re) !=T]))

# newarticles /criticism: Significatif au seuil de 10%

round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_critiscism[is.na(wiki_survey_en$Q9_newarticles_re) !=T],
                      wiki_survey_en$Q9_newarticles_re[is.na(wiki_survey_en$Q9_newarticles_re) !=T]))

# newarticles /notfun: non significatif

round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_notfun[is.na(wiki_survey_en$Q9_newarticles_re) !=T],
                      wiki_survey_en$Q9_newarticles_re[is.na(wiki_survey_en$Q9_newarticles_re) !=T]))

# tnewarticles access to research materials : non significatif

round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_access[is.na(wiki_survey_en$Q9_newarticles_re) !=T],
                      wiki_survey_en$Q9_newarticles_re[is.na(wiki_survey_en$Q9_newarticles_re) !=T]))

# newarticles /mission: non significatif

round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_mission[is.na(wiki_survey_en$Q9_newarticles_re) !=T],
                      wiki_survey_en$Q9_newarticles_re[is.na(wiki_survey_en$Q9_newarticles_re) !=T]))

# newarticles / fellow_quality: non significatif

round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_fellowquality[is.na(wiki_survey_en$Q9_newarticles_re) !=T],
                      wiki_survey_en$Q9_newarticles_re[is.na(wiki_survey_en$Q9_newarticles_re) !=T]))

# newarticles /opinion: non significatif: significatif au seuil de 5%

round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_newarticles_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_opinion[is.na(wiki_survey_en$Q9_newarticles_re) !=T],
                      wiki_survey_en$Q9_newarticles_re[is.na(wiki_survey_en$Q9_newarticles_re) !=T]))

### discussion /too much rules: Non significatif

round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_toomuchrules[is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_discussion_re) !=T]))

# discussion /criticism: non significatif

round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_critiscism[is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_discussion_re) !=T]))

# discussion /notfun: non significatif

round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_notfun[is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_discussion_re) !=T]))

# discussion access to research materials : non significatif

round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_access[is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_discussion_re) !=T]))

# discussion /mission: non significatif

round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_mission[is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_discussion_re) !=T]))

# discussion / fellow_quality: non significatif

round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_fellowquality[is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_discussion_re) !=T]))

# discussion /newarticles : significatif au seuil de 1%

round(prop.table(table(wiki_survey_en$Q9_discussion_re, wiki_survey_en$Q9_technical_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q9_discussion_re, wiki_survey_en$Q9_technical_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_discussion_re) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_discussion_re) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T]))


# discussion /opinion: significatif au seuil de 10%

round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_discussion_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_opinion[is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_discussion_re) !=T]))


### regulation /too much rules: Non significatif

round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_toomuchrules[is.na(wiki_survey_en$Q9_regulation_re) !=T],
                      wiki_survey_en$Q9_regulation_re[is.na(wiki_survey_en$Q9_regulation_re) !=T]))

# regulation /criticism: non significatif

round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_critiscism[is.na(wiki_survey_en$Q9_regulation_re) !=T],
                      wiki_survey_en$Q9_regulation_re[is.na(wiki_survey_en$Q9_regulation_re) !=T]))

# regulation /notfun: non significatif

round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_notfun[is.na(wiki_survey_en$Q9_regulation_re) !=T],
                      wiki_survey_en$Q9_regulation_re[is.na(wiki_survey_en$Q9_regulation_re) !=T]))

# regulation / access to research materials : non significatif

round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_access[is.na(wiki_survey_en$Q9_regulation_re) !=T],
                      wiki_survey_en$Q9_regulation_re[is.na(wiki_survey_en$Q9_regulation_re) !=T]))

# regulation /mission: non significatif

round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_mission[is.na(wiki_survey_en$Q9_regulation_re) !=T],
                      wiki_survey_en$Q9_regulation_re[is.na(wiki_survey_en$Q9_regulation_re) !=T]))

# regulation / fellow_quality: non significatif

round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_fellowquality[is.na(wiki_survey_en$Q9_regulation_re) !=T],
                      wiki_survey_en$Q9_regulation_re[is.na(wiki_survey_en$Q9_regulation_re) !=T]))

# discussion / regulation : significatif au seuil de 1%

round(prop.table(table(wiki_survey_en$Q9_discussion_re, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q9_discussion_re, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_regulation_re) !=T & is.na(wiki_survey_en$Q9_discussion_re) !=T],
                      wiki_survey_en$Q9_discussion_re[is.na(wiki_survey_en$Q9_regulation_re) !=T & is.na(wiki_survey_en$Q9_discussion_re) !=T]))


# regulation /opinion: non significatif

round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_opinion[is.na(wiki_survey_en$Q9_regulation_re) !=T],
                      wiki_survey_en$Q9_regulation_re[is.na(wiki_survey_en$Q9_regulation_re) !=T]))


# technical / regulation : significatif au seuil de 1%

round(prop.table(table(wiki_survey_en$Q9_technical_re, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q9_technical_re, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_regulation_re) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_regulation_re) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T]))

### content /too much rules: Non significatif

round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_content_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_toomuchrules, wiki_survey_en$Q9_content_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_toomuchrules[is.na(wiki_survey_en$Q9_content_re) !=T],
                      wiki_survey_en$Q9_content_re[is.na(wiki_survey_en$Q9_content_re) !=T]))

# content /criticism: non significatif

round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_content_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_critiscism, wiki_survey_en$Q9_content_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_critiscism[is.na(wiki_survey_en$Q9_content_re) !=T],
                      wiki_survey_en$Q9_content_re[is.na(wiki_survey_en$Q9_content_re) !=T]))

# content /notfun: non significatif

round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_content_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_notfun, wiki_survey_en$Q9_content_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_notfun[is.na(wiki_survey_en$Q9_content_re) !=T],
                      wiki_survey_en$Q9_content_re[is.na(wiki_survey_en$Q9_content_re) !=T]))

# content / access to research materials : non significatif

round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_content_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q17_access, wiki_survey_en$Q9_content_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q17_access[is.na(wiki_survey_en$Q9_content_re) !=T],
                      wiki_survey_en$Q9_content_re[is.na(wiki_survey_en$Q9_content_re) !=T]))

# content /mission: non significatif

round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_content_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_mission, wiki_survey_en$Q9_content_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_mission[is.na(wiki_survey_en$Q9_content_re) !=T],
                      wiki_survey_en$Q9_content_re[is.na(wiki_survey_en$Q9_content_re) !=T]))

# content / fellow_quality: non significatif

round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_content_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_fellowquality, wiki_survey_en$Q9_content_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_fellowquality[is.na(wiki_survey_en$Q9_content_re) !=T],
                      wiki_survey_en$Q9_content_re[is.na(wiki_survey_en$Q9_content_re) !=T]))

# content / regulation : significatif au seuil de 1%

round(prop.table(table(wiki_survey_en$Q9_content_re, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q9_content_re, wiki_survey_en$Q9_regulation_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q9_content_re[is.na(wiki_survey_en$Q9_regulation_re) !=T & is.na(wiki_survey_en$Q9_content_re) !=T],
                      wiki_survey_en$Q9_content_re[is.na(wiki_survey_en$Q9_regulation_re) !=T & is.na(wiki_survey_en$Q9_content_re) !=T]))


# content /opinion: non significatif

round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_content_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q18_opinion, wiki_survey_en$Q9_content_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q18_opinion[is.na(wiki_survey_en$Q9_content_re) !=T],
                      wiki_survey_en$Q9_content_re[is.na(wiki_survey_en$Q9_content_re) !=T]))


# technical / content : significatif au seuil de 1%

round(prop.table(table(wiki_survey_en$Q9_technical_re, wiki_survey_en$Q9_content_re, useNA = "ifany"),1)*100,3)
round(prop.table(table(wiki_survey_en$Q9_technical_re, wiki_survey_en$Q9_content_re, useNA = "ifany"),2)*100,3)
a <- chisq.test(table(wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_content_re) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T],
                      wiki_survey_en$Q9_technical_re[is.na(wiki_survey_en$Q9_content_re) !=T & is.na(wiki_survey_en$Q9_technical_re) !=T]))



#### ACM ####

## ACM sur les premières Questions pour y voir un peu plus clair: (! beaucoup de NA, on les supprime, mais quand même...)


res.acm <- MCA(acm, quali.sup=1:26, graph=T, level.ventil=0.005)

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

## Recodages pour regrouper les modalités extrêmes:

for (i in c("Q9_newarticles","Q9_content","Q9_spellcheck", "Q9_translation","Q9_vandalism","Q9_readerscomplaint",
            "Q9_mediation","Q9_technical", "Q9_discussion", "Q9_regulation", "Q9_featuredreview", "Q9_suppression",
            "Q9_helpdesk", "Q16_editorial", "Q16_technical", "Q16_article", "Q16_references", "Q16_content")){
  acm2[,i] <- as.character(acm2[,i])
}

for (i in c("Q9_newarticles","Q9_content","Q9_spellcheck", "Q9_translation","Q9_vandalism","Q9_readerscomplaint",
            "Q9_mediation","Q9_technical", "Q9_discussion", "Q9_regulation", "Q9_featuredreview", "Q9_suppression",
            "Q9_helpdesk", "Q16_editorial", "Q16_technical", "Q16_article", "Q16_references", "Q16_content")){
  acm2[acm2[,i] == "often" | acm2[,i] == "very often",i] <- "regularly"
  acm2[acm2[,i] == "seldom" | acm2[,i] == "sometimes",i] <- "occasionally"
}


acm2 <- as.data.frame(lapply(acm2, factor))


# 2eme ACM:

res.acm <- MCA(acm2, quali.sup=1:26, graph=T, level.ventil=0.005)

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

plot.MCA(res.acm, invisible=c("ind"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2), 
         autoLab="yes", unselect=1,
         selectMod=c(moda12))

plot.MCA(res.acm, invisible=c("ind"), 
         title="Nuage des modalites actives Plan 2-3", axes=c(2,3), 
         autoLab="yes", unselect=1,
         selectMod=c(moda23))


#### Tentative factoshiny ####

res.shiny <- MCAshiny(res.acm)

## Représentation graphique de l'ACM - axe 1-2

round(res.acm$var$coord[modatot, 1:2], 2)

# cadre

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

etiquettes <- c("contrib: non","contrib: oui","contrib: pas sur","Nart: non", "Nart:occas","Nart: reg","Contenu: non","Contenu: reg",
                "Format: non", "Format: reg", "Trad: reg","Vanda: non","Vanda: occas", "Vanda: reg", "Plaintes: non", "Plaintes: occas",
                "Plaintes: reg", "Médiation: occas", "Médiation: reg", "Tech: occas", "Tech: reg", "Discut: non", "Discut: occas", "Discut: reg",
                "Regl: occas", "Regl: reg", "RevueBArt: occas", "RevueBArt: reg", "Suppr: non", "Suppr: occas", "Suppr: reg", "Aide: occas", "Aide: reg",
                "DAEdito: non", "DAEdito: oui", "DATech: non", "DATech: oui", "DAArt: non", "DAArt: oui", "DARef: non", "DARef: oui", "DACont: non",
                "DAcont: oui")

# projection des etiquettes

text(res.acm$var$coord[modatot,1:2], labels=etiquettes,
     col="black", cex=1, pos=c(4))

## Représentation graphique de l'ACM - axe 2-3

round(res.acm$var$coord[modatot, 2:3], 2)

# cadre

plot(res.acm$var$coord[modatot, 2:3]*1.4, type="n", 
     xlab=paste0("Axe 2 (", round(res.acm$eig[2,2], 1), "%)"),
     ylab=paste0("Axe 3 (", round(res.acm$eig[3,2], 1), "%)"),
     main="Troisième plan factoriel",
     cex.main=1, cex.axis=0.8, cex.lab=0.7, font.lab=3, 
     asp=1) ## asp=1 on définis la meme échelle pour les axes x et y
abline(h=0, v=0, col="grey", lty=3, lwd=1)

# Representer les points et les etiquettes des modalites actives
points(res.acm$var$coord[modatot, 2:3],
       col="#da4d45",
       pch=c(16))

etiquettes <- c("contrib: non","contrib: oui","contrib: pas sur","Nart: non", "Nart:occas","Nart: reg","Contenu: non","Contenu: reg",
                "Format: non", "Format: reg", "Trad: reg","Vanda: non","Vanda: occas", "Vanda: reg", "Plaintes: non", "Plaintes: occas",
                "Plaintes: reg", "Médiation: occas", "Médiation: reg", "Tech: occas", "Tech: reg", "Discut: non", "Discut: occas", "Discut: reg",
                "Regl: occas", "Regl: reg", "RevueBArt: occas", "RevueBArt: reg", "Suppr: non", "Suppr: occas", "Suppr: reg", "Aide: occas", "Aide: reg",
                "DAEdito: non", "DAEdito: oui", "DATech: non", "DATech: oui", "DAArt: non", "DAArt: oui", "DARef: non", "DARef: oui", "DACont: non",
                "DAcont: oui")


# projection des etiquettes

text(res.acm$var$coord[modatot,2:3], labels=etiquettes,
     col="#da4d45", cex=1, pos=c(4, 2, 4, 3, 3, 2, 4, 4, 1, 2, 
                               4, 2, 3))

# Ajout des modalites supplementaire: temps de contrib et sociodemo
modasup=c("educ_doctorate", "educ_master_degree", "educ_primary_education", "educ_secondary_education", "educ_tertiary_education", "currentlyin_school_no", "currentlyin_school_yes", 
          "married", "have_partner", "single", "have_child_no", "have_child_yes", "female", "male", "age[>50]", "age[18-30]",
          "age[30-50]", "age[<18]", "inscription[2001-2004]", "inscription[2005-2009]", "inscription[2009-2012]", "temps_edit[>15 hours]", "temps_edit[1-3 hours]", "temps_edit[4-15 hours]",
          "employement_no", "employement_yes(part time)", "employement_yes(full time)")

text(res.acm$quali.sup$coord[c(35:38,39:42,44,45,47:49,51,52,55:58,61:63,65:67,69:71), 2:3]*1.4, 
     labels=modasup,
     cex=0.8, col="#3892e0", font=3)

# Ajout des supplementaires: ressenti wiki et autres contributeurs

modasup2=c("too_much_rules","support_from_other_editors","lack_access_resarch_material","critiscism_of_work","editors_not_fun",
           "editors_own_pages","wiki_mission_feel_important","fellow_volunteers_produce_quality","my_opinion_count","helpful",
           "friendly","collaborative","rude","unfriendly","dumb","arrogrant")

text(res.acm$quali.sup$coord[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34), 1:2]*1.4, 
     labels=modasup2,
     cex=0.8, col="#3892e0", font=3)
