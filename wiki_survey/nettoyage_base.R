#### Import de la base brute: ####

wiki_survey <- read.csv("Wikipedia_Editor_Survey_2012_-_anonymized_dataset.csv", header=T)

#### Subset, observations, et recodages des variables necessaires au subset (en) #####

## Qu'est ce qui est dans la base?

str(wiki_survey)

# Replacer la première ligne en étiquettes:

etiquettes <- wiki_survey[1,]
wiki_survey <- wiki_survey[2:nrow(wiki_survey),]
wiki_survey <- droplevels(wiki_survey)

table(wiki_survey$V10)
table(wiki_survey$project, useNA = "ifany")

# wiki_survey_en <- subset(wiki_survey[wiki_survey$project == "enwiki",]) 
# donne des résultats inconsistants, parfois le wikiproject est différent de la langue principale déclarée.


# Environ 50% des répondants ont fini le survey

table(wiki_survey$V10)

# Transformer tous les "" en NA:

for (i in 1:ncol(wiki_survey)){
  wiki_survey[,i] <- as.character(wiki_survey[,i])
}

for(i in colnames(wiki_survey)){
  wiki_survey[wiki_survey[,i] == "", i]  <- NA
}

# Wich langage version do you primarily contribute to? (just to make sure we have the english wiki people)

table(wiki_survey$Q6, useNA = "ifany")
wiki_survey$Q6_re[wiki_survey$Q6 == "8"] <- "Old English"
wiki_survey$Q6_re[wiki_survey$Q6 == "9"] <- "Arabic"
wiki_survey$Q6_re[wiki_survey$Q6 == "53"] <- "German"
wiki_survey$Q6_re[wiki_survey$Q6 == "61"] <- "English"
wiki_survey$Q6_re[wiki_survey$Q6 == "63"] <- "Spanish"
wiki_survey$Q6_re[wiki_survey$Q6 == "72"] <- "French"
wiki_survey$Q6_re[wiki_survey$Q6 == "94"] <- "Hebrew"
wiki_survey$Q6_re[wiki_survey$Q6 == "110"] <- "Italian"
wiki_survey$Q6_re[wiki_survey$Q6 == "112"] <- "Japanese"
wiki_survey$Q6_re[wiki_survey$Q6 == "178"] <- "Netherland"
wiki_survey$Q6_re[wiki_survey$Q6 == "197"] <- "Polish"
wiki_survey$Q6_re[wiki_survey$Q6 == "202"] <- "Portuges"
wiki_survey$Q6_re[wiki_survey$Q6 == "210"] <- "Russian"
wiki_survey$Q6_re[wiki_survey$Q6 == "236"] <- "Swedish"
wiki_survey$Q6_re[wiki_survey$Q6 == "256"] <- "Ukrainian"
wiki_survey$Q6_re[wiki_survey$Q6 == "278"] <- "Chinese"
wiki_survey$Q6_re[wiki_survey$Q6 == "281"] <- "Other"
table(wiki_survey$Q6_re, useNA = "ifany")

## Which langage version do you contribute to?

wiki_survey$Q5_old_english[wiki_survey$Q5_8_Group == "0"] <- "1"
wiki_survey$Q5_old_english[is.na(wiki_survey$Q5_8_Group) == T] <- "0"
wiki_survey$Q5_old_english_rank <- wiki_survey$Q5_8_Rank
table(wiki_survey$Q5_old_english)

wiki_survey$Q5_arabic[wiki_survey$Q5_9_Group == "0"] <- "1"
wiki_survey$Q5_arabic[is.na(wiki_survey$Q5_9_Group) == T] <- "0"
wiki_survey$Q5_arabic_rank <- wiki_survey$Q5_9_Rank
table(wiki_survey$Q5_arabic)

wiki_survey$Q5_german[wiki_survey$Q5_53_Group == "0"] <- "1"
wiki_survey$Q5_german[is.na(wiki_survey$Q5_53_Group) == T] <- "0"
wiki_survey$Q5_german_rank <- wiki_survey$Q5_53_Rank
table(wiki_survey$Q5_german)

wiki_survey$Q5_english[wiki_survey$Q5_61_Group == "0"] <- "1"
wiki_survey$Q5_english[is.na(wiki_survey$Q5_61_Group) == T] <- "0"
wiki_survey$Q5_english_rank <- wiki_survey$Q5_61_Rank
table(wiki_survey$Q5_english)
table(wiki_survey$Q5_english_rank)

wiki_survey$Q5_spanish[wiki_survey$Q5_63_Group == "0"] <- "1"
wiki_survey$Q5_spanish[is.na(wiki_survey$Q5_63_Group) == T] <- "0"
wiki_survey$Q5_spanish_rank <- wiki_survey$Q5_63_Rank
table(wiki_survey$Q5_spanish)

wiki_survey$Q5_french[wiki_survey$Q5_72_Group == "0"] <- "1"
wiki_survey$Q5_french[is.na(wiki_survey$Q5_72_Group) == T] <- "0"
wiki_survey$Q5_french_rank <- wiki_survey$Q5_72_Rank
table(wiki_survey$Q5_french)

wiki_survey$Q5_hebrew[wiki_survey$Q5_94_Group == "0"] <- "1"
wiki_survey$Q5_hebrew[is.na(wiki_survey$Q5_94_Group) == T] <- "0"
wiki_survey$Q5_hebrew_rank <- wiki_survey$Q5_94_Rank
table(wiki_survey$Q5_hebrew)

wiki_survey$Q5_italian[wiki_survey$Q5_110_Group == "0"] <- "1"
wiki_survey$Q5_italian[is.na(wiki_survey$Q5_110_Group) == T] <- "0"
wiki_survey$Q5_italian_rank <- wiki_survey$Q5_110_Rank
table(wiki_survey$Q5_italian)

wiki_survey$Q5_japanese[wiki_survey$Q5_112_Group == "0"] <- "1"
wiki_survey$Q5_japanese[is.na(wiki_survey$Q5_112_Group) == T] <- "0"
wiki_survey$Q5_japanese_rank <- wiki_survey$Q5_112_Rank
table(wiki_survey$Q5_japanese)

wiki_survey$Q5_netherland[wiki_survey$Q5_178_Group == "0"] <- "1"
wiki_survey$Q5_netherland[is.na(wiki_survey$Q5_178_Group) == T] <- "0"
wiki_survey$Q5_netherland_rank <- wiki_survey$Q5_178_Rank
table(wiki_survey$Q5_netherland)

wiki_survey$Q5_polish[wiki_survey$Q5_197_Group == "0"] <- "1"
wiki_survey$Q5_polish[is.na(wiki_survey$Q5_197_Group) == T] <- "0"
wiki_survey$Q5_polish_rank <- wiki_survey$Q5_197_Rank
table(wiki_survey$Q5_polish)

wiki_survey$Q5_portuguese[wiki_survey$Q5_202_Group == "0"] <- "1"
wiki_survey$Q5_portuguese[is.na(wiki_survey$Q5_202_Group) == T] <- "0"
wiki_survey$Q5_portuguese_rank <- wiki_survey$Q5_202_Rank
table(wiki_survey$Q5_portuguese)

wiki_survey$Q5_russian[wiki_survey$Q5_210_Group == "0"] <- "1"
wiki_survey$Q5_russian[is.na(wiki_survey$Q5_210_Group) == T] <- "0"
wiki_survey$Q5_russian_rank <- wiki_survey$Q5_210_Rank
table(wiki_survey$Q5_russian)

wiki_survey$Q5_swedish[wiki_survey$Q5_236_Group == "0"] <- "1"
wiki_survey$Q5_swedish[is.na(wiki_survey$Q5_236_Group) == T] <- "0"
wiki_survey$Q5_swedish_rank <- wiki_survey$Q5_236_Rank
table(wiki_survey$Q5_swedish)

wiki_survey$Q5_ukrainian[wiki_survey$Q5_258_Group == "0"] <- "1"
wiki_survey$Q5_ukrainian[is.na(wiki_survey$Q5_258_Group) == T] <- "0"
wiki_survey$Q5_ukrainian_rank <- wiki_survey$Q5_258_Rank
table(wiki_survey$Q5_ukrainian)

wiki_survey$Q5_chinese[wiki_survey$Q5_278_Group == "0"] <- "1"
wiki_survey$Q5_chinese[is.na(wiki_survey$Q5_278_Group) == T] <- "0"
wiki_survey$Q5_chinese_rank <- wiki_survey$Q5_278_Rank
table(wiki_survey$Q5_chinese)

wiki_survey$Q5_other[wiki_survey$Q5_281_Group == "0"] <- "1"
wiki_survey$Q5_other[is.na(wiki_survey$Q5_281_Group) == T] <- "0"
wiki_survey$Q5_other_rank <- wiki_survey$Q5_281_Rank
table(wiki_survey$Q5_other)

# calcul du taux total de NA pour les questions de langues de contribution: en tout: 3637 NA sur les 17577 observations

table(mapply(FUN=sum ,as.numeric(wiki_survey$Q5_old_english), as.numeric(wiki_survey$Q5_arabic), as.numeric(wiki_survey$Q5_german),
             as.numeric(wiki_survey$Q5_english), as.numeric(wiki_survey$Q5_spanish), as.numeric(wiki_survey$Q5_french),
             as.numeric(wiki_survey$Q5_hebrew), as.numeric(wiki_survey$Q5_italian), as.numeric(wiki_survey$Q5_japanese),
             as.numeric(wiki_survey$Q5_netherland), as.numeric(wiki_survey$Q5_polish), as.numeric(wiki_survey$Q5_portuguese),
             as.numeric(wiki_survey$Q5_russian), as.numeric(wiki_survey$Q5_swedish), as.numeric(wiki_survey$Q5_ukrainian),
             as.numeric(wiki_survey$Q5_chinese), as.numeric(wiki_survey$Q5_other)), useNA = "ifany")

wiki_survey$Q5_langsum <- mapply(FUN=sum ,as.numeric(wiki_survey$Q5_old_english), as.numeric(wiki_survey$Q5_arabic), as.numeric(wiki_survey$Q5_german),
                                 as.numeric(wiki_survey$Q5_english), as.numeric(wiki_survey$Q5_spanish), as.numeric(wiki_survey$Q5_french),
                                 as.numeric(wiki_survey$Q5_hebrew), as.numeric(wiki_survey$Q5_italian), as.numeric(wiki_survey$Q5_japanese),
                                 as.numeric(wiki_survey$Q5_netherland), as.numeric(wiki_survey$Q5_polish), as.numeric(wiki_survey$Q5_portuguese),
                                 as.numeric(wiki_survey$Q5_russian), as.numeric(wiki_survey$Q5_swedish), as.numeric(wiki_survey$Q5_ukrainian),
                                 as.numeric(wiki_survey$Q5_chinese), as.numeric(wiki_survey$Q5_other))

length(wiki_survey[wiki_survey$Q5_langsum == 0 & is.na(wiki_survey$Q6) == T,1])

# subset sur le wiki anglais

# Meme resultat que si on selectionne juste par subset(wiki_survey[wiki_survey$Q6_re == "English",])

wiki_survey_en <- NULL

wiki_survey_en <- subset(wiki_survey[(wiki_survey$Q6_re == "English" & (is.na(wiki_survey$Q6_re) == F) | wiki_survey$Q5_english_rank == "1" ),])
wiki_survey_en <- subset(wiki_survey_en[is.na(wiki_survey_en$V1) == F,])
table(wiki_survey_en$V10)
table(wiki_survey_en$Q6)
table(wiki_survey_en$Q5_english_rank)
table(wiki_survey_en$Q6_re)

##### Quelles variables sont utilisables? et recodages #####

# have you ever edited wikipedia?
table(wiki_survey_en$Q1, useNA="ifany")
wiki_survey_en$Q1_everedited <- factor(wiki_survey_en$Q1, labels=c("Non","Oui","Pas sur"))
table(wiki_survey_en$Q1_everedited, useNA="ifany")

# In what year did you started editing wikipedia? ouch. 1457 NA....

table(wiki_survey_en$Q2, useNA="ifany")
table(is.na(wiki_survey_en$Q2))

# wikitest: Qui sont ceux qui renseignent l'anglais en second rang des langues utilisées

wikitest <- subset(wiki_survey[wiki_survey$Q5_english_rank == "2",])
wikitest <- subset(wikitest[is.na(wikitest$V1) == F,])
table(wikitest$Q6_re, useNA = "ifany")
table(wikitest$Q5_arabic_rank)

#### Premières visualisation et recodages généraux ####

# Repasse les variables en factor

for (i in 1:ncol(wiki_survey)){
  wiki_survey_en[,i] <- as.factor(wiki_survey_en[,i])
}

#### Recent participiation in wikipedia ####

# New articles:

Q9_labels <- c("not at all", "seldom", "sometimes", "often", "very often")

table(wiki_survey_en$Q9_1)
wiki_survey_en$Q9_newarticles <- factor(wiki_survey_en$Q9_1, labels=Q9_labels)
table(wiki_survey_en$Q9_newarticles, useNA = "ifany")

# Writing content to existing articles:

table(wiki_survey_en$Q9_2)
wiki_survey_en$Q9_content <- factor(wiki_survey_en$Q9_2, labels=Q9_labels)
table(wiki_survey_en$Q9_content, useNA = "ifany")

# Fix formating, grammar, etc. (spellcheck):

table(wiki_survey_en$Q9_3)
wiki_survey_en$Q9_spellcheck <- factor(wiki_survey_en$Q9_3, labels=Q9_labels)
table(wiki_survey_en$Q9_spellcheck, useNA = "ifany")

# Translation work:

table(wiki_survey_en$Q9_4)
wiki_survey_en$Q9_translation <- factor(wiki_survey_en$Q9_4, labels=Q9_labels)
table(wiki_survey_en$Q9_translation, useNA = "ifany")

# Patrol for vandalism, mass deletion, copyright violation:

table(wiki_survey_en$Q9_5)
wiki_survey_en$Q9_vandalism <- factor(wiki_survey_en$Q9_5, labels=Q9_labels)
table(wiki_survey_en$Q9_vandalism, useNA = "ifany")

# Answer reader questions and complaints:

table(wiki_survey_en$Q9_6)
wiki_survey_en$Q9_readerscomplaint <- factor(wiki_survey_en$Q9_6, labels=Q9_labels)
table(wiki_survey_en$Q9_readerscomplaint, useNA = "ifany")

# Resolve dispute among volonteers:

table(wiki_survey_en$Q9_7)
wiki_survey_en$Q9_mediation <- factor(wiki_survey_en$Q9_7, labels=Q9_labels)
table(wiki_survey_en$Q9_mediation, useNA = "ifany")

# Help to organize meetings, workshop, events:

table(wiki_survey_en$Q9_8)
wiki_survey_en$Q9_irl <- factor(wiki_survey_en$Q9_8, labels=Q9_labels)
table(wiki_survey_en$Q9_irl, useNA = "ifany")

# Public outreach, advocatcy, etc:

table(wiki_survey_en$Q9_9)
wiki_survey_en$Q9_evangelist <- factor(wiki_survey_en$Q9_9, labels=Q9_labels)
table(wiki_survey_en$Q9_evangelist, useNA = "ifany")

# Technical work (server administration and technical work):

table(wiki_survey_en$Q9_10)
wiki_survey_en$Q9_technical <- factor(wiki_survey_en$Q9_10, labels=Q9_labels)
table(wiki_survey_en$Q9_technical, useNA = "ifany")

# Participate in chapter work:

table(wiki_survey_en$Q9_11)
wiki_survey_en$Q9_chapter <- factor(wiki_survey_en$Q9_11, labels=Q9_labels)
table(wiki_survey_en$Q9_chapter, useNA = "ifany")

# Participate in  userdiscussions:

table(wiki_survey_en$Q9_12)
wiki_survey_en$Q9_discussion <- factor(wiki_survey_en$Q9_12, labels=Q9_labels)
table(wiki_survey_en$Q9_discussion, useNA = "ifany")

# Develop and maintain policies, guidelines and regulations:

table(wiki_survey_en$Q9_13)
wiki_survey_en$Q9_regulation <- factor(wiki_survey_en$Q9_13, labels=Q9_labels)
table(wiki_survey_en$Q9_regulation, useNA = "ifany")

#### Frequence of participation in activites: ####

# Conduct quality review and assess articles for featured article selection:

table(wiki_survey_en$Q10_2)
wiki_survey_en$Q9_featuredreview <- factor(wiki_survey_en$Q10_2, labels=Q9_labels)
table(wiki_survey_en$Q9_featuredreview, useNA = "ifany")

# I participate in deletion process (PaS):

table(wiki_survey_en$Q10_3)
wiki_survey_en$Q9_suppression <- factor(wiki_survey_en$Q10_3, labels=Q9_labels)
table(wiki_survey_en$Q9_suppression, useNA = "ifany")

# Help or welcome new editors:

table(wiki_survey_en$Q10_3)
wiki_survey_en$Q9_helpdesk <- factor(wiki_survey_en$Q10_3, labels=Q9_labels)
table(wiki_survey_en$Q9_helpdesk, useNA = "ifany")

#### How likely are you to recommend editing wikipedia as an activity? (0-10) (note: beaucoup de NA) ####

table(wiki_survey_en$Q11_4, useNA = "ifany")

##### If you think back half a year ago: (note: trop de NA: quasimment 50%. Je pense pas pouvoir en faire grand chose) ####

labels_Q12 <- c("more", "less", "same")

# Witness more or less conflict in the community?

table(wiki_survey_en$Q12a_1, useNA = "ifany")
wiki_survey_en$Q12a_conflict <-factor(wiki_survey_en$Q12a_1, labels=labels_Q12)
table(wiki_survey_en$Q12a_conflict, useNA = "ifany")

# More or less motivated to contribute?

table(wiki_survey_en$Q12a_2, useNA = "ifany")
wiki_survey_en$Q12a_motivation <-factor(wiki_survey_en$Q12a_2, labels=labels_Q12)
table(wiki_survey_en$Q12a_motivation, useNA = "ifany")

# Where you more or less active?

table(wiki_survey_en$Q12a_3, useNA = "ifany")
wiki_survey_en$Q12a_activity <-factor(wiki_survey_en$Q12a_3, labels=labels_Q12)
table(wiki_survey_en$Q12a_activity, useNA = "ifany")

####  Interaction with other wikipedians (how would you describe: words): 0 = unselected, 1= selected ####

# helpful:

table(wiki_survey_en$Q14_1, useNA = "ifany")
wiki_survey_en$Q14_helpful <- as.character(wiki_survey_en$Q14_1)
wiki_survey_en$Q14_helpful[is.na(wiki_survey_en$Q14_1) == T] <- "0"
table(wiki_survey_en$Q14_helpful, useNA = "ifany")

# friendly:

table(wiki_survey_en$Q14_2, useNA = "ifany")
wiki_survey_en$Q14_friendly <- as.character(wiki_survey_en$Q14_2)
wiki_survey_en$Q14_friendly[is.na(wiki_survey_en$Q14_2) == T] <- "0"
table(wiki_survey_en$Q14_friendly, useNA = "ifany")

# Collaborative:

table(wiki_survey_en$Q14_3, useNA = "ifany")
wiki_survey_en$Q14_collaborative <- as.character(wiki_survey_en$Q14_3)
wiki_survey_en$Q14_collaborative[is.na(wiki_survey_en$Q14_3) == T] <- "0"
table(wiki_survey_en$Q14_collaborative, useNA = "ifany")

# Rude:

table(wiki_survey_en$Q14_4, useNA = "ifany")
wiki_survey_en$Q14_rude <- as.character(wiki_survey_en$Q14_4)
wiki_survey_en$Q14_rude[is.na(wiki_survey_en$Q14_4) == T] <- "0"
table(wiki_survey_en$Q14_rude, useNA = "ifany")

# Unfriendly:

table(wiki_survey_en$Q14_5, useNA = "ifany")
wiki_survey_en$Q14_unfriendly <- as.character(wiki_survey_en$Q14_5)
wiki_survey_en$Q14_unfriendly[is.na(wiki_survey_en$Q14_5) == T] <- "0"
table(wiki_survey_en$Q14_unfriendly, useNA = "ifany")

# Intelligent:

table(wiki_survey_en$Q14_6, useNA = "ifany")
wiki_survey_en$Q14_intelligent <- as.character(wiki_survey_en$Q14_6)
wiki_survey_en$Q14_intelligent[is.na(wiki_survey_en$Q14_6) == T] <- "0"
table(wiki_survey_en$Q14_intelligent, useNA = "ifany")

# Dumb:

table(wiki_survey_en$Q14_7, useNA = "ifany")
wiki_survey_en$Q14_dumb <- as.character(wiki_survey_en$Q14_7)
wiki_survey_en$Q14_dumb[is.na(wiki_survey_en$Q14_7) == T] <- "0"
table(wiki_survey_en$Q14_dumb, useNA = "ifany")

# Arrogant:

table(wiki_survey_en$Q14_8, useNA = "ifany")
wiki_survey_en$Q14_arrogant <- as.character(wiki_survey_en$Q14_8)
wiki_survey_en$Q14_arrogant[is.na(wiki_survey_en$Q14_8) == T] <- "0"
table(wiki_survey_en$Q14_arrogant, useNA = "ifany")

#### Wich one of those statement do you agree with? (note: là encore, beaucoup de NA) ####
# if you had to choose: The feedback from other editors through reverts, discussions, etc. has helped me become a better editor
# The feedback from other editors through reverts, discussions, etc. has been a bad experience for me.
# Neither

table(wiki_survey_en$Q15, useNA = "ifany")
wiki_survey_en$Q15_re <- factor(wiki_survey_en$Q15, labels=c("positive", "negative", "neutral"))
table(wiki_survey_en$Q15_re, useNA = "ifany")

#### Have you asked a fellow editor for help in: ####

# Editorial policy:

table(wiki_survey_en$Q16a_1, useNA = "ifany")
wiki_survey_en$Q16_editorial <- factor(wiki_survey_en$Q16a_1, labels=c("no","yes"))
table(wiki_survey_en$Q16_editorial, useNA = "ifany")

# Technical help (xiki markup, etc)

table(wiki_survey_en$Q16a_2, useNA = "ifany")
wiki_survey_en$Q16_technical <- factor(wiki_survey_en$Q16a_2, labels=c("no","yes"))
table(wiki_survey_en$Q16_technical, useNA = "ifany")

# Editing an article

table(wiki_survey_en$Q16a_3, useNA = "ifany")
wiki_survey_en$Q16_article <- factor(wiki_survey_en$Q16a_3, labels=c("no","yes"))
table(wiki_survey_en$Q16_article, useNA = "ifany")

# Reference related questions:

table(wiki_survey_en$Q16a_4, useNA = "ifany")
wiki_survey_en$Q16_references <- factor(wiki_survey_en$Q16a_4, labels=c("no","yes"))
table(wiki_survey_en$Q16_references, useNA = "ifany")

# Content related question

table(wiki_survey_en$Q16a_5, useNA = "ifany")
wiki_survey_en$Q16_content <- factor(wiki_survey_en$Q16a_5, labels=c("no","yes"))
table(wiki_survey_en$Q16_content, useNA = "ifany")

# Q16b: Thinking about the last time you asked a fellow editor for help, how happy were you with the help you got: (laisse tomber, plus de 50% de NA)

table(wiki_survey_en$Q16b, useNA = "ifany")
wiki_survey_en$Q16_helpsatisfaction <- factor(wiki_survey_en$Q16b, labels=c("extremely unhappy","very unhappy", "neither happy nor unhappy",
                                                                            "very happy", "extremely happy"))
table(wiki_survey_en$Q16_helpsatisfaction, useNA = "ifany")

#### Identified problems with wikimedia culture: select all that apply ####

# Too many rules and policies:

table(wiki_survey_en$Q17_1, useNA = "ifany")
wiki_survey_en$Q17_toomuchrules <- as.character(wiki_survey_en$Q17_1)
wiki_survey_en$Q17_toomuchrules[is.na(wiki_survey_en$Q17_1) == T] <- "0"
table(wiki_survey_en$Q17_toomuchrules, useNA = "ifany")

# Editors feels like they own articles and don't let other edit:

table(wiki_survey_en$Q17_2, useNA = "ifany")
wiki_survey_en$Q17_ownership <- as.character(wiki_survey_en$Q17_2)
wiki_survey_en$Q17_ownership[is.na(wiki_survey_en$Q17_2) == T] <- "0"
table(wiki_survey_en$Q17_ownership, useNA = "ifany")

# The editing interface is hard to use;

table(wiki_survey_en$Q17_3, useNA = "ifany")
wiki_survey_en$Q17_interface <- as.character(wiki_survey_en$Q17_3)
wiki_survey_en$Q17_interface[is.na(wiki_survey_en$Q17_3) == T] <- "0"
table(wiki_survey_en$Q17_interface, useNA = "ifany")

# Editors who are not fun to work with

table(wiki_survey_en$Q17_4, useNA = "ifany")
wiki_survey_en$Q17_notfun <- as.character(wiki_survey_en$Q17_4)
wiki_survey_en$Q17_notfun[is.na(wiki_survey_en$Q17_4) == T] <- "0"
table(wiki_survey_en$Q17_notfun, useNA = "ifany")

# Software for maintenance task is too hard to use:

table(wiki_survey_en$Q17_5, useNA = "ifany")
wiki_survey_en$Q17_patrolsoftware <- as.character(wiki_survey_en$Q17_5)
wiki_survey_en$Q17_patrolsoftware[is.na(wiki_survey_en$Q17_5) == T] <- "0"
table(wiki_survey_en$Q17_patrolsoftware, useNA = "ifany")

# Lack of support from other editors:

table(wiki_survey_en$Q17_6, useNA = "ifany")
wiki_survey_en$Q17_support <- as.character(wiki_survey_en$Q17_6)
wiki_survey_en$Q17_support[is.na(wiki_survey_en$Q17_6) == T] <- "0"
table(wiki_survey_en$Q17_support, useNA = "ifany")

# Lack of access to research material (books, etc.)

table(wiki_survey_en$Q17_7, useNA = "ifany")
wiki_survey_en$Q17_access <- as.character(wiki_survey_en$Q17_7)
wiki_survey_en$Q17_access[is.na(wiki_survey_en$Q17_7) == T] <- "0"
table(wiki_survey_en$Q17_access, useNA = "ifany")

# Critiscism of you and your work

table(wiki_survey_en$Q17_8, useNA = "ifany")
wiki_survey_en$Q17_critiscism <- as.character(wiki_survey_en$Q17_8)
wiki_survey_en$Q17_critiscism[is.na(wiki_survey_en$Q17_8) == T] <- "0"
table(wiki_survey_en$Q17_critiscism, useNA = "ifany")

# Warning message on your talk page

table(wiki_survey_en$Q17_9, useNA = "ifany")
wiki_survey_en$Q17_warning <- as.character(wiki_survey_en$Q17_9)
wiki_survey_en$Q17_warning[is.na(wiki_survey_en$Q17_9) == T] <- "0"
table(wiki_survey_en$Q17_warning, useNA = "ifany")

# Harassment by other editors

table(wiki_survey_en$Q17_10, useNA = "ifany")
wiki_survey_en$Q17_harassment <- as.character(wiki_survey_en$Q17_10)
wiki_survey_en$Q17_harassment[is.na(wiki_survey_en$Q17_10) == T] <- "0"
table(wiki_survey_en$Q17_harassment, useNA = "ifany")

#### Below is a list of levels of impressions editors may get from their experience with Wikipedia: please select all that apply: ####

# I understand what is expected from me:

table(wiki_survey_en$Q18_1, useNA = "ifany")
wiki_survey_en$Q18_expected <- as.character(wiki_survey_en$Q18_1)
wiki_survey_en$Q18_expected[is.na(wiki_survey_en$Q18_1) == T] <- "0"
table(wiki_survey_en$Q18_expected, useNA = "ifany")

# I am provided with the tools and materials I need to work well

table(wiki_survey_en$Q18_2, useNA = "ifany")
wiki_survey_en$Q18_tools <- as.character(wiki_survey_en$Q18_2)
wiki_survey_en$Q18_tools[is.na(wiki_survey_en$Q18_2) == T] <- "0"
table(wiki_survey_en$Q18_tools, useNA = "ifany")

#  I have the opportunity to do what I do best

table(wiki_survey_en$Q18_3, useNA = "ifany")
wiki_survey_en$Q18_dobest <- as.character(wiki_survey_en$Q18_3)
wiki_survey_en$Q18_dobest[is.na(wiki_survey_en$Q18_3) == T] <- "0"
table(wiki_survey_en$Q18_dobest, useNA = "ifany")

# Someone cares about me as a person

table(wiki_survey_en$Q18_4, useNA = "ifany")
wiki_survey_en$Q18_dobest <- as.character(wiki_survey_en$Q18_4)
wiki_survey_en$Q18_dobest[is.na(wiki_survey_en$Q18_4) == T] <- "0"
table(wiki_survey_en$Q18_dobest, useNA = "ifany")

# I feel my opinion count

table(wiki_survey_en$Q18_5, useNA = "ifany")
wiki_survey_en$Q18_opinion <- as.character(wiki_survey_en$Q18_5)
wiki_survey_en$Q18_opinion[is.na(wiki_survey_en$Q18_5) == T] <- "0"
table(wiki_survey_en$Q18_opinion, useNA = "ifany")

# The mission of wikipedia makes me feel that my work is important:

table(wiki_survey_en$Q18_6, useNA = "ifany")
wiki_survey_en$Q18_mission <- as.character(wiki_survey_en$Q18_6)
wiki_survey_en$Q18_mission[is.na(wiki_survey_en$Q18_6) == T] <- "0"
table(wiki_survey_en$Q18_mission, useNA = "ifany")

# My fellow volunteers are comitted to doing quality work

table(wiki_survey_en$Q18_7, useNA = "ifany")
wiki_survey_en$Q18_fellowquality <- as.character(wiki_survey_en$Q18_7)
wiki_survey_en$Q18_fellowquality[is.na(wiki_survey_en$Q18_7) == T] <- "0"
table(wiki_survey_en$Q18_fellowquality, useNA = "ifany")

# There is somebody in the wikipedia community who seems commited to helping me do good work

table(wiki_survey_en$Q18_8, useNA = "ifany")
wiki_survey_en$Q18_helpgoodwork <- as.character(wiki_survey_en$Q18_8)
wiki_survey_en$Q18_helpgoodwork[is.na(wiki_survey_en$Q18_8) == T] <- "0"
table(wiki_survey_en$Q18_helpgoodwork, useNA = "ifany")

# The decisions I have made as part of the community feel fair

table(wiki_survey_en$Q18_9, useNA = "ifany")
wiki_survey_en$Q18_fairness <- as.character(wiki_survey_en$Q18_9)
wiki_survey_en$Q18_fairness[is.na(wiki_survey_en$Q18_9) == T] <- "0"
table(wiki_survey_en$Q18_fairness, useNA = "ifany")

# I would recomend a friend to get involved in wikipedia

table(wiki_survey_en$Q18_10, useNA = "ifany")
wiki_survey_en$Q18_recomendinvolvement <- as.character(wiki_survey_en$Q18_10)
wiki_survey_en$Q18_recomendinvolvement[is.na(wiki_survey_en$Q18_10) == T] <- "0"
table(wiki_survey_en$Q18_recomendinvolvement, useNA = "ifany")

#### How much time have you spent contributing to Wikipedia during the last seven days (approximately)? - Hours ####

table(wiki_survey_en$Q20_1_TEXT, useNA = "ifany")

# minutes

table(wiki_survey_en$Q20_2_TEXT, useNA = "ifany")

#### different participants of the Wikimedia community: efficacity: ####

# the performance of Wikimedia volunteers overall, in contributing to the Wikimedia movement?

table(wiki_survey_en$T1_1, useNA = "ifany")

# your own performance, in contributing to the Wikimedia movement?

table(wiki_survey_en$T4_1, useNA = "ifany")


#### Demographics: quasiment 50% de NA ####

# Age

table(wiki_survey_en$Q21, useNA = "ifany")

# Education

table(wiki_survey_en$Q22, useNA = "ifany")
wiki_survey_en$Q22_education <- factor(wiki_survey_en$Q22, labels=c("Primary education", "sedondary education", "tertiary education", "master degree", "doctorate"))
table(wiki_survey_en$Q22_education, useNA = "ifany")

# Curently in school (or university)?:

table(wiki_survey_en$Q23, useNA = "ifany")
wiki_survey_en$Q23_currentlyinschool <- factor(wiki_survey_en$Q23, labels=c("non","oui"))
table(wiki_survey_en$Q23_currentlyinschool, useNA = "ifany")

# Are you employed?

table(wiki_survey_en$Q24, useNA = "ifany")
wiki_survey_en$Q24_employement <- factor(wiki_survey_en$Q24, labels=c("Yes, full time", "Yes, part time", "No"))
table(wiki_survey_en$Q24_employement, useNA = "ifany")

# Are you married / partner

table(wiki_survey_en$Q25, useNA = "ifany")
wiki_survey_en$Q25_conjugal <- factor(wiki_survey_en$Q25, labels= c("married", "partner", "single"))
table(wiki_survey_en$Q25_conjugal, useNA = "ifany")

# Do you have children

table(wiki_survey_en$Q26, useNA = "ifany")
wiki_survey_en$Q26_child <- factor(wiki_survey_en$Q26, labels= c("No", "Yes"))
table(wiki_survey_en$Q26_child, useNA = "ifany")

# Gender

table(wiki_survey_en$Q27, useNA = "ifany")
wiki_survey_en$Q27_gender <- factor(wiki_survey_en$Q27, labels= c("Male", "Female", "refuse to answer"))
table(wiki_survey_en$Q27_gender, useNA = "ifany")

