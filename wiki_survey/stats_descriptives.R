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
boxplot(as.numeric(wikitest$Q5_langsum), main="nombre de projet wikipédia dans des langues différentes ou les contributeurs interviennent")

#### Stats bivariées ####


