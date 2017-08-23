#### Recupperer la liste des pages controversees ####

controversy <- page_links('Wikipedia:List_of_controversial_issues',"en")
controversy_data <- as.character(controversy[[1]])
controversy_data <- gsub(" ","_",controversy_data)

write(controversy_data, file="controversy_list.txt", sep="/n")

# sample de 200: 

controversy_sample <- sample(controversy_data, 100)

# export

write(controversy_sample, file="controversy_sample.txt", sep="/n")

#### Recupperer la liste des articles mis en avant #####

featured <- page_links('Wikipedia:Featured_articles',"en")
featured_data <- as.character(featured[[1]])
featured_data <- gsub(" ","_",featured_data)

write(featured_data, file="featured_list.txt", sep="/n")

# sample de 200:

featured_sample <- sample(featured_data, 100)

# export

write(featured_sample, file="featured_sample.txt", sep="/n")

#### Et un sample de 200 pages au hasard ####

page_sample_data <- random_page("en", project="wikipedia", namespaces = 0, limit=100)
page_sample <- character()

for (i in 1:100){
  page_sample[i] <- paste(page_sample_data[[i]][1])
}

# export

write(page_sample, file="page_sample.txt", sep="/n")

# Obtenir la liste complète des administrateurs en activité, et précédants:

admin_list_former <- read.table('C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/former_admin_list.txt', header = F)
admin_list_active <- read.table('C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/admin_list_active.txt', header = F)
admin_list_semi_active <- read.table('C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/admin_list_semi_active.txt', header = F)
admin_list_inactive <- read.table('C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/admin_list_inactive.txt', header = F, sep="#")
admin_list_inactive$V1 <- gsub(" - .*$", "", as.character(admin_list_inactive$V1))
admin_list_inactive$V1 <- gsub(" ","_",admin_list_inactive$V1)
temp <- character()
admin_list_definitive <- levels(as.factor(append(temp, c(as.character(admin_list_active$V1), as.character(admin_list_semi_active$V1),
                                       as.character(admin_list_former$V1), as.character(admin_list_inactive$V1)))))

write(admin_list_definitive, file="admin_list_definitive.txt", sep="/n")