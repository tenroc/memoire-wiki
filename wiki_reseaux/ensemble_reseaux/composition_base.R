#### Recupperer la liste des pages controversees ####

controversy <- page_links('Wikipedia:List_of_controversial_issues',"en")
controversy_data <- as.character(controversy[[1]])

# sample de 200: 

controversy_sample <- sample(controversy_data, 200)

#### Recupperer la liste des articles mis en avant #####

featured <- page_links('Wikipedia:Featured_articles',"en")
featured_data <- as.character(featured[[1]])

# sample de 200:

featured_sample <- sample(featured_data, 200)

#### Et un sample de 200 pages au hasard ####

page_sample_data <- random_page("en", project="wikipedia", namespaces = 0, limit=200)
