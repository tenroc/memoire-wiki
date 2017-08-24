## Setwd script:

##### Linux: ####

setwd('~/Documents/EHESS/SocStat-M2/memoire/')

library('igraph')
library('WikipediaR')
library('WikiSocio')
library('WikipediR')

## setwd pour le wikisurvey

setwd('~/Documents/memoire-wiki/wiki_survey/')

library('FactoMineR')
library('lmtest')
library('foreign')
library('R2HTML')
library('readODS')

## Pour les réseaux page/edit

setwd('~/Documents/EHESS/SocStat-M2/memoire/networks/pages/edits/')

## Pour les réseaux page/talk

setwd('~/Documents/EHESS/SocStat-M2/memoire/networks/pages/talk/')

## Charger les bases:

# List admin:

admin_list_definitive <- read.table("~/Documents/memoire-wiki/wiki_reseaux/admin_list_definitive.txt")
admin_list_definitive <- as.character(admin_list_definitive$V1)

# Liste des pages controversees

controversy_data <- read.table("~/Documents/memoire-wiki/wiki_reseaux/controversy_list.txt")
controversy_data <- as.character(controversy_data)

# Liste des pages mises en avant

featured_data <- read.table("~/Documents/memoire-wiki/wiki_reseaux/featured_list.txt")
featured_data <- as.character(featured_data)

# Liste des pages 

# nodes_atttributes_page

attributes_page <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/page_network/nodes_attributes_page_network.csv")


#### Windows: #####

setwd('C:/Users/tenroc/Documents/contro')

library('igraph')
library('WikipediaR')
library('WikiSocio')
library('WikipediR')

# wikisurvey

setwd('C:/Users/tenroc/Documents/memoire-wiki/wiki_survey/')

library('FactoMineR')
library('lmtest')
library('foreign')
library('R2HTML')
library('readODS')
library('Factoshiny')

## Reseau page edit

setwd('C:/Users/tenroc/Documents/networks/pages/edits/')

## Reseau page talk

setwd('C:/Users/tenroc/Documents/networks/pages/talk/')

## charger les bases:

# list admins

admin_list_definitive <- read.table("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/admin_list_definitive.txt")
admin_list_definitive <- as.character(admin_list_definitive$V1)

# Liste des pages controversees

controversy_data <- read.table("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/controversy_list.txt")
controversy_data <- as.character(controversy_data)

# Liste des pages mises en avant

featured_data <- read.table("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/featured_list.txt")
featured_data <- as.character(featured_data)

# Liste des pages 

# nodes_atttributes_page

attributes_page <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/page_network/nodes_attributes_page_network.csv")

