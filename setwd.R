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

## Reseau contro edit

setwd('~/Documents/EHESS/SocStat-M2/memoire/networks/contro/edits/')

## Reseau contro talk

setwd('~/Documents/EHESS/SocStat-M2/memoire/networks/contro/talk/')

## Reseau featured edit

setwd('~/Documents/EHESS/SocStat-M2/memoire/networks/featured/edit/')

## Reseau featured talk

setwd('~/Documents/EHESS/SocStat-M2/memoire/networks/featured/talk/')


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

# Et page_attributes

page_attributes_edits <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/page_network/page_page_attributes_edits.csv")
page_attributes_talks <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/page_network/page_page_attributes_talks.csv")


# node_attributes_contro

attributes_contro <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/contro_network/nodes_attributes_contro_network.csv")
contro_attributes_edits <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/contro_network/contro_contro_attributes_edits.csv")
contro_attributes_talks <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/contro_network/contro_contro_attributes_talks.csv")
contro_attributes_cross <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/contro_network/contro_contro_attribute_cross.csv")


# node_attributes_featured

attributes_featured <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/featured_network/nodes_attributes_featured_network.csv")
featured_attributes_edits <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/featured_network/featured_page_attributes_edits.csv")
featured_attributes_talks <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/featured_network/featured_page_attributes_talks.csv")
featured_attributes_cross <- read.csv2("~/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/featured_network/featured_page_attributes_cross.csv")



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

## Reseau page edit

setwd('C:/Users/tenroc/Documents/networks/pages/edits/')

## Reseau page talk

setwd('C:/Users/tenroc/Documents/networks/pages/talk/')

## Reseau contro edit

setwd('C:/Users/tenroc/Documents/networks/contro/edits/')

## Reseau contro talk

setwd('C:/Users/tenroc/Documents/networks/contro/talk/')

## Reseau featured edit

setwd('C:/Users/tenroc/Documents/networks/featured/edit/')

## Reseau featured talk

setwd('C:/Users/tenroc/Documents/networks/featured/talk/')

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

# nodes_atttributes_page + bases

attributes_page <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/page_network/nodes_attributes_page_network.csv")
page_attributes_edits <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/page_network/page_page_attributes_talks.csv")
page_attributes_talks <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/page_network/page_page_attributes_edits.csv")

# node_attributes_contro

attributes_contro <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/contro_network/nodes_attributes_contro_network.csv")
contro_attributes_edits <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/contro_network/contro_contro_attributes_edits.csv")
contro_attributes_talks <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/contro_network/contro_contro_attributes_talks.csv")
contro_attributes_cross <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/contro_network/contro_contro_attribute_cross.csv")

# node_attributes_featured

attributes_featured <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/featured_network/nodes_attributes_featured_network.csv")
featured_attributes_edits <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/featured_network/featured_page_attributes_edits.csv")
featured_attributes_talks <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/featured_network/featured_page_attributes_talks.csv")
featured_attributes_cross <- read.csv2("C:/Users/tenroc/Documents/memoire-wiki/wiki_reseaux/ensemble_reseaux/featured_network/featured_page_attribute_cross.csv")

