## Tests: bases attributes: si quelque chose foire dans le script de composition des bases atttributes

# tests

a <-c("Bugboy52.40", "trala67.89","seim","eiufhuze","168.365.13","222:333:444:5","64.3oulala","2607:F470:6:5002:A1FC:502:7F3D:5655")

grepl("^*\\d+\\.\\d+\\.\\d+", a)

grepl("^*.*\\:.*\\:.*\\:.*\\:",a)

grepl("^*.*\\:.*\\:.*\\:.*\\:",a)

attributes_page[grepl("^*.*\\:.*\\:.*\\:.*\\:",attributes_page$contributeurs) == T & attributes_page$isanon == 0,"contributeurs"]

grepl("*[a-z]",a)


attributes_contro[as.character(attributes_contro$contributeurs) %in% as.character(ledits[[i]][j,"ActiveUser"]),
                  "contributeurs"]

attributes_contro[as.character(attributes_contro$contributeurs) %in% as.character(ledits[[i]][j,"ActiveUser"]),
                  "isanon"]

View(ledits[["Hentai.csv_edits"]])
levels(as.factor(ledits[["Hentai.csv_edits"]][,"ActiveUser"]))


attributes_contro$X[attributes_contro$contributeurs == "Larry_Sanger"][2] <-102729
a <- attributes_contro[attributes_contro$contributeurs == "Larry_Sanger", ]
attributes_contro3 <- subset(attributes_contro[attributes_contro$contributeurs != "209.240.222.xxx",])
attributes_contro3 <- merge(attributes_contro3, a, by ="contributeurs", all = T)

a <- attributes_contro[attributes_contro$contributeurs == "132.235.232.xxx", ][1,]
attributes_contro3 <- subset(attributes_contro[attributes_contro$contributeurs != "132.235.232.xxx",])
attributes_contro3 <- merge(attributes_contro3, a, by ="contributeurs", all = T)

attributes_contro[attributes_contro$contributeurs.x == "209.240.222.xxx",]
attributes_contro[attributes_contro$contributeurs.x == "213.253.40.xxx",]
attributes_contro[attributes_contro$contributeurs.x == "141.211.45.xxx",]
attributes_contro[attributes_contro$contributeurs.x == "213.76.2.xxx",]

attributes_contro$X <- rownames(attributes_contro)
attributes_contro2 <- merge(attribute_contro3, attributes_contro, by="X")

attributes_contro <- attributes_contro2
attributes_contro <- attributes_contro[,c(1:6)]
attributes_contro <- unique(attributes_contro)
colnames(attributes_contro) <- c("X","contributeurs","isanon","status_contrib","total_rev_count","registration_year")