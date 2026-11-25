data <- read.csv("C:/Users/audre/Downloads/Reponses - Réponses au formulaire 1.csv")
head(data)
str(data)
summary(data)
data <- data[, -1]
##on renommer les noms 
colnames(data)<-c(
  "promotion",
  "boursier",
  "frequence",
  "motivation",
  "temps_trajet",
  "regime_alimentaire",
  "quantite",
  "satisfaction"
)
###Analyse descriptive
##Etude sur la promotion 
table(data$promotion)
prop.table(table(data$promotion))*100
barplot(table(data$promotion),
        main = "repartition des etudiants par promotion",
        ylab="Nombre des etudiants",
        col="blue",
        las = 2)

##Etude  sur les boursiers ou non 
effectifs <- table(data$boursier)
proportions <- prop.table(effectifs)
pourcentages <- proportions *100
effectifs
proportions
pourcentages

#Diagramme en cercle
couleurs <- c("lightgreen", "lightyellow")
pie(effectifs,
    labels = paste0(names(pourcentages), "(", pourcentages, "%)"),
    col = couleurs,
    main = "repartition des etudiants boursiers / non boursiers")

###Etude sur la frequence au RU
summary(data$frequence)
mean(data$frequence,na.rm = TRUE)
median(data$frequence,na.rm = TRUE)
sd(data$frequence,na.rm = TRUE)
hist(data$frequence,
     main = "La frequence hebdomadaire de frequentation au RU",
     xlab = "nombre de jours par semaine",
     ylab = "Nombre des etudiants")
##Etude sur la motivation 
table(data$motivation)
prop.table(table(data$motivation))
###
table_motivation <- table(data$motivation)
pct <- round(prop.table(table_motivation) * 100, 1)
labels_complets <- paste0(names(table_motivation), " (", pct, "%)")
pie(table_motivation, 
    labels = labels_complets, 
    main = "Raisons de fréquentation du Resto U", 
    col = c("gold", "tomato", "skyblue","orange"),
)

###Etude sur le trajet 
table(data$temps_trajet)
prop.table(table(data$temps_trajet)) * 100
barplot(table(data$temps_trajet),
        main = "Temps de trajet logement ↔ IUT",
        xlab = "Nombre des  etudiants",
        ylab = "",
        horiz= TRUE,
        las = 2)

###Etude sur le regime alimentaire 
table_regime_alimentaire <- table(data$regime_alimentaire)
table_regime_alimentaire
par(mar = c(5, 8, 4, 2))##c est pour configurer l espace et le reglage specifique pour que les legendes soient entierment lisibles
barplot(table_regime_alimentaire, 
        main = "Répartition des régimes alimentaires",
        xlab = "Nombre d'étudiants",
        col = "skyblue",
        horiz = TRUE, 
        las = 1)

##Etude sur la quantite 
table_quantite <- table(data$quantite)
table_quantite
barplot(table_quantite,
        main = "La quantité est-elle suffisante ?",
        xlab = "Nombre d'étudiants",  
        ylab = "",                  
        col = "mediumpurple",
        horiz = TRUE,               
        las = 1)

###Etude sur la satisfaction 
table_satisfaction <- table(data$satisfaction)
table_satisfaction
barplot(table_satisfaction,
        main = "Satisfaction globale",
        xlab= "Nombre d'étudiants",
        ylab="",
        col = "palegreen",
        horiz = TRUE,
        las=1)


data$motivation <- factor(data$motivation,
                          levels = c("nécessité", "choix", "ça dépend", "pas au RU"))

# Boxplot 
boxplot(frequence ~ motivation,
        data = data,
        main = "Fréquentation du RU selon la motivation",
        xlab = "Motivation",
        ylab = "Nombre de jours par semaine",
        col = "lightblue",
        border = "darkblue",
        las = 2,
        cex.axis = 1,
        cex.main = 1.2)

### Dotplot : fréquence RU

dotchart(table(data$frequence),
         main = "Répartition des étudiants selon la fréquence RU",
         xlab = "Nombre d'étudiants",
         ylab = "Jours par semaine",
         color = "purple")
