
#**Exploration des données**
  
data=read.csv("C:\Users\user\R projects\Customers data.csv")
str(data)

names(data)

# Nous allons afficher les six premières lignes de notre jeu de données à l'aide de la fonction head() 
# et utiliser la fonction summary() pour en produire un résumé.

head(data)

summary(data$Age)

sd(data$Age)

summary(data$Annual.Income..k..)

sd(data$Annual.Income..k..)

summary(data$Spending.Score..1.100)

sd(data$Spending.Score..1.100)

#**Visualisation du sexe des clients**

# Dans ce cas, nous allons créer un graphique à barres et un graphique circulaire pour montrer la distribution du 
# Genre dans notre dataset.

i = table(data$Gender)
barplot(i,main="Distribution du Genre",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(i))

# En observant le graphique à barres, on remarque que le nombre de femmes est plus élevé que celui des hommes. 

# Visualisons maintenant, un graphique circulaire pour observer le ratio de la distribution des hommes et des femmes.

pct=round(i/sum(i)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Représentation du ratio entre les femmes et les hommes")

# En observant le graphique, on remarque que le pourcentage de clients feminins est de 56% tandis que celui des
# clients masculins est de 44%.


#**Visualisation de la distribution des âges**

# Utilisons ici un histogramme pour visualiser la distribution de la fréquence des âges des clients. 
# Mais dabords, procédons à un résumé de la variable Age.

summary(data$Age)


hist(data$Age,
     col="blue",
     main="Nombre de client par tranche d'ages",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)


boxplot(data$Age,
        col="#ff0066",
        main="Représentation graphique en boîte à moustaches de l'âge")

#Les deux visualisations nous permettent de conclure que l'âge moyen des clients se situe
#entre 30 et 35 ans. L'âge minimum est de 18 ans, tandis que l'âge maximum est de 70 ans.


#**Analyse du revenu annuel des clients**

# Nous allons créer des visualisations pour analyser le revenu annuel des clients. 
# Nous allons tracer un histogramme, puis nous examinerons ces données à l'aide d'un graphique de densité.

summary(data$Annual.Income..k..)
hist(data$Annual.Income..k..,
     col="#660033",
     main="Revenu annuel des clients",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)


plot(density(data$Annual.Income..k..),
     col="yellow",
     main="Densité du revenu annuel des clients",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(data$Annual.Income..k..),
        col="#ccff66")

# L'analyse descriptive nous permet de conclure que le revenu annuel minimum des clients est de 15 
# et le revenu maximum est de 137. 
# Les personnes ayant un revenu moyen de 70 ont la fréquence la plus élevée et le salaire moyen de 
# tous les clients est de 60,56. 
# Le graphe de la densité nous permet de conclure que le revenu annuel suit une distribution normale.


#**Analyse du score des dépenses des clients**

summary(data$Spending.Score..1.100.)


boxplot(data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="green",
        main="Représentation graphique en boîte à moustaches 
        du score des dépenses")


hist(data$Spending.Score..1.100.,
     main="Histogramme du score des dépenses",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="green",
     labels=TRUE)

# Le score de dépenses minimum est de 1, le maximum est de 99 et la moyenne est de 50.20. A partir de 
# l'histogramme, nous concluons que les clients entre les classes 40 et 50 ont le score de dépenses le 
# plus élevé parmi toutes les classes.


#** Implementation de l'algorithme K-means*

#** Résumé des etapes de l'algo.*

# Nous spécifierons le nombre de clusters que nous devons créer.

# L'algorithme sélectionne k objets au hasard dans l'ensemble de données. Cet objet est le cluster 
# initial ou la moyenne.

# Le centroïde le plus proche obtient l'affectation d'une nouvelle observation. Nous basons cette 
# affectation sur la distance euclidienne entre l'objet et le centroïde.

# k clusters dans les points de données mettent à jour le centroïde par le calcul des nouvelles valeurs 
# moyennes présentes dans tous les points de données du cluster. Le centroïde du k-ième cluster a une 
# longueur de p qui contient les moyennes de toutes les variables pour les observations du k-ième cluster. 
# Nous désignons le nombre de variables par p.

# Minimisation itérative du total dans la somme des carrés. Ensuite, par la minimisation itérative du 
# total de la somme des carrés, l'affectation cesse de vaciller lorsque nous atteignons l'itération 
# maximale. La valeur par défaut est 10 que le logiciel R utilise pour le maximum d'itérations.


#** Détermination des clusters optimaux.*

# Pour déterminer le nombre optimal de clusters, il existe trois méthodes populaires : 

# La méthode du coude ou Elbow method en anglais.
# La méthode de la silhouette ou Silhouette method en anglais.
# Statistique de l'écart ou Gap statistic en anglais.

1. #**La methode Elbow**

# L'objectif principal des méthodes de partitionnement des clusters comme les k-means est de définir 
# les clusters de telle sorte que la variation intra-clusters reste minimale.


library(purrr)
set.seed(123)
# function pour calculer la somme totale des carrés intra-clusters. 
iss <- function(k) {
  kmeans(data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10


iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

# D'après le graphique, nous concluons que 4 est le nombre approprié de clusters puisqu'il 
# semble apparaître au niveau du coude du graphique.


#**Méthode de la silhouette moyenne**

# La méthode de la silhouette moyenne calcule la moyenne des observations de la silhouette
# pour différentes valeurs de k. Avec le nombre optimal de k clusters, on peut maximiser la silhouette 
# moyenne sur les valeurs significatives pour k clusters.

# En utilisant la fonction silhouette dans le package cluster, nous pouvons calculer la largeur moyenne 
# de la silhouette en utilisant la fonction kmean. Ici, le cluster optimal possédera la moyenne la plus 
# élevée.

library(cluster) 
library(gridExtra)
library(grid)

k2<-kmeans(data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(data[,3:5],"euclidean")))


k3<-kmeans(data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(data[,3:5],"euclidean")))

k4<-kmeans(data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(data[,3:5],"euclidean")))


k5<-kmeans(data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(data[,3:5],"euclidean")))


k6<-kmeans(data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(data[,3:5],"euclidean")))

k7<-kmeans(data[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(data[,3:5],"euclidean")))


k8<-kmeans(data[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(data[,3:5],"euclidean")))

k9<-kmeans(data[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(data[,3:5],"euclidean")))


k10<-kmeans(data[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(data[,3:5],"euclidean")))

# Nous utiliserons maintenant la fonction fviz_nbclust() pour déterminer et visualiser le nombre optimal 
# de clusters.

library(NbClust)
library(factoextra)

fviz_nbclust(data[,3:5], kmeans, method = "silhouette")


#** Méthode de la statistique de l'écart (Gap Statistic Method)**

# Nous pouvons utiliser la fonction clusGap qui fournit les statistiques d'écart ainsi que l'erreur
# standard pour une sortie donnée.

set.seed(125)
stat_gap <- clusGap(data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

# Prenons k = 6 comme cluster optimal.

k6<-kmeans(data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6


# Dans la sortie de notre opération kmeans, nous observons une liste avec plusieurs informations clés. 
# Nous en déduisons que l'information utile est :

#**cluster* : C'est un vecteur de plusieurs entiers qui dénotent le cluster qui a une allocation de 
# chaque point.

#**totss* : représente la somme totale des carrés.

#**centers* : Matrice comprenant plusieurs centres de cluster.

#**withinss* : C'est un vecteur représentant la somme des carrés intra-clusters ayant une composante par 
# cluster.

#**tot.withinss* : Il s'agit de la somme totale des carrés intra-clusters.

#**betweenss* : Il s'agit de la somme des carrés inter-clusters.

#**size* : Le nombre total de points que chaque cluster contient.


#**Visualisation des résultats de la classification à l'aide des deux premières composantes principales*

acpclust=prcomp(data[,3:5],scale=FALSE) # Analyse des composantes principales (ACP)
summary(acpclust)


acpclust$rotation[,1:2]


## Visualisons les clusters

set.seed(1)
ggplot(data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments des clients de l'entreprise.", subtitle = "En utilisant le K-means clustering.")


# À partir de la visualisation, nous observons qu'il y a une distribution de 6 clusters comme suit :

# Cluster 1 et 5: Ces clusters représentent les clients ayant un revenu salarial moyen ainsi qu'une 
# dépense annuelle moyenne.

# Cluster 2: Ce groupe représente les clients ayant un revenu annuel faible et des dépenses annuelles faibles.

# Cluster 3: Ce groupe indique un revenu annuel élevé et des dépenses annuelles faibles.

# Cluster 4: Ce cluster représente les clients ayant un revenu annuel élevé ainsi qu'une dépense annuelle 
# élevée.

# Cluster 6: Ce cluster représente un revenu annuel faible mais des dépenses annuelles élevées.


ggplot(data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments des clients de l'entreprise", subtitle = "En utilisant le K-means clustering.")


kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(acpclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))


# Cluster 5 et 1: Ces deux clusters sont constitués de clients ayant un score PCA1 et PCA2 moyen.

# Cluster 4: Ce cluster représente les clients ayant un PCA2 moyen et un PCA1 faible.

# Cluster 3: Ce cluster regroupe des clients ayant un score PCA1 moyen et un score PCA2 faible.

# Cluster 2: Ce cluster comprend des clients ayant un revenu PCA1 élevé et un PCA2 élevé.

# Cluster 6: Ce groupe comprend des clients ayant un PCA2 élevé et des dépenses annuelles moyennes.


# Avec l'aide du clustering, nous pouvons beaucoup mieux comprendre les variables, ce qui nous incite à
# prendre des décisions prudentes. 
# Grâce à l'identification des clients, les entreprises peuvent lancer des 
# produits et des services qui ciblent les clients en fonction de plusieurs paramètres tels que le revenu, 
# l'âge, les habitudes de consommation, etc. En outre, des modèles plus complexes, comme les avis sur les 
# produits, sont pris en considération pour une meilleure segmentation.



