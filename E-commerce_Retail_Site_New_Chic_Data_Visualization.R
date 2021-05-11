

#Inclusion des PACKAGES: 
#- REST API DATA WORLD pour la recuperation des donnees 
#- dplyr pour la manipulation des dataframes
#- Sampling pour l'echantiollonage
#- ggplot & plotly et les autres packages pour la création de graphismes visuels agréables

library(sampling)
library(data.world)
library(dplyr)
library(ggplot2)
library(plotly)
library(plotrix)
library(ggthemes)
library(gganimate)


#EXPLORATION DE DONNÉES AVEC R:

sql_stmt0 <- qry_sql("SELECT * FROM shoes")
query_results_df0 <- data.world::query(sql_stmt0,"jfreex/products-catalog-from-newchiccom")


names(query_results_df0)
head(query_results_df0, n=3)
tail(query_results_df0)
print(paste("Nombre d'observations",nrow(query_results_df0) , sep = " "))
print(paste("Nombre de colonnes:",ncol(query_results_df0)  , sep = " "))
print(paste("Le nombre de lignes contient des valeurs na:",sum(is.na(query_results_df0$brand)), sep = " "))
print(paste("le nombre de lignes ne contient pas de valeurs na",nrow(query_results_df0) - sum(is.na(query_results_df0$brand)), sep = " "))
#sapply(query_results_df0, function(y) sum(is.na(y)))
#colSums(is.na(query_results_df0))


#- calculer des statistiques descriptives de base

summary(query_results_df0)
length(query_results_df0$subcategory)
mean(query_results_df0$current_price)
sd(query_results_df0$current_price)
#summary(query_results_df0[3])
#str(query_results_df0)



#- Echantiollonage et filtrage des donnees
#Niveau de confiance: 95%
#Intervalle de confiance: 2.70% #5%
#Population : 11823
#Taille de l'échantillon nécessaire: 1186 #373
#roportion de la population: 50%

shoesDataFrame = subset(query_results_df0, select = -c(category,brand,brand_url,codcountry,variation_0_image,variation_1_image,image_url,id)) %>% 
  relocate(model, .before = subcategory)


set.seed(2) #2 représente 20% de chaque groupe
n = 1186 #1186
shoesDataFrame = shoesDataFrame[sample(1:nrow(shoesDataFrame),n), ]


#- show tooltips / compare data on hover
#- click on subcategory to isolate it
#- modele SKU364616 example
  

graph1 = shoesDataFrame  %>% ggplot(aes(x = ventes, y = current_price, color = subcategory)) + 
 geom_point(size = 1.5, alpha = 0.5) + 
  labs(title="La sous-catégorie de chaussures la plus et la moins vendu en termes de prix",
       subtitle="New Chic",
       x= "Nombre de ventes",
       y= "Prix actuel",
       color = "Subcategory") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

ggplotly(graph1)


sql_stmtw <- qry_sql("SELECT subcategory,sum(likes_count) as Nbr_ventes from shoes group by subcategory ")
query_results_dfw = data.world::query(sql_stmtw,"jfreex/products-catalog-from-newchiccom")

View(query_results_dfw)
  


sql_stmt2 <- qry_sql("SELECT * FROM shoes WHERE subcategory 
IN ('Sandales & Mules', 'Derbies & Mocassins', 'Bottes & Bottines')")
query_results_df2 <- data.world::query(sql_stmt2,"jfreex/products-catalog-from-newchiccom")
shoesDataFrame2 = subset(query_results_df2, select = -c(category,brand,brand_url,codcountry,variation_0_image,variation_1_image,image_url,id)) %>% 
  relocate(model, .before = subcategory)
set.seed(2) #2 représente 20% de chaque groupe
n = 1186 #1186
shoesDataFrame2 = shoesDataFrame2[sample(1:nrow(shoesDataFrame),n), ]
names(shoesDataFrame2)[8] <- "ventes"

graph2 = shoesDataFrame2  %>% ggplot(aes(y = discount, color = subcategory)) + 
 geom_bar(size = 2, alpha = 0.5) + 
  labs(title="Graphique de réduction de les chaussures les plus desirables",
       subtitle="",
       y="Discount",
       x="Quantite",
       color = "Subcategory") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

# = graph2 + transition_time(discount)
ggplotly(graph2)


#- Prouve que les plats, les claquettes et pinces sont des produits indésirables

sql_stmt3 <- qry_sql("SELECT DISTINCT subcategory,discount FROM shoes WHERE discount = (SELECT max(discount) From shoes)")
query_results_df3 <- data.world::query(sql_stmt3,"jfreex/products-catalog-from-newchiccom")
query_results_df3
#head(query_results_df3, n=8) 

#Nous pouvons prédire que l'entreprise n'investit pas d'argent dans le bon produit
#car les plats, les claquettes ont un pourcentage énorme par rapport aux produits les plus desirables que sont les derbies et les mocassins

subcategory = table(query_results_df0$subcategory)
lbs_subc = c(query_results_df0$subcategory)
fig = plot_ly(type='pie', labels=lbs_subc[!duplicated(lbs_subc)], values=subcategory,  textinfo='label+percent')
fig <- fig %>% layout(title = 'Les différentes sous-catégories de chaussure du site new chic de vente au détail en ligne')

fig



sql_stmt4 <- qry_sql("SELECT variation_0_color,subcategory FROM shoes WHERE subcategory 
IN ('Sandales & Mules', 'Derbies & Mocassins', 'Bottes & Bottines')")
query_results_df4 <- data.world::query(sql_stmt4,"jfreex/products-catalog-from-newchiccom")
set.seed(2) #2 représente 20% de chaque groupe
n = 1186 #1186
query_results_df4 = query_results_df4[sample(1:nrow(shoesDataFrame),n), ]
graph4 = query_results_df4 %>% ggplot(aes(x = variation_0_color, color = subcategory)) + 
 	geom_bar(size = 1.5, alpha = 0.5) + 
  labs(title="Les couleurs les plus desirables de la sous-catégorie la plus appréciée",
       subtitle="",
       x= "Couleurs",
       y="Quantite",
       color = "Sous-catégorie") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

ggplotly(graph4)