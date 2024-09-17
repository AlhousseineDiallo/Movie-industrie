# Chargement des librairies nécessaires
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

# Charger les données à partir du chemin
path <- "/content/drive/MyDrive/datasets/movies.csv"
df <- read_csv(path)

# Regarder un aperçu des données
df

# Afficher les premières lignes
head(df)

# Afficher les dernières lignes
tail(df)

# Afficher la taille du dataset
dim(df)

# Informations utiles sur notre dataset
str(df)

# Types de données des colonnes
sapply(df, class)

# Vérifions s'il y a des données manquantes
for (col in colnames(df)) {
  pct_missing <- mean(is.na(df[[col]])) * 100
  print(paste("La colonne", col, "a", pct_missing, "% de valeurs manquantes"))
}

# Supprimer les valeurs manquantes
df <- drop_na(df)

# Vérification de la taille après suppression des valeurs manquantes
dim(df)

# Changer le type des données pour la colonne budget
df$budget <- as.integer(df$budget)
df$budget

# Afficher les 15 dernières lignes du dataset
tail(df, 15)

# Extraire l'année correcte de la colonne 'released'
df$correct_year <- as.integer(str_extract(df$released, '\\d{4}'))
df$correct_year

# Trier les données par les gains (gross) de manière décroissante
df <- df %>% arrange(desc(gross))
df

# Supprimer les doublons
df <- distinct(df)

# Styles de visualisation disponibles dans ggplot2
# ggplot2 ne possède pas de styles directement comparables à plt.style mais propose des thèmes prédéfinis

# Recherche de corrélation dans les données
# Scatter plot avec budget et revenu (gross)
ggplot(df, aes(x = budget, y = gross)) +
  geom_point() +
  ggtitle("Budget vs Gains") +
  xlab("Budget du film") +
  ylab("Gains bruts")

# Régression linéaire avec ggplot2
ggplot(df, aes(x = budget, y = gross)) +
  geom_point(color = 'red') +
  geom_smooth(method = 'lm', color = 'blue', size = 1.5)

# Commencer à examiner les corrélations
cor(df %>% select_if(is.numeric), use = "complete.obs")

# Matrice de corrélation avec heatmap
correlation_matrix <- cor(df %>% select_if(is.numeric), use = "complete.obs")
ggplot(melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle('Matrice de corrélation des caractéristiques numériques') +
  xlab('Caractéristiques des films') +
  ylab('Caractéristiques des films')

# Analyse des catégories comme 'company'
head(df)

# Conversion des colonnes 'object' (factor en R) en codes numériques
df_num <- df
for (col_name in names(df_num)) {
  if (class(df_num[[col_name]]) == 'character') {
    df_num[[col_name]] <- as.numeric(as.factor(df_num[[col_name]]))
  }
}
df_num

# Calcul et visualisation de la matrice de corrélation des caractéristiques numériques
correlation_matrix_1 <- cor(df_num, use = "complete.obs")
ggplot(melt(correlation_matrix_1), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle('Matrice de corrélation des caractéristiques numériques')

# Afficher les paires de corrélation triées
corr_pairs <- melt(correlation_matrix_1)
corr_pairs <- corr_pairs[order(-abs(corr_pairs$value)), ]
head(corr_pairs)

# Afficher les paires avec une corrélation > 0.5
high_correlation <- corr_pairs[abs(corr_pairs$value) > 0.5, ]
high_correlation

# En conclusion, les gains bruts dépendent fortement des votes et du budget
