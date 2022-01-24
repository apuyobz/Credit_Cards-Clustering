library(tidyverse)

cred_dat <- read_csv("../Tarjetas de crédito.csv")
head(cred_dat)


# 1. Análisis de datos
library(skimr)

# Estructura general
glimpse(cred_dat)
skim(cred_dat)
str(cred_dat)
summary(cred_dat)

# Existencia de NA´s
any(!complete.cases(cred_dat))
map_dbl(cred_dat, .f = function(x){sum(is.na(x))})

# comportamiento de NA´s
library(VIM)

aggr(cred_dat,
     numbers = T,
     sortVars = T)

# Puesto que sólo es el 3% de los datos de esa columna que están son NA, entonces se decide eliminar esas filas
cred_dat_limp <- na.omit(cred_dat)

# Escalado de las variables
cred_dat_limp <- as.data.frame(scale(cred_dat_limp[, 2:18]))

# Comportamiento de las variables
library(GGally)

png(height=1200, width=1500, pointsize=15, file="Comp_Var.png")
ggpairs(cred_dat_limp[, 2:18], 
        aes(alpha = 0.4),
        title = "Comportamiento entre Variables") +
  theme(plot.title = element_text(hjust = 0.5))

# Correlaciones:

# Con ggcorr
png(height=1200, width=1500, pointsize=15, file="Corr_var.png")
ggcorr(data = cred_dat_limp[, 2:18], 
       method = c("everything", "pearson"),
       digits = 2,
       low = "darkred",
       mid = "white",
       high = "steelblue",
       label = T,
       size = 4,
       color = "grey50",
       angle = -45)

# Con corrplot
library(corrplot)

png(height=1200, width=1500, pointsize=15, file="Corr_var_2.png")
corrplot(cor(cred_dat_limp[, 2:18]), 
         method = "circle",
         type = "upper",
         shade.col = NA, 
         tl.col = "black",
         addCoef.col="gray50",
         tl.srt = 45)

#------------------------------------------------------------------------------

# 2. Clustering
library(factoextra)
library(cluster)
library(NbClust)

# Sugerencia de N° de clusters
# Debido al tamaño de datos, no se usaran todos los metodos para tomar una decisión por lo que index no será "all" si no, unos tantos
n_clust <- NbClust(cred_dat_limp, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "kmeans",
              index = c("gap", "kl", "silhouette", "db"))

n_clust

# Gráfico especial de factoextra, para los clusters de nb
fviz_nbclust(n_clust) + theme_minimal()

# Probando el kmeans con la recomendación 
km <- kmeans(cred_dat_limp, 12)

# Gráficos del resultado de los clusters
fviz_cluster(km, cred_dat_limp)

fviz_cluster(km, cred_dat_limp, ellipse.type = "euclid",
             repel = T, star.plot = T)

# Composición de los clusters
skm_silh <- silhouette(km$cluster,
                     dist(cred_dat_limp))

resum_silh <- summary(skm_silh)
resum_silh

# Gráfica de la composición del clustering
fviz_silhouette(skm_silh)

#-----------------------------------------------------------------------------

# 3. Análisis de los clusters y uso de la información

# Creo un Df donde se le anexe el cluster de cada usuario
df_cred <- cred_dat
df_cred <- df_cred %>%
  na.omit() %>%
  mutate(Cluster = km$cluster)

# Modifico sus clasifiaciones y escalo las variables numericas
df_cred$Cluster <- as.factor(df_cred$Cluster)
df_cred <- map_if(df_cred, 
                  .p = is.numeric, 
                  .f = function(x){scale(x)})
df_cred <- as.data.frame(df_cred)

# Se realiza un gather para manipular y análizar mejor los datos
data_long <- gather(df_cred, 
                    Variable, 
                    Valor, 
                    BALANCE:TENURE, 
                    factor_key = T)
head(data_long, 20)

# Gráfico los valores de la nueva tabla para entender mejor el comportamiento de los clusters
ggplot(data_long, aes(as.factor(x = Variable),
                      y = Valor,
                      group = Cluster,
                      colour = Cluster)) +
  stat_summary(fun = mean,
               geom = "pointrange",
               size = 1) +
  stat_summary(geom = "line") +
  theme(axis.text.x = element_text(angle = 90))

