#am incarcat setul de date prelucra
date <- read.csv('set_de_date_prelucrat.csv')

colnames(date) <- c("nume_tara", "cod_tara", "exporturi", "exporturi (%pib)", "pib", 
                    "pib/nr_loc", "economie_interna (%pib)", "consum_intern (%pib)", 
                    "investitii_publice (%pib)", "inflatie_consum", 
                    "investitii_straini (%pib)", "urbanizare")

# lucram doar cu coloanele numerice
coloane_numerice <- date[, c("exporturi", "exporturi (%pib)", "pib", "pib/nr_loc",
                           "economie_interna (%pib)", "consum_intern (%pib)", 
                           "investitii_publice (%pib)", "inflatie_consum", 
                           "investitii_straini (%pib)", "urbanizare")]

#distanta dintre observatii prin metoda euclidiana
distante_euclidiene <- dist(coloane_numerice, method = "euclidean")

#metoda Manhattan
distante_manhattan <- dist(coloane_numerice, method = "manhattan")

#matricea distantelor 
matrice_eucl <- as.matrix(distante_euclidiene)
matrice_manh <- as.matrix(distante_manhattan)

print("Distante euclidiene pentru primele 10 observatii:")
print(matrice_eucl[1:10, 1:10])

print("Distante Manhattan pentru primele 10 observatii:")
print(matrice_manh[1:10, 1:10])

date_standardizate <- scale(coloane_numerice)

#clusterizare comeplta
clusterizare_completa <- hclust(dist(date_subset, method = "euclidean"), method = "complete")
print(clusterizare_completa)

#clusterizare prin metoda Ward
clusterizare_ward <- hclust(dist(date_subset, method = "euclidean"), method = "ward.D2")
print(clusterizare_ward)

date_subset <- date_standardizate[1:50, ]
#dendograme
par(mfrow = c(1, 2))
#dendograma metoda completa
plot(clusterizare_completa, main = "Dendograma - metoda completa", xlab = "", sub = "", cex = 0.6)
par(mfrow = c(1, 2))
#dendograma metoda ward
plot(clusterizare_ward, main = "Dendograma - metoda ward", xlab = "", sub = "", cex = 0.6)

#numarul optim de clustere
#metoda elbow
install.packages("factoextra")
library(factoextra)
fviz_nbclust(date_standardizate, FUN = hcut, method = "wss") + 
  ggtitle("Indicele Elbow pentru numarul optim de clustere")

#indicele Silhouette
fviz_nbclust(date_standardizate, FUN = hcut, method = "silhouette") + 
  ggtitle("Indicele Silhouette pentru numărul optim de clustere")

#calcul gap statistic
install.packages("cluster")
library(cluster)
set.seed(123) 
gap_stat <- clusGap(date_standardizate, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) + 
  ggtitle("Gap Statistic pentru numarul optim de clustere")

#adaugam numarul de 4 clustere la date
hclust_ward <- hclust(dist(date_standardizate, method = "euclidean"), method = "ward.D2")
clustere <- cutree(hclust_ward, k = 4)
date_cu_clustere <- cbind(date, Cluster = clustere)

install.packages("dplyr")
library(dplyr)
#indicatori statistici clustere

pib_medie <- tapply(date_cu_clustere$pib, date_cu_clustere$Cluster, mean, na.rm = TRUE)
pib_mediana <- tapply(date_cu_clustere$pib, date_cu_clustere$Cluster, median, na.rm = TRUE)
pib_sd <- tapply(date_cu_clustere$pib, date_cu_clustere$Cluster, sd, na.rm = TRUE)

exporturi_medie <- tapply(date_cu_clustere$exporturi, date_cu_clustere$Cluster, mean, na.rm = TRUE)
exporturi_mediana <- tapply(date_cu_clustere$exporturi, date_cu_clustere$Cluster, median, na.rm = TRUE)
exporturi_sd <- tapply(date_cu_clustere$exporturi, date_cu_clustere$Cluster, sd, na.rm = TRUE)

urbanizare_medie <- tapply(date_cu_clustere$urbanizare, date_cu_clustere$Cluster, mean, na.rm = TRUE)
urbanizare_mediana <- tapply(date_cu_clustere$urbanizare, date_cu_clustere$Cluster, median, na.rm = TRUE)
urbanizare_sd <- tapply(date_cu_clustere$urbanizare, date_cu_clustere$Cluster, sd, na.rm = TRUE)

rezultate <- data.frame(
  Cluster = 1:4,
  pib_medie = pib_medie,
  pib_mediana = pib_mediana,
  pib_sd = pib_sd,
  exporturi_medie = exporturi_medie,
  exporturi_mediana = exporturi_mediana,
  exporturi_sd = exporturi_sd,
  urbanizare_medie = urbanizare_medie,
  urbanizare_mediana = urbanizare_mediana,
  urbanizare_sd = urbanizare_sd
)

#tabel conclusiv
numar_observatii = table(date_cu_clustere$Cluster)
tabel_conclusiv <- data.frame(
  Cluster = names(numar_observatii),
  nr_observatii = as.numeric(numar_observatii),
  pib_medie = pib_medie,
  pib_mediana = pib_mediana,
  pib_sd = pib_sd,
  exporturi_medie = exporturi_medie,
  exporturi_mediana = exporturi_mediana,
  exporturi_sd = exporturi_sd,
  urbanizare_medie = urbanizare_medie,
  urbanizare_mediana = urbanizare_mediana,
  urbanizare_sd = urbanizare_sd
)

print(tabel_conclusiv)

#algoritmul k-means
set.seed(123) 
kmeans_result <- kmeans(date_standardizate, centers = 4, nstart = 25)
print(kmeans_result)

data_cu_kmeans <- cbind(date, Cluster = kmeans_result$cluster)

fviz_cluster(kmeans_result, data = date_standardizate, geom = "point", ellipse.type = "convex") +
  ggtitle("clusterele rezultate din K-Means")

#tss
tss <- sum((date_standardizate - rowMeans(date_standardizate))^2)
wss <- sum(kmeans_result$withinss)
bss <- tss - wss
calitate_partitie <- bss / tss
print(tss)
print(wss)
print(bss)
print(calitate_partitie)

#alegem un alt numar de clustere
valori_k <- 2:6
rezultate_k <- data.frame(K = valori_k, BSS_TSS = numeric(length(valori_k)))
for (k in valori_k) {
  set.seed(123)
  test_kmeans <- kmeans(date_standardizate, centers = k, nstart = 25)
  wss_test <- sum(test_kmeans$withinss)
  bss_test <- tss - wss_test
  rezultate_k[rezultate_k$K == k, "BSS_TSS"] <- bss_test / tss
}
print(tss)
print(wss_test)
print(bss_test)
print(rezultate_k)

#indicatori statistici (media, mediana si deviatie standard)
#economie interna
economie_medie <- tapply(data_cu_kmeans$`economie_interna (%pib)`, data_cu_kmeans$Cluster, mean, na.rm = TRUE)
economie_mediana <- tapply(data_cu_kmeans$`economie_interna (%pib)`, data_cu_kmeans$Cluster, median, na.rm = TRUE)
economie_ds <- tapply(data_cu_kmeans$`economie_interna (%pib)`, data_cu_kmeans$Cluster, sd, na.rm = TRUE)

#consum intern
consum_medie <- tapply(data_cu_kmeans$`consum_intern (%pib)`, data_cu_kmeans$Cluster, mean, na.rm = TRUE)
consum_mediana <- tapply(data_cu_kmeans$`consum_intern (%pib)`, data_cu_kmeans$Cluster, median, na.rm = TRUE)
consum_ds <- tapply(data_cu_kmeans$`consum_intern (%pib)`, data_cu_kmeans$Cluster, sd, na.rm = TRUE)

#investitii publice
investitii_publice_medie <- tapply(data_cu_kmeans$`investitii_publice (%pib)`, data_cu_kmeans$Cluster, mean, na.rm = TRUE)
investitii_mediana <- tapply(data_cu_kmeans$`investitii_publice (%pib)`, data_cu_kmeans$Cluster, median, na.rm = TRUE)
investitii_ds <- tapply(data_cu_kmeans$`investitii_publice (%pib)`, data_cu_kmeans$Cluster, sd, na.rm = TRUE)

#inflatie
inflatie_medie <- tapply(data_cu_kmeans$inflatie_consum, data_cu_kmeans$Cluster, mean, na.rm = TRUE)
inflatie_mediana <- tapply(data_cu_kmeans$inflatie_consum, data_cu_kmeans$Cluster, median, na.rm = TRUE)
inflatie_ds <- tapply(data_cu_kmeans$inflatie_consum, data_cu_kmeans$Cluster, sd, na.rm = TRUE)

#investitii straini
investitii_straini_medie <- tapply(data_cu_kmeans$`investitii_straini (%pib)`, data_cu_kmeans$Cluster, mean, na.rm = TRUE)
investitii_straini_mediana <- tapply(data_cu_kmeans$`investitii_straini (%pib)`, data_cu_kmeans$Cluster, median, na.rm = TRUE)
investitii_straini_ds <- tapply(data_cu_kmeans$`investitii_straini (%pib)`, data_cu_kmeans$Cluster, sd, na.rm = TRUE)

indicatori_statistici_kmeans <- data.frame(
  Cluster = names(economie_medie),
  economie_medie = economie_medie,
  economie_mediana = economie_mediana,
  economie_ds = economie_ds,
  consum_medie = consum_medie,
  consum_mediana = consum_mediana,
  consum_ds = consum_ds,
  investitii_publice_medie = investitii_publice_medie,
  investitii_publice_mediana = investitii_mediana,
  investitii_publice_ds = investitii_ds,
  inflatie_medie = inflatie_medie,
  inflatie_mediana = inflatie_mediana,
  inflatie_ds = inflatie_ds,
  investitii_straini_medie = investitii_straini_medie,
  investitii_straine_mediana = investitii_straini_mediana,
  investitii_straini_ds = investitii_straini_ds
)

print(indicatori_statistici_kmeans)

#tabel conclusiv
num_observatii <- table(data_cu_kmeans$Cluster)
tabel_conclusiv <- data.frame(
  cluster = names(num_observatii),
  numar_observatii = as.numeric(num_observatii),
  economie_medie = economie_medie,
  economie_mediana = economie_mediana,
  economie_ds = economie_ds,
  consum_medie = consum_medie,
  consum_mediana = consum_mediana,
  consum_ds = consum_ds,
  investitii_publice_medie = investitii_publice_medie,
  investitii_publice_mediana = investitii_mediana,
  investitii_publice_ds = investitii_ds,
  inflatie_medie = inflatie_medie,
  inflatie_mediana = inflatie_mediana,
  inflatie_ds = inflatie_ds,
  investitii_straini_medie = investitii_straini_medie,
  investitii_straine_mediana = investitii_straini_mediana,
  investitii_straini_ds = investitii_straini_ds
)

View(tabel_conclusiv)
