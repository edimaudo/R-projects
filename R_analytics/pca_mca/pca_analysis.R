rm(list = ls()) #clear environment

#===============
# Packages
#===============
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','scales','dplyr','psy','nFactors',
              'MASS','psych','cluster','lattice','NbClust','gridExtra','doParallel','readxl')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===============
# Load data
#===============
brand <- read_excel("Brands.data.xlsx")
manufacture <- read_excel("Manufacture.data.xlsx")
opinion_survey <- read_excel("OpinionSurvey.data.xlsx")
pollution <- read_excel("Pollution.data.xlsx")
symptom <- read_excel("symptoms.data.xlsx")
set.seed(1) #set seed
#==============
# Question 1
#==============
pollutant <- pollution[,c(2:10)]
res.pca <- prcomp(pollutant,  scale = TRUE)

# (a) Construct a PCA biplot of the Pollution data without showing alpha bags.
fviz_pca_biplot(res.pca)

# (b) Repeat (a) but instead of sample labels show the different groups (clusters) as 90% bags.
fviz_pca_biplot(res.pca, 
                geom.ind = "point",
                habillage=pollution$Cluster,
                addEllipses=TRUE, 
                confidence = 0.9,
                ellipse.level=0.95)

# (c) optimal two-dimensional display of the correlations between the variables.
corr <- cor(pollutant)
corrplot(corr, method = 'number',bg="#808080")

# (e) Construct a CVA biplot of the Pollution data with 90% bags added. 
pollution_lda<- lda(Cluster ~ ., pollution)

pollutant2 <- 
  pollution %>% 
  dplyr::select(-Cluster) %>%
  as.matrix 

# calculate CV scores
CVA.scores <- pollutant2 %*% pollution_lda$scaling
pollution.CV <- data.frame(CVA.scores)
pollution.CV$Cluster <- pollution$Cluster

# Initial plot
pollution_cva_plot <- ggplot(pollution.CV, aes(x = LD1, y = LD2)) + 
geom_point(aes(color=as.factor(Cluster)), alpha=0.9) + 
labs(x = "CV1", y = "CV2", color = "Cluster") + 
coord_fixed(ratio=1) + theme_minimal()

#install.packages("ggforce")
library(ggforce)
chi2 <-  qchisq(0.1,2, lower.tail=FALSE)
CIregions.mean.and.pop <-
  pollution.CV %>%
  group_by(Cluster) %>%
  summarize(CV1.mean = mean(LD1),
            CV2.mean = mean(LD2),
            mean.radii = sqrt(chi2/n()),
            popn.radii = sqrt(chi2))

# 90% bags plot
pollution_cva_plot + 
  ggforce::geom_circle(data = CIregions.mean.and.pop,
              mapping = aes(x0 = CV1.mean, y0 = CV2.mean, r = mean.radii),
              inherit.aes = FALSE) +
  ggforce::geom_circle(data = CIregions.mean.and.pop,
              mapping = aes(x0 = CV1.mean, y0 = CV2.mean, r = popn.radii),
              linetype = "dashed", 
              inherit.aes = FALSE) 
#==============
# Question 2
#==============
pollutant_cluster4 <- pollution %>%
  dplyr::filter(Cluster == 4) %>%
  dplyr::select(-c(Cluster))

# a) 
pollutant_cluster4_euclidean <- dist(pollutant_cluster4, method = "euclidean", 
                                     diag = TRUE, upper = TRUE)
pollutant_cluster4_canberra <- dist(pollutant_cluster4, method = "canberra", 
                                     diag = TRUE, upper = TRUE)

# b)
pollutant_cluster4_euclidean_scaled <- cmdscale(pollutant_cluster4_euclidean, eig=FALSE, k=2)

# plot solution
x <- pollutant_cluster4_euclidean_scaled$points[,1]
y <- pollutant_cluster4_euclidean_scaled$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(pollutant_cluster4), cex=.7)
                                   
pollutant_cluster4_canberra_scaled <- cmdscale(pollutant_cluster4_canberra,eig=TRUE, k=2)
x <- pollutant_cluster4_canberra_scaled$points[,1]
y <- pollutant_cluster4_canberra_scaled$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(pollutant_cluster4), cex=.7)

#==============
# Question 3
#==============
brand_info <- brand %>%
  dplyr::select(-c(Brand))

# a) 
brand_euclidean <- dist(brand_info, method = "euclidean", 
                                     diag = TRUE, upper = TRUE)

# c)
branddistmat <- as.matrix(brand_euclidean)

distance <- function(sq) {  # Target function
  sq2 <- embed(sq, 2)
  sum(branddistmat[cbind(sq2[,2], sq2[,1])])
}

sq <- c(1:nrow(branddistmat), 1)  # Initial sequence: alphabetic
distance(sq)

# rotate for conventional orientation
loc <- -cmdscale(brand_euclidean, add = TRUE)$points
x <- loc[,1]; y <- loc[,2]
s <- seq_len(nrow(branddistmat))
tspinit <- loc[sq,]
tspres <- loc[res$par,]
res <- optim(sq,distance,gr=NULL)
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "",
     main = "Using optim", axes = FALSE)
arrows(tspres[s,1], tspres[s,2], tspres[s+1,1], tspres[s+1,2],
       angle = 10, col = "red")
text(x, y, labels(eurodist), cex = 0.8)


# e)
Brands.data.ord <- apply(brand_info, 2, sort, decreasing=F)

# f)
# ??daisy
brand_disimilarity <- daisy(Brands.data.ord, metric = c("gower"))

# g)
#install.packages("smacof")
library(smacof)
brand_info_nmds <- smacof::mds(dist(brand_info),type = "ordinal")
plot(brand_info_nmds)
#brand_info_mds3  <- smacofSym(dist(brand_info), type = c("ratio", "interval", "ordinal", "mspline"))
#brand_info_mds <- isoMDS(dist(brand_info))  
#plot(brand_info_mds$points, type = "n")

# h)
brand_info_feat5 <- brand %>%
  dplyr::select(Feat5)
brand_info_feat5_mds <- smacof::mds(dist(brand_info_feat5))
plot(brand_info_feat5_mds)

#==============
# Question 4
#==============
#install.packages("vegan")
#install.packages("smacof")
library(vegan)
library(smacof)

brand_info <- brand %>%
  dplyr::select(-c(Brand))

#metric mds
brand_metric_mds1 <- cmdscale(dist(brand_info), k = 2)

#non metric mds
brand_non_metric_mds <- smacof::mds(dist(brand_info), type = "ordinal")

brand_procrustes <-  procrustes(X = brand_metric_mds1, Y = brand_non_metric_mds$init, symmetric = FALSE)
summary(brand_procrustes)
plot(brand_procrustes)

#==============
# Question 5
#==============
# (a) Ensure that the answer to each question is a categorical variable.
unique(opinion_survey$Q1)
unique(opinion_survey$Q2)
unique(opinion_survey$Q3)
unique(opinion_survey$Q4)
unique(opinion_survey$Q5)

# b) Construct an MCA biplot on the associated indicator matrix of the questions. Do not
# colour the sample points but label them using their IDs as labels. Represent each
# categorical variable in a different colour. Add a suitable legend to the MCA biplot.
opinion <- opinion_survey[,c(2:6)]
res.mca <- MCA(opinion, graph = FALSE)
summary(res.mca)

fviz_mca_biplot(res.mca,
                col.ind = "black",
                col.var = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Groups", color = "Categories"),
                )

# c) Repeat (b) but this time colour all CLPs in the same colour while distinguishing the
# samples from the different districts using colour coding.
fviz_mca_biplot(res.mca,
                col.ind="cos2",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(color = "Individual Groups"),
)


#==============
# Question 6
#==============

# a)
unique(manufacture$Feature.1)
unique(manufacture$Feature.2)
unique(manufacture$Feature.3)
unique(manufacture$Feature.4)
unique(manufacture$Feature.5)
unique(manufacture$Feature.6)

# b) Use function CATPCAbipl as given in package UBbipl40 to carry out a Categorical PCA on the Manufacture data
# install.packages("UBbipl40")
# package ‘UBbipl40’ is not available for this version of R (4.1.0)

#==============
# Question 7
#==============
# install.packages('CTT')
library(CTT)

# a)

# i) ??`CTT-package`

# ii)
symptom_item_analysis <- CTT::itemAnalysis(as.data.frame(symptom))

## (b) Obtain the ‘person’ scores and transform these scores to a scale having a mean of 100
## and a standard deviation of 15.
symptom_score <- CTT::score(as.data.frame(symptom))
symptom_score_transform <- CTT::score.transform(symptom_score$score,mu.new = 100,sd.new = 15)

## (c) Represent the transformed person scores in the form of a unidimensional scaling graph.
symptom_score_transform_new_scores <- as.data.frame(symptom_score_transform$new.scores)
colnames(symptom_score_transform_new_scores) <- c('Scores')

# install.packages('smacof')
# library(smacof)
#symptom_uni <- (symptom_score_transform_new_scores)
#fit.uni <- uniscale(symptom_uni)
#plot(fit.uni)

#ggplot(symptom_score_transform_new_scores,aes(x=Scores)) + geom_histogram()

## (d) Construct a unidimensional scale (in table and graph form) for the items and explain
## how to interpret the scale.


#==============
# Question 8
#==============

