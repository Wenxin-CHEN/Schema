library("FactoMineR")
library("factoextra")
library("corrplot")
library("tidyverse")

pall <- readRDS("data/new_exp/p.rds")
pall <- pall %>% 
  group_by(ID,afterbreak) %>% summarise(across(OB_1:performance, mean))

pall.pca <- PCA(pall,ncp = 8)
eig.val <- get_eigenvalue(pall.pca)
eig.val
fviz_eig(pall.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(pall.pca)
var$coord
fviz_pca_var(pall.pca, col.var = "black",repel = TRUE)

# contribution
corrplot(var$contrib, is.corr=FALSE)

# Contributions of variables to PC1
fviz_contrib(pall.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pall.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pall.pca, choice = "var", axes = 3, top = 10)
fviz_contrib(pall.pca, choice = "var", axes = 4, top = 10)
fviz_contrib(pall.pca, choice = "var", axes = 5, top = 10)

