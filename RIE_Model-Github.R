library(tidyverse)
library(caTools)
library(caret)

# input data measured RIE data with compound descriptors
dataset
# select data which is numeric
dataset <- dataset[,sapply(dataset, is.numeric)]
# remove columns which have all zero's
dataset <- dataset[,-(which(colSums(dataset) == 0))]
# remove columns with only one value
dataset <- dataset[vapply(dataset, function(x) length(unique(x)) > 1, logical(1L))]
# remove columns containing at least one NA value
dataset <- dataset %>% select_if(~ !any(is.na(.)))
# select columns which have a correlation greater than 0.8 (R > 0.8) to another column in the dataset
Pair_corr <- findCorrelation(cor(dataset), cutoff = 0.8, names = FALSE, exact = TRUE)
#Remove columns with a high correlation to another
dataset <- dataset[,-(Pair_corr)]
# select descriptors which have a R correlation of greater than 0.3
dataset %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column(var  = "var1") %>%
  gather(var2, value, -var1) %>%
  filter(var1 == "logrie") %>%
  arrange(desc(value)) %>%
  filter(value > 0.3)

dataset <- dataset %>% select(names, logP)%>% select(-c(fr_nitro_arom_nonortho, fr_phenol))

grid <- expand.grid(mtry = 10,coefReg = 1, coefImp = 0)
model <- train(logrie ~ .,
                data = dataset,
                method = "RRF",
                ntree = 100,
                trControl = trainControl(method = "LOOCV"),
                min.node.size = 1,
                tuneGrid = grid,
                importance = T)
