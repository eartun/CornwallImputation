library(tidyverse)
library(readxl)
library(VIM)
library(mice)
library(ggpubr)
library(vip)
library(RColorBrewer)
library(tidymodels)

myData <- read_xlsx("data.xlsx", sheet="Sheet1")
#myData %>% plotpattern2(rotate = TRUE)

myData_knn <- kNN(myData, k = 10) 
myData_knn <- myData_knn[,1:14]
myData_mean <- myData %>% mutate_if(is.numeric, function(x) ifelse(is.na(x), mean(x, na.rm = T), x))
myData$TD <- myData$TD*1000000
myDatarf <- myData %>% mice(m = 5, method = "rf") 
myData_rf <-complete(myDatarf)
myData_rf$TD <- myData_rf$TD/1000000
myData$TD <- myData$TD/1000000

myData$Type <- "Original"
myData_knn$Type <- "Imputed (kNN)"
myData_mean$Type <-  "Imputed (Mean)"
myData_rf$Type <- "Imputed (MICE)"
all <- rbind(myData,myData_mean,myData_knn,myData_rf)

#CLASSIFICATION
dataKNN <- myData_knn[,1:14]
dataRF <- myData_rf[,1:14]
dataMEAN <- myData_mean[,1:14]
dataKNN$Pluton <- as.factor(dataKNN$Pluton)
dataRF$Pluton <- as.factor(dataRF$Pluton)
dataMEAN$Pluton <- as.factor(dataMEAN$Pluton)

seedn <- 4
#mean data
set.seed(seedn)
split <- initial_split(dataMEAN, prop = 0.9, strata = Pluton)
train <- training(split)
test <- testing(split)


#preprocessıng
tree_rec <- recipe(Pluton ~ ., data = train)
tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)
tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")
tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)
set.seed(seedn)
trees_folds <- vfold_cv(rbind(train,test))

rf_grid <- grid_regular(
  mtry(range = c(1, 5)),
  trees(range = c(200, 1000)),
  min_n(range = c(10, 50)),
  levels = 5
)
set.seed(seedn)
regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

hyp_mean1 <- regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  scale_colour_manual(values = c("black",  "grey50", "navy","blue2","lightblue"))+
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "Area Under the ROC Curve")+theme_bw()
hyp_mean2 <- regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(trees, mean, color = min_n)) +
  scale_colour_manual(values = c("black",  "grey50", "navy","blue2","lightblue"))+
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "Area Under the ROC Curve")+theme_bw()
best <- select_best(regular_res)
final_rf1 <- finalize_model(tune_spec,best)

model <- final_rf1 %>% set_engine("ranger", importance="permutation") %>% fit(Pluton ~ .,data = train) 
imp_mean <- model %>% vip(geom = "point")+theme_bw()

#TRAINING PERFORMANCE
Predicted <-predict(model,train, type = "class")
Predicted <- Predicted %>% mutate(true_class=train$Pluton)
conf_mat(Predicted,estimate = .pred_class,truth = true_class)
meanac1 <- accuracy(Predicted, estimate = .pred_class,truth = true_class)
meansp1 <-  spec(Predicted, true_class, .pred_class)
meanse1 <- sens(Predicted, true_class, .pred_class)
#TESTING PERFORMANCE
Predicted <-predict(model,test, type = "class")
Predicted <- Predicted %>% mutate(true_class=test$Pluton)
conf_mat(Predicted,estimate = .pred_class,truth = true_class)
meanac2 <- accuracy(Predicted, estimate = .pred_class,truth = true_class)
meansp2 <- spec(Predicted, true_class, .pred_class)
meanse2 <- sens(Predicted, true_class, .pred_class)
mean_metrics <- data.frame(meanac1,meansp1,meanse1,meanac2,meansp2,meanse2)

#knn data
set.seed(seedn)
split <- initial_split(dataKNN, prop = 0.9, strata = Pluton)
train <- training(split)
test <- testing(split)


#preprocessıng
tree_rec <- recipe(Pluton ~ ., data = train)
tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)
tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")
tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)
set.seed(seedn)
trees_folds <- vfold_cv(rbind(train,test))

rf_grid <- grid_regular(
  mtry(range = c(1, 5)),
  trees(range = c(200, 1000)),
  min_n(range = c(10, 50)),
  levels = 5
)
set.seed(seedn)
regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

hyp_knn1 <- regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  scale_colour_manual(values = c("black",  "grey50", "navy","blue2","lightblue"))+
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "Area Under the ROC Curve")+theme_bw()
hyp_knn2 <- regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(trees, mean, color = min_n)) +
  scale_colour_manual(values = c("black",  "grey50", "navy","blue2","lightblue"))+
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "Area Under the ROC Curve")+theme_bw()
best <- select_best(regular_res)

final_rf2 <- finalize_model(tune_spec,best)

model <- final_rf2 %>% set_engine("ranger", importance="permutation") %>%
  fit(Pluton ~ .,data = train) 
imp_knn <- model %>% vip(geom = "point")+theme_bw()

#TRAINING PERFORMANCE
Predicted <-predict(model,train, type = "class")
Predicted <- Predicted %>% mutate(true_class=train$Pluton)
conf_mat(Predicted,estimate = .pred_class,truth = true_class)
knnac1 <- accuracy(Predicted, estimate = .pred_class,truth = true_class)
knnsp1 <- spec(Predicted, true_class, .pred_class)
knnse1 <- sens(Predicted, true_class, .pred_class)
#TESTING PERFORMANCE
Predicted <-predict(model,test, type = "class")
Predicted <- Predicted %>% mutate(true_class=test$Pluton)
conf_mat(Predicted,estimate = .pred_class,truth = true_class)
knnac2 <- accuracy(Predicted, estimate = .pred_class,truth = true_class)
knnsp2 <- spec(Predicted, true_class, .pred_class)
knnse2 <- sens(Predicted, true_class, .pred_class)
knn_metrics <- data.frame(knnac1,knnsp1,knnse1,knnac2,knnsp2,knnse2)


#RF data
set.seed(seedn)
split <- initial_split(dataRF, prop = 0.9, strata = Pluton)
train <- training(split)
test <- testing(split)


#preprocessıng
tree_rec <- recipe(Pluton ~ ., data = train)
tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)
tune_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")
tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)
set.seed(5)
trees_folds <- vfold_cv(rbind(train,test))

rf_grid <- grid_regular(
  mtry(range = c(1, 5)),
  trees(range = c(200, 1000)),
  min_n(range = c(10, 50)),
  levels = 5
)
set.seed(seedn)
regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

hyp_RF1 <- regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  scale_colour_manual(values = c("black",  "grey50", "navy","blue2","lightblue"))+
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "Area Under the ROC Curve")+theme_bw()
hyp_rf2 <- regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(trees, mean, color = min_n)) +
  scale_colour_manual(values = c("black",  "grey50", "navy","blue2","lightblue"))+
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "Area Under the ROC Curve")+theme_bw()
best <- select_best(regular_res)

final_rf3 <- finalize_model(tune_spec,best)

model <- final_rf3 %>% set_engine("ranger", importance="permutation") %>%
  fit(Pluton ~ .,data = train) 
imp_rf <- model %>% vip(geom = "point")+theme_bw()

#TRAINING PERFORMANCE
Predicted <-predict(model,train, type = "class")
Predicted <- Predicted %>% mutate(true_class=train$Pluton)
conf_mat(Predicted,estimate = .pred_class,truth = true_class)
rfac1 <- accuracy(Predicted, estimate = .pred_class,truth = true_class)
rfsp1 <- spec(Predicted, true_class, .pred_class)
rfse1 <- sens(Predicted, true_class, .pred_class)
#TESTING PERFORMANCE
Predicted <-predict(model,test, type = "class")
Predicted <- Predicted %>% mutate(true_class=test$Pluton)
conf_mat(Predicted,estimate = .pred_class,truth = true_class)
rfac2 <- accuracy(Predicted, estimate = .pred_class,truth = true_class)
rfsp2 <- spec(Predicted, true_class, .pred_class)
rfse2 <- sens(Predicted, true_class, .pred_class)
rf_metrics <- data.frame(rfac1,rfsp1,rfse1,rfac2,rfsp2,rfse2)
metrics <- rbind(mean_metrics,knn_metrics,rf_metrics) %>% select(.estimate,.estimate.1, .estimate.2, .estimate.3, , .estimate.4, , .estimate.5) 
colnames(metrics) <- c("Accuracy", "Specificity", "Sensitivity","Accuracy", "Specificity", "Sensitivity")
rownames(metrics) <- c("Mean", "k-NN", "RF")






write.csv(metrics, "metrics.csv")




summary <- read_xlsx("sum.xlsx", sheet="Sheet1")
sumtrain <- ggplot(summary%>%filter(Set=="Train"), aes(x=Method, y=Value, fill=Metric)) + 
  xlab("Imputation Method")+ylab("Evaluation Metric")+
  geom_bar(stat="identity", width=0.6, position=position_dodge(width=0.8))+
  theme_classic()+scale_fill_brewer("Blues")+guides(fill=guide_legend(title="Measure"))
sumtest <- ggplot(summary%>%filter(Set=="Test"), aes(x=Method, y=Value, fill=Metric)) + 
  xlab("Imputation Method")+ylab("Evaluation Metric")+
  geom_bar(stat="identity", width=0.6, position=position_dodge(width=0.8))+
  theme_classic()+scale_fill_brewer("Blues")+guides(fill=guide_legend(title="Measure"))
ggarrange(sumtrain,sumtest,ncol=1,nrow=2, labels=c("a)","b)"))
ggsave("Fig09.pdf",width = 5, height =6,dpi=300)











