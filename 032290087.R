install.packages(c("tidyverse", "rsample", "caret"))

library(tidyverse)
library(rsample)
library(caret)

data <- read_csv("~/Desktop/H8O7_032290087/Significant_Earthquakes.csv")
head(data)

colnames(data)

# is_strong değişkeni: Magnitude >= 6.0 ise "Yes", aksi halde "No"
data <- data %>%
  mutate(is_strong = ifelse(mag >= 6.0, "Yes", "No"),
         is_strong = factor(is_strong))  # kategorik değişkenler için faktöre çevir

#hedef değişkeni datadan çıkar
selected <- data %>%
  select(latitude, longitude, depth, nst, gap, dmin, is_strong)

#eksik veri
colSums(is.na(selected))

#eksik verileri sil
clean <- na.omit(selected)


#veriyi ayırma
set.seed(42)
split <- initial_split(clean, prop = 0.7, strata = is_strong)
train_data <- training(split)
test_data <- testing(split)


#standardizasyonu öğren
pre_proc <- preProcess(train_data %>% select(-is_strong), method = c("center", "scale"))

#datalara uygula
train_scaled <- predict(pre_proc, train_data %>% select(-is_strong))
test_scaled  <- predict(pre_proc, test_data %>% select(-is_strong))

# Geri hedef değişkeni ekle
train_final <- bind_cols(train_scaled, is_strong = train_data$is_strong)
test_final  <- bind_cols(test_scaled,  is_strong = test_data$is_strong)


############################################################################################

library(caret)
library(tidyverse)
library(pROC)  # ROC AUC

#kontrol: ROC metrikleri için "Yes" pozitif sınıf olmalı
train_final$is_strong <- factor(train_final$is_strong, levels = c("Yes", "No"))
`test_final$is_strong  <- factor(test_final$is_strong, levels = c("Yes", "No"))`


ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(123)
log_model <- train(is_strong ~ .,
                   data = train_final,
                   method = "glm",
                   family = "binomial",
                   trControl = ctrl,
                   metric = "ROC")


pred_class <- predict(log_model, newdata = test_final)
pred_prob  <- predict(log_model, newdata = test_final, type = "prob")


confusionMatrix(pred_class, test_final$is_strong, positive = "Yes")

roc_curve <- roc(response = test_final$is_strong,
                 predictor = pred_prob$Yes,
                 levels = c("No", "Yes"))  # pozitif sınıf en sonda olmalı

auc(roc_curve)



################################################################################


install.packages("glmnet")

library(caret)
library(glmnet)
library(pROC)


ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(123)

#ridge için grid oluştur (alpha = 0, sadece lambda değişiyor)
ridge_grid <- expand.grid(
  alpha = 0,
  lambda = 10^seq(-4, 1, length = 10)  # 0.0001'den 10'a kadar log-scale lambda'lar
)

#ridge modeli eğit
set.seed(123)
ridge_model <- train(
  is_strong ~ .,
  data = train_final,
  method = "glmnet",
  trControl = ctrl,
  metric = "ROC",
  tuneGrid = ridge_grid
)


#lasso (alpha = 1)
set.seed(123)
lasso_grid <- expand.grid(alpha = 1,
                          lambda = 10^seq(-4, 1, length = 10))

lasso_model <- train(is_strong ~ .,
                     data = train_final,
                     method = "glmnet",
                     trControl = ctrl,
                     metric = "ROC",
                     tuneGrid = lasso_grid)


#elastik net (0 < alpha < 1)
set.seed(123)
elastic_grid <- expand.grid(alpha = seq(0, 1, by = 0.1),
                            lambda = 10^seq(-4, 1, length = 10))

elastic_model <- train(is_strong ~ .,
                       data = train_final,
                       method = "glmnet",
                       trControl = ctrl,
                       metric = "ROC",
                       tuneGrid = elastic_grid)



# En iyi ROC değerleri
ridge_model$results %>% filter(ROC == max(ROC))
lasso_model$results %>% filter(ROC == max(ROC))
elastic_model$results %>% filter(ROC == max(ROC))

# En iyi modeli belirle (ROC değeri en yüksek olan)
best_model <- lasso_model 



#test veri seti üzerinde performans değerlendirmesi
pred_class <- predict(best_model, newdata = test_final)
pred_prob  <- predict(best_model, newdata = test_final, type = "prob")

# Confusion matrix
confusionMatrix(pred_class, test_final$is_strong, positive = "Yes")

# ROC AUC
roc_curve <- roc(response = test_final$is_strong,
                 predictor = pred_prob$Yes,
                 levels = c("No", "Yes"))

auc(roc_curve)








