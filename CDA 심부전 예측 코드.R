# 패키지 로드
library(tidyverse)
library(caret)
library(pROC)
library(MLmetrics)
library(readxl)
library(dplyr)
library(janitor)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(e1071)
library(gains)
library(randomForest)
library(reshape2)

# 데이터 불러오기
df <-  read.csv("C:/Users/82102/Downloads/heart.csv")

# 1. 데이터 탐색
# 2. 데이터 전처리 
# 2.1 자료형을 명시적으로 변환(factor로 변환)
df <- df %>%
  mutate(
    Sex = factor(Sex),
    ChestPainType = factor(ChestPainType),
    RestingECG = factor(RestingECG),
    ExerciseAngina = factor(ExerciseAngina),
    ST_Slope = factor(ST_Slope),
    FastingBS = factor(FastingBS),
    HeartDisease = factor(HeartDisease, levels = c(0,1), labels = c("No", "Yes"))
  )

# 1.1 변수별 결측값 수 확인
colSums(is.na(df))

# 1.2 이상치 탐색 시각화
df %>% 
  pivot_longer(cols = c(Age, RestingBP, Cholesterol, MaxHR, Oldpeak)) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "수치형 변수의 Boxplot (이상치 탐색)", x = "변수", y = "값") +
  theme_minimal()

# 1.3 수치형 변수 분포 확인 
df %>% 
  select(Age, RestingBP, Cholesterol, MaxHR, Oldpeak) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "orange", color = "white") +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "수치형 변수 분포 확인")

# 1.4 범주형 변수 분포 확인 
df %>%
  select(Sex, ChestPainType, FastingBS, RestingECG, ExerciseAngina, ST_Slope, HeartDisease) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value)) +
  geom_bar(fill = "lightpink") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal() +
  labs(title = "범주형 변수 분포 확인")

# 2.1 연속형 변수 정규화 (Gaussian NB 가정)
num_vars <- df %>% select(Age, RestingBP, Cholesterol, MaxHR, Oldpeak)
preproc <- preProcess(num_vars, method = c("center", "scale"))
norm_data <- predict(preproc, num_vars)

# 2.2 정규화된 수치형 변수와 나머지 변수(범주형 등)를 결합,최종 데이터프레임 생성
df_norm <- df %>%
  select(-Age, -RestingBP, -Cholesterol, -MaxHR, -Oldpeak) %>%
  bind_cols(norm_data)
summary(df_norm)
str(df_norm)

# 3.데이터 분할
set.seed(123)
train_index <- createDataPartition(df_norm$HeartDisease, p = 0.7, list = FALSE)
train_data <- df_norm[train_index, ]
test_data  <- df_norm[-train_index, ]

###################################KNN########################################

# 추천 k값 범위 지정
grid <- expand.grid(k = seq(3, 15, by = 2))

# 4. 교차검증 기반 KNN 모델
ctrl_cv <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
knn_cv <- train(
  HeartDisease ~ ., 
  data = train_data,    #train()함수는 범주형 변수 더미변수 변환을 자동 수행!
  method = "knn", 
  trControl = ctrl_cv, 
  tuneGrid = grid,           # 숫자 벡터가 아니라 그냥 숫자!
  metric = "ROC"
)

# 어떤 k값들이 실제로 시도됐는지 확인
knn_cv$results

# 최종적으로 선택된 최적의 k 확인
knn_cv$bestTune

# 확률 예측
knn_prob_cv <- predict(knn_cv, newdata = test_data, type = "prob")[, "Yes"]
knn_pred_cv <- predict(knn_cv, newdata = test_data)
confusionMatrix(knn_pred_cv, test_data$HeartDisease,positive = "Yes")

# F1-score 기준 최적 threshold 탐색 
# 예측 확률은 이미 있음
# knn_prob_cv <- predict(knn_cv, newdata = test_data, type = "prob")[, "Yes"]
actual <- test_data$HeartDisease

# threshold 후보 설정
thresholds <- seq(0.1, 0.9, by = 0.01)

# 각 threshold에 대해 F1-score 계산
f1_scores <- sapply(thresholds, function(t) {
  pred <- ifelse(knn_prob_cv > t, "Yes", "No") %>%
    factor(levels = c("No", "Yes"))
  F1_Score(y_true = actual, y_pred = pred, positive = "Yes")
})

# 최적 threshold 선택
best_threshold <- thresholds[which.max(f1_scores)]
cat("최적 F1-score 기준 threshold:", best_threshold, "\n")
cat("해당 F1-score:", max(f1_scores), "\n")

# 최적 threshold 기반 재예측 및 혼동행렬 
knn_pred_opt <- ifelse(knn_prob_cv > best_threshold, "Yes", "No") %>%
  factor(levels = c("No", "Yes"))

cm_df <- data.frame(
  Prediction = factor(c("No", "No", "Yes", "Yes"), levels = c("Yes", "No")),
  Reference = factor(c("No", "Yes", "No", "Yes"), levels = c("Yes", "No")),
  Count = c(102, 10, 21, 142)
)

conf_opt <- confusionMatrix(knn_pred_opt, actual, positive = "Yes")
print(conf_opt)

# KNN 임계값 시각화
plot(thresholds, f1_scores, type = "l", col = "darkgreen", lwd = 2,
     xlab = "Threshold", ylab = "F1 Score", main = "F1 Score vs Threshold (KNN)")
abline(v = best_threshold, col = "red", lty = 2)

# 혼동행렬 히트맵 
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), size = 4, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Confusion Matrix (KNN)", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(text = element_text(size = 14))

#################################나이브베이즈###############################################
set.seed(123)

# 5.교차검증 기반 나이브 베이즈 모델
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

nb_model <- train(
  HeartDisease ~ ., data = train_data, 
  method = "naive_bayes",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 5
)

# 확률 예측
nb_prob <- predict(nb_model, newdata = test_data, type = "prob")[, "Yes"]  # ← valid.df → test_data
actual <- test_data$HeartDisease  

# F1-score 기준 최적 threshold 탐색
thresholds <- seq(0.1, 0.9, by = 0.01)
f1_scores <- sapply(thresholds, function(t) {
  pred <- factor(ifelse(nb_prob > t, "Yes", "No"), levels = c("No", "Yes"))
  F1_Score(y_true = actual, y_pred = pred, positive = "Yes")
})

# 최적 threshold 선택, 재예측 
best_thresh <- thresholds[which.max(f1_scores)]
nb_pred <- factor(ifelse(nb_prob > best_thresh, "Yes", "No"), levels = c("No", "Yes"))

# 나이브베이즈 임계값
plot(
  thresholds, f1_scores,
  type = "l",
  col = "darkgreen",
  lwd = 2,                 
  xlab = "Threshold",
  ylab = "F1 Score",
  main = "F1 Score vs Threshold (Naive Bayes)",
  cex.lab = 1.3,           
  cex.main = 1.5,          
  cex.axis = 1.1           
)

# 수직선 및 숫자만 라벨로 표시
abline(v = best_thresh, col = "red", lty = 2, lwd = 2)
text(
  x = best_thresh + 0.03,
  y = max(f1_scores) - 0.02,
  labels = round(best_thresh, 2),  # 숫자만 표시
  col = "red",
  cex = 1.1
)
# 혼동행렬 
cat("\n📊 Confusion Matrix (F1 기준 threshold 적용: ", round(best_thresh, 2), ")\n")

conf_mat <- confusionMatrix(nb_pred, actual, positive = "Yes")
print(conf_mat)

# 시각화용 데이터 생성
cm_table <- conf_mat$table
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("예측값", "정답값", "빈도")

# 혼동행렬 히트맵
ggplot(cm_df, aes(x = 정답값, y = 예측값, fill = 빈도)) +
  geom_tile(color = "white") +
  geom_text(aes(label = 빈도), size = 6, color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  labs(
    title = paste0("혼동행렬 (F1 기준 threshold = ", round(best_thresh, 2), ")"),
    x = "실제값",
    y = "예측값"
  ) +
  theme_minimal(base_size = 14)

# 향상도 차트
actual_bin <- ifelse(actual == "Yes", 1, 0)
gain <- gains(actual_bin, nb_prob, groups = 10)
nactual <- sum(actual_bin)

plot(
  c(0, gain$cume.pct.of.total * nactual) ~ c(0, gain$cume.obs),
  type = "l",
  xlab = "# Cases",
  ylab = "Cumulative # of Heart Disease Cases",
  main = "Lift Chart (Naive Bayes - F1 Optimal Threshold)"
)
lines(c(0, nactual) ~ c(0, nrow(test_data)), lty = 2)

###############################나무모델######################################
set.seed(123)

# 6. 교차검증 기반 나무 모델
cv_model <- rpart(HeartDisease ~ ., data = train_data,
                  method = "class",
                  control = rpart.control(cp = 0.0001, xval = 10))

# 최적 cp 찾기
best_cp <- cv_model$cptable[which.min(cv_model$cptable[, "xerror"]), "CP"]
cat("선택된 최적 cp:", best_cp, "\n")

# 가지치기 수행
pruned_model <- prune(cv_model, cp = best_cp)

# 트리 시각화
prp(pruned_model,
    type = 4,                # 예측 결과는 노드 안, 분할조건은 노드 위
    extra = 104,             # 예측 클래스, 확률, 샘플 수
    fallen.leaves = TRUE,
    box.palette = "BuGn",
    shadow.col = "gray",
    branch.lty = 3,
    varlen = 0,
    faclen = 0,
    cex = 0.7,
    split.cex = 1.2,         # 분기 텍스트 크기 조절
    split.font = 2,          # 분기 조건을 bold로 표시
    split.yshift = -0.5,     # 분기 텍스트를 아래로 살짝 내려서 겹침 방지
    space = 0.4,             # 노드 간 간격 조정
    main = "Heart Disease Decision Tree (Pruned)"
)


# 확률 예측 (type = "prob")
pruned_prob <- predict(pruned_model, newdata = test_data, type = "prob")[, "Yes"]
actual <- test_data$HeartDisease

# F1-score 기준 최적 threshold 탐색
thresholds <- seq(0.1, 0.9, by = 0.01)
f1_scores <- sapply(thresholds, function(t) {
  pred <- ifelse(pruned_prob > t, "Yes", "No") %>%
    factor(levels = levels(actual))
  F1_Score(y_true = actual, y_pred = pred, positive = "Yes")
})

# 최적 threshold 및 평가
best_threshold_1 <- thresholds[which.max(f1_scores)]
cat("최적 F1 기준 threshold:", best_threshold_1, "\n")
cat("해당 F1-score:", max(f1_scores), "\n")

# 재예측 및 혼동행렬 평가
final_pred <- ifelse(pruned_prob > best_threshold_1, "Yes", "No") %>%
  factor(levels = levels(actual))
confusionMatrix(final_pred, actual,positive = "Yes")

# CART 임계값 
data.frame(thresholds, f1_scores) %>%
  ggplot(aes(x = thresholds, y = f1_scores)) +
  geom_line(color = "slateblue", size = 1.2) +
  geom_vline(xintercept = best_threshold, linetype = "dashed", color = "gray30") +
  geom_point(aes(x = best_threshold, y = max(f1_scores)), color = "red", size = 3) +
  labs(title = "CART 모델 - F1-score vs Threshold",
       x = "Threshold", y = "F1-score") +
  theme_minimal()

# 혼동행렬 히트맵
cm <- confusionMatrix(final_pred, actual)
cm_table <- as.table(cm$table)
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Prediction", "Reference", "Freq")

ggplot(cm_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "slateblue") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()

########################################랜덤포레스트####################
set.seed(123)

# 7.랜덤포레스트 모델 학습
rf_model <- randomForest(
  HeartDisease ~ ., 
  data = train_data, 
  ntree = 300,
  mtry = floor(sqrt(ncol(train_data) - 1)),
  importance = TRUE
)

# 확률 예측
rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[, "Yes"]
actual <- test_data$HeartDisease

# 다양한 threshold에 대해 F1-score 계산
thresholds <- seq(0.1, 0.9, by = 0.01)
f1_scores <- sapply(thresholds, function(t) {
  pred <- ifelse(rf_prob > t, "Yes", "No") %>%
    factor(levels = c("No", "Yes"))
  F1_Score(y_true = actual, y_pred = pred, positive = "Yes")
})

# 최적 threshold 추출
best_threshold_2 <- thresholds[which.max(f1_scores)]
cat("최적 F1 기준 threshold:", best_threshold_2, "\n")
cat("해당 F1-score:", max(f1_scores), "\n")

# 최적 threshold로 재예측 및 혼동행렬
final_pred <- ifelse(rf_prob > best_threshold_2, "Yes", "No") %>%
  factor(levels = c("No", "Yes"))
conf_mat <- confusionMatrix(final_pred, actual, positive = "Yes")
print(conf_mat)

# 랜덤포레스트 임계값 
data.frame(threshold = thresholds, f1 = f1_scores) %>%
  ggplot(aes(x = threshold, y = f1)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_vline(xintercept = best_threshold, linetype = "dashed", color = "gray30") +
  geom_point(aes(x = best_threshold, y = max(f1)), color = "red", size = 3) +
  labs(title = "랜덤포레스트 - F1-score vs Threshold",
       x = "Threshold", y = "F1-score") +
  theme_minimal()

# 혼동행렬 히트맵 
cm <- confusionMatrix(final_pred, actual, positive = "Yes")
cm_df <- as.data.frame(cm$table)
colnames(cm_df) <- c("Prediction", "Reference", "Freq")

ggplot(cm_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "랜덤포레스트 혼동행렬", x = "실제값", y = "예측값") +
  theme_minimal()

##############################전체 한번에 비교#########################################

# 나무와 랜덤포레스트 모델 각각의 최적 threshold에 기반한 예측 결과 
tree_pred <- ifelse(pruned_prob > best_threshold_1, "Yes", "No") %>%
  factor(levels = c("No", "Yes"))

rf_pred <- ifelse(rf_prob > best_threshold_2, "Yes", "No") %>%
  factor(levels = c("No", "Yes"))

# 8.모든 성능 수치 저장
results <- data.frame(
  Model = c("KNN", "Naive Bayes", "Decision Tree", "Random Forest"),
  Threshold = c(best_threshold, best_thresh, best_threshold_1, best_threshold_2),
  Accuracy = c(
    Accuracy(actual, knn_pred_opt),
    Accuracy(actual, nb_pred),
    Accuracy(actual, tree_pred),
    Accuracy(actual, rf_pred)
  ),
  Recall = c(
    Recall(actual, knn_pred_opt, positive = "Yes"),
    Recall(actual, nb_pred, positive = "Yes"),
    Recall(actual, tree_pred, positive = "Yes"),
    Recall(actual, rf_pred, positive = "Yes")
  ),
  Specificity = c(
    Specificity(actual, knn_pred_opt, positive = "Yes"),
    Specificity(actual, nb_pred, positive = "Yes"),
    Specificity(actual, tree_pred, positive = "Yes"),
    Specificity(actual, rf_pred, positive = "Yes")
  ),
  Precision = c(
    Precision(actual, knn_pred_opt, positive = "Yes"),
    Precision(actual, nb_pred, positive = "Yes"),
    Precision(actual, tree_pred, positive = "Yes"),
    Precision(actual, rf_pred, positive = "Yes")
  ),
  F1_Score = c(
    F1_Score(actual, knn_pred_opt, positive = "Yes"),
    F1_Score(actual, nb_pred, positive = "Yes"),
    F1_Score(actual, tree_pred, positive = "Yes"),
    F1_Score(actual, rf_pred, positive = "Yes")
  )
)

print(results)

