library(readxl)
library(dplyr)
library(stringr)
library(caret)
library(survey)
library(pROC)

file_path <- "C:/Users/USER/Desktop/헬스케어/프로젝트/ADHD_전처리_최종.xlsx"
df <- read_excel(file_path)

# 1. 파생변수 생성 및 asrs1_y 안전 변환
df <- df %>%
  # (1) has_disorder 파생
  mutate(
    has_anxiety    = as.factor(has_anxiety),
    has_depression = as.factor(has_depression),
    has_adhd       = as.factor(has_adhd),
    has_disorder   = as.factor(
      ifelse(has_anxiety=="1" |
               has_depression=="1" |
               has_adhd=="1", "1", "0")
    )
  ) %>%
  # (2) asrs1_y 팩터→문자→숫자 안전 변환
  mutate(
    asrs1_y_chr = str_trim(as.character(asrs1_y)),
    asrs1_y_num = case_when(
      grepl("^[0-9]+(?:\\.[0-9]+)?$", asrs1_y_chr) ~ 
        suppressWarnings(as.numeric(asrs1_y_chr)),
      TRUE ~ NA_real_
    ),
    asrs1_y = if_else(
      is.na(asrs1_y_num),
      median(asrs1_y_num, na.rm = TRUE),
      asrs1_y_num
    )
  ) %>%
  select(-asrs1_y_chr, -asrs1_y_num)

# 2. 학습/검증 데이터 분할 (70% train / 30% test)
set.seed(2025)
train_idx  <- createDataPartition(df$has_disorder, p = 0.7, list = FALSE)
train_data <- df[train_idx, ]
test_data  <- df[-train_idx, ]

############################################################################################
# 종속변수 제외하고 나머지 모든 열을 후보 변수로 지정
vars <- c(
  # — 인구통계학
  "age",                    # 연령: 주요 리스크 팩터  
  "sex",                    # 성별: ADHD/우울·불안 분포 차이  
  # — 치료 이력
  "ever_medicated",         # 과거 약물 치료 경험  
  "current_medicated",      # 현재 약물 복용 여부  
  "ever_therapy",           # 과거 심리치료 경험  
  "current_therapy",        # 현재 심리치료 여부  
  
  # — 심리검사 점수
  "bdi",                    # Beck Depression Inventory
  "bai",                    # Beck Anxiety Inventory
  "audit",                  # AUDIT (음주 장애 지표)
  "aas",                    # ADHD 자가보고 척도
  "asrs1_x",                # ASRS 워드별 점수(예: Part A)
  "asrs1_y",                # ASRS 총점(Part B)
  
  # — 학업/인지 지표
  "psy_grade",              # 심리·정신건강 자기평가 등급
  "nbt_year",               # NBT 응시 연도
  "nbt_al",                 # NBT 언어 점수
  "nbt_math",               # NBT 수리 점수
  "nbt_ql",                 # NBT 정성 평가
  "nbt_avg",                # NBT 평균 점수
  "alql_avg",               # 언어·정성 평균
  "math_done",              # 수학 수행여부(집중력 대리변수)
  "matric"
  
)


# 단변량 로지스틱 회귀 수행
univ_list <- lapply(vars, function(v) {
  fm <- as.formula(paste("has_disorder ~", v))
  m  <- try(glm(fm, data = train_data, family = binomial), silent = TRUE)
  
  # 오류가 없는 경우에만 결과 반환
  if (inherits(m, "glm")) {
    p <- coef(summary(m))[2, 4]
    data.frame(variable = v, p_value = p)
  } else {
    data.frame(variable = v, p_value = NA)
  }
})

# 결과 통합 및 후보 변수 추출
cand_df <- do.call(rbind, univ_list)
candidates <- cand_df$variable[cand_df$p_value < 0.20]

cat("선별된 후보 변수:\n")
print(candidates)


#######################


# 3. 단변량 로지스틱 회귀로 후보 변수 선별 (p < 0.20)
vars <- c("bdi","bai","audit","aas","ever_medicated", "current_medicated", "ever_therapy", "current_therapy")
# 예측 후보 변수 목록 갱신

univ_list <- lapply(vars, function(v) {
  fm <- as.formula(paste("has_disorder ~", v))
  m  <- glm(fm, data = train_data, family = binomial)
  p  <- coef(summary(m))[2,4]
  data.frame(variable = v, p_value = p)
})
cand_df    <- do.call(rbind, univ_list)
candidates <- cand_df$variable[cand_df$p_value < 0.20]
cat("선별된 후보 변수:\n"); print(candidates)
# [1] "bdi" "bai" "audit" "aas" "ever_medicated" "current_therapy"

# 4. 다변량 로지스틱 회귀 & 단계적 선택

multiv_mod <- glm(
  as.formula(paste("has_disorder ~", paste(candidates, collapse = " + "))),
  data = train_data, family = binomial
) 

step_mod <- step(multiv_mod, direction = "both", trace = FALSE)
summary(step_mod)
# 최종 모형: has_disorder ~ bai + aas + ever_medicated + current_therapy
#변수: 최종적으로 bai, aas, ever_medicated, current_therapy 네 개만 남음
#Fisher Scoring 6회 만에 잘 수렴
#Deviance 감소: Null deviance 464 → Residual deviance 287로 충분히 떨어짐
#AIC: 301.67로 변수 투입 전(절편만) 보다 모형이 좋아졌음

# 5. 성향점수(PS) 추정 모형 — 비유의한 asrs1_y_cat 제거
ps_mod <- glm(
  has_disorder ~ bai + aas + ever_medicated,
  data = train_data, family = binomial
)
summary(ps_mod)

#############################

# 1) 검증 데이터에 대한 예측 확률 계산
pred_prob_final <- predict(step_mod, newdata = test_data, type = "response")

# 2) ROC 객체 생성
library(pROC)
roc_final <- roc(as.numeric(test_data$has_disorder) - 1, pred_prob_final)

# 3) Youden’s J 최적 임계값, 민감도, 특이도 추출
coords_df  <- coords(
  roc_final,
  x = "best",
  input  = "threshold",
  ret    = c("threshold", "sensitivity", "specificity"),
  transpose = FALSE
)

# 4) 임계값만 뽑아서 숫자형으로 변환
opt_thresh <- coords_df[1, "threshold"]

# 5) 최적 임계값 확인
cat("Optimal threshold:", opt_thresh, "\n")

# 6) 최적 임계값 적용한 예측 클래스
pred_class_opt <- factor(
  ifelse(pred_prob_final > opt_thresh, "1", "0"),
  levels = c("0","1")
)

# 7) 성능 평가: 혼동행렬
library(caret)
conf_mat_opt <- confusionMatrix(pred_class_opt, test_data$has_disorder)
print(conf_mat_opt)  

cat("AUC (final logistic):", auc(roc_final), "\n")


####################################################
missing_pct_all <- sapply(df, function(x) mean(is.na(x)) * 100)
missing_df_all <- data.frame(
  variable    = names(missing_pct_all),
  missing_pct = round(missing_pct_all, 2)
)
print(missing_df_all)

# 9) 고위험 잠재질환군 생성(이미 수행)
train_data <- train_data %>%
  mutate(
    train_pred_prob = predict(step_mod, newdata = train_data, type = "response"),
    disorder_group  = if_else(train_pred_prob >= opt_thresh, 1L, 0L)
  )

# 10) Firth’s 보정 PS 모형 실행 및 ps_grp 한 번에 할당
library(logistf)
ps_mod_grp_fir <- suppressWarnings(logistf(
  disorder_group ~ bai + aas + ever_medicated + current_therapy,
  data    = train_data,
  maxiter = 50
))

# 여기서 fitted.values 길이가 train_data의 행 수(nrow)와 정확히 일치하는지 확인
length(ps_mod_grp_fir$fitted.values); nrow(train_data)
# 둘 다 같은 숫자가 나와야 합니다.

# 11) logistf PS 모형에서 예측확률(ps_grp) 뽑기
ps_vec <- predict(ps_mod_grp_fir,
                  newdata = train_data,
                  type    = "response")

# 길이 확인 (둘 다 355여야 합니다)
length(ps_vec); nrow(train_data)

# train_data에 ps_grp 할당
train_data$ps_grp <- ps_vec

# 12) raw IPTW 가중치 계산 → Inf/NaN→NA → 1% 컷오프→ w_grp_trim 생성
train_data$w_grp_raw <- ifelse(
  train_data$disorder_group == 1L,
  1 / train_data$ps_grp,
  1 / (1 - train_data$ps_grp)
)
train_data$w_grp_raw[!is.finite(train_data$w_grp_raw)] <- NA
q_grp <- quantile(train_data$w_grp_raw, c(0.01, 0.99), na.rm = TRUE)
train_data$w_grp_trim <- pmin(pmax(train_data$w_grp_raw, q_grp[1]), q_grp[2])

# 13) 가중치 NA 제거 및 그룹 분포 확인
ipw_data <- subset(train_data, !is.na(w_grp_trim))
table(ipw_data$disorder_group)

# 14) survey 디자인 & IPTW 로지스틱 회귀
library(survey)
dsgn_grp <- svydesign(ids = ~1, weights = ~w_grp_trim, data = ipw_data)

ipw_mod_grp <- svyglm(
  has_disorder ~ disorder_group,
  design = dsgn_grp,
  family = quasibinomial()
)
summary(ipw_mod_grp)

# 15) Adjusted OR 및 95% CI
coef_g      <- coef(ipw_mod_grp)["disorder_group"]
ci_g        <- confint(ipw_mod_grp)["disorder_group", ]
or_g        <- exp(coef_g)
or_ci_lo    <- exp(ci_g[1]); or_ci_hi <- exp(ci_g[2])
cat(sprintf(
  "Adjusted OR for high-risk group = %.2f (95%% CI: %.2f–%.2f)\n",
  or_g, or_ci_lo, or_ci_hi
))

