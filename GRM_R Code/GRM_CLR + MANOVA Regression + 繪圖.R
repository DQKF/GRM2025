#=============================================
# GRM CLR + MANOVA Regression + plot of Topics
#=============================================

# 說明：這份script生產文本中的Figure 2，以及Appendix中的 Table A & Table B.
# Note. This script generates Figure 2 in the main text, and Table A & B in Appendix.

1. setting factor baseline（China Times / OneMonthAhead）

library(dplyr)
library(compositions)  # CLR 轉換用

# relevel 
C15段落罷3 <- C15段落罷3 %>%
  mutate(
    source2 = factor(source2),
    period  = factor(period)
  )

C15段落罷3$source2 <- relevel(C15段落罷3$source2, ref = "China Times")
C15段落罷3$period  <- relevel(C15段落罷3$period,  ref = "OneMonthAhead")


2. 建立 topic 組成矩陣 + CLR 轉換/Build the topic composition matrix and apply the CLR transformation.

# 取出四個 Topic，形成組成矩陣/Extract the four topics to construct the compositional matrix.
topic_mat <- as.matrix(
  C15段落罷3[, c("Topic01", "Topic02", "Topic03", "Topic04")]
)

# 避免 0，先加一個很小的 pseudo-count/To avoid zeros, first add a small pseudo-count.
topic_mat[topic_mat == 0] <- 1e-6

# 轉成 acomp 物件，再做 CLR/Convert to an acomp object, then apply the CLR transformation.
topic_clr <- clr(acomp(topic_mat))   # 每一列四個 clr 值，和 = 0


3. 把 CLR 值放回資料框/Attach the CLR-transformed values back to the data frame.
C15_clr <- C15段落罷3 %>%
  mutate(
    clr_T1 = topic_clr[, 1],
    clr_T2 = topic_clr[, 2],
    clr_T3 = topic_clr[, 3],
    clr_T4 = topic_clr[, 4]
  )

  fit_clr <- lm(
  cbind(clr_T1, clr_T2, clr_T3) ~ source2 + period,
  data = C15_clr
)

Call:
lm(formula = cbind(clr_T1, clr_T2, clr_T3) ~ source2 + period, 
    data = C15_clr)

Coefficients:
                          clr_T1    clr_T2    clr_T3  
(Intercept)               -0.91372  -0.37761   1.02701
source2CN & HK Outlets     0.54098  -0.79796   1.30833
source2Jornal San Wa Ou    3.15025  -1.57149  -0.78826
source2Liberty Times       0.77031   1.43066  -3.19073
source2United Daily News   0.56420  -0.10402  -0.31161
periodTwoMonthAhead        0.63792  -0.03595  -0.37176


# MANOVA：檢驗 source2 / period 對「整體議題組成」的影響
 #MANOVA: Testing the effects of source2 and period on the overall agenda composition

manova_clr <- manova(fit_clr)
summary(manova_clr, test = "Pillai")

            Df  Pillai approx F num Df den Df    Pr(>F)    
source2      4 0.72894   162.65     12   6081 < 2.2e-16 ***
period       1 0.04083    28.73      3   2025 < 2.2e-16 ***
Residuals 2027                                             
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




# 4. CLR + MANOVA：為了避免線性相依，只放 3 個 clr 維度
# 4. CLR + MANOVA: To avoid linear dependence, only three CLR dimensions are included in the model.

# 說明:CLR 的四個維度互相相依（和 = 0），
# 所以在迴歸裡只要任意丟掉一個（例如 clr_T4），
# 就可以做標準的 multivariate lm + MANOVA。

# Explanation: The four CLR dimensions are linearly dependent (their sum equals 0).
# Therefore, dropping any one dimension (e.g., clr_T4) allows the estimation of a
# standard multivariate linear model and MANOVA.

# 多變量線性迴歸：三個 CLR 應變項/Multivariate linear regression: three CLR-dependent variables
fit_clr <- lm(
  cbind(clr_T1, clr_T2, clr_T3) ~ source2 + period,
  data = C15_clr
)


Call:
lm(formula = cbind(clr_T1, clr_T2, clr_T3) ~ source2 + period, 
    data = C15_clr)

Coefficients:
                          clr_T1    clr_T2    clr_T3  
(Intercept)               -0.91372  -0.37761   1.02701
source2CN & HK Outlets     0.54098  -0.79796   1.30833
source2Jornal San Wa Ou    3.15025  -1.57149  -0.78826
source2Liberty Times       0.77031   1.43066  -3.19073
source2United Daily News   0.56420  -0.10402  -0.31161
periodTwoMonthAhead        0.63792  -0.03595  -0.37176


# MANOVA：檢驗 source2 / period 對「整體議題組成」的影響
# MANOVA: Testing the effects of source2 and period on the 
# overall agenda composition.


manova_clr <- manova(fit_clr)
summary(manova_clr, test = "Pillai")

manova_clr <- manova(fit_clr)
summary(manova_clr, test = "Pillai")


5. 看「可比較的回歸係數」/View comparable regression coefficients
summary(fit_clr)
Response clr_T1 :

Call:
lm(formula = clr_T1 ~ source2 + period, data = C15_clr)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.1847 -1.2923 -0.5044  1.1716  4.7723 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)              -0.91372    0.06495 -14.069  < 2e-16 ***
source2CN & HK Outlets    0.54098    0.20632   2.622  0.00881 ** 
source2Jornal San Wa Ou   3.15025    0.16826  18.722  < 2e-16 ***
source2Liberty Times      0.77031    0.09370   8.221 3.56e-16 ***
source2United Daily News  0.56420    0.09600   5.877 4.87e-09 ***
periodTwoMonthAhead       0.63792    0.08124   7.852 6.56e-15 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.682 on 2027 degrees of freedom
Multiple R-squared:  0.1882,	Adjusted R-squared:  0.1861 
F-statistic: 93.95 on 5 and 2027 DF,  p-value: < 2.2e-16



Response clr_T2 :

Call:
lm(formula = clr_T2 ~ source2 + period, data = C15_clr)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.8883 -1.3624 -0.4401  1.1538  4.8112 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)              -0.37761    0.06391  -5.908 4.04e-09 ***
source2CN & HK Outlets   -0.79796    0.20303  -3.930 8.77e-05 ***
source2Jornal San Wa Ou  -1.57149    0.16558  -9.491  < 2e-16 ***
source2Liberty Times      1.43066    0.09221  15.515  < 2e-16 ***
source2United Daily News -0.10402    0.09447  -1.101    0.271    
periodTwoMonthAhead      -0.03595    0.07994  -0.450    0.653    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.655 on 2027 degrees of freedom
Multiple R-squared:  0.1988,	Adjusted R-squared:  0.1968 
F-statistic: 100.6 on 5 and 2027 DF,  p-value: < 2.2e-16


Response clr_T3 :

Call:
lm(formula = clr_T3 ~ source2 + period, data = C15_clr)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.0106 -0.9025 -0.2730  0.9718  4.4569 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)               1.02701    0.05248  19.570  < 2e-16 ***
source2CN & HK Outlets    1.30833    0.16672   7.848 6.81e-15 ***
source2Jornal San Wa Ou  -0.78826    0.13596  -5.798 7.79e-09 ***
source2Liberty Times     -3.19073    0.07572 -42.141  < 2e-16 ***
source2United Daily News -0.31161    0.07757  -4.017 6.11e-05 ***
periodTwoMonthAhead      -0.37176    0.06564  -5.663 1.70e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.359 on 2027 degrees of freedom
Multiple R-squared:  0.5326,	Adjusted R-squared:  0.5314 
F-statistic: 461.9 on 5 and 2027 DF,  p-value: < 2.2e-16


summary(lm(clr_T4 ~ source2 + period, data = C15_clr))

Call:
lm(formula = clr_T4 ~ source2 + period, data = C15_clr)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.5860 -1.1084 -0.2602  0.9727  3.8257 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)               0.26433    0.05176   5.107 3.59e-07 ***
source2CN & HK Outlets   -1.05135    0.16444  -6.394 2.01e-10 ***
source2Jornal San Wa Ou  -0.79051    0.13411  -5.895 4.39e-09 ***
source2Liberty Times      0.98976    0.07468  13.253  < 2e-16 ***
source2United Daily News -0.14857    0.07651  -1.942 0.052295 .  
periodTwoMonthAhead      -0.23021    0.06475  -3.556 0.000386 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.34 on 2027 degrees of freedom
Multiple R-squared:  0.1597,	Adjusted R-squared:  0.1576 
F-statistic: 77.05 on 5 and 2027 DF,  p-value: < 2.2e-16



# Plot/繪圖

# 取出 mean model 係數/ extract coef. of mean imn the model

# 線性迴歸 (linear model)
m_clrT01<- lm(clr_T1 ~ source2 + period, data = C15_clr)

# 取出係數（直接用 coefficients，不需要 $mean）
# Extract the coefficients directly (use coefficients; no need to use $mean).
coef_summary <- summary(m_clrT01)$coefficients

# 整理成 data frame
ci_df <- data.frame(
  term_raw  = rownames(coef_summary),
  estimate  = coef_summary[, "Estimate"],
  std_error = coef_summary[, "Std. Error"],
  t_value   = coef_summary[, "t value"],      # 改成 t value（不是 z value）
  p_value   = coef_summary[, "Pr(>|t|)"]      # 改成 Pr(>|t|)
) %>%
  mutate(
    term = term_raw,
    term = gsub("\\(Intercept\\)", "截距", term),
    term = gsub("source2", "", term),
    term = if_else(term == "periodTwoMonthAhead", "Two Month Ahead", term),
    
    # 95% 信賴區間（用 t 分布）
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error,
    
    # 轉成 Odds Ratio（指數轉換）
    or       = exp(estimate),
    or_lower = exp(ci_lower),
    or_upper = exp(ci_upper)
  ) %>%
  filter(term != "截距")

# 顯著性符號
ci_df <- ci_df %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE           ~ ""
    )
  )

# 設定 Y 軸順序
desired_order <- c(
  "Two Month Ahead",
  "United Daily News",
  "Liberty Times",
  "Jornal San Wa Ou",
  "CN & HK Outlets"
)
ci_df$term <- factor(ci_df$term, levels = desired_order)

# 計算標籤位置
or_range <- range(c(ci_df$or_lower, ci_df$or_upper), na.rm = TRUE)
offset   <- 0.03 * diff(or_range)
ci_df <- ci_df %>%
  mutate(
    label_y = ifelse(or >= 1, or_upper + offset, or_lower - offset),
    label_text = paste0(sprintf("%.2f", or), " ", significance)
  )

# 檢查結果
View(ci_df)

# 計算放文字的位置（在 CI 線尾＋一點點）
or_range <- range(c(ci_df$or_lower, ci_df$or_upper), na.rm = TRUE)
offset   <- 0.03 * diff(or_range)  # 右邊小小位移

ci_df <- ci_df %>%
  mutate(
    # 針對每個項目設定標籤位置
    label_x = case_when(
      # Liberty Times: 強制放在 CI 右側
      term == "Liberty Times" ~ or_upper + offset * 1,
      # CN & HK Outlets: 放在 CI 右側
      term == "CN & HK Outlets" ~ or_upper + offset,
      # Jornal San Wa Ou: 放在點估計右側
      term == "Jornal San Wa Ou" ~ or + offset * 11,
      # United Daily News: 放在點估計右側
      term == "United Daily News" ~ or + offset * 1 ,
      # Two Month Ahead: 放在點估計右側
      term == "Two Month Ahead" ~ or + offset * 1,
      # 預設
      TRUE ~ or + offset
    ),
    # ⭐ 對應的對齊方式（這個很重要！）
    hjust_value = case_when(
      term == "Liberty Times" ~ -0.1,
      term == "CN & HK Outlets" ~ -0.1,
      or >= 0.8 & or <= 1.2 ~ -0.1,
      or > 1.2 ~ -0.1,
      TRUE ~ 1.1
    ),
    # 標籤文字
    label_text = paste0(sprintf("%.2f", or), " ", significance)
  )


# 畫圖：points＋ error bar

library(ggplot2)
library(dplyr)


#=============
# plot Topic01
#=============

p_clrT01 <- ggplot(ci_df, aes(x = or, y = term)) +
  # 參考線
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  
  # 信賴區間
  geom_errorbarh(
    aes(xmin = or_lower, xmax = or_upper),
    height = 0.25,
    linewidth = 0.7,
    color = "black"
  ) +
  
  # 點估計
  geom_point(size = 4, shape = 19, color = "black") +
  
  # 數值標籤
  geom_text(
    aes(x = label_x, label = label_text, hjust = hjust_value),
    vjust = 0.5,
    size = 10,  # ⭐ 放大數值標籤
    color = "black"
  ) +
  
  # 標題
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  
  # X 軸設定
  scale_x_continuous(
    limits = c(0, max(ci_df$or_upper, na.rm = TRUE) * 1.2),
    expand = expansion(mult = c(0.02, 0.15)),
    breaks = seq(0,50,by = 10)
  ) +
  
  # 主題設定
  theme_minimal(base_size = 12) +
  theme(
    # ⭐ 標題：左對齊，對齊 Y 軸
    plot.title = element_text(
      hjust = -0.2,           # 改為左對齊（0 = 左，0.5 = 中，1 = 右）
      face = "bold", 
      size = 28,           # 可選：稍微放大標題
      margin = margin(b = 15, l = 0)  # 左邊距設為 0
    ),
    
    # ⭐ Y 軸標籤：放大字體
    axis.text.y = element_text(
      size = 30,           # 從 12 放大到 14
      color = "black", 
      hjust = 1,
      margin = margin(r = 15)
    ),
    
    # ⭐ X 軸標籤：放大字體
    axis.text.x = element_text(
      size = 28,           # 從 11 放大到 13
      color = "black"
    ),
    
    # 網格線
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # 邊距
    plot.margin = margin(t = 15, r = 5, b = 15, l = 15)
  )

print(p_clrT01)



#=============
# plot Topic03
#=============

# 一般線性迴歸
m_clrT03<- lm(clr_T3 ~ source2 + period, data = C15_clr)

# 取出係數（直接用 coefficients，不需要 $mean）
coef_summary <- summary(m_clrT03)$coefficients

# 整理成 data frame
ci_df <- data.frame(
  term_raw  = rownames(coef_summary),
  estimate  = coef_summary[, "Estimate"],
  std_error = coef_summary[, "Std. Error"],
  t_value   = coef_summary[, "t value"],      # 改成 t value（不是 z value）
  p_value   = coef_summary[, "Pr(>|t|)"]      # 改成 Pr(>|t|)
) %>%
  mutate(
    term = term_raw,
    term = gsub("\\(Intercept\\)", "截距", term),
    term = gsub("source2", "", term),
    term = if_else(term == "periodTwoMonthAhead", "Two Month Ahead", term),
    
    # 95% 信賴區間（用 t 分布）
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error,
    
    # 轉成 Odds Ratio（指數轉換）
    or       = exp(estimate),
    or_lower = exp(ci_lower),
    or_upper = exp(ci_upper)
  ) %>%
  filter(term != "截距")

# 顯著性符號
ci_df <- ci_df %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE           ~ ""
    )
  )

# 設定 Y 軸順序
desired_order <- c(
  "Two Month Ahead",
  "United Daily News",
  "Liberty Times",
  "Jornal San Wa Ou",
  "CN & HK Outlets"
)
ci_df$term <- factor(ci_df$term, levels = desired_order)

# 計算標籤位置
or_range <- range(c(ci_df$or_lower, ci_df$or_upper), na.rm = TRUE)
offset   <- 0.03 * diff(or_range)
ci_df <- ci_df %>%
  mutate(
    label_y = ifelse(or >= 1, or_upper + offset, or_lower - offset),
    label_text = paste0(sprintf("%.2f", or), " ", significance)
  )

# 檢查結果
View(ci_df)

# 計算放文字的位置（在 CI 線尾＋一點點）
or_range <- range(c(ci_df$or_lower, ci_df$or_upper), na.rm = TRUE)
offset   <- 0.03 * diff(or_range)  # 右邊小小位移

ci_df <- ci_df %>%
  mutate(
    # 針對每個項目設定標籤位置
    label_x = case_when(
      # Liberty Times: 強制放在 CI 右側
      term == "Liberty Times" ~ or_upper + offset * 6,
      # CN & HK Outlets: 放在 CI 右側
      term == "CN & HK Outlets" ~ or_upper + offset,
      # Jornal San Wa Ou: 放在點估計右側
      term == "Jornal San Wa Ou" ~ or + offset * 13.3,
      # United Daily News: 放在點估計右側
      term == "United Daily News" ~ or + offset * 12 ,
      # Two Month Ahead: 放在點估計右側
      term == "Two Month Ahead" ~ or + offset * 12,
      # 預設
      TRUE ~ or + offset
    ),
    # ⭐ 對應的對齊方式（這個很重要！）
    hjust_value = case_when(
      term == "Liberty Times" ~ -0.1,
      term == "CN & HK Outlets" ~ -0.1,
      or >= 0.8 & or <= 1.2 ~ -0.1,
      or > 1.2 ~ -0.1,
      TRUE ~ 1.1
    ),
    # 標籤文字
    label_text = paste0(sprintf("%.2f", or), " ", significance)
  )


# 畫圖：點＋誤差線，沒有 bar

library(ggplot2)
library(dplyr)

p_clrT03 <- ggplot(ci_df, aes(x = or, y = term)) +
  # 參考線
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  
  # 信賴區間
  geom_errorbarh(
    aes(xmin = or_lower, xmax = or_upper),
    height = 0.25,
    linewidth = 0.7,
    color = "black"
  ) +
  
  # 點估計
  geom_point(size = 4, shape = 19, color = "black") +
  
  # 數值標籤
  geom_text(
    aes(x = label_x, label = label_text, hjust = hjust_value),
    vjust = 0.5,
    size = 10,  # ⭐ 放大數值標籤
    color = "black"
  ) +
  
  # 標題
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  
  # X 軸設定
  scale_x_continuous(
    limits = c(0, max(ci_df$or_upper, na.rm = TRUE) * 1.2),
    expand = expansion(mult = c(0.02, 0.15)),
    breaks = seq(0,8,by = 2)
  ) +
  
  # 主題設定
  theme_minimal(base_size = 12) +
  theme(
    # ⭐ 標題：左對齊，對齊 Y 軸
    plot.title = element_text(
      hjust = -0.3,           # 改為左對齊（0 = 左，0.5 = 中，1 = 右）
      face = "bold", 
      size = 28,           # 可選：稍微放大標題
      margin = margin(b = 15, l = 0)  # 左邊距設為 0
    ),
    
    # ⭐ Y 軸標籤：放大字體
    axis.text.y = element_text(
      size = 30,           # 從 12 放大到 14
      color = "black", 
      hjust = 1,
      margin = margin(r = 15)
    ),
    
    # ⭐ X 軸標籤：放大字體
    axis.text.x = element_text(
      size = 28,           # 從 11 放大到 13
      color = "black"
    ),
    
    # 網格線
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # 邊距
    plot.margin = margin(t = 15, r = 5, b = 15, l = 15)
  )

print(p_clrT03)



#=============
# plot Topic02
#=============

# 一般線性迴歸
m_clrT02<- lm(clr_T2 ~ source2 + period, data = C15_clr)

# 取出係數（直接用 coefficients，不需要 $mean）
coef_summary <- summary(m_clrT02)$coefficients

# 整理成 data frame
ci_df <- data.frame(
  term_raw  = rownames(coef_summary),
  estimate  = coef_summary[, "Estimate"],
  std_error = coef_summary[, "Std. Error"],
  t_value   = coef_summary[, "t value"],      # 改成 t value（不是 z value）
  p_value   = coef_summary[, "Pr(>|t|)"]      # 改成 Pr(>|t|)
) %>%
  mutate(
    term = term_raw,
    term = gsub("\\(Intercept\\)", "截距", term),
    term = gsub("source2", "", term),
    term = if_else(term == "periodTwoMonthAhead", "Two Month Ahead", term),
    
    # 95% 信賴區間（用 t 分布）
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error,
    
    # 轉成 Odds Ratio（指數轉換）
    or       = exp(estimate),
    or_lower = exp(ci_lower),
    or_upper = exp(ci_upper)
  ) %>%
  filter(term != "截距")

# 顯著性符號
ci_df <- ci_df %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE           ~ ""
    )
  )

# 設定 Y 軸順序
desired_order <- c(
  "Two Month Ahead",
  "United Daily News",
  "Liberty Times",
  "Jornal San Wa Ou",
  "CN & HK Outlets"
)
ci_df$term <- factor(ci_df$term, levels = desired_order)

# 計算標籤位置
or_range <- range(c(ci_df$or_lower, ci_df$or_upper), na.rm = TRUE)
offset   <- 0.03 * diff(or_range)
ci_df <- ci_df %>%
  mutate(
    label_y = ifelse(or >= 1, or_upper + offset, or_lower - offset),
    label_text = paste0(sprintf("%.2f", or), " ", significance)
  )

# 檢查結果
View(ci_df)

# 計算放文字的位置（在 CI 線尾＋一點點）
or_range <- range(c(ci_df$or_lower, ci_df$or_upper), na.rm = TRUE)
offset   <- 0.03 * diff(or_range)  # 右邊小小位移

ci_df <- ci_df %>%
  mutate(
    # 針對每個項目設定標籤位置
    label_x = case_when(
      # Liberty Times: 強制放在 CI 右側
      term == "Liberty Times" ~ or_upper + offset * 1.5,
      # CN & HK Outlets: 放在 CI 右側
      term == "CN & HK Outlets" ~ or_upper + offset * 2,
      # Jornal San Wa Ou: 放在點估計右側
      term == "Jornal San Wa Ou" ~ or + offset * 13.7,
      # United Daily News: 放在點估計右側
      term == "United Daily News" ~ or + offset * 2.5 ,
      # Two Month Ahead: 放在點估計右側
      term == "Two Month Ahead" ~ or + offset * 2.4,
      # 預設
      TRUE ~ or + offset
    ),
    # ⭐ 對應的對齊方式（這個很重要！）
    hjust_value = case_when(
      term == "Liberty Times" ~ -0.1,
      term == "CN & HK Outlets" ~ -0.1,
      or >= 0.8 & or <= 1.2 ~ -0.1,
      or > 1.2 ~ -0.1,
      TRUE ~ 1.1
    ),
    # 標籤文字
    label_text = paste0(sprintf("%.2f", or), " ", significance)
  )


# 畫圖：點＋誤差線，沒有 bar

p_clrT02 <- ggplot(ci_df, aes(x = or, y = term)) +
  # 參考線
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  
  # 信賴區間
  geom_errorbarh(
    aes(xmin = or_lower, xmax = or_upper),
    height = 0.25,
    linewidth = 0.7,
    color = "black"
  ) +
  
  # 點估計
  geom_point(size = 4, shape = 19, color = "black") +
  
  # 數值標籤
  geom_text(
    aes(x = label_x, label = label_text, hjust = hjust_value),
    vjust = 0.5,
    size = 10,  # ⭐ 放大數值標籤
    color = "black"
  ) +
  
  # 標題
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  
  # X 軸設定
  scale_x_continuous(
    limits = c(0, max(ci_df$or_upper, na.rm = TRUE) * 1.2),
    expand = expansion(mult = c(0.02, 0.15)),
    breaks = seq(0,8,by = 2)
  ) +
  
  # 主題設定
  theme_minimal(base_size = 12) +
  theme(
    # ⭐ 標題：左對齊，對齊 Y 軸
    plot.title = element_text(
      hjust = -0.3,           # 改為左對齊（0 = 左，0.5 = 中，1 = 右）
      face = "bold", 
      size = 28,           # 可選：稍微放大標題
      margin = margin(b = 15, l = 0)  # 左邊距設為 0
    ),
    
    # ⭐ Y 軸標籤：放大字體
    axis.text.y = element_text(
      size = 30,           # 從 12 放大到 14
      color = "black", 
      hjust = 1,
      margin = margin(r = 15)
    ),
    
    # ⭐ X 軸標籤：放大字體
    axis.text.x = element_text(
      size = 28,           # 從 11 放大到 13
      color = "black"
    ),
    
    # 網格線
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # 邊距
    plot.margin = margin(t = 15, r = 0, b = 15, l = 15)
  )

print(p_clrT02)


p_clrT02A <-p_clrT02+ theme(
axis.title.y = element_blank(),  # 移除 Y 軸標題
  axis.text.y  = element_blank(),  # 移除 Y 軸文字
  axis.ticks.y = element_blank())
print(p_clrT02A)


#=============
# plot Topic04
#=============

# 一般線性迴歸
m_clrT04 <- lm(clr_T4 ~ source2 + period, data = C15_clr)

# 取出係數（直接用 coefficients，不需要 $mean）
coef_summary <- summary(m_clrT04)$coefficients

# 整理成 data frame
ci_df <- data.frame(
  term_raw  = rownames(coef_summary),
  estimate  = coef_summary[, "Estimate"],
  std_error = coef_summary[, "Std. Error"],
  t_value   = coef_summary[, "t value"],      # 改成 t value（不是 z value）
  p_value   = coef_summary[, "Pr(>|t|)"]      # 改成 Pr(>|t|)
) %>%
  mutate(
    term = term_raw,
    term = gsub("\\(Intercept\\)", "截距", term),
    term = gsub("source2", "", term),
    term = if_else(term == "periodTwoMonthAhead", "Two Month Ahead", term),
    
    # 95% 信賴區間（用 t 分布）
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error,
    
    # 轉成 Odds Ratio（指數轉換）
    or       = exp(estimate),
    or_lower = exp(ci_lower),
    or_upper = exp(ci_upper)
  ) %>%
  filter(term != "截距")

# 顯著性符號
ci_df <- ci_df %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE           ~ ""
    )
  )

# 設定 Y 軸順序
desired_order <- c(
  "Two Month Ahead",
  "United Daily News",
  "Liberty Times",
  "Jornal San Wa Ou",
  "CN & HK Outlets"
)
ci_df$term <- factor(ci_df$term, levels = desired_order)

# 計算標籤位置
or_range <- range(c(ci_df$or_lower, ci_df$or_upper), na.rm = TRUE)
offset   <- 0.03 * diff(or_range)
ci_df <- ci_df %>%
  mutate(
    label_y = ifelse(or >= 1, or_upper + offset, or_lower - offset),
    label_text = paste0(sprintf("%.2f", or), " ", significance)
  )

# 檢查結果
View(ci_df)

# 計算放文字的位置（在 CI 線尾＋一點點）
or_range <- range(c(ci_df$or_lower, ci_df$or_upper), na.rm = TRUE)
offset   <- 0.03 * diff(or_range)  # 右邊小小位移

ci_df <- ci_df %>%
  mutate(
    # 針對每個項目設定標籤位置
    label_x = case_when(
      # Liberty Times: 強制放在 CI 右側
      term == "Liberty Times" ~ or_upper + offset * 1.7,
      # CN & HK Outlets: 放在 CI 右側
      term == "CN & HK Outlets" ~ or_upper + offset * 5.5,
      # Jornal San Wa Ou: 放在點估計右側
      term == "Jornal San Wa Ou" ~ or + offset * 15.5,
      # United Daily News: 放在點估計右側
      term == "United Daily News" ~ or + offset * 1.8 ,
      # Two Month Ahead: 放在點估計右側
      term == "Two Month Ahead" ~ or + offset * 11,
      # 預設
      TRUE ~ or + offset
    ),
    # ⭐ 對應的對齊方式（這個很重要！）
    hjust_value = case_when(
      term == "Liberty Times" ~ -0.1,
      term == "CN & HK Outlets" ~ -0.1,
      or >= 0.8 & or <= 1.2 ~ -0.1,
      or > 1.2 ~ -0.1,
      TRUE ~ 1.1
    ),
    # 標籤文字
    label_text = paste0(sprintf("%.2f", or), " ", significance)
  )


# 畫圖：點＋誤差線，沒有 bar

p_clrT04 <- ggplot(ci_df, aes(x = or, y = term)) +
  # 參考線
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  
  # 信賴區間
  geom_errorbarh(
    aes(xmin = or_lower, xmax = or_upper),
    height = 0.25,
    linewidth = 0.7,
    color = "black"
  ) +
  
  # 點估計
  geom_point(size = 4, shape = 19, color = "black") +
  
  # 數值標籤
  geom_text(
    aes(x = label_x, label = label_text, hjust = hjust_value),
    vjust = 0.5,
    size = 10,  # ⭐ 放大數值標籤
    color = "black"
  ) +
  
  # 標題
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  
  # X 軸設定
  scale_x_continuous(
    limits = c(0, max(ci_df$or_upper, na.rm = TRUE) * 1.2),
    expand = expansion(mult = c(0.02, 0.15)),
    breaks = seq(0,8,by = 2)
  ) +
  
  # 主題設定
  theme_minimal(base_size = 12) +
  theme(
    # ⭐ 標題：左對齊，對齊 Y 軸
    plot.title = element_text(
      hjust = -0.3,           # 改為左對齊（0 = 左，0.5 = 中，1 = 右）
      face = "bold", 
      size = 28,           # 可選：稍微放大標題
      margin = margin(b = 15, l = 0)  # 左邊距設為 0
    ),
    
    # ⭐ Y 軸標籤：放大字體
    axis.text.y = element_text(
      size = 30,           # 從 12 放大到 14
      color = "black", 
      hjust = 1,
      margin = margin(r = 15)
    ),
    
    # ⭐ X 軸標籤：放大字體
    axis.text.x = element_text(
      size = 28,           # 從 11 放大到 13
      color = "black"
    ),
    
    # 網格線
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # 邊距
    plot.margin = margin(t = 15, r = 0, b = 15, l = 15)
  )

print(p_clrT04)


p_clrT04A <-p_clrT04+ theme(
axis.title.y = element_blank(),  # 移除 Y 軸標題
  axis.text.y  = element_blank(),  # 移除 Y 軸文字
  axis.ticks.y = element_blank())
print(p_clrT04A)





#--------------------------------------------
# combine 4 plots == Figure 2. in main artice
#--------------------------------------------

plot_1x4 <- plot_grid(
  p_clrT01, p_clrT02A, p_clrT03, p_clrT04A,
  labels        = c("A. Topic 1", "B. Topic 2", "C. Topic 3", "D. Topic 4"),
  label_size    = 30,
  label_fontface = "bold",
  nrow          = 2,
  ncol          = 2,
  align         = "h",
  axis          = "tb",
  rel_widths    = c(1.2,1)   # ⭐ 第一張圖比較寬
)

plot_1x4

plot_final <- ggdraw() +
  draw_label("Media", x = 0.02, angle = 90, size = 30) +
  draw_label("Odds Ratio exp(β)", y = 0.02, size = 30) +
  draw_plot(plot_1x4, x = 0.05, y = 0.05, width = 0.90, height = 0.90)

plot_final # (Figure 2. in main artice)