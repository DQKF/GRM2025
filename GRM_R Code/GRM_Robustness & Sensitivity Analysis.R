#==================================
# Robustness & Sensitivity Analysis
#==================================

# 這份script 生產 出Appendix 中 Figure E （即本篇中的圖 LOO）
# This script generates Appendix Figure E (i.e., the leave-one-out figure used in this paper).


# 主旨 #
# 由於 CN-HK 媒體集群的樣本數（n）較小，
# 且包含多個不同的主題分佈，統計結果可能對個別主題的變動較為敏感。
# 為了排除單一極端主題（Outliers）對整體趨勢的誤導，
# 並驗證「定向重構」現象的結構穩健性，本研究採用了
# 基於 Jackknife 邏輯的「留一法」（LOO）進行壓力測試。

# Rationale for Robustness Testing #
# Given that the CN-HK outlets constitute a relatively 
# small sample size (n), the aggregate statistical measures
# (such as the Symmetric KL divergence) may be disproportionately
# influenced by high-volatility topics. 
# To ensure that the observed findings are not artifacts
# of specific outliers but represent a systemic pattern,
# a leave-one-topic-out (LOO) sensitivity analysis was conducted.
# Following the "Jackknife" resampling logic (Tukey, 1958), 
# this procedure iteratively excludes individual topics 
# to assess the structural stability of the results. 
# This test is essential for validating the 
# Directed Reconfiguration hypothesis, 
# proving that the strategic pivot of external media 
# is a robust, top-down phenomenon that persists 
# even when the most volatile thematic drivers are removed.


# data: GRECALL_F02.csv

GRECALL_F02$source2 <- as.factor(GRECALL_F02$source2)
GRECALL_F02$period <- as.factor(GRECALL_F02$period)
GRECALL_F02$Topic <- as.factor(GRECALL_F02$Topic)

summary(GRECALL_F02)
#              source2              period   Topic         ETP       
# China Times News :8   One Month Ahead:20   T01:10   Min.   : 2.46  
# CN & HK Outlets  :8   Two Month Ahead:20   T02:10   1st Qu.:13.92  
# Jornal San Wa Ou :8                        T03:10   Median :21.96  
# Liberty Times    :8                        T04:10   Mean   :25.00  
# United Daily News:8                                 3rd Qu.:35.78  
#                                                     Max.   :73.11                                           
#                                                     Mean   :25.00  

# 1. 穩健版 KL 計算函數 (處理 T-1 的狀況)
    # 1. Robust KL Calculation Function (Handling $T-1$ Scenarios)

calc_symmetric_kl_robust <- function(data) {
     epsilon <- 1e-10
     kl_res <- data %>%
         group_by(period, Topic) %>%
         summarise(ETP = sum(ETP, na.rm = TRUE), .groups = "drop") %>%
         group_by(period) %>%
         mutate(proportion = (ETP + epsilon) / (sum(ETP) + epsilon * n())) %>%
         ungroup() %>%
         pivot_wider(names_from = period, values_from = proportion, id_cols = Topic) %>%
         summarise(
             KL_OneToTwo = sum(`One Month Ahead` * log2(`One Month Ahead` / `Two Month Ahead`), na.rm = TRUE),
             KL_TwoToOne = sum(`Two Month Ahead` * log2(`Two Month Ahead` / `One Month Ahead`), na.rm = TRUE),
             KL_Symmetric = (KL_OneToTwo + KL_TwoToOne) / 2
         )
     return(kl_res$KL_Symmetric)
 }


# 2. 針對主題執行留一法 (以 CN & HK Outlets 為例)
  # 2. Implementation of the Leave-One-Out (LOO) Method on Topics: 
       # A Case Study of CN & HK Outlets

  target_cluster <- "CN & HK Outlets"
  cluster_data <- GRECALL_F02 %>% filter(source2 == target_cluster)
  all_topics <- unique(cluster_data$Topic)
  
  loo_topic_results <- data.frame(Excluded_Topic = character(), KL_Symmetric = numeric())

  for (top in all_topics) {
     reduced_data <- cluster_data %>% filter(Topic != top)
     current_kl <- calc_symmetric_kl_robust(reduced_data)
     loo_topic_results <- rbind(loo_topic_results, 
                                data.frame(Excluded_Topic = top, KL_Symmetric = current_kl))
   }
 
# 3. 計算 Topic-level Coefficient of Variation (CV) 
  # 3. 並顯示結果/Calculate the topic-level coefficient of variation (CV) and display the results.

cv_topic <- sd(loo_topic_results$KL_Symmetric) / mean(loo_topic_results$KL_Symmetric)

print(paste("主題層級變異係數 (CV):", round(cv_topic, 4)))
[1] "主題層級變異係數 (CV): 0.3657"。 # Topic-level Coefficient of Variation

print(loo_topic_results)
#  Excluded_Topic KL_Symmetric
#1            T01    0.1049762
#2            T02    0.1962016
#3            T03    0.1201164
#4            T04    0.2284155


#------
# plot
#------

library(ggplot2)

# 4. 準備數據/ data preparation
loo_data <- data.frame(
  Excluded = c("None (Original)", "T01", "T02", "T03", "T04"),
  KL = c(0.216, 0.105, 0.196, 0.120, 0.228)
)

# 2. 繪製圖表 / plot
LOO <- ggplot(loo_data, aes(x = reorder(Excluded, -KL), y = KL, fill = (Excluded == "None (Original)"))) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(KL, 3)), vjust = -0.5, size = 5, fontface = "bold") + 
  
  # --- 增加雙基準線 --- add basic lines.
  # 高基準線. high KL value: UDN (0.163)
  geom_hline(yintercept = 0.163, linetype = "dashed", color = "#E63946", size = 0.8) +
  annotate("text", x = 5, y = 0.173, label = "UDN (Max Domestic: 0.163)", color = "#E63946", fontface = "bold") +
  
  # 低基準線. low KL value: China Times (0.023)
  geom_hline(yintercept = 0.023, linetype = "dotted", color = "#1D3557", size = 0.8) +
  annotate("text", x = 5, y = 0.033, label = "China Times (Min: 0.023)", color = "#1D3557", fontface = "italic") +
  # --------------------
  
  scale_fill_manual(values = c("#A8DADC", "#457B9D")) +
  labs(title = "Sensitivity Analysis (LOO): CN-HK Media Volatility",
       subtitle = "Symmetric KL Divergence when excluding individual topics",
       x = "Excluded Topic (None = Original Aggregate)",
       y = "Symmetric KL Divergence") +
  
  scale_y_continuous(limits = c(0, 0.28)) + # 稍微拉高 y 軸確保文字不被切掉
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12))

LOO