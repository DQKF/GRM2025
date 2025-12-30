#===============
#Pedersen Index
#===============

#說明：這份script生產article 中的 Table 2.
#Note: This script generates Table 2 in the main article.


data: GRECALL_F02.csv
summary(GRECALL_F02)
#              source2              period   Topic         ETP       
# China Times News :8   One Month Ahead:20   T01:10   Min.   : 2.46  
# CN & HK Outlets  :8   Two Month Ahead:20   T02:10   1st Qu.:13.92  
# Jornal San Wa Ou :8                        T03:10   Median :21.96  
# Liberty Times    :8                        T04:10   Mean   :25.00  
# United Daily News:8                                 3rd Qu.:35.78  
#                                                     Max.   :73.11                                           
#   


# 計算各媒體在兩個時期間的變化 (by Topic)
# Calculating Cross-Period Thematic Shifts for 
# Individual Outlets (By Topic)


pedersen_by_topic <- GRECALL_F02 %>%
  tidyr::pivot_wider(
    names_from = period, 
    values_from = ETP,
    id_cols = c(source2, Topic)
  ) %>%
  dplyr::mutate(change = abs(`Two Month Ahead` - `One Month Ahead`)) %>%
  dplyr::group_by(source2) %>%
  dplyr::summarise(Pedersen_Index = sum(change, na.rm = TRUE) / 2) %>%
  dplyr::arrange(desc(Pedersen_Index))

write.csv(pedersen_by_topic, "pedersen_by_topic.csv", row.names = FALSE)
write.csv(pedersen_overall, "pedersen_overall.csv", row.names = FALSE)

# 顯示結果 / Results
print("Pedersen Index of Thematic Shifts across Media Outlets")
print(pedersen_by_topic)

# A tibble: 5 × 2
#  source2           Pedersen_Index
#  <chr>                      <dbl>
#1 CN & HK Outlets            25.7 
#2 United Daily News          19.1 
#3 Jornal San Wa Ou           15.2 
#4 Liberty Times              11.0 
#5 China Times News           8.27


#===============
# KL divergence
#===============

# The Kullback-Leibler (KL) divergence
# A tibble: 5 × 2
#  source2           KL_Divergence
#  <chr>                     <dbl>
#1 CN & HK Outlets          0.141 
#2 United Daily News        0.107 
#3 Jornal San Wa Ou         0.0743
#4 Liberty Times            0.0325
#5 China Times News         0.0158

# The Kullback-Leibler (KL) Divergence
kl_divergence_both <- GRECALL_F02 %>%

library(dplyr)
library(tidyr)

# 使用 log2 計算 KL Divergence（與文獻完全一致）
kl_divergence_log2 <- GRECALL_F02 %>%
  dplyr::group_by(source2, period) %>%
  dplyr::mutate(proportion = ETP / sum(ETP)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = proportion,
    id_cols = c(source2, Topic)
  ) %>%
  dplyr::group_by(source2) %>%
  dplyr::summarise(
    KL_OneToTwo = sum(`One Month Ahead` * log2(`One Month Ahead` / `Two Month Ahead`), na.rm = TRUE),
    KL_TwoToOne = sum(`Two Month Ahead` * log2(`Two Month Ahead` / `One Month Ahead`), na.rm = TRUE),
    KL_Symmetric = (KL_OneToTwo + KL_TwoToOne) / 2
  ) %>%
  dplyr::arrange(desc(KL_Symmetric))

# 輸出結果/export results
write.csv(kl_divergence_log2, "kl_divergence_log2_results.csv", row.names = FALSE)
print(kl_divergence_log2)

# A tibble: 5 × 4
#  source2           KL_OneToTwo KL_TwoToOne KL_Symmetric
#  <chr>                   <dbl>       <dbl>        <dbl>
#1 CN & HK Outlets        0.204       0.228        0.216 
#2 United Daily News      0.155       0.172        0.163 
#3 Jornal San Wa Ou       0.107       0.105        0.106 
#4 Liberty Times          0.0469      0.0518       0.0493
#5 China Times News       0.0228      0.0231       0.0230