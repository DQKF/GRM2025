#===============================================================
#計算各Topic在PCA中的weight/ calculating topics' weight in each PC 
#===============================================================

# 說明：本Script負責製作article中的 Table 3 & Figure 3.
# Note: This script produces Table 3 and Figure 3 in the article.

# files: C15段落罷3.csv

#Step01 先準備「主題比例矩陣/Prepare the topic proportion matrix

#Step03 看每個 Topic 在各 PC 的長條圖/ Examine topic loadings on each principal component
# 3-1. 看每個 Topic 在各 PC 的長條圖/Plot bar charts of each topic’s loadings across PCs

library(dplyr)

topic_mat <- C15段落罷3 %>%
  arrange(source2, period) %>%            # 排序（方便對照）
  dplyr::select(dplyr::starts_with("Topic")) %>%  # 抓所有 Topic 欄
  as.matrix()

topic_mat
#         Topic01      Topic02      Topic03     Topic04
#[1,] 0.009623892 0.0049105414 0.9759925773 0.009472989
#[2,] 0.009399708 0.0066097451 0.8497003342 0.134290213
#[3,] 0.079852726 0.0077986570 0.6954854838 0.216863134
#[4,] 0.003836055 0.0084194204 0.9824904227 0.005254102
#[5,] 0.003731645 0.0040527948 0.9875540700 0.004661490
#[6,] 0.004593962 0.0039239722 0.9857415346 0.005740531
   .........
#[248,] 0.009497249 0.3973834511 0.5668135630 0.026305737
#[249,] 0.003756930 0.0083413988 0.9770412922 0.010860379
#[250,] 0.241175497 0.0139183460 0.6815642871 0.063341870
#[ reached 'max' / getOption("max.print") -- omitted 1783 rows ]



# Step02 用 PCA 把 Topic 投影到 PC 空間/
 #Project topics into principal component (PC) space using PCA


pca_topic <- prcomp(topic_mat, center = TRUE, scale. = TRUE)
pca_topic

# Standard deviations (1, .., p=4):
#[1] 1.182948e+00 1.167546e+00 1.112417e+00 9.219790e-16

# Rotation (n x k) = (4 x 4):
                PC1         PC2        PC3        PC4
#Topic01  0.35731128  0.62894534  0.4774666 -0.4987806
#Topic02  0.08276827 -0.75679854  0.4116241 -0.5009700
#Topic03 -0.81305081  0.16997418 -0.1695254 -0.5303945
#Topic04  0.45213542 -0.05284416 -0.7575305 -0.4678981


# Step03 Topic Loadings in each PC/

topic_loading <- pca_topic$rotation[, 1:3]  # PC1~PC3
topic_loading_df <- as.data.frame(topic_loading) %>%
  tibble::rownames_to_column("Topic") %>%
  rename(PC1 = `PC1`, PC2 = `PC2`, PC3 = `PC3`)

topic_loading_df
#    Topic         PC1         PC2        PC3
#1 Topic01  0.35731128  0.62894534  0.4774666
#2 Topic02  0.08276827 -0.75679854  0.4116241
#3 Topic03 -0.81305081  0.16997418 -0.1695254
#4 Topic04  0.45213542 -0.05284416 -0.7575305





# Step04 視覺化幫助詮釋（可選）/ barplot...optional

# 看每個 Topic 在各 PC 的長條圖/ barplot...topic loadings on each PC
library(tidyr)
library(ggplot2)

topic_loading_long <- topic_loading_df %>%
  pivot_longer(cols = starts_with("PC"),
               names_to = "PC",
               values_to = "loading")

ggplot(topic_loading_long,
       aes(x = Topic, y = loading, fill = PC)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Loading", x = "Topic") +
  theme_minimal()


ggplot(topic_loading_df,
       aes(x = PC1, y = PC2, label = Topic)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point() +
  ggrepel::geom_text_repel() +
  coord_equal() +
  theme_minimal()




# Step05

# 5.1 從原始 Topic 比例 → CLR 轉換/From raw topic proportions → CLR transformation


library(dplyr)
library(compositions)  # for acomp() and clr()

# 確保 factor 設定/ as.factor
C15段落罷3 <- C15段落罷3 %>%
  mutate(
    source2 = factor(source2),
    period  = factor(period)
  )

# 取出四個 topic 組成矩陣/ # Extract the four topics to construct the topic matrix.
topic_mat <- C15段落罷3 %>%
  select(Topic01, Topic02, Topic03, Topic04) %>%
  as.matrix()

# 避免 0：加一個很小的 pseudo-count，再重新正規化
 #Avoid zeros: add a very small pseudo-count and then re-normalize.
topic_mat[topic_mat == 0] <- 1e-6
topic_mat1 <- topic_mat / rowSums(topic_mat)

# 轉為 acomp 物件，再做 CLR/Avoid zeros: add a very small pseudo-count and then re-normalize
 topic_clr1 <- clr(acomp(topic_mat1))   # 每一列是一則新聞的 CLR 座標
 dim(topic_clr1)
[1] 2033    4

head(topic_clr1)
#        Topic01    Topic02     Topic03    Topic04
#[1,] -1.6974423  3.0483159  0.37394048 -1.7248140
#[2,] -0.2392291  0.2872279  0.01307036 -0.0610691
#[3,] -1.1488129 -1.3641678  3.84669401 -1.3337133
#[4,] -1.5445183 -1.3408861  1.40973288  1.4756715
#[5,] -1.2668454 -1.1392254  3.54758276 -1.1415119
#[6,]  2.5617827 -0.4301004 -0.41202995 -1.7196524

attr(,"class")
[1] "rmult"

# PCA
pca_fit <- prcomp(topic_clr1, center = TRUE, scale. = FALSE)


# 看 PC1, PC2, PC3 解釋的變異比例
 #Check the proportion of variance explained by PC1, PC2, and PC3

summary(pca_fit)       

# Importance of components:
#                          PC1    PC2    PC3       PC4
#Standard deviation     2.3080 2.2621 1.5849 3.046e-15
#Proportion of Variance 0.4112 0.3950 0.1939 0.000e+00
#Cumulative Proportion  0.4112 0.8061 1.0000 1.000e+00

# PC1 = 41.12%
# PC2 = 39.50%
# PC3 = 19.39%
# PC4 = 0%

pca_fit$rotation       # Topics' weight in each PC

#               PC1        PC2         PC3 PC4
#Topic01  0.6435084  0.4037636 -0.41577867 0.5
#Topic02 -0.1889116 -0.7394923 -0.40922311 0.5
#Topic03 -0.6999929  0.5097483  0.01290699 0.5
#Topic04  0.2453961 -0.1740195  0.81209479 0.5

pca_scores <- pca_fit$x %>%
    as.data.frame() %>%
    dplyr::select(PC1, PC2, PC3) %>%   # ⭐ 指定用 dplyr::select
    bind_cols(
        C15段落罷3 %>%
            dplyr::select(source2, period)
    )

head(pca_scores)
#          PC1        PC2        PC3         source2        period
#1 -2.4109382 -2.3923960 -2.3375800 CN & HK Outlets TwoMonthAhead
#2 -0.2900813 -0.2352963 -0.4675121 CN & HK Outlets TwoMonthAhead
#3 -3.5592498  2.7942893 -0.3975645 CN & HK Outlets TwoMonthAhead
#4 -1.4230200  0.8861767  2.0074669 CN & HK Outlets TwoMonthAhead
#5 -3.4211576  2.3943706 -0.2883156 CN & HK Outlets TwoMonthAhead
#6  1.5384623  1.4980408 -2.6909794 CN & HK Outlets TwoMonthAhead

centroids_pca <- pca_scores %>%
    group_by(source2, period) %>%
    summarise(
        PC1 = mean(PC1),
        PC2 = mean(PC2),
        PC3 = mean(PC3),
        .groups = "drop"
    )
 
centroids_pca
# A tibble: 10 × 5
   source2           period           PC1    PC2      PC3
   <fct>             <fct>          <dbl>  <dbl>    <dbl>
 1 China Times       OneMonthAhead -1.15   0.530  0.336  
 2 China Times       TwoMonthAhead -0.832  0.333 -0.00354
 3 CN & HK Outlets   OneMonthAhead -1.97   2.16  -0.330  
 4 CN & HK Outlets   TwoMonthAhead -0.936  1.93  -1.03   
 5 Jornal San Wa Ou  OneMonthAhead  1.49   2.79  -0.770  
 6 Jornal San Wa Ou  TwoMonthAhead  2.03   2.57  -1.59   
 7 Liberty Times     OneMonthAhead  1.55  -2.22   0.197  
 8 Liberty Times     TwoMonthAhead  1.96  -1.74  -0.181  
 9 United Daily News OneMonthAhead -0.862  0.567  0.0730 
10 United Daily News TwoMonthAhead  0.357  0.849 -0.452  

centroids_pca $ group_label<- c("China Times|1MA","China Times|2MA",
	                             "CN & HK Outlets|1MA","CN & HK Outlets|2MA",
	                             "Jornal San Wa Ou|1MA","Jornal San Wa Ou|2MA",
	                             "Liberty Times|1MA","Liberty Times|2MA",
	                             "United Daily News|1MA","United Daily News|2MA"
	                             ) 

# plot PC1 & PC2

Centroid_PC0102 <- ggplot(centroids_pca,
       aes(x = PC1, y = PC2,
           color = source2, shape = period)) +
  geom_point(size = 5, alpha = 0.9) +
geom_vline(xintercept = 0, color = "#A7D8F0", linewidth = 0.8)+
geom_hline(yintercept = 0, color = "#A7D8F0", linewidth = 0.8)+
   # 🔹 標示象限 I, II, III, IV
  annotate("text", x = max(centroids_pca$PC1)*1.1,
                 y = max(centroids_pca$PC3)*8.1,
                 label = "I", size = 7, fontface = "bold", color = "gray40") +
  annotate("text", x = min(centroids_pca$PC1)*1.1,
                 y = max(centroids_pca$PC3)*8.1,
                 label = "II", size = 7, fontface = "bold", color = "gray40") +
  annotate("text", x = min(centroids_pca$PC1)*1.1,
                 y = min(centroids_pca$PC3)*1.4,
                 label = "III", size = 7, fontface = "bold", color = "gray40") +
  annotate("text", x = max(centroids_pca$PC1)*1.1,
                 y = min(centroids_pca$PC3)*1.4,
                 label = "IV", size = 7, fontface = "bold", color = "gray40") +
  # 🔹 調整文字大小與距離/ font size adj.
  geom_text_repel(
    aes(label = group_label),
    size = 5.2,           # ← 放大文字 (預設約3.5~4)
    box.padding = 0.6,    # ← 與點的距離（字框 padding）
    point.padding = 0.5,  # ← 點與文字之間距離
    segment.size = 0.4,   # ← 連線粗細
    segment.color = "grey50", # ← 連線顏色
    max.overlaps = Inf,
    show.legend = FALSE
  ) +

  labs(
    title = "A.  Media × Period Centroids in PC Space",
    subtitle = "PC1  Vs. PC2",
    x = "PC1 (Centroid, 41.12%)",
    y = "PC2 (Centroid, 39.50%)",
    color = "Media",
    shape = "Period"
  ) +

  scale_color_manual(
    values = c(
      "Jornal San Wa Ou"   = "#1f77b4",
      "China Times"        = "#ff7f0e",
      "United Daily News"  = "#9467bd",
      "CN & HK Outlets"   = "red",
      "Liberty Times"      = "darkgreen"
    )
  ) +

  theme_minimal(base_size = 16) +
  theme(
  	legend.position = "none",     # 🔥🔥 關閉所有 legends
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 13),
    axis.text.x  = element_text(size = 13, face = "bold"),  # X 軸數字
    axis.text.y  = element_text(size = 13, face = "bold"),  # Y 軸數字
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10))
  )


Centroid_PC0102



# plot PC1 & PC3

Centroid_PC0103 <- ggplot(centroids_pca,
       aes(x = PC1, y = PC3,
           color = source2, shape = period)) +
  geom_point(size = 5, alpha = 0.9) +
geom_vline(xintercept = 0, color = "#A7D8F0", linewidth = 0.8)+
geom_hline(yintercept = 0, color = "#A7D8F0", linewidth = 0.8)+
   # 🔹 標示象限 I, II, III, IV
  annotate("text", x = max(centroids_pca$PC1)*1.1,
                 y = max(centroids_pca$PC3)*1.1,
                 label = "I", size = 7, fontface = "bold", color = "gray40") +
  annotate("text", x = min(centroids_pca$PC1)*1.1,
                 y = max(centroids_pca$PC3)*1.1,
                 label = "II", size = 7, fontface = "bold", color = "gray40") +
  annotate("text", x = min(centroids_pca$PC1)*1.1,
                 y = min(centroids_pca$PC3)*1.15,
                 label = "III", size = 7, fontface = "bold", color = "gray40") +
  annotate("text", x = max(centroids_pca$PC1)*1.1,
                 y = min(centroids_pca$PC3)*1.15,
                 label = "IV", size = 7, fontface = "bold", color = "gray40") +

  # 🔹 調整文字大小與距離
  geom_text_repel(
    aes(label = group_label),
    size = 5.2,           # ← 放大文字 (預設約3.5~4)
    box.padding = 0.6,    # ← 與點的距離（字框 padding）
    point.padding = 0.5,  # ← 點與文字之間距離
    segment.size = 0.4,   # ← 連線粗細
    segment.color = "grey50", # ← 連線顏色
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  labs(
    title = "B.  Media × Period Centroids in PC Space",
    subtitle = "PC1  Vs. PC3",
    x = "PC1 (Centroid, 41.12%)",
    y = "PC3 (Centroid, 19.39%)",
    color = "Media",
    shape = "Period"
  ) +

  scale_color_manual(
    values = c(
      "Jornal San Wa Ou"   = "#1f77b4",
      "China Times"        = "#ff7f0e",
      "United Daily News"  = "#9467bd",
      "CN & HK Outlets"   = "red",
      "Liberty Times"      = "darkgreen"
    )
  ) +

  theme_minimal(base_size = 16) +
  theme(
  	legend.position = "none",     # 🔥🔥 關閉所有 legends
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 13),
    axis.text.x  = element_text(size = 13, face = "bold"),  # X 軸數字
    axis.text.y  = element_text(size = 13, face = "bold"),  # Y 軸數字
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10))
  )


Centroid_PC0103