#==============================================================
# Figure 1. Short‑run agenda trajectories across media outlets.
#==============================================================

#-------------------------------
# Script for Figure 1 in article
#-------------------------------



library(tibble)
mdt04<-make.dt(大罷免_stm.out_04W, meta = NULL)
View(mdt04)

mdt04[, docnum := NULL]

mdt04tibble<-tibble(mdt04)

mdt04_ave<-tibble(colMeans(mdt04tibble))

C15段落罷_stm<-cbind(C15段落罷_stm,mdt04)
View(mdt04_ave) #每個主題的平均數

colnames(mdt04tibble)<-c("Topic01", "Topic02", "Topic03", "Topic04")
                         
colnames(mdt04tibble)<-c("Topic01", "Topic02", "Topic03", "Topic04","Topic05")


C15段落罷3<- cbind(C15段落罷3, mdt04tibble)


topic01_mean <- aggregate(Topic01 ~ source2 + period , data = C15段落罷_stm, FUN = mean)
View(topic01_mean)
colnames(topic01_mean)[3]<-"ETPP"


topic02_mean <- aggregate(Topic02~ source2 + period , data = C15段落罷_stm, FUN = mean)
colnames(topic02_mean)[3]<-"ETPP"

topic03_mean <- aggregate(Topic03~ source2 + period , data = C15段落罷_stm, FUN = mean)
colnames(topic03_mean)[3]<-"ETPP"

topic04_mean <- aggregate(Topic04~ source2 + period , data = C15段落罷_stm, FUN = mean)
colnames(topic04_mean)[3]<-"ETPP"

topic01_mean $ Topic <- "T01"
topic02_mean $ Topic <- "T02"
topic03_mean $ Topic <- "T03"
topic04_mean $ Topic <- "T04"

topic01_mean$ETP <- round(topic01_mean$ETPP * 100, digits = 2)
topic02_mean$ETP <- round(topic02_mean$ETPP * 100, digits = 2)
topic03_mean$ETP <- round(topic03_mean$ETPP * 100, digits = 2)
topic04_mean$ETP <- round(topic04_mean$ETPP * 100, digits = 2)

GRECALL_F02<-rbind(topic01_mean, topic02_mean, topic03_mean, topic04_mean)


# 載入必要套件 / loading R packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
install.packages("cowplot")  # 若尚未安裝
library(cowplot)


# 讀取資料/ read data
# GRECALL_F02  
# GRECALL_F02 是用來做 GRM（Great Recall Movement）分析的「長格式（long-format）」主題資料表，
 # 重點是把 不同媒體 × 不同期間 × 不同主題 的 ETP（Expected Topic Proportion）整理成可直接做
 # 比較/作圖/統計檢定的結構。

#GRECALL_F02 is a long-format topic dataset used for the GRM 
# (Great Recall Movement) analysis. It is structured to organize
# ETPs (Expected Topic Proportions) across media 
# outlets × time periods × topics, making the results directly 
# usable for comparison, visualization, and statistical testing.

summary(GRECALL_F02) 
#              source2              period   Topic         ETP       
# China Times News :8   One Month Ahead:20   T01:10   Min.   : 2.46  
# CN & HK Outlets  :8   Two Month Ahead:20   T02:10   1st Qu.:13.92  
# Jornal San Wa Ou :8                        T03:10   Median :21.96  
# Liberty Times    :8                        T04:10   Mean   :25.00  
# United Daily News:8                                 3rd Qu.:35.78  
                                                     Max.   :73.11  

data <- read.csv("GRECALL_F02.csv")


# Extract media data and plot each.


# CN & HK outlets
library(ggplot2)
library(dplyr)

# 假設數據已經載入為 df
# df <- read.csv("C15段落罷主題平均_時期.csv")

# 

中港_data <- GRECALL_F02 %>%
            filter(source2 == "CN & HK Outlets") %>%
            dplyr::select(period, Topic, ETP) %>%
            mutate(ETP = as.numeric(ETP)) %>%
            # 重新排序 period 因子，確保時間順序正確
            mutate(period = factor(period, 
            levels = c("Two Month Ahead", "One Month Ahead")))


P大罷免_中港 <- ggplot(中港_data, 
                   aes(x = period,
                       y = ETP,
                       group = Topic,
                       color = Topic)) +
             geom_line(linewidth = 1.2,
                       alpha = 0.7,
                       arrow = arrow(type = "closed", 
                                   length = unit(0.3, "cm"))) +
             geom_point(size = 3) +

  # 左邊：Two Month Ahead → Topic + 數值
             geom_text_repel(data = dplyr::filter(中港_data, period == "Two Month Ahead"),
                         aes(label = paste0(Topic, " ", sprintf("%.2f", ETP))),
                             size = 7.5, 
                             direction = "y", 
                             hjust = 1.1, 
                             nudge_x = -0.1, 
                             show.legend = FALSE
                             ) +

  # 右邊：One Month Ahead → 只顯示數值
             geom_text_repel(data = dplyr::filter(中港_data, period == "One Month Ahead"),
                         aes(label = sprintf("%.2f", ETP)),
                             size = 7.5, 
                             direction = "y", 
                             hjust = -0.1, 
                             nudge_x = 0.1, 
                             show.legend = FALSE
                             ) +
scale_x_discrete(
  labels = c(
    "Two Month Ahead" = "2MA",
    "One Month Ahead" = "1MA"
  )
)+
             labs(title = "", x = "", y = "") +
             scale_y_continuous(breaks = seq(5, 70, by = 5), 
                     limits = c(5, 70)) +
             theme_minimal() +
             theme(legend.position = "none",
                   axis.text.x = element_text(size = 24, face = "bold"),
                   axis.text.y = element_text(size = 22, face = "bold")
                   ) +
                   scale_color_manual(values = c("T01" = "#E31A1C",  # 紅色
                                                 "T02" = "#1F78B4",  # 藍色  
                                                 "T03" = "#33A02C",  # 綠色
                                                 "T04" = "#FF7F00")) # 紫色


P大罷免_中港 <- ggdraw(P大罷免_中港) +
  draw_label("B. CN & HK Outlets", x = 0, y = 1, hjust = -0.12, vjust = 1.2,
             size = 26, fontface = "bold")

P大罷免_中港

# ------------------------------------------------------------
# Jornal San Wa Ou

library(ggplot2)
library(dplyr)

# 假設數據已經載入為 df
# df <- read.csv("C15段落罷主題平均_時期.csv")

# 篩選中時報系的數據

澳報_data <- GRECALL_F02 %>%
            filter(source2 == "Jornal San Wa Ou") %>%
            dplyr::select(period, Topic, ETP) %>%
            mutate(ETP = as.numeric(ETP)) %>%
            # 重新排序 period 因子，確保時間順序正確
            mutate(period = factor(period, 
            levels = c("Two Month Ahead", "One Month Ahead")))

P大罷免_澳 <- ggplot(澳報_data, 
                   aes(x = period,
                       y = ETP,
                       group = Topic,
                       color = Topic)) +
             geom_line(linewidth = 1.2,
                       alpha = 0.7,
                       arrow = arrow(type = "closed", 
                                   length = unit(0.3, "cm"))) +
             geom_point(size = 3) +

  # 左邊：Two Month Ahead → Topic + 數值
             geom_text_repel(data = dplyr::filter(澳報_data, period == "Two Month Ahead"),
                         aes(label = paste0(Topic, " ", sprintf("%.2f", ETP))),
                             size = 7.5, 
                             direction = "y", 
                             hjust = 1.1, 
                             nudge_x = -0.1, 
                             show.legend = FALSE
                             ) +

  # 右邊：One Month Ahead → 只顯示數值
             geom_text_repel(data = dplyr::filter(澳報_data, period == "One Month Ahead"),
                         aes(label = sprintf("%.2f", ETP)),
                             size = 7.5, 
                             direction = "y", 
                             hjust = -0.1, 
                             nudge_x = 0.1, 
                             show.legend = FALSE
                             ) +
scale_x_discrete(
  labels = c(
    "Two Month Ahead" = "2MA",
    "One Month Ahead" = "1MA"
  )
)+
             labs(title = "", x = "", y = "") +
             scale_y_continuous(breaks = seq(0, 75, by = 5), 
                     limits = c(0, 75)) +
             theme_minimal() +
             theme(legend.position = "none",
                   axis.text.x = element_text(size = 24, face = "bold"),
                   axis.text.y = element_text(size = 22, face = "bold")
                   ) +
                   scale_color_manual(values = c("T01" = "#E31A1C",  # 紅色
                                                 "T02" = "#1F78B4",  # 藍色  
                                                 "T03" = "#33A02C",  # 綠色
                                                 "T04" = "#FF7F00")) # 紫色


P大罷免_澳報 <- ggdraw(P大罷免_澳) +
  draw_label("C. Jornal San Wa Ou", x = 0, y = 1, hjust = -0.12, vjust = 1.2,
             size = 26, fontface = "bold")
P大罷免_澳報
--------------------------------------------
# Liberty Times

library(ggplot2)
library(dplyr)

自由_data <- GRECALL_F02 %>%
            filter(source2 == "Liberty Times") %>%
            dplyr::select(period, Topic, ETP) %>%
            mutate(ETP = as.numeric(ETP)) %>%
            # 重新排序 period 因子，確保時間順序正確
            mutate(period = factor(period, 
            levels = c("Two Month Ahead", "One Month Ahead")))

P大罷免_自由 <- ggplot(自由_data, 
                   aes(x = period,
                       y = ETP,
                       group = Topic,
                       color = Topic)) +
             geom_line(linewidth = 1.2,
                       alpha = 0.7,
                       arrow = arrow(type = "closed", 
                                   length = unit(0.3, "cm"))) +
             geom_point(size = 3) +

  # 左邊：Two Month Ahead → Topic + 數值
             geom_text_repel(data = dplyr::filter(自由_data, period == "Two Month Ahead"),
                         aes(label = paste0(Topic, " ", sprintf("%.2f", ETP))),
                             size = 7.5, 
                             direction = "y", 
                             hjust = 1.1, 
                             nudge_x = -0.1, 
                             show.legend = FALSE
                             ) +

  # 右邊：One Month Ahead → 只顯示數值
             geom_text_repel(data = dplyr::filter(自由_data, period == "One Month Ahead"),
                         aes(label = sprintf("%.2f", ETP)),
                             size = 7.5, 
                             direction = "y", 
                             hjust = -0.1, 
                             nudge_x = 0.1, 
                             show.legend = FALSE
                             ) +
scale_x_discrete(
  labels = c(
    "Two Month Ahead" = "2MA",
    "One Month Ahead" = "1MA"
  )
)+
             labs(title = "", x = "", y = "") +
             scale_y_continuous(breaks = seq(0, 45, by = 5), 
                     limits = c(0, 45)) +
             theme_minimal() +
             theme(legend.position = "none",
                   axis.text.x = element_text(size = 24, face = "bold"),
                   axis.text.y = element_text(size = 22, face = "bold")
                   ) +
                   scale_color_manual(values = c("T01" = "#E31A1C",  # 紅色
                                                 "T02" = "#1F78B4",  # 藍色  
                                                 "T03" = "#33A02C",  # 綠色
                                                 "T04" = "#FF7F00")) # 紫色

P大罷免_自由報系 <- ggdraw(P大罷免_自由) +
  draw_label("D. Liberty Times", x = 0, y = 1, hjust = -0.12, vjust = 1.2,
             size = 26, fontface = "bold")
P大罷免_自由報系

# ---------------------------------------------------------
# United Daily News

聯合_data <- GRECALL_F02 %>%
            filter(source2 == "United Daily News") %>%
            dplyr::select(period, Topic, ETP) %>%
            mutate(ETP = as.numeric(ETP)) %>%
            # 重新排序 period 因子，確保時間順序正確
            mutate(period = factor(period, 
            levels = c("Two Month Ahead", "One Month Ahead")))

P大罷免_聯合報 <- ggplot(聯合_data, 
                   aes(x = period,
                       y = ETP,
                       group = Topic,
                       color = Topic)) +
             geom_line(linewidth = 1.2,
                       alpha = 0.7,
                       arrow = arrow(type = "closed", 
                                   length = unit(0.3, "cm"))) +
             geom_point(size = 3) +

  # 左邊：Two Month Ahead → Topic + 數值
             geom_text_repel(data = dplyr::filter(聯合_data, period == "Two Month Ahead"),
                         aes(label = paste0(Topic, " ", sprintf("%.2f", ETP))),
                             size = 7.5, 
                             direction = "y", 
                             hjust = 1.1, 
                             nudge_x = -0.1, 
                             show.legend = FALSE
                             ) +

  # 右邊：One Month Ahead → 只顯示數值
             geom_text_repel(data = dplyr::filter(聯合_data, period == "One Month Ahead"),
                         aes(label = sprintf("%.2f", ETP)),
                             size = 7.5, 
                             direction = "y", 
                             hjust = -0.1, 
                             nudge_x = 0.1, 
                             show.legend = FALSE
                             ) +
scale_x_discrete(
  labels = c(
    "Two Month Ahead" = "2MA",
    "One Month Ahead" = "1MA"
  )
)+
             labs(title = "", x = "", y = "") +
             scale_y_continuous(breaks = seq(10, 45, by = 5), 
                     limits = c(15, 40)) +
             theme_minimal() +
             theme(legend.position = "none",
                   axis.text.x = element_text(size = 26, face = "bold"),
                   axis.text.y = element_text(size = 22, face = "bold")
                   ) +
                   scale_color_manual(values = c("T01" = "#E31A1C",  # 紅色
                                                 "T02" = "#1F78B4",  # 藍色  
                                                 "T03" = "#33A02C",  # 綠色
                                                 "T04" = "#FF7F00")) # 紫色
P大罷免_聯合報系 <- ggdraw(P大罷免_聯合報) +
  draw_label("E. United Daily News", x = 0, y = 1, hjust = -0.12, vjust = 1.4,
             size = 26, fontface = "bold")

P大罷免_聯合報系
--------------------------------------------------------
# China Times

library(ggplot2)
library(dplyr)

# 假設數據已經載入為 df
# df <- read.csv("C15段落罷主題平均_時期.csv")

# 篩選中時報系的數據

中時_data <- GRECALL_F02 %>%
            filter(source2 == "China Times News") %>%
            dplyr::select(period, Topic, ETP) %>%
            mutate(ETP = as.numeric(ETP)) %>%
            # 重新排序 period 因子，確保時間順序正確
            mutate(period = factor(period, 
            levels = c("Two Month Ahead", "One Month Ahead")))

P大罷免_中時報 <- ggplot(中時_data, 
                   aes(x = period,
                       y = ETP,
                       group = Topic,
                       color = Topic)) +
             geom_line(linewidth = 1.2,
                       alpha = 0.7,
                       arrow = arrow(type = "closed", 
                                   length = unit(0.3, "cm"))) +
             geom_point(size = 3) +

  # 左邊：Two Month Ahead → Topic + 數值
             geom_text_repel(data = dplyr::filter(中時_data, period == "Two Month Ahead"),
                         aes(label = paste0(Topic, " ", sprintf("%.2f", ETP))),
                             size = 7.5, 
                             direction = "y", 
                             hjust = 1.1, 
                             nudge_x = -0.1, 
                             show.legend = FALSE
                             ) +

  # 右邊：One Month Ahead → 只顯示數值
             geom_text_repel(data = dplyr::filter(中時_data, period == "One Month Ahead"),
                         aes(label = sprintf("%.2f", ETP)),
                             size = 7.5, 
                             direction = "y", 
                             hjust = -0.1, 
                             nudge_x = 0.1, 
                             show.legend = FALSE
                             ) +
scale_x_discrete(
  labels = c(
    "Two Month Ahead" = "2MA",
    "One Month Ahead" = "1MA"
  )
)+
             labs(title = "", x = "", y = "") +
             scale_y_continuous(breaks = seq(10, 45, by = 5), 
                     limits = c(10, 45)) +
             theme_minimal() +
             theme(legend.position = "none",
                   axis.text.x = element_text(size = 26, face = "bold"),
                   axis.text.y = element_text(size = 22, face = "bold")
                   ) +
                   scale_color_manual(values = c("T01" = "#E31A1C",  # 紅色
                                                 "T02" = "#1F78B4",  # 藍色  
                                                 "T03" = "#33A02C",  # 綠色
                                                 "T04" = "#FF7F00")) # 紫色
 
P大罷免_中時報系 <- ggdraw(P大罷免_中時報) +
  draw_label("A. China Times", x = 0, y = 1, hjust = -0.12, vjust = 1.2,
             size = 26, fontface = "bold")
P大罷免_中時報系




#-----------------------

# MERGE 5 plots into Figure 1. (in article)

library(cowplot)

# 合併五個圖表，排列為 1 行 5 列 / merge
merged_plot <- plot_grid(P大罷免_中時報系,P大罷免_中港, P大罷免_澳報, P大罷免_自由報系,P大罷免_聯合報系,
                        nrow = 1, ncol = 5)

merged_plot