#====================
# stm for topic Model
#====================

# 說明：這份script生產Appendix中的Figure A,B,C,D
# Note: This script generates Appendix Figures A, B, C, and D.


#-------------------
#file: C15段落罷3.csv
#-------------------

# Step01 進行主題建模及其前置作業/Conduct topic modeling and its preprocessing steps.

library(stm)
library(parallel)

processed <- textProcessor(
  documents         = C15段落罷3$newc,  # 文字欄位
  metadata          = C15段落罷3,       # 對應的 meta
  lowercase         = TRUE,
  removestopwords   = FALSE,            # 中文用自訂 stopwords
  removenumbers     = FALSE,            # ← 不保留數字
  removepunctuation = TRUE,
  stem              = FALSE,
  wordLengths       = c(2, Inf)         # 最少詞長 2
)

out <-  prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
#-----------------------------------

# 記錄開始時間 / start time
start_time <- Sys.time() 

# Step 03 找尋最合適的主題數目K (search K)

library(stm)
library(parallel)


start_time <- Sys.time()

ncores <- max(1, detectCores() - 1) #平行運算 

results_大罷免3 <- searchK(
  documents       = docs,
  vocab           = vocab,
  K               = c(3:15),
  prevalence      = ~ source2,
  data            = C15段落罷3
  init.type       = "Spectral",
  max.em.its      = 400,
  cores           = ncores,     # ← 使用多核心
  verbose         = TRUE,
  seed            = 5555
)

# 記錄結束時間 / end time
end_time <- Sys.time()

# 顯示執行時間
cat("\n✅ searchK() 執行完成！\n",
    "開始時間：", start_time, "\n",
    "結束時間：", end_time, "\n",
    "總耗時：", round(difftime(end_time, start_time, units = "mins"), 2), " 分鐘\n")


saveRDS(results_大罷免3, file = "results_searchK_大罷免3.rds")


C15段落罷3<-C15段落罷_stm
C15段落罷3<-C15段落罷3[,-c(19:26)]
C15段落罷3$period<-as.factor(C15段落罷3$period)
C15段落罷3$source2<-as.factor(C15段落罷3$source2)




#Step02 Search K ,to sort best number of topics.

plot(results_大罷免3)

results_大罷免3$results

results_dfs <- results_大罷免3$results


str(results_dfs)

#轉為數值型資料/ transform into numeric data
results_dfs$semcoh <- as.numeric(results_dfs$semcoh)
results_dfs$exclus <- as.numeric(results_dfs$exclus)


#plot searchK
  ggplot(results_dfs, aes(x = semcoh, y = exclus, label = K)) +
  geom_point(color = "blue", size = 3) +  # Add points
  geom_text(vjust = -0.5, hjust = 0.5) +   # Add labels above points
  labs(title = "Exclusiveness vs. Semantic Coherence",
       x = "Semantic Coherence",
       y = "Exclusiveness") +
  theme_minimal()  # Use a minimal theme for better aesthetics



#LDAvis方法 / LDAvis

  ## 候選 K= 4, 5, 6 

# K= ? is fine to topc model
set.seed(89078)
大罷免_stm.out_04W <- stm(out$documents, out$vocab, 
                      K = 4, prevalence = ~ source2, 
                      data= C15段落罷3,
                      init.type = "Spectral") 
nrow(C15段落罷3)
set.seed(89079)
大罷免_stm.out_05W <- stm(out$documents, out$vocab, 
                      K = 5, prevalence = ~ source2, 
                      data= C15段落罷3,
                      init.type = "Spectral") 



set.seed(89078)
大罷免_stm.out_06W <- stm(out$documents, out$vocab, 
                      K = 6, prevalence = ~ source2, 
                      data= C15段落罷3,
                      init.type = "Spectral") 

set.seed(89078)
大罷免_stm.out_07W <- stm(out$documents, out$vocab, 
                      K = 7, prevalence = ~ source2, 
                      data= C15段落罷3,
                      init.type = "Spectral") 

set.seed(89078)
大罷免_stm.out_08W <- stm(out$documents, out$vocab, 
                      K = 8, prevalence = ~ source2, 
                      data= C15段落罷3,
                      init.type = "Spectral") 

set.seed(89078)
大罷免_stm.out_19W <- stm(out$documents, out$vocab, 
                      K = 11, prevalence = ~ source2, 
                      data= C15段落罷_stm,
                      init.type = "Spectral") 






toLDAvis(
 大罷免_stm.out_04W,                              ##I Choose K =4
  docs,
  R = 30, #前30個詞彙
  plot.opts = list(xlab = "PC1", ylab = "PC2"),
  lambda.step = 0.1,
  out.dir = tempfile(),
  open.browser = interactive(),
  as.gist = FALSE,
  reorder.topics = TRUE
)

saveRDS(大罷免_stm.out_04W, file = "大罷免_stm.out_04W.rds")

toLDAvis(
 大罷免_stm.out_05W,
  docs,
  R = 30, #前30個詞彙
  plot.opts = list(xlab = "PC1", ylab = "PC2"),
  lambda.step = 0.1,
  out.dir = tempfile(),
  open.browser = interactive(),
  as.gist = FALSE,
  reorder.topics = TRUE
)

toLDAvis(
 大罷免_stm.out_06W,
  docs,
  R = 30, #前30個詞彙
  plot.opts = list(xlab = "PC1", ylab = "PC2"),
  lambda.step = 0.1,
  out.dir = tempfile(),
  open.browser = interactive(),
  as.gist = FALSE,
  reorder.topics = TRUE
)

toLDAvis(
 大罷免_stm.out_07W,
  docs,
  R = 30, #前30個詞彙
  plot.opts = list(xlab = "PC1", ylab = "PC2"),
  lambda.step = 0.1,
  out.dir = tempfile(),
  open.browser = interactive(),
  as.gist = FALSE,
  reorder.topics = TRUE
)


toLDAvis(
 大罷免_stm.out_08W,
  docs,
  R = 30, #前30個詞彙
  plot.opts = list(xlab = "PC1", ylab = "PC2"),
  lambda.step = 0.1,
  out.dir = tempfile(),
  open.browser = interactive(),
  as.gist = FALSE,
  reorder.topics = TRUE
)

toLDAvis(
 大罷免_stm.out_09W,
  docs,
  R = 30, #前30個詞彙
  plot.opts = list(xlab = "PC1", ylab = "PC2"),
  lambda.step = 0.1,
  out.dir = tempfile(),
  open.browser = interactive(),
  as.gist = FALSE,
  reorder.topics = TRUE
)






#Step03 Show keyterms in each topic

plot.model(大罷免_stm.out_04W, frex)

labelTopics(大罷免_stm.out_04W, topics = c(1), n = 20, frexweight = 0.8)

# Topic 1 Top Words:
#   Highest Prob: 國民黨, 罷免, 大罷免, 民進黨, 藍營, 投票, 朱立倫, 罷團, 立委, 罷免案, 藍委, 連署, 綠營, 中選會, 盧秀燕, 國民黨立委, 選舉, 選區, 民眾, 罷免投票 
#   FREX: 連署書, 羈押, 送件, 搜索, 以罷制罷, 羅智強, 罷綠, 李進勇, 地方黨部, 公告, 起訴, 偽造文書, 死亡連署, 二階, 國民黨中央, 新竹市, 檢察官, 告急, 陳玉鈴, 第一階段 
#   Lift: 24件, 24小時, 31件, 359, 3天, 577, 一宗, 一概而論, 一樓, 一節, 七月底, 三席, 三方, 三月圍城, 三萬, 不合格, 不強, 不想辦, 不符規定, 不耐 
#   Score: 送件, 選區, 黨主席, 陳玉鈴, 以罷制罷, 地方黨部, 罷免案, 連署, 羈押, 告急, 檢調機構, 新竹市, 羅智強, 連署書, 李進勇, 柯文哲, 魯明哲, 地動刪瑤, 參選, 吳沛憶 
# labelTopics(大罷免_stm.out_04W, topics = c(2), n = 20, frexweight = 0.8)

labelTopics(大罷免_stm.out_04W, topics = c(2), n = 20, frexweight = 0.8)

#Topic 2 Top Words:
#   Highest Prob: 台灣, 大罷免, 中共, 美國, 中國, 立法院, 藍白, 預算, 政府, 行政院, 國民黨, 川普, 國家, 賴政府, 民進黨, 台灣人, 關稅, 立委, 日本, 卓榮泰 
#   FREX: 普發現金, 普發一萬元, 談判, 一萬元, 習近平, 撥補, 一般性補助款, 債留子孫, 兩蔣, 台積電, 補貼, 李俊俋, 投資, 追加預算, 編列, 稅率, 刪凍, 財政, 特別預算, 買票 
#   Lift: 1300, 30多年, chatgpt, tpass, 一五〇〇億元, 一條鞭, 一萬塊, 丈夫, 不公道, 不合法, 不得為增加支出之提議, 不打, 不漲電價, 不無小補, 不然就是, 不經, 不編, 不起, 世界人權宣言, 中共官媒 
#   Score: 中共, 美國, 川普, 台電, 普發現金, 預算, 談判, 撥補, 一萬元, 普發一萬元, 天理, 追加預算, 台積電, 習近平, 舉債, 關稅, 債留子孫, 中配, 行政院, 大陸 

labelTopics(大罷免_stm.out_04W, topics = c(3), n = 20, frexweight = 0.8)
#Topic 3 Top Words:
#   Highest Prob: 賴清德, 大罷免, 民進黨, 團結十講, 台灣, 團結, 柯建銘, 總統, 綠營, 在野黨, 民眾, 台獨, 罷免, 兩岸, 蔡英文, 曹興誠, 雜質, 大陸, 罷團, 賴政府 
#   FREX: 團結十講, 災民, 刑法一百條, 打掉雜質, 國安簡報, 勘災, 漢光演習, 雜質說, 第三講, 災區, 第二講, 尹錫悅, 伺候, 府方, 李在明, 南台灣, 在野黨領袖, 台獨, 在野黨主席, 曹派 
#   Lift: 14天, 19, 1946, 232, 447, q塔, 一家五口, 一家人槍口絕不對內, 三講, 上映, 上萬, 不到一個月, 不及, 不受控, 不堪設想, 不是表決多數贏就可以了, 不會走回頭路, 不滿意度, 不甩, 不自覺 
#   Score: 團結十講, 賴清德, 台獨, 災民, 刑法一百條, 國安簡報, 打掉雜質, 勘災, 雜質, 柯建銘, 第三講, 雜質說, 大陸, 兩岸, 災區, 漢光演習, 屋頂, 台南, 第二講, 第一講 
# labelTopics(大罷免_stm.out_04W, topics = c(4), n = 20, frexweight = 0.8)

labelTopics(大罷免_stm.out_04W, topics = c(4), n = 20, frexweight = 0.8)

#Topic 4 Top Words:
#   Highest Prob: 罷免, 民主, 大罷免, 台灣, 人民, 國會, 立委, 民進黨, 政黨, 政治, 國民黨, 選舉, 立法院, 制度, 社會, 選民, 民意, 行動, 公民, 國家 
#   FREX: 民主制度, 機制, 徐重仁, 賦予, 制度, 運作, 民主社會, 民主, 罷免制度, 社會信任, 政黨, 制衡, 工具, 監督, 台灣民主, 公民, 世代, 理性, 民主國家, 權利 
#   Lift: 徐重仁, 自發, 賦予, 2029, 28, 38, 48, 73, 76, 8席, johnrawls, ktv, linz, stepan, 一一三席, 一人說了算, 一傳十十傳百, 一兩秒, 一書, 一見 
#   Score: 民主, 制度, 罷免制度, 民主制度, 徐重仁, 公民, 代議士, 監督, 罷免權, 憲政, 制度設計, 機制, 商界, 賦予, 病毒, 國家, 自由, 公民社會, 國會, 無煤中火 






# Step04 將每個text在四個主題的ETPs 附加上去/ Append each text’s ETPs across the four topics.


library(tibble)
mdt04A<-make.dt(大罷免_stm.out_04W, meta = NULL)
View(mdt04A)

mdt04A[, docnum := NULL]

mdt04Atibble<-tibble(mdt04A)

mdt04A_ave<-tibble(colMeans(mdt04Atibble))

C15段落罷3<-cbind(C15段落罷3,mdt04A)
View(mdt04A_ave) #每個主題的平均數

colnames(mdt04Atibble)<-c("T01", "T02", "T03", "T04")

C15段落罷3<- cbind(C15段落罷3, mdt04Atibble)

write.csv(C15段落罷3,"C15段落罷3.csv")