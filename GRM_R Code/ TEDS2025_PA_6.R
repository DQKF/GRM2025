#==============
# TEDS2025_P06
#==============

#說明：本Script包括論文本文中的Figure 4 與 Appendix中的 Table E , F
# Note: This script generates Figure 4 in the main text and Tables E and F in the Appendix.

#採用檔案：TEDS2025_P06.sav
#        TEDS2025P06GRM_C.csv

# Trans. TEDS2025_P06.sav into "D" in order to clean.

library(haven)
TEDS2025P06GRM_clean <- haven::zap_labels(TEDS2025P06GRM)
write.csv(
  TEDS2025P06GRM_clean,
  "TEDS2025P06GRM_C.csv",
  row.names = FALSE
)

# “TEDS2025P06GRM_C.csv is the processed file and serves as an additional Dataset D.”





# Step 01 Data claeaning and merging

# Plotical Attitudes on recall: Rights or Vengeance?

D25<-D[c("Q25")]
D25$GRM <- ifelse(D25$Q25 == 1, "Rights",
                ifelse(D$Q25 == 2, "Vengeance", NA))
x <character> 
# total N=1227 valid N=1141 mean=1.46 sd=0.50

Value     |   N | Raw % | Valid % | Cum. %
------------------------------------------
Rights    | 616 | 50.20 |   53.99 |  53.99
Vengeance | 525 | 42.79 |   46.01 | 100.00
<NA>      |  86 |  7.01 |    <NA> |   <NA>

D25$GRM <- relevel(factor(D25$GRM),
                             ref = "Rights")


# 黨派/ Party ID
D$28,29, 30
D28$<-D[c("Q28","Q29","Q30","PARTY","PARTYID")]

政黨四分類
Pan-Green、Pan-Blue、TPP、Neutral

D28 <- D28 %>%
  mutate(partisanship4 = case_when(
    PARTYID %in% c(1, 3, 4) ~ "Pan-Blue",   # 國民黨、親民黨、新黨等
    PARTYID %in% c(2, 5, 6) ~ "Pan-Green",  # 民進黨、台灣團結聯盟等
    PARTYID == 7 ~ "TPP",                   # 民眾黨
    PARTYID == 8 ~ "Neutral",  
    TRUE ~ NA_character_
  ))

# 政黨三分類 / Party ID: 3 & 4 level classifications.
D28 <- D28 %>%
  mutate(partisanship3 = case_when(
    PARTYID %in% c(1, 3,4,7) ~ "KMT-TPP",   # 國民黨、親民黨、新黨、等
    PARTYID %in% c(2, 5, 6) ~ "Pan-Green",  # 民進黨、台灣團結聯盟等
    PARTYID == 8 ~ "Neutral",  
    TRUE ~ NA_character_
  ))


# D28$Q28 == 20 and 35, transform D28$partisanship4 == "Pan-Green"
                                D28$partisanship3 == "Pan-Green"

D28 <- D28 %>%
  mutate(
    partisanship4 = case_when(
      Q28 %in% c(20, 35) ~ "Pan-Green",
      TRUE ~ partisanship4   # 保留原本的分類
    ),
    partisanship3 = case_when(
      Q28 %in% c(20, 35) ~ "Pan-Green",
      TRUE ~ partisanship3
    )
  )

frq(D28$partisanship4)
x <character> 
# total N=1227 valid N=1196 mean=2.33 sd=1.04

Value     |   N | Raw % | Valid % | Cum. %
------------------------------------------
Neutral   | 355 | 28.93 |   29.68 |  29.68
Pan-Blue  | 257 | 20.95 |   21.49 |  51.17
Pan-Green | 422 | 34.39 |   35.28 |  86.45
TPP       | 162 | 13.20 |   13.55 | 100.00
<NA>      |  31 |  2.53 |    <NA> |   <NA>

frq(D28$partisanship3)
x <character> 
# total N=1227 valid N=1196 mean=2.00 sd=0.84

Value     |   N | Raw % | Valid % | Cum. %
------------------------------------------
KMT-TPP   | 419 | 34.15 |   35.03 |  35.03
Neutral   | 355 | 28.93 |   29.68 |  64.72
Pan-Green | 422 | 34.39 |   35.28 | 100.00
<NA>      |  31 |  2.53 |    <NA> |   <NA>


D28$partisanship4, set "Pan-Green" as reference group
D28$partisanship4<-as.factor(D28$partisanship4)
D28$partisanship3<-as.factor(D28$partisanship3)
# D28$partisanship4, set "Pan-Green" as reference group
D28$partisanship4 <- relevel(factor(D28$partisanship4),
                             ref = "Pan-Green")

# D28$partisanship3, set "Pan-Green" as reference group
D28$partisanship3 <- relevel(factor(D28$partisanship3),
                             ref = "Pan-Green")



#國族身份認同/national Identity

D31<-D[c("Q31","T_Cidentity")]

frq(D31$Q31)
我們社會上，有人說自己是「臺灣人」，也有人說自己是「中國人」，也有人說都是。請問您認為自己是「臺灣人」、「中國人」，或者都是? (x) <numeric> 
# total N=1227 valid N=1227 mean=4.99 sd=18.23

Value |  Label |   N | Raw % | Valid % | Cum. %
-----------------------------------------------
    1 | 臺灣人 | 748 | 60.96 |   60.96 |  60.96
    2 |   都是 | 403 | 32.84 |   32.84 |  93.81
    3 | 中國人 |  30 |  2.44 |    2.44 |  96.25
   95 |   拒答 |   7 |  0.57 |    0.57 |  96.82
   96 | 看情形 |   4 |  0.33 |    0.33 |  97.15
   97 | 無意見 |   4 |  0.33 |    0.33 |  97.47
   98 | 不知道 |  31 |  2.53 |    2.53 | 100.00
 <NA> |   <NA> |   0 |  0.00 |    <NA> |   <NA>

frq(D31$T_Cidentity)
臺灣人/中國人認同 (x) <numeric> 
# total N=1227 valid N=1227 mean=1.68 sd=1.54

Value |  Label |   N | Raw % | Valid % | Cum. %
-----------------------------------------------
    1 | 臺灣人 | 748 | 60.96 |   60.96 |  60.96
    2 |   都是 | 403 | 32.84 |   32.84 |  93.81
    3 | 中國人 |  30 |  2.44 |    2.44 |  96.25
    9 | 無反應 |  46 |  3.75 |    3.75 | 100.00
 <NA> |   <NA> |   0 |  0.00 |    <NA> |   <NA>
D31$identity <- ifelse(D31$T_Cidentity == 1 ,"臺灣人",
                     ifelse(D31$T_Cidentity == 2 ,"雙重認同",
                     ifelse(D31$T_Cidentity == 3 ,"中國人", NA
                        )))

frq(D31$identity)
x <character> 
# total N=1227 valid N=1181 mean=2.32 sd=0.52

Value    |   N | Raw % | Valid % | Cum. %
-----------------------------------------
中國人   |  30 |  2.44 |    2.54 |   2.54
臺灣人   | 748 | 60.96 |   63.34 |  65.88
雙重認同 | 403 | 32.84 |   34.12 | 100.00
<NA>     |  46 |  3.75 |    <NA> |   <NA>



# 居住地點/residence
D39<-D[c("Q39","AREAR")]


D39$Q39_New <- case_when(
  D39$Q39 == 2  ~ "宜蘭縣",
  D39$Q39 == 4  ~ "新竹縣",
  D39$Q39 == 5  ~ "苗栗縣",
  D39$Q39 == 7  ~ "彰化縣",
  D39$Q39 == 8  ~ "南投縣",
  D39$Q39 == 9  ~ "雲林縣",
  D39$Q39 == 10 ~ "嘉義縣",
  D39$Q39 == 13 ~ "屏東縣",
  D39$Q39 == 14 ~ "臺東縣",
  D39$Q39 == 15 ~ "花蓮縣",
  D39$Q39 == 16 ~ "澎湖縣",
  D39$Q39 == 17 ~ "基隆市",
  D39$Q39 == 18 ~ "新竹市",
  D39$Q39 == 20 ~ "嘉義市",
  D39$Q39 == 28 ~ "福建省",
  D39$Q39 == 63 ~ "臺北市",
  D39$Q39 == 64 ~ "高雄市",
  D39$Q39 == 65 ~ "新北市",
  D39$Q39 == 66 ~ "臺中市",
  D39$Q39 == 67 ~ "臺南市",
  D39$Q39 == 68 ~ "桃園市",
  D39$Q39 %in% c(95, 98) ~ NA_character_,
  TRUE ~ as.character(D39$Q39)  # Keep other values or use NA_character_
)


D39$region <- case_when(
  D39$Q39_New %in% c("基隆市","宜蘭縣","新竹縣","新竹市","桃園市","臺北市","新北市","福建省") ~ "北臺灣",
  D39$Q39_New  %in% c("苗栗縣","臺中市","彰化縣","南投縣","花蓮縣") ~ "中臺灣",
  D39$Q39_New  %in% c("雲林縣","嘉義縣","嘉義市","臺南市") ~ "雲嘉南",
  D39$Q39_New  %in% c("高雄市","屏東縣","澎湖縣","臺東縣") ~ "南臺灣", 
  is.na(D39$Q39_New) ~ NA_character_,
  TRUE ~ NA_character_  # 其他未分類的也設為 NA
)

D39$region <- relevel(factor(D39$region),
                             ref = "北臺灣")


# 性別/gender
# Dgender
frq(D$SEX)
性別 (x) <numeric> 
# total N=1227 valid N=1227 mean=1.44 sd=0.50

Value | Label |   N | Raw % | Valid % | Cum. %
----------------------------------------------
    1 |  男性 | 684 | 55.75 |   55.75 |  55.75
    2 |  女性 | 543 | 44.25 |   44.25 | 100.00
 <NA> |  <NA> |   0 |  0.00 |    <NA> |   <NA>


Dgender<-D[c("SEX")]
Dgender$gender <- case_when(
  Dgender$SEX %in% c("1") ~ "Male",
  Dgender$SEX  %in% c("2") ~ "Female",
  is.na(Dgender$SEX) ~ NA_character_,
  TRUE ~ NA_character_ ) # 其他未分類的也設為 NA
Dgender$gender <- relevel(factor(Dgender$gender),
                             ref = "Male")

frq(Dgender$gender)
x <categorical> 
# total N=1227 valid N=1227 mean=1.44 sd=0.50

Value  |   N | Raw % | Valid % | Cum. %
---------------------------------------
Male   | 684 | 55.75 |   55.75 |  55.75
Female | 543 | 44.25 |   44.25 | 100.00
<NA>   |   0 |  0.00 |    <NA> |   <NA>




#年齡 D32/ age
D$Q32, D$AGE
D32<-D[c("Q32","AGE")]
D32$age <- ifelse(D32$AGE == 1 ,"20s",
                     ifelse(D32$AGE == 2 ,"30s",
                     ifelse(D32$AGE == 3 ,"40s",
                     ifelse(D32$AGE == 4 ,"50s",
                     ifelse(D32$AGE == 5 ,"60+",NA
                        )))))

frq(D32$age)
x <character> 
# total N=1227 valid N=1217 mean=3.52 sd=1.37

Value |   N | Raw % | Valid % | Cum. %
--------------------------------------
20s   | 126 | 10.27 |   10.35 |  10.35
30s   | 187 | 15.24 |   15.37 |  25.72
40s   | 252 | 20.54 |   20.71 |  46.43
50s   | 233 | 18.99 |   19.15 |  65.57
60+   | 419 | 34.15 |   34.43 | 100.00
<NA>  |  10 |  0.81 |    <NA> |   <NA>

D32$age <- relevel(factor(D32$age),
                             ref = "60+")

# 教育程度 D33/ edu. level
D$EDU
D33<-D[c("Q33","EDU")]
View(D33)

D33$Q33  recode, 1, 2 == Elemantary & Below
                 3, 4 == Secondary
                 5, 6 == College
                 7 = Graduated,
                 else == NA  as new column D33$edu



D33 <- D33 %>% 
  mutate(edu = case_when(
    Q33 %in% c(1, 2) ~ "Elementary & Below",   # 未入學 + 小學
    Q33 %in% c(3, 4) ~ "Secondary",            # 國中 + 高中
    Q33 %in% c(5, 6) ~ "College",              # 專科 + 大學
    Q33 == 7 ~ "Graduate",                     # 研究所（含碩博）
    TRUE ~ NA_character_
))

frq(D33$edu)
frq(D33$edu)
x <character> 
# total N=1227 valid N=1220 mean=2.27 sd=1.34

Value              |   N | Raw % | Valid % | Cum. %
---------------------------------------------------
College            | 589 | 48.00 |   48.28 |  48.28
Elementary & Below |  92 |  7.50 |    7.54 |  55.82
Graduate           | 158 | 12.88 |   12.95 |  68.77
Secondary          | 381 | 31.05 |   31.23 | 100.00
<NA>               |   7 |  0.57 |    <NA> |   <NA>

D33$edu <- relevel(factor(D33$edu),
                             ref = "College")
TEDS2025P06GRM <- cbind(D25,D28,D31,D39,Dgender,D32,D33)


D25$GRM <- relevel(factor(D25$GRM),
                             ref = "Rights")
TEDS2025P06GRM$GRM<- relevel(factor(TEDS2025P06GRM$GRM),
                             ref = "Rights")
TEDS2025P06GRM$identity<- relevel(factor(TEDS2025P06GRM$identity),
                             ref = "臺灣人")


TEDS2025P06GRM$GRM_binary <- TEDS2025P06GRM$GRM

TEDS2025P06GRM$GRM_binary <- gsub("Vengeance","1",TEDS2025P06GRM$GRM_binary)
TEDS2025P06GRM$GRM_binary <- gsub("Rights","0",TEDS2025P06GRM$GRM_binary)
TEDS2025P06GRM$GRM_binary <- as.numeric(TEDS2025P06GRM$GRM_binary)

levels(TEDS2025P06GRM$region)
[1] "北臺灣" "中臺灣" "南臺灣" "雲嘉南"

TEDS2025P06GRM$region <- gsub("北臺灣","N_Taiwan",TEDS2025P06GRM$region)
TEDS2025P06GRM$region <- gsub("中臺灣","M_Taiwan",TEDS2025P06GRM$region)
TEDS2025P06GRM$region <- gsub("雲嘉南","Yun_Chia_Nan",TEDS2025P06GRM$region)
TEDS2025P06GRM$region <- gsub("南臺灣","S_Taiwan",TEDS2025P06GRM$region)
TEDS2025P06GRM$region <- relevel(factor(TEDS2025P06GRM$region),
                             ref = "N_Taiwan")


levels(TEDS2025P06GRM$identity)
[1] "臺灣人"   "中國人"   "雙重認同"

TEDS2025P06GRM$identity <- gsub("臺灣人","Taiwanese",TEDS2025P06GRM$identity)
TEDS2025P06GRM$identity <- gsub("雙重認同","Dual identity",TEDS2025P06GRM$identity)
TEDS2025P06GRM$identity <- gsub("中國人","Chinese",TEDS2025P06GRM$identity)
TEDS2025P06GRM$identity <- relevel(factor(TEDS2025P06GRM$identity),
                             ref = "Taiwanese")

# Check collinearility
library(car)

vif(m1_grm) # see below

                  GVIF Df GVIF^(1/(2*Df))
partisanship4 1.484047  3        1.068008
identity      1.238641  2        1.054961
gender        1.064950  1        1.031964
edu           1.366798  3        1.053458
region        1.066455  3        1.010781
age           1.564009  4        1.057499

# Multicollinearity diagnostics.
# Multicollinearity among the independent variables was assessed using generalized
# variance inflation factors (GVIFs). Following Fox and Monette (1992), 
# the adjusted GVIF^(1/(2·Df)) values were examined for multi-degree-of-freedom 
# categorical predictors. All adjusted GVIF values were close to 1 and well below
# conventional thresholds, indicating that multicollinearity is not a concern in
# the estimated models.



# Binary logit Model

m1_grm <- glm(GRM_binary ~ partisanship4 + 
                   identity + 
                   gender + 
                   edu + 
                   region + 
                   age, 
              data = TEDS2025P06GRM,
              family = binomial())  # This specifies logistic regression

# View results (Figure F in Appendix)
summary(m1_grm)
Call:
glm(formula = GRM_binary ~ partisanship4 + identity + gender + 
    edu + region + age, family = binomial(), data = TEDS2025P06GRM)

Coefficients:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)           -1.47975    0.23237  -6.368 1.91e-10 ***
partisanship4Neutral   1.60734    0.19616   8.194 2.53e-16 ***
partisanship4Pan-Blue  2.42550    0.22996  10.547  < 2e-16 ***
partisanship4TPP       2.28675    0.24691   9.262  < 2e-16 ***
identityChinese        0.98079    0.53620   1.829 0.067378 .  
identityDual identity  0.75391    0.16653   4.527 5.97e-06 ***
genderFemale           0.27367    0.14996   1.825 0.068017 .  
eduElementary & Below  0.15196    0.35100   0.433 0.665065    
eduGraduate           -0.34833    0.22869  -1.523 0.127719    
eduSecondary          -0.10682    0.17377  -0.615 0.538734    
regionM_Taiwan         0.02591    0.18809   0.138 0.890441    
regionS_Taiwan        -0.16752    0.21542  -0.778 0.436774    
regionYun_Chia_Nan     0.22457    0.20755   1.082 0.279250    
age20s                -1.00670    0.27530  -3.657 0.000255 ***
age30s                -0.69704    0.25127  -2.774 0.005536 ** 
age40s                -0.65382    0.22130  -2.954 0.003132 ** 
age50s                -0.22714    0.22048  -1.030 0.302915    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1489.0  on 1079  degrees of freedom
Residual deviance: 1158.6  on 1063  degrees of freedom
  (147 observations deleted due to missingness)
AIC: 1192.6

Number of Fisher Scoring iterations: 4


m1_tidy1<-m1_tidy
m1_tidy1$term <- as.character(m1_tidy1$term)

# Now you can assign the value
m1_tidy1[1,"term"]<-"Party ID.: Neutral"
m1_tidy1[2,"term"]<-"Party ID.: Pan-Blue"
m1_tidy1[3,"term"]<-"Party ID.: TPP"
m1_tidy1[4,"term"]<-"Natl. ID.: Chinese"
m1_tidy1[5,"term"]<-"Natl. ID.: Dual Identity"
m1_tidy1[6,"term"]<-"Gender: Female"
m1_tidy1[7,"term"]<-"Edu. Level: Elementary & Below"
m1_tidy1[8,"term"]<-"Edu. Level: Secondary"
m1_tidy1[9,"term"]<-"Edu. Level: Graduate"
m1_tidy1[10,"term"]<-"Region: Middle Taiwan"
m1_tidy1[11,"term"]<-"Region: Southern Taiwan"
m1_tidy1[12,"term"]<-"Region: Sub-Southern Taiwan"
m1_tidy1[13,"term"]<-"Age Group: 20s"
m1_tidy1[14,"term"]<-"Age Group: 30s"
m1_tidy1[15,"term"]<-"Age Group: 40s"
m1_tidy1[16,"term"]<-"Age Group: 50s"

m1_tidy1 $ variable_group <- c("Party ID.","Party ID.","Party ID.",
                            "Natl. ID.","Natl. ID.",
                            "Gender",
                            "Edu. Level","Edu. Level","Edu. Level",
                            "Region","Region","Region",
                            "Age Group","Age Group","Age Group","Age Group")




m1_tidy1
# A tibble: 16 × 8
   term      estimate std.error statistic  p.value conf.low conf.high variable_group
   <chr>        <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <chr>         
 1 Party ID…    4.99      0.196     8.19  2.53e-16    3.41      7.37  Party ID.     
 2 Party ID…   11.3       0.230    10.5   5.22e-26    7.25     17.9   Party ID.     
 3 Party ID…    9.84      0.247     9.26  2.01e-20    6.11     16.1   Party ID.     
 4 Natl. ID…    2.67      0.536     1.83  6.74e- 2    0.988     8.31  Natl. ID      
 5 Natl. ID…    2.13      0.167     4.53  5.97e- 6    1.53      2.95  Natl. ID      
 6 Gender: …    1.31      0.150     1.82  6.80e- 2    0.980     1.77  Gender        
 7 Edu. Lev…    1.16      0.351     0.433 6.65e- 1    0.582     2.31  Edu. Level    
 8 Edu. Lev…    0.706     0.229    -1.52  1.28e- 1    0.449     1.10  Edu. Level    
 9 Edu. Lev…    0.899     0.174    -0.615 5.39e- 1    0.639     1.26  Edu. Level    
10 Region: …    1.03      0.188     0.138 8.90e- 1    0.710     1.48  Region        
11 Region: …    0.846     0.215    -0.778 4.37e- 1    0.553     1.29  Region        
12 Region: …    1.25      0.208     1.08  2.79e- 1    0.834     1.88  Region        
13 Age Grou…    0.365     0.275    -3.66  2.55e- 4    0.212     0.624 Age Group     
14 Age Grou…    0.498     0.251    -2.77  5.54e- 3    0.303     0.813 Age Group     
15 Age Grou…    0.520     0.221    -2.95  3.13e- 3    0.336     0.801 Age Group     
16 Age Grou…    0.797     0.220    -1.03  3.03e- 1    0.516     1.23  Age Group  

#繪圖/plot
library(broom)
library(ggplot2)
library(dplyr)

# 將邏輯斯回歸模型轉成 tidy 格式
m1_tidy <- tidy(m1_grm, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%   # 不畫截距
  mutate(term = factor(term, levels = rev(term)))  # 反轉順序便於呈現

# OR Forest Plot
library(ggplot2)
library(dplyr)

# 準備數據：添加變數分組和顯著性標記
m1_tidy1 <- m1_tidy1 %>%
  mutate(
    # 添加顯著性標記
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ))



# 將 term 轉換為因子，保持原始順序
m1_tidy1$term <- factor(m1_tidy1$term, levels = rev(m1_tidy1$term))

# 繪製森林圖 / forest plot
GRM_TEDS2025_P06 <- ggplot(m1_tidy1, aes(x = term, y = estimate, color = variable_group)) +
  
  # 繪製信賴區間（error bars）
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth = 1.0) + 
  # 繪製點
  geom_point(size = 4.5) + 
  
  # *** 調整數值標籤位置到點的右上方 ***
  geom_text(aes(label = paste0(sprintf("%.2f", estimate), sig)), 
            # hjust = -0.2: 水平向右上方推（即增加 x 軸值）
            # vjust = 0: 垂直微幅向上推（將標籤基線對齊點的頂部）
            hjust = -0.55, 
            vjust = -0.25,   
            size = 5.5,   
            show.legend = FALSE) +
  
  # 繪製虛線（Odds Ratio = 1）
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  
  # 設定 X 軸的刻度限制和間隔 (由於 coord_flip，y 軸現在是 x 軸)
  scale_y_continuous(
    limits = c(0, 20),      # 設定範圍從 0 到 20
    breaks = seq(0, 20, 5)  # 設定主要刻度為 0, 5, 10, 15, 20
  ) +
  
  # 翻轉座標軸
  coord_flip() +
  
  # 顏色手動調整
  scale_color_manual(
    values = c(
      "Party ID." = "#E41A1C",
      "Natl. ID." = "#377EB8",
      "Gender" = "#4DAF4A",
      "Edu. Level" = "#984EA3",
      "Region" = "#FF7F00",
      "Age Group" = "#A65628"
    ),
    name = NULL 
  ) +
  
  # 取消 title 與 legend
  labs(
    title = NULL,
    x = NULL,
    y = "Odds Ratio (exp β)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    # 調整軸標籤字體大小
    axis.text.y = element_text(size = 16, face = "bold"), 
    axis.text.x = element_text(size = 16),               
    
    # 調整軸標題字體大小
    axis.title.x = element_text(size = 16, face = "bold"),
    
    # 取消 plot title
    plot.title = element_blank(),
    
    # 取消 legend
    legend.position = "none",
    
    # 其他細節
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

GRM_TEDS2025_P06 # Figure 4 in the main text.


# Table E in Appendix (recall attitude)

table(TEDS2025P06GRM$partisanship4,TEDS2025P06GRM$GRM)

            Rights Vengeance
  Pan-Green    340        67
  Neutral      149       166
  Pan-Blue      58       188
  TPP           58        97