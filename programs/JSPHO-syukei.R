# JSPHO-BDR 年次集計
# Mamiko Yonejima
#  2017/8/31 created
#  2019/8/19 update

# *********************************
kOrganization  <- "JSPHO"
kDateCutoff <- "20230601"
kYear <- "2022"
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JSPHO/Registry/10.03.10 データレビュー書/2023/二次集計"
# *********************************

library(tidyverse)

# 関数の定義
YearDif <- function(starting, ending) {
  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

# csv読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
list <- list.files(rawdatapath)
registration_index <- grep(paste(kOrganization, "registration", sep="_"), list)
registration_csv <- read_csv(paste0(rawdatapath, list[registration_index]))


list <- list.files(paste0(prtpath, "/input"))
df.name <- sub(".csv.*", "", list)
for (i in 1:length(list)) {
     assign(df.name[i], read.csv(paste0(prtpath, "/input/", list[i]), as.is=T, na.strings = c(""), fileEncoding='UTF-8-BOM'))
    }

# adsの作成
# 診断年を抽出
registration_csv$year <- substr(registration_csv$診断年月日, 1, 4)
registration_csv <- registration_csv[!is.na(registration_csv$year) & registration_csv$year == kYear, ]
# shimekiri Cut
registration_csv1 <-registration_csv[format(as.Date(registration_csv$作成日), "%Y%m%d") < kDateCutoff, ]

# age diagnosisを計算
registration_csv1$age_diagnosis <- YearDif(registration_csv1$生年月日, registration_csv1$診断年月日)
# WHO2016のコードに置換する
registration_csv1$MHDECOD <- ifelse(nchar(registration_csv1$field1) != 5, round(registration_csv1$field1 * 10 + 10000, digits = 0)
                                                                       , registration_csv1$field1)
# Disease Name v2をマージ
registration_csv1$MHDECOD <- ifelse(registration_csv1$MHDECOD == 10930, 10931, registration_csv1$MHDECOD)
ads_cleaning <- merge(registration_csv1, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all.x = T)


# Cut registration_csv /age diagnosis is over　20
ads <- ads_cleaning[ads_cleaning$age_diagnosis < 20, ]

# category age diagnosis
ads$cat_age_diagnosis <- cut(ads$age_diagnosis, breaks = c(0, 1, 5, 10, 15, 20),
                                     labels= c(" 0"," 1-4"," 5-9"," 10-14"," 15-19"), right=FALSE)
# chiku code
ads$prefecture_cd <-  sub(".*-", "", ads$field173)
ads$JIScode <- floor(as.integer(ads$prefecture_cd)/1000)
ads <- merge(ads, Prefecture, by = "JIScode", all.x = T)

# BRTHDTC, MHSTDTCが逆転している症例を除く
ads <- ads[ads$生年月日 <= ads$診断年月日, ]

# 集計

# 中分類集計
# sex
code_sex <- xtabs( ~ MHDECOD +  性別, data = ads)
mat_code_sex <- matrix(code_sex, nrow(code_sex ), ncol(code_sex ))
rownames(mat_code_sex) <- rownames(code_sex)
# area
code_area <- xtabs( ~ MHDECOD +  Area_name, data = ads)
mat_code_area <- matrix(code_area , nrow(code_area), ncol(code_area))
rownames(mat_code_area) <- rownames(code_area)
# age
code_age <- xtabs( ~ MHDECOD + cat_age_diagnosis, data = ads)
mat_code_age  <- matrix(code_age , nrow(code_age ), ncol(code_age ))
rownames(mat_code_age ) <- rownames(code_age )
#マージとcolnameの整理
name <- c(colnames(code_sex), colnames(code_area), colnames(code_age))
merge_sex_area <- merge(mat_code_sex, mat_code_area, by = 0, all = T)

rownames(merge_sex_area) <- merge_sex_area[, 1]
# merge_sex_area <- merge_sex_area[, 2:10]
# middle <- merge(merge_sex_area, mat_middle.class.age, by = 0, all = T)
middle <- cbind(merge_sex_area, mat_code_age)
colnames(middle) <- c("disease", name)



# csvの書き出し
setwd(paste0(prtpath, "/output"))
ads_cleaning[is.na(ads_cleaning)] <- ""
write.csv(ads_cleaning, "ads_cleaning.csv", fileEncoding="CP932")
write.csv(middle, "results.csv", row.names = F, fileEncoding="CP932")

