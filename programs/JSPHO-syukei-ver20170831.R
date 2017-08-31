# JSPHO年次集計 ver20170831
# Mamiko Yonejima
# 20170712

kDateCutoff <- "20170531"
kYear1 <- "2014"
kYear2 <- "2015"
kYear3 <- "2016"

# 関数の定義
YearDif <- function(starting, ending) {
  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

# csv読み込み
setwd("./rawdata") 
list <- list.files()
dl_data <- read.csv(list, as.is = T, na.strings = c(""))
setwd("../input")
list <- list.files()
df.name <- sub(".csv.*", "", list)  
for (i in 1:length(list)) {
     assign(df.name[i], read.csv(list[i], as.is=T, na.strings = c(""), fileEncoding='UTF-8-BOM'))
    }

# adsの作成
dl_data$year <- substr(dl_data$診断年月日, 1, 4)
dl_data <- dl_data[!is.na(dl_data$year) & dl_data$year >= kYear1 &  dl_data$year <= kYear3, ]

# Cut dl_data /age diagnosis is over　20
dl_data$age_diagnosis <- YearDif(dl_data$生年月日, dl_data$診断年月日)
dl_data_20 <- dl_data[dl_data$age_diagnosis < 20, ]
# 参加施設のみ抽出
dl_data_shisetsu <- merge(dl_data_20, facilities2017, by.x = "field161", by.y = "Ptosh施設CD", all.x = T)
dl_data_shisetsu  <- dl_data_shisetsu[!is.na(dl_data_shisetsu$施設CD), ]                    
# shimekiri Cut
ads <-dl_data_shisetsu [format(as.Date(dl_data_shisetsu$作成日), "%Y%m%d") <= kDateCutoff , ]
# category age diagnosis
ads$cat_age_diagnosis <- cut(ads$age_diagnosis, breaks = c(0, 1, 5, 10, 15, 20),
                                     labels= c(" 0"," 1-4"," 5-9"," 10-14"," 15-19"), right=FALSE)
# chiku code
ads$prefecture_cd <-  sub(".*-", "", ads$初発時住所)
ads$JIScode <- floor(as.integer(ads$prefecture_cd)/1000)
ads <- merge(ads, Prefecture, by = "JIScode", all.x = T)

# 中分類病名変数を作成
ads$middle.class <- ifelse(ads$field7 == 2, ads$非腫瘍性血液疾患名,
                    ifelse(ads$field7 == 1 & ads$field37 == 4 & ads$field159 == 1, "CML",
                    ifelse(ads$field7 == 1 & ads$field37 == 4 & ads$field159 == 2, "MDS・MPD(CMLを除く)", ads$血液腫瘍性疾患名)))
# 小分類病名変数を作成
ads$small.class <- ifelse(ads$field7 == 1 & ads$field37 == 1, paste0("ALL.", ads$ALL_免疫学的分類),
                   ifelse(ads$field7 == 1 & ads$field37 == 2, paste0("AML.", ads$AML_FAB分類), 
                   ifelse(ads$field7 == 1 & ads$field37 == 3, paste0("まれな白血病.", ads$疾患分類),
                   ifelse(ads$field7 == 1 & ads$field37 == 4 & ads$field159 == 2 & ads$field164 ==3, paste0("MDS.", ads$MDS_WHO分類),
                   ifelse(ads$middle.class == "CML", "CML",
                   ifelse(ads$field7 == 1 & ads$field37 == 4 & ads$field159 == 2 & ads$field164 == 1, paste0("MPD.", ads$CMLを除くMPD_疾患名),
                   ifelse(ads$field7 == 1 & ads$field37 == 4 & ads$field159 == 2 & ads$field164 == 2, paste0("MDS.MPD",ads$MDS.MPD_疾患名),
                   ifelse(ads$field7 == 1 & ads$field37 == 5, paste0("NHL.",ads$NHL_病理診断),
                   ifelse(ads$field7 == 1 & ads$field37 == 6, paste0("HL.",ads$HL_病理診断),
                   ifelse(ads$field7 == 1 & ads$field37 == 8 & ads$field69 == 2, paste0("HLH.",ads$X2.HLH_原発性.続発性),
                   ifelse(ads$field7 == 1 & ads$field37 == 8 & ads$field69 == 1, paste0("LCH.",ads$X1..1.LCH_病期),
                   ifelse(ads$field7 == 2 & ads$field84 == 1, paste0("再生不良性貧血.", ads$再生不良性貧血),
                   ifelse(ads$field7 == 2 & ads$field84 == 8, paste0("赤芽球癆.", ads$赤芽球癆),
                   ifelse(ads$field7 == 2 & ads$field84 == 9, paste0("溶血性貧血.", ads$溶血性貧血),
                   ifelse(ads$field7 == 2 & ads$field84 == 3, paste0("その他の貧血.", ads$その他の貧血),
                   ifelse(ads$field7 == 2 & ads$field84 == 10, paste0("血小板機能異常症.", ads$血小板機能異常症),
                   ifelse(ads$field7 == 2 & ads$field84 == 11, paste0("血小板減少症.", ads$血小板減少症),
                   ifelse(ads$field7 == 2 & ads$field84 == 5, paste0("凝固異常.", ads$凝固異常),
                   ifelse(ads$field7 == 2 & ads$field84 == 12, paste0("血栓傾向.", ads$血栓傾向),
                   ifelse(ads$field7 == 2 & ads$field84 == 13, paste0("好中球減少症.", ads$好中球減少症),
                   ifelse(ads$field7 == 2 & ads$field84 == 7, paste0("白血球機能異常.", ads$白血球機能異常),
                   ifelse(ads$field7 == 2 & ads$field84 == 14, paste0("免疫不全症.", ads$免疫不全症),
                   ifelse(ads$field7 == 2 & ads$field84 == 16, paste0("組織球性疾患.", ads$組織球性疾患), NA)))))))))))))))))))))))
# BRTHDTC, MHSTDTCが逆転している症例を除く
ads <- ads[ads$生年月日 <= ads$診断年月日, ]
# 集計
years <- c(kYear1, kYear2, kYear3)
# facilities
for(i in 1:2){
dataframe <- ads[ads$field7 == i, ] 
dataframe$cd_facilities <- paste0(dataframe$field161, "_", dataframe$初発時施設名)
facilities <- xtabs( ~ cd_facilities +  year, data = dataframe)
mat_facilities <- matrix(facilities, nrow(facilities), ncol(facilities))
rownames(mat_facilities) <- rownames(facilities)
colnames(mat_facilities) <- colnames(facilities)
df_facilities <- as.data.frame(mat_facilities)
df_facilities$shisetsu_code <-  sub("_.*", "",  rownames(df_facilities))
df_facilities$name_dep <- sub(".*_", "", rownames(df_facilities))
df_facilities$facilities_name <- sub("-.*", "", df_facilities$name_dep)
df_facilities$department <- sub(".*-", "",df_facilities$name_dep)
by.facilities <- df_facilities[order(as.integer(df_facilities$shisetsu_code)),] 

assign(paste0("df", i), by.facilities )
}
res_by.facilities <- merge(df1, df2, by = c("shisetsu_code", "facilities_name", "department"), all = T) 
colnames(res_by.facilities)[c(4:6)] <- paste("tumor",years)
colnames(res_by.facilities)[c(8:10)] <- paste("non tumor",years)
res_by.facilities_0 <- res_by.facilities[c(1:6, 8:10)]
res_by.facilities_0[is.na(res_by.facilities_0)] <- 0

# disease
for(i in 1:2){
  for(j in 1:length(years)){
dataframe <- ads[ads$field7 == i & ads$year == years[j], ]  # 集計年
#dataframe <- ads[ads$field7 == 1 & ads$year == "2014", ] 
# 中分類
# sex 
middle.class.sex <- xtabs( ~ middle.class +  性別, data = dataframe) 
mat_middle.class.sex <- matrix(middle.class.sex, nrow(middle.class.sex), ncol(middle.class.sex))
rownames(mat_middle.class.sex) <- rownames(middle.class.sex)
# area
middle.class.area <- xtabs( ~ middle.class +  Area_name, data = dataframe) 
mat_middle.class.area <- matrix(middle.class.area , nrow(middle.class.area ), ncol(middle.class.area ))
rownames(mat_middle.class.area) <- rownames(middle.class.area )
# age
middle.class.age <- xtabs( ~ middle.class + cat_age_diagnosis, data = dataframe) 
mat_middle.class.age <- matrix(middle.class.age, nrow(middle.class.age), ncol(middle.class.age))
rownames(mat_middle.class.age) <- rownames(middle.class.age)
#マージとcolnameの整理
name <- c(colnames(middle.class.sex), colnames(middle.class.area), colnames(middle.class.age))
merge_sex_area <- merge(mat_middle.class.sex, mat_middle.class.area, by = 0, all = T)
rownames(merge_sex_area) <- merge_sex_area[, 1]
# merge_sex_area <- merge_sex_area[, 2:10]
# middle <- merge(merge_sex_area, mat_middle.class.age, by = 0, all = T)
middle <- cbind(merge_sex_area, mat_middle.class.age)
colnames(middle) <- c("disease", name)

# 小分類
# sex 
small.class.sex <- xtabs( ~ small.class + 性別, data = dataframe)       
mat_small.class.sex <- matrix(small.class.sex, nrow(small.class.sex), ncol(small.class.sex))
rownames(mat_small.class.sex) <- rownames(small.class.sex)
# area
small.class.area <- xtabs( ~ small.class +  Area_name, data = dataframe) 
mat_small.class.area <- matrix(small.class.area , nrow(small.class.area), ncol(small.class.area))
rownames(mat_small.class.area) <- rownames(small.class.area )
# age
small.class.age <- xtabs( ~ small.class + cat_age_diagnosis, data = dataframe) 
mat_small.class.age <- matrix(small.class.age, nrow(small.class.age), ncol(small.class.age))
rownames(mat_small.class.age) <- rownames(small.class.age)
#マージとcolnameの整理####
name <- c(colnames(small.class.sex), colnames(small.class.area), colnames(small.class.age))
merge_sex_area_small <- merge(mat_small.class.sex, mat_small.class.area, by = 0, all = T)
rownames(merge_sex_area_small) <- merge_sex_area_small[, 1]
# merge_sex_area_small <- merge_sex_area_small[, 2:8]
# small <- merge(merge_sex_area_small, mat_small.class.age, by = 0, all = T)
small <-cbind(merge_sex_area_small, mat_small.class.age)
colnames(small) <- c("disease", name)
results <- rbind(middle, small)
results[is.na(results)] <- 0
ifelse(i == 1, assign(paste0("tumor_results",  years[j]), results), assign(paste0("non_tumor_results", years[j]), results))

if(i == 2) next  # 非腫瘍性の場合 残りのループをスキップ
syouai_kiso <- xtabs( ~ 血液腫瘍性疾患名 + 基礎疾患, data = dataframe)   
mat_syouai_kiso <- matrix(syouai_kiso, nrow(syouai_kiso), ncol(syouai_kiso))
rownames(mat_syouai_kiso) <- rownames(syouai_kiso)
colnames(mat_syouai_kiso) <- colnames(syouai_kiso)
syouai_keishiki <- xtabs( ~ 血液腫瘍性疾患名 + 発病形式, data = dataframe)
mat_syouai_keishiki <- matrix(syouai_keishiki, nrow(syouai_keishiki), ncol(syouai_keishiki))
rownames(mat_syouai_keishiki) <- rownames(syouai_keishiki)
colnames(mat_syouai_keishiki) <- colnames(syouai_keishiki)
wrk_1_syousai <- merge(mat_syouai_kiso, mat_syouai_keishiki, by = 0, all = T)
# mat_wrk_1_syousai <- matrix(wrk_1_syousai, nrow(wrk_1_syousai), ncol(wrk_1_syousai))
# rownames(mat_wrk_1_syousai) <- rownames(wrk_1_syousai)
# colnames(mat_wrk_1_syousai) <- colnames(wrk_1_syousai)
niji <- dataframe[dataframe$field12 == 2, ]  # 二次性詳細
syousai_niji <- xtabs( ~ 血液腫瘍性疾患名 + 発病形式_一次疾患名, data = niji)
mat_syousai_niji  <- matrix(syousai_niji , nrow(syousai_niji ), ncol(syousai_niji ))
rownames(mat_syousai_niji) <- rownames(syousai_niji )
colnames(mat_syousai_niji) <- colnames(syousai_niji )
results_syousai <- merge(wrk_1_syousai, mat_syousai_niji , by.x = "Row.names", by.y = 0, all = T)
results_syousai[is.na(results_syousai)] <- 0
assign(paste0("syousai_results",  years[j]), results_syousai)
  }
}
# csvの書き出し
setwd("../output")
write.csv(res_by.facilities_0, "facilities_results.csv")
for(i in 1:length(years)){
  write.csv(eval(parse(text = paste0("tumor_results", years[i]))), eval(parse(text = paste0("'tumor_", years[i], ".csv'"))))
}
for(i in 1:length(years)){
  write.csv(eval(parse(text = paste0("non_tumor_results", years[i]))), eval(parse(text = paste0("'non_tumor_", years[i], ".csv'"))))
}
for(i in 1:length(years)){
  write.csv(eval(parse(text = paste0("syousai_results", years[i]))), eval(parse(text = paste0("'syousai_", years[i], ".csv'"))))
}