# JSPHO-BDR seiiku
# Mamiko Yonejima
# ver.1.0 2019/8/9 created
# *********************************
kDateCutoff <- "20190601"
kYear <- "2018"
kJsphoCsv <- "JSPHO_190806_0937.csv"
kJsphoRegistrationCsv <- "JSPHO_registration_190806_0937.csv"
path <- "//192.168.200.222/Datacenter/学会事務/110_日本小児血液がん学会関連/04.03.02 データ集計/小児がん全数把握フォーマット/2019"
# *********************************
# 関数の定義
YearDif <- function(starting, ending) {
  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

# csv読み込み
rawdatapath <- paste0(path , "/rawdata/")
jspho_csv <- read.csv(paste0(rawdatapath, kJsphoCsv), as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F)
colnames(jspho_csv) <- jspho_csv[1, ]
jspho_csv<- jspho_csv[-1, ]
jspho_registration_csv <- read.csv(paste0(rawdatapath, kJsphoRegistrationCsv), as.is=T, fileEncoding="CP932", stringsAsFactors=F, header=F)
colnames(jspho_registration_csv) <- jspho_registration_csv[1, ]
jspho_registration_csv <- jspho_registration_csv[-1, ]

m_jspho_registration_csv <- merge(jspho_registration_csv, jspho_csv, by = "登録コード", all.x = T )

# adsの作成
# 診断年を抽出
m_jspho_registration_csv$year <- substr(m_jspho_registration_csv$診断年月日, 1, 4)
dxt_jspho_registration_csv  <- m_jspho_registration_csv[!is.na(m_jspho_registration_csv$year) & m_jspho_registration_csv$year == kYear, ]
# shimekiri Cut
jspho_registration_csv1 <-dxt_jspho_registration_csv [format(dxt_jspho_registration_csv$作成日), "%Y%m%d" < kDateCutoff, ] 

# age diagnosisを計算
jspho_registration_csv1$age_diagnosis <- YearDif(jspho_registration_csv1$生年月日, jspho_registration_csv1$診断年月日)
# Cut registration_csv /age diagnosis is over　20
dxt_jspho_registration <- jspho_registration_csv1[jspho_registration_csv1$age_diagnosis < 20, ]
dxt_jspho_registration$初発時住所_県名  <- ifelse(substr(dxt_jspho_registration$初発時住所, 4, 4) == "県",
                                           substr(dxt_jspho_registration$初発時住所, 1, 4) , substr(dxt_jspho_registration$初発時住所, 1, 3))

#  koko
dxt_jspho_registration$住所詳細 <- ifelse(substr(dxt_jspho_registration$初発時住所, 4, 4) == "県", substr(dxt_jspho_registration$初発時住所, 5, 100) ,
                                      substr(dxt_jspho_registration$初発時住所, 4, 100))
#血液腫瘍性疾患の症例のみ抽出
df_tumor <- dxt_jspho_registration[dxt_jspho_registration$field176 != "", ]

ads <- df_tumor[, c("登録コード", "初発時施設名", "シート作成時団体別施設コード", "作成日", "生死", "最終確認日", "和文名前の一文字目",
                    "性別", "生年月日", "診断年月日", "初発時住所_県名", "住所詳細", "age_diagnosis", "診断名", "血液腫瘍性 疾患名", 
                    "ダウン症",　"ダウン症以外の基礎疾患",　"Fanconi貧血",　"Noonan症候群",　"神経線維腫症（NF1）",　"先天性角化不全症",　
                    "Shwachman-Diamond症候群",　"Diamond-Blackfan貧血",　"特発性再生不良性貧血",　"GATA2異常症",　"RUNX1異常症",
                    "重症先天性好中球減少症",　"その他の基礎疾患",　"発病形式",　"発病形式が二次性の場合、一次疾患名", "AML FAB分類",
                    "その他の白血病の疾患分類", "Acute mixed lineage leukemiaの細分類", "CMLの細分類", "NHL Stage(St.Jude)", "NHL 原発部位" ,
                    "HL Stage(Ann Arbor)", "HL B症状", "HL 原発部位" , "LCH 病期", "LCH 部位（複数選択可）", "先天性免疫不全に随伴するLPD 細分類" )]

