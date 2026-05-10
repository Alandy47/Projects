library(rvest)
library(dplyr)
library(stringr)
library(readxl)
library(jsonlite)
library(stringdist)
library(writexl)

safe_read <- function(url) {
  tryCatch({
    pg <- read_html(
      url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
    )
    Sys.sleep(0.5)
    pg
  }, error = function(e) NULL)
}

# ============================
# 1. Load anime list
# ============================
anime_list <- read_excel("C:/Users/aland/OneDrive/Desktop/HGEN612/Projects/Assignment_3/anime_with_circulation.xlsx")

# ============================
# 2. Strong title cleaning
# ============================
clean_title <- function(x) {
  x <- trimws(x)
  x <- gsub('[\"“”‘’＂]', '', x)
  x <- gsub("[\u200B-\u200F\u202A-\u202E\u2060-\u206F]", "", x)
  x <- gsub("\u00A0", " ", x)
  x <- gsub("\ufeff", "", x)
  x <- gsub("[[:cntrl:]]", "", x)
  x <- iconv(x, to = "UTF-8", sub = "")
  x
}

anime_list$Title_Clean <- sapply(anime_list$Title, clean_title)

# ============================
# 3. ENGLISH WIKIPEDIA EXTRACTOR
# ============================
extract_circulation_year_pairs <- function(text) {
  pattern <- "(\\d{4}).{0,40}?(\\d+[\\d,\\.]*)(?=\\s*million|\\s*copies|\\s*in circulation)"
  matches <- str_match_all(text, regex(pattern, ignore_case = TRUE))[[1]]
  if (nrow(matches) == 0) return(NULL)
  
  df <- data.frame(
    Year = as.numeric(matches[,2]),
    RawNum = matches[,3],
    stringsAsFactors = FALSE
  )
  
  df$RawNum <- str_replace_all(df$RawNum, ",", "")
  df$Circulation <- as.numeric(df$RawNum)
  
  if (grepl("million", text, ignore.case = TRUE)) {
    df$Circulation <- df$Circulation * 1e6
  }
  
  df
}

# ============================
# 4. ENGLISH WIKIPEDIA SCRAPER
# ============================
get_pre_anime_circulation <- function(title, anime_year) {
  url <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", title))
  
  page <- tryCatch(read_html(url), error = function(e) return(NA))
  if (is.na(page)) return(NA)
  
  text <- page %>% html_text()
  df <- extract_circulation_year_pairs(text)
  if (is.null(df)) return(NA)
  
  df <- df[df$Year < anime_year, ]
  if (nrow(df) == 0) return(NA)
  
  max(df$Circulation, na.rm = TRUE)
}

# ============================
# 5. ENGLISH WIKI LOOP
# ============================
preanime_circ <- mapply(function(title, year) {
  cat("EN Wiki:", title, "| Anime year:", year, "\n")
  Sys.sleep(1)
  get_pre_anime_circulation(title, year)
}, anime_list$Title_Clean, anime_list$SeasonYear)

anime_list$Circulation_PreAnime <- preanime_circ

# ============================================================
# ⭐ SCRAPE JP WIKI, NATALIE, MANTAN, ANN
# ============================================================
extract_circulation_number <- function(text) {
  # Normalize text
  text <- gsub(",", "", text)
  
  # Pattern A: YYYY年 ... 数字 万?部
  p_date_num <- gregexpr("([0-9]{4})年.*?([0-9]+)万?部", text, perl = TRUE)
  
  # Pattern B: 数字 万?部 (no date)
  p_num_only <- gregexpr("([0-9]+)万?部", text, perl = TRUE)
  
  # Pattern C: 数字 万?突破 (no 部)
  p_break <- gregexpr("([0-9]+)万?突破", text, perl = TRUE)
  
  # Pattern D: 累計 数字 万
  p_total <- gregexpr("累計([0-9]+)万", text, perl = TRUE)
  
  extract_first <- function(match) {
    if (match[1] == -1) return(NA_real_)
    m <- regmatches(text, match)[[1]][1]
    nums <- as.numeric(gsub("[^0-9]", "", m))
    return(nums)
  }
  
  n1 <- extract_first(p_date_num)
  if (!is.na(n1)) return(n1 * 10000)
  
  n2 <- extract_first(p_num_only)
  if (!is.na(n2)) return(n2 * 10000)
  
  n3 <- extract_first(p_break)
  if (!is.na(n3)) return(n3 * 10000)
  
  n4 <- extract_first(p_total)
  if (!is.na(n4)) return(n4 * 10000)
  
  return(NA_real_)
}

get_circulation_all_sources <- function(title) {
  cat("Circulation:", title, "\n")
  
  if (is.na(title) || title == "") return(NA)
  
  # 1. JP Wikipedia with suffix fallback
  jp_suffixes <- c(
    "",
    "_(漫画)",
    "_(小説)",
    "_(ライトノベル)",
    "_(作品)",
    "_(アニメ)"
  )
  
  encoded_title <- URLencode(title, reserved = TRUE)
  
  for (suf in jp_suffixes) {
    jp_url <- paste0("https://ja.wikipedia.org/wiki/", encoded_title, suf)
    jp_page <- safe_read(jp_url)
    
    if (!is.null(jp_page)) {
      jp_text <- jp_page %>% html_text()
      val <- extract_circulation_number(jp_text)
      if (!is.na(val)) return(val)
    }
  }
  
  # 2. Comic Natalie
  nat_query <- paste0(
    "https://search.yahoo.co.jp/search?p=",
    URLencode(paste(title, "累計 部数 ナタリー"))
  )
  
  nat_page <- safe_read(nat_query)
  
  if (!is.null(nat_page)) {
    links <- nat_page %>% html_elements("a") %>% html_attr("href")
    nat_links <- links[grepl("natalie.mu", links)]
    
    for (u in nat_links) {
      pg <- safe_read(u)
      if (!is.null(pg)) {
        txt <- pg %>% html_text()
        val <- extract_circulation_number(txt)
        if (!is.na(val)) return(val)
      }
    }
  }
  
  # 3. Mantan Web
  man_query <- paste0(
    "https://search.yahoo.co.jp/search?p=",
    URLencode(paste(title, "累計 発行部数 まんたん"))
  )
  
  man_page <- safe_read(man_query)
  
  if (!is.null(man_page)) {
    links <- man_page %>% html_elements("a") %>% html_attr("href")
    man_links <- links[grepl("mantan-web.jp", links)]
    
    for (u in man_links) {
      pg <- safe_read(u)
      if (!is.null(pg)) {
        txt <- pg %>% html_text()
        val <- extract_circulation_number(txt)
        if (!is.na(val)) return(val)
      }
    }
  }
  
  # 4. ANN
  ann_query <- paste0(
    "https://www.google.com/search?q=",
    URLencode(paste(title, "manga copies ANN"))
  )
  
  ann_page <- safe_read(ann_query)
  
  if (!is.null(ann_page)) {
    txt <- ann_page %>% html_text()
    val <- extract_circulation_number(txt)
    if (!is.na(val)) return(val)
  }
  
  return(NA_real_)
}

# ============================================================
# ⭐ APPLY TO DATASET
# ============================================================
# Use the manga/LN Japanese title instead of the anime title
anime_list$Title_JP_Source <- enc2utf8(anime_list$Title_JP_Source)

anime_list$Circulation_JP <- sapply(
  anime_list$Title_JP_Source,
  get_circulation_all_sources
)

anime_list$Circulation_Final <- ifelse(
  !is.na(anime_list$Circulation_JP) & anime_list$Circulation_JP != "",
  anime_list$Circulation_JP,
  anime_list$Circulation_Num
)

# ============================
# 7. Save
# ============================
write_xlsx(anime_list, "anime_with_preanime_circulation.xlsx")
