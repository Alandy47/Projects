library(rvest)
library(dplyr)
library(stringr)
library(readxl)
library(stringdist)
library(writexl)
library(purrr)

anime_list <- read_excel("C:/Users/aland/OneDrive/Desktop/HGEN612/Projects/Assignment_3/anime_with_circulation.xlsx")

clean_title <- function(x) {
  x <- trimws(x)
  x <- gsub('[\\"“”‘’＂]', '', x)
  x <- gsub("[[:cntrl:]]", "", x)
  x
}

anime_list$Title_Clean <- sapply(anime_list$Title, clean_title)

ann_tv_urls <- c(
  # Example URLs – replace/extend with the weeks you care about
  # "https://www.animenewsnetwork.com/news/2021-10-10/japan-animation-tv-ranking/.178300",
  # "https://www.animenewsnetwork.com/news/2021-10-17/japan-animation-tv-ranking/.178400"
)

scrape_ann_tv_page <- function(url) {
  cat("ANN page:", url, "\n")
  
  page <- tryCatch(
    read_html(httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0 Safari/537.36"
      )
    )),
    error = function(e) NULL
  )
  
  if (is.null(page)) return(tibble())
  
  tables <- page %>% html_table(fill = TRUE)
  if (length(tables) == 0) return(tibble())
  
  # Heuristic: pick the first table that looks like a ranking (has a numeric first column)
  for (tbl in tables) {
    if (ncol(tbl) < 2) next
    
    rank_col <- suppressWarnings(as.numeric(tbl[[1]]))
    if (all(is.na(rank_col))) next
    
    title_col <- as.character(tbl[[2]])
    
    return(tibble(
      URL   = url,
      Rank  = rank_col,
      Title = title_col
    ) %>% filter(!is.na(Rank)))
  }
  
  tibble()
}

ann_tv_raw <- map_dfr(ann_tv_urls, scrape_ann_tv_page)

match_ann_to_anime <- function(anime_row, ann_tbl) {
  title_en <- anime_row[["Title_Clean"]]
  title_jp <- anime_row[["Title_Japanese"]]
  
  if (nrow(ann_tbl) == 0) return(NA_real_)
  
  # Compute distance to both EN and JP titles
  d_en <- stringdist::stringdist(title_en, ann_tbl$Title, method = "jw")
  d_jp <- stringdist::stringdist(title_jp, ann_tbl$Title, method = "jw")
  
  d_min <- pmin(d_en, d_jp, na.rm = TRUE)
  
  # Keep only reasonably close matches
  hits <- ann_tbl[d_min < 0.30, , drop = FALSE]
  if (nrow(hits) == 0) return(NA_real_)
  
  weeks_top10 <- sum(hits$Rank <= 10, na.rm = TRUE)
  avg_rank    <- mean(hits$Rank, na.rm = TRUE)
  
  score <- weeks_top10 * (11 - avg_rank)
  if (is.nan(score) || is.infinite(score)) return(NA_real_)
  score
}

anime_list$ANN_TV_Score <- apply(anime_list, 1, match_ann_to_anime, ann_tbl = ann_tv_raw)

abema_pages <- c(
  # Example placeholders – replace with real ABEMA pages you can access
  # "https://abema.tv/video/genre/animation",
  # "https://abema.tv/video/genre/animation/season/2021"
)

scrape_abema_page <- function(url) {
  cat("ABEMA page:", url, "\n")
  
  page <- tryCatch(
    read_html(httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0 Safari/537.36"
      )
    )),
    error = function(e) NULL
  )
  
  if (is.null(page)) return(tibble())
  
  text <- page %>% html_text()
  
  tibble(
    URL  = url,
    Text = text
  )
}

abema_raw <- map_dfr(abema_pages, scrape_abema_page)

get_abema_popularity_direct <- function(title_jp, title_en, abema_tbl) {
  if (nrow(abema_tbl) == 0) return(NA_real_)
  
  hits <- abema_tbl %>%
    mutate(
      dist_jp = stringdist::stringdist(title_jp, Text, method = "jw"),
      dist_en = stringdist::stringdist(title_en, Text, method = "jw")
    )
  
  # Count pages where either JP or EN is reasonably close
  sum(hits$dist_jp < 0.20 | hits$dist_en < 0.20, na.rm = TRUE)
}

anime_list$ABEMA_Popularity <- mapply(
  function(jp, en) get_abema_popularity_direct(jp, en, abema_raw),
  anime_list$Title_Japanese,
  anime_list$Title_Clean
)

zscore <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}

anime_list$z_tv    <- zscore(anime_list$ANN_TV_Score)
anime_list$z_abema <- zscore(anime_list$ABEMA_Popularity)

w_tv    <- 0.60
w_abema <- 0.40

anime_list$UPS_Lite <- with(
  anime_list,
  w_tv * z_tv + w_abema * z_abema
)

write_xlsx(anime_list, "anime_tv_abema_direct.xlsx")
