# ============================
# Libraries
# ============================
library(dplyr)
library(readxl)
library(rvest)
library(stringr)
library(openxlsx)

# ============================
# 1. Load anime list
# ============================
anime_list <- read_excel("C:/Users/aland/OneDrive/Desktop/HGEN612/Projects/Assignment_3/anime_data2.xlsx")

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
# 3. Scrape ALL circulation tables from Wikipedia
# ============================
url <- "https://en.wikipedia.org/wiki/List_of_best-selling_manga"
page <- read_html(url)

# Get all wikitable tables
tables <- page %>% html_nodes("table.wikitable")

# Function to extract title + circulation from any table
extract_circ <- function(tbl) {
  df <- tryCatch(html_table(tbl, fill = TRUE), error = function(e) NULL)
  if (is.null(df)) return(NULL)
  
  # Find title column
  title_col <- grep("Manga|Series|Title", names(df), ignore.case = TRUE)
  # Find circulation column
  circ_col <- grep("Approximate|Sales|Circulation", names(df), ignore.case = TRUE)
  
  if (length(title_col) == 0 || length(circ_col) == 0) return(NULL)
  
  out <- df[, c(title_col[1], circ_col[1])]
  names(out) <- c("Title_Raw", "Circulation_Raw")
  out
}

# Apply extractor to all tables
circ_list <- lapply(tables, extract_circ)

# Remove NULLs
circ_list <- circ_list[!sapply(circ_list, is.null)]

# Combine all circulation tables
circ_df <- bind_rows(circ_list)

# Clean titles
circ_df$Title_Clean <- sapply(circ_df$Title_Raw, clean_title)

# Clean circulation numbers
circ_df$Circulation_Num <- circ_df$Circulation_Raw %>%
  str_replace_all("[^0-9]", "") %>%
  as.numeric()

# Drop rows with no numeric circulation
circ_df <- circ_df %>% filter(!is.na(Circulation_Num))
# ============================
# 4. Merge circulation into anime list
# ============================
final_df <- anime_list %>%
  left_join(
    circ_df %>% select(Title_Clean, Circulation_Num),
    by = "Title_Clean"
  )

# ============================
# 5. Save
# ============================
write.xlsx(final_df, "anime_with_circulation.xlsx")