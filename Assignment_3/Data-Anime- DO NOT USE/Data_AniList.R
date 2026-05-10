library(httr)
library(jsonlite)
library(dplyr)
library(readxl)

# --- Load anime list ---
anime_list <- read_excel("Data-Anime/anime_list.xlsx")

# --- Strong title cleaning ---
clean_title <- function(x) {
  x <- trimws(x)
  x <- gsub('[\"“”‘’＂]', '', x)
  x <- gsub("[\u200B-\u200F\u202A-\u202E\u2060-\u206F]", "", x)
  x <- gsub("\u00A0", " ", x)
  x <- gsub("\ufeff", "", x)
  x <- gsub("[[:cntrl:]]", "", x)
  x <- iconv(x, to = "UTF-8", sub = "")
  return(x)
}

anime_list$Title <- sapply(anime_list$Title, clean_title)

# --- Safe NULL operator ---
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# --- GraphQL query with inline fragment ---
query <- '
query ($search: String) {
  Media(search: $search, type: ANIME) {
    ... on Media {
      title {
        romaji
        english
        native
      }
      season
      seasonYear
      genres
      tags { name }
      studios(isMain: true) { nodes { name } }
      relations {
  nodes {
    type
    title {
      native
      romaji
      english
    }
  }
}
    }
  }
}
'

# --- Helper: safely extract error message ---
extract_error_message <- function(err) {
  if (is.null(err)) return(NULL)
  if (is.data.frame(err) && "message" %in% names(err)) return(err$message[1])
  if (is.list(err) && !is.null(err[[1]]$message)) return(err[[1]]$message)
  if (is.list(err) && is.character(err[[1]])) return(err[[1]])
  if (is.atomic(err)) return(err[1])
  return(NULL)
}

# --- Request function with retry + backoff ---
make_request <- function(name) {
  wait <- 1
  repeat {
    response <- POST(
      url = "https://graphql.anilist.co",
      body = list(query = query, variables = list(search = name)),
      encode = "json"
    )
    
    json <- fromJSON(content(response, "text"))
    # Detect AniList API shutdown
    if (!is.null(json$errors)) {
      err_msg <- extract_error_message(json$errors)
      
      if (!is.null(err_msg) &&
          grepl("temporarily disabled", err_msg, ignore.case = TRUE)) {
        
        cat("\n---------------------------------------------\n")
        cat(" AniList API is currently DISABLED.\n")
        cat(" Message: ", err_msg, "\n")
        cat(" Your script has stopped to avoid NA output.\n")
        cat(" Try again later when the API is back online.\n")
        cat("---------------------------------------------\n\n")
        
        stop("AniList API disabled — stopping script.")
      }
    }
    
    if (is.null(json$errors)) {
      return(json$data$Media)
    }
    
    err_msg <- extract_error_message(json$errors)
    
    if (!is.null(err_msg) && grepl("Too Many Requests", err_msg, ignore.case = TRUE)) {
      cat("Rate limited. Waiting", wait, "seconds...\n")
      Sys.sleep(wait)
      wait <- wait * 1.5
      next
    }
    
    return(NULL)
  }
}

# --- Main loop ---
results <- lapply(anime_list$Title, function(name) {
  
  print(paste0("Searching for: '", name, "'"))
  Sys.sleep(1.2)
  
  media <- make_request(name)
  
  if (is.null(media)) {
    return(data.frame(
      Title_Search   = name,
      Title_Romaji   = NA,
      Title_English  = NA,
      Title_Japanese = NA,
      Title_JP_Source = NA,
      Season         = NA,
      SeasonYear     = NA,
      Genre1         = NA,
      Genre2         = NA,
      Genre3         = NA,
      Demographic    = NA,
      Studio         = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # --- Titles ---
  title_romaji   <- media$title$romaji   %||% NA
  title_english  <- media$title$english  %||% NA
  title_japanese <- media$title$native   %||% NA
  
  # --- Source title extraction (manga/LN) ---
  source_nodes <- media$relations$nodes
  title_jp_source <- NA
  
  if (!is.null(source_nodes) && length(source_nodes) > 0) {
    
    types <- source_nodes$type %||% NA
    idx <- which(types %in% c("MANGA", "NOVEL", "LIGHT_NOVEL"))
    
    if (length(idx) > 0) {
      title_jp_source <- source_nodes$title$native[idx[1]] %||% NA
    }
  }
  
  # --- Season ---
  season      <- media$season     %||% NA
  season_year <- media$seasonYear %||% NA
  
  # --- Genres ---
  g <- media$genres %||% NA
  genre1 <- ifelse(length(g) >= 1, g[1], NA)
  genre2 <- ifelse(length(g) >= 2, g[2], NA)
  genre3 <- ifelse(length(g) >= 3, g[3], NA)
  
  # --- Demographic ---
  tag_names <- media$tags$name %||% NA
  demo <- tag_names[tag_names %in% c("Shounen", "Shoujo", "Seinen", "Josei")]
  demographic <- ifelse(length(demo) > 0, demo[1], NA)
  
  # --- Studio ---
  studio_list <- media$studios$nodes$name %||% NA
  studio <- ifelse(length(studio_list) >= 1, studio_list[1], NA)
  
  data.frame(
    Title_Search    = name,
    Title_Romaji    = title_romaji,
    Title_English   = title_english,
    Title_Japanese  = title_japanese,
    Title_JP_Source = title_jp_source,
    Season          = season,
    SeasonYear      = season_year,
    Genre1          = genre1,
    Genre2          = genre2,
    Genre3          = genre3,
    Demographic     = demographic,
    Studio          = studio,
    stringsAsFactors = FALSE
  )
})

# --- Final dataframe ---
df <- dplyr::bind_rows(results)

# --- Save to Excel ---
library(openxlsx)
write.xlsx(df, "anime_data2.xlsx")