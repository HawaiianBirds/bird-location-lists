#### Backup 15 - Final - Fix PDF table order & footer ####

options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

# Force session timezone to Hawaiʻi
Sys.setenv(TZ = "Pacific/Honolulu")

suppressPackageStartupMessages({
  library(shiny); library(shinydashboard)
  library(readxl); library(dplyr); library(tidyr)
  library(lubridate); library(stringr); library(purrr)
  library(gt); library(htmltools)
  # For PDF export:
  library(pagedown)
  library(webshot2)
  library(writexl)
})

`%||%` <- function(a,b) if(!is.null(a)) a else b
brand_green <- "#1B5E20"
STRINGS_PATHS <- c("./data/STRINGS_new.xlsx","data/STRINGS_new.xlsx","STRINGS_new.xlsx")

# Date helper for UTC to HST -----
# Always use Hawaiʻi time for "today"
today_hst <- function() {
  # Always compute "today" in Hawaiʻi time, regardless of system TZ
  as.Date(format(Sys.time(), tz = "Pacific/Honolulu"))
}

# ---- Chrome path discovery for local use (Mac/RStudio) ----
.find_chrome <- function() {
  # 1) Option set in R
  ch <- getOption("pagedown.chrome", "")
  if (nzchar(ch)) return(ch)
  
  # 2) Env var
  ch <- Sys.getenv("PAGEDOWN_CHROMIUM", "")
  if (nzchar(ch)) return(ch)
  
  # 3) chrome-path.txt next to app.R (for local override)
  cfg <- "chrome-path.txt"
  if (file.exists(cfg)) {
    ch <- trimws(readLines(cfg, warn = FALSE)[1])
    if (nzchar(ch)) return(ch)
  }
  
  # 4) Try pagedown's finder (works on many platforms)
  if (requireNamespace("pagedown", quietly = TRUE)) {
    ch <- try(pagedown::find_chrome(), silent = TRUE)
    if (!inherits(ch, "try-error") && length(ch) && nzchar(ch)) return(ch)
  }
  
  # 5) Fallback: look for common Chrome binaries (Mac/Linux)
  candidates <- Sys.which(c(
    "google-chrome",
    "chromium",
    "chrome",
    "Google Chrome",
    "Google Chrome.app/Contents/MacOS/Google Chrome"
  ))
  candidates <- unique(candidates[nzchar(candidates)])
  if (length(candidates) > 0) return(candidates[[1]])
  
  ""  # let caller handle "no chrome found"
}

# ---- Detect Chrome path in container ----
chrome_path <- unname(Sys.which("google-chrome"))

if (!nzchar(chrome_path)) {
  message("[INIT] google-chrome NOT found on PATH.")
} else {
  message("[INIT] Using Chrome at: ", chrome_path)
  
  options(pagedown.chrome = chrome_path)
  Sys.setenv(PAGEDOWN_CHROMIUM = chrome_path)
  Sys.setenv(CHROMOTE_CHROME = chrome_path)
  Sys.setenv(CHROME_BIN       = chrome_path)  # <<< NEW
}

message(
  "[INIT] App starting. chrome_path=", chrome_path,
  " CHROME_BIN=", Sys.getenv("CHROME_BIN"),
  " PAGEDOWN_CHROMIUM=", Sys.getenv("PAGEDOWN_CHROMIUM"),
  " CHROMOTE_CHROME=", Sys.getenv("CHROMOTE_CHROME")
)


# ---- Debug logging for Chrome / pagedown on startup ----
try({
  chrome_detected <- tryCatch(
    pagedown::find_chrome(),
    error = function(e) paste("ERROR from find_chrome():", e$message)
  )
  
  message(
    "\n[DEBUG] pagedown.chrome option: ", getOption("pagedown.chrome", "NULL"),
    "\n[DEBUG] PAGEDOWN_CHROMIUM env: ", Sys.getenv("PAGEDOWN_CHROMIUM", "EMPTY"),
    "\n[DEBUG] find_chrome(): ", chrome_detected, "\n"
  )
}, silent = TRUE)


# ---------- Helpers ----------
as_posix_safe <- function(x){
  suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("ymd HMS","ymd HM","ymd","mdy HMS","mdy HM","mdy","dmy HMS","dmy HM","dmy"),
    tz = "UTC", quiet = TRUE
  ))
}
format_move_mmddyy <- function(x){
  x <- as_posix_safe(as.character(x))
  ifelse(is.na(x), "", format(as.Date(x), "%m/%d/%y"))
}
require_cols <- function(df, cols, name){
  miss <- setdiff(cols, names(df))
  if(length(miss)) stop(sprintf("%s missing columns: %s", name, paste(miss, collapse=", ")))
  invisible(TRUE)
}

normalize_enclosure <- function(enc) {
  # Standardize enclosure strings like "Kolii/AA" -> "Kolii/A", "Kolii/AB" -> "Kolii/B",
  # "Education/NAAB" -> "Education/A,B", etc.
  if (is.na(enc) || !nzchar(enc)) return(NA_character_)
  
  s <- gsub("_", " ", enc, fixed = TRUE)
  s <- gsub("[[:space:]]*/[[:space:]]*", "/", s)     # normalize spaces around '/'
  s <- gsub("/+$", "", s)                             # drop trailing '/'
  s <- sub("^[[:space:]]*(KBCC|MBCC)[[:space:]]+", "", s, ignore.case = TRUE)  # drop facility prefix
  
  parts <- strsplit(s, "/", fixed = TRUE)[[1]]
  if (length(parts) == 0) return(NA_character_)
  
  if (length(parts) == 1) {
    bldg <- trimws(parts[1])
    return(ifelse(nzchar(bldg), bldg, NA_character_))
  }
  
  bldg <- trimws(parts[1])
  
  toks <- unlist(strsplit(paste(parts[-1], collapse = ","), ",", fixed = TRUE))
  toks <- trimws(toks)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(bldg)
  
  # Drop NA-like tokens
  toks <- ifelse(toks %in% c("NA","N/A","N","Na","na"), "", toks)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(bldg)
  
  # Upper-case for consistent matching
  toks <- toupper(toks)
  
  # Special case: AICUNA = AICU + NA → keep only AICU
  toks[toks == "AICUNA"] <- "AICU"
  
  # Drop NA-like tokens
  toks <- ifelse(toks %in% c("NA","N/A","N"), "", toks)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(bldg)
  
  # Strip leading N (e.g., NAAB -> AAB)
  toks <- sub("^N(.*)$", "\\1", toks)
  
  # Expand NAAB -> AAB -> AA,B
  toks <- unlist(lapply(toks, function(t) {
    if (grepl("^AA[A-Z]$", t)) c("AA", substr(t, 3, 3)) else t
  }), use.names = FALSE)
  
  # Collapse AA -> A and AB -> B
  toks <- ifelse(toks == "AA", "A", toks)
  toks <- ifelse(toks == "AB", "B", toks)
  
  
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(bldg)
  
  paste0(bldg, "/", paste(toks, collapse = ","))
}

pretty_name <- function(x){ x <- gsub('_',' ', x, fixed = TRUE); trimws(x) }

# ----- ORDERING HELPERS -----
order_for_ui_grid <- function(n_items, n_cols = 2){
  if(n_items == 0) return(integer())
  if(n_cols <= 1) return(seq_len(n_items))
  n_rows <- ceiling(n_items / n_cols)
  left  <- seq_len(n_rows)
  right <- if(n_items > n_rows) (n_rows + 1):n_items else integer(0)
  out <- integer(0)
  for(i in seq_len(n_rows)){
    out <- c(out, left[i], if(length(right) >= i) right[i] else integer(0))
  }
  out
}
order_for_pdf_columns <- function(n_items, n_cols = 2){
  if(n_items == 0) return(integer())
  if(n_cols <= 1) return(seq_len(n_items))
  n_rows <- ceiling(n_items / n_cols)
  c(seq_len(n_rows), if(n_items > n_rows) (n_rows + 1):n_items else integer(0))
}

# ---------- Build one mini-table per aviary ----------
make_aviary_gt <- function(df_aviary, aviary_name, string_hint = NA_character_, compact = FALSE){
  
  # --------- PREP TABLE DATA ---------
  df <- df_aviary %>%
    mutate(
      Enclosure = purrr::map_chr(`Bird moved to enclosure ID:`, ~normalize_enclosure(.x) %||% NA_character_),
      Enclosure = gsub("INNER CHAMBER,? ?OUTER CHAMBER", "IN/OUT", Enclosure, ignore.case = TRUE),
      Enclosure = gsub("INNER CHAMBER", "INNER", Enclosure, ignore.case = TRUE),
      Enclosure = gsub("OUTER CHAMBER", "OUTER", Enclosure, ignore.case = TRUE),
      Name      = ifelse(is.na(`Name:`), "", as.character(`Name:`)),
      Name      = substr(Name, 1, if (isTRUE(compact)) 10 else 12),
      Bands     = ifelse(is.na(`Short color bands:`), "", as.character(`Short color bands:`)),
      Bands     = gsub("[[:space:]]+", " ", Bands),
      Move      = format_move_mmddyy(`Corrected date & time for data export:`)
    ) %>%
    arrange(Enclosure, desc(`Studbook number:`)) %>%
    group_by(Enclosure) %>% mutate(.last_in_encl = dplyr::row_number() == dplyr::n()) %>% ungroup() %>%
    transmute(
      `Aviary/Ch` = Enclosure,
      `SB#`  = `Studbook number:`,
      Sex    = `Sex:`,
      Name   = Name,
      Bands  = Bands,
      Moved  = Move,
      .last_in_encl
    )
  
  # --------- ORDER (MBCC FBB quirks preserved) ---------
  facility_here <- unique(as.character(df_aviary$`Facility:`))[1] %||% NA_character_
  aviary_title  <- pretty_name(aviary_name)
  
  first_token <- function(x){
    rest <- sub("^[^/]+/?", "", x)
    if (identical(rest, x) || !nzchar(rest)) return(NA_character_)
    sub(",.*$", "", rest)
  }
  
  if (identical(facility_here, "MBCC") && grepl("^FBB1\\b", aviary_title, ignore.case = TRUE)) {
    tail <- sub("^[^/]+/", "", df$`Aviary/Ch`)
    letters_only <- toupper(gsub("[^A-Za-z]", "", tail))
    lett_key <- ifelse(nzchar(letters_only),
                       substring(letters_only, nchar(letters_only), nchar(letters_only)), "")
    get_min_num <- function(s) {
      m <- gregexpr("[0-9]+", s, perl = TRUE)
      v <- suppressWarnings(as.integer(unlist(regmatches(s, m))))
      v <- v[!is.na(v)]
      if (length(v)) min(v) else Inf
    }
    num_key <- vapply(tail, get_min_num, numeric(1))
    df <- df %>% mutate(.lett_key = lett_key, .num_key = num_key) %>%
      arrange(desc(.lett_key), .num_key, `Aviary/Ch`, desc(`SB#`)) %>%
      select(-.lett_key, -.num_key)
    
  } else if (identical(facility_here, "MBCC") && grepl("^FBB(2|3)\\b", aviary_title, ignore.case = TRUE)) {
    num_key <- suppressWarnings(as.integer(sub("^[^/]+/(\\d+).*", "\\1", df$`Aviary/Ch`)))
    num_key[is.na(num_key)] <- Inf
    df <- df %>% mutate(.num_key = num_key) %>%
      arrange(.num_key, `Aviary/Ch`, desc(`SB#`)) %>%
      select(-.num_key)
    
  } else {
    tok   <- vapply(df$`Aviary/Ch`, first_token, character(1))
    num   <- suppressWarnings(as.integer(tok))
    isnum <- !is.na(num)
    df <- df %>% mutate(.tok = tok, .num = ifelse(isnum, num, Inf), .isn = isnum) %>%
      arrange(desc(.isn), .num, .tok, desc(`SB#`)) %>% select(-.tok, -.num, -.isn)
  }
  
  # --------- BUILD gt TABLE ---------
  suffix     <- if (!is.na(string_hint) && nzchar(string_hint)) paste0(" (", string_hint, ")") else ""
  title_text <- paste0("**", pretty_name(aviary_name), suffix, "**")
  
  g <- gt::gt(df %>% select(-.last_in_encl)) |>
    gt::tab_header(title = gt::md(title_text)) |>
    gt::sub_missing(gt::everything(), missing_text = "") |>
    gt::tab_options(
      table.width = gt::px(if (isTRUE(compact)) 300 else 360),
      heading.align = "left",
      column_labels.background.color = brand_green,
      column_labels.font.weight = "bold",
      table.font.size = gt::px(if (isTRUE(compact)) 8.2 else 10.5),
      data_row.padding = gt::px(if (isTRUE(compact)) 0.8 else 2.8),
      heading.padding  = gt::px(if (isTRUE(compact)) 2.2 else 5.5)
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_labels(gt::everything())
    ) |>
    gt::cols_width(
      `Aviary/Ch` ~ gt::px(110),
      `SB#`       ~ gt::px(60),
      Sex         ~ gt::px(34),
      Name        ~ gt::px(80),
      Bands       ~ gt::px(65),
      Moved       ~ gt::px(66)
    ) |>
    gt::cols_align(align = "left", columns = gt::everything())
  
  if (nrow(df)) {
    rows_with_rule <- which(df$.last_in_encl)
    g <- gt::tab_style(
      g,
      style = gt::cell_borders(sides = "bottom", color = "#6e6e6e", weight = gt::px(2)),
      locations = gt::cells_body(rows = rows_with_rule)
    )
  }
  g
}

# ---------- STRINGS (optional) ----------
strings_df <- function(){
  path <- STRINGS_PATHS[file.exists(STRINGS_PATHS)][1]
  if(is.na(path)) return(NULL)
  df <- tryCatch(read_xlsx(path), error = function(e) NULL)
  if(is.null(df)) return(NULL)
  aviary_col <- intersect(c("AviaryName", "Aviary", "Aviary:"), names(df))[1]
  string_col <- intersect(c("String:", "String", "STRING"), names(df))[1]
  if(is.na(aviary_col) || is.na(string_col)) return(NULL)
  df %>% select(Aviary = all_of(aviary_col), String = all_of(string_col)) %>%
    mutate(Aviary = as.character(Aviary), String = as.character(String))
}

# ---------- Build a data.frame version of an aviary table for Excel ----------
make_aviary_df_for_excel <- function(df_aviary, aviary_name,
                                     string_hint = NA_character_,
                                     compact = FALSE) {
  
  # --------- PREP TABLE DATA (same as make_aviary_gt) ---------
  df <- df_aviary %>%
    mutate(
      Enclosure = purrr::map_chr(`Bird moved to enclosure ID:`,
                                 ~ normalize_enclosure(.x) %||% NA_character_),
      Enclosure = gsub("INNER CHAMBER,? ?OUTER CHAMBER", "IN/OUT",
                       Enclosure, ignore.case = TRUE),
      Enclosure = gsub("INNER CHAMBER", "INNER", Enclosure, ignore.case = TRUE),
      Enclosure = gsub("OUTER CHAMBER",  "OUTER", Enclosure, ignore.case = TRUE),
      Name      = ifelse(is.na(`Name:`), "", as.character(`Name:`)),
      Name      = substr(Name, 1, if (isTRUE(compact)) 10 else 12),
      Bands     = ifelse(is.na(`Short color bands:`), "", as.character(`Short color bands:`)),
      Bands     = gsub("[[:space:]]+", " ", Bands),
      Move      = format_move_mmddyy(`Corrected date & time for data export:`)
    ) %>%
    arrange(Enclosure, desc(`Studbook number:`)) %>%
    group_by(Enclosure) %>% 
    mutate(.last_in_encl = dplyr::row_number() == dplyr::n()) %>%
    ungroup() %>%
    transmute(
      `Aviary/Ch` = Enclosure,
      `SB#`       = `Studbook number:`,
      Sex         = `Sex:`,
      Name        = Name,
      Bands       = Bands,
      Moved       = Move,
      .last_in_encl
    )
  
  # --------- ORDERING (same rules as make_aviary_gt) ---------
  facility_here <- unique(as.character(df_aviary$`Facility:`))[1] %||% NA_character_
  aviary_title  <- pretty_name(aviary_name)
  
  first_token <- function(x){
    rest <- sub("^[^/]+/?", "", x)
    if (identical(rest, x) || !nzchar(rest)) return(NA_character_)
    sub(",.*$", "", rest)
  }
  
  if (identical(facility_here, "MBCC") && grepl("^FBB1\\b", aviary_title, ignore.case = TRUE)) {
    tail <- sub("^[^/]+/", "", df$`Aviary/Ch`)
    letters_only <- toupper(gsub("[^A-Za-z]", "", tail))
    lett_key <- ifelse(
      nzchar(letters_only),
      substring(letters_only, nchar(letters_only), nchar(letters_only)),
      ""
    )
    get_min_num <- function(s) {
      m <- gregexpr("[0-9]+", s, perl = TRUE)
      v <- suppressWarnings(as.integer(unlist(regmatches(s, m))))
      v <- v[!is.na(v)]
      if (length(v)) min(v) else Inf
    }
    num_key <- vapply(tail, get_min_num, numeric(1))
    df <- df %>%
      mutate(.lett_key = lett_key, .num_key = num_key) %>%
      arrange(desc(.lett_key), .num_key, `Aviary/Ch`, desc(`SB#`)) %>%
      select(-.lett_key, -.num_key)
    
  } else if (identical(facility_here, "MBCC") &&
             grepl("^FBB(2|3)\\b", aviary_title, ignore.case = TRUE)) {
    
    num_key <- suppressWarnings(
      as.integer(sub("^[^/]+/(\\d+).*", "\\1", df$`Aviary/Ch`))
    )
    num_key[is.na(num_key)] <- Inf
    df <- df %>%
      mutate(.num_key = num_key) %>%
      arrange(.num_key, `Aviary/Ch`, desc(`SB#`)) %>%
      select(-.num_key)
    
  } else {
    tok   <- vapply(df$`Aviary/Ch`, first_token, character(1))
    num   <- suppressWarnings(as.integer(tok))
    isnum <- !is.na(num)
    df <- df %>%
      mutate(.tok = tok, .num = ifelse(isnum, num, Inf), .isn = isnum) %>%
      arrange(desc(.isn), .num, .tok, desc(`SB#`)) %>%
      select(-.tok, -.num, -.isn)
  }
  
  # Drop helper column for export
  df %>% select(-.last_in_encl)
}



# ---------- UI ----------
header <- dashboardHeader(title = "Bird Location Lists")
sidebar <- dashboardSidebar(
  width = 300,
  tags$style(HTML(sprintf(
    ".skin-blue .main-header .logo, .skin-blue .main-header .navbar { background-color: %s; }
     .skin-blue .main-sidebar { background-color: #0e2f11; }
     .skin-blue .sidebar a { color:#fff;}
     .skin-blue .content-wrapper { background:#f6f8f7;}", brand_green))),
  sidebarMenu(
    menuItem("KBCC — ‘Alalā", tabName = "kbcc_al", icon = icon("crow")),
    menuItem("KBCC — Small forest birds", tabName = "kbcc_fb", icon = icon("feather")),
    menuItem("MBCC — ‘Alalā", tabName = "mbcc_al", icon = icon("crow")),
    menuItem("MBCC — Small forest birds", tabName = "mbcc_fb", icon = icon("feather")),
    hr(),
    fileInput("details_file", "Load bird details:", accept = ".xlsx"),
    fileInput("moves_file",   "Load bird moves:", accept = ".xlsx"),
    dateInput("subset_date",  "List all bird locations as of:", value = today_hst()),
    helpText("")
  )
)
body <- dashboardBody(
  tags$head(tags$style(HTML(sprintf(
    ".box.box-solid>.box-header{color:#fff;background:%s;}
     .box.box-solid{border:1px solid %s;}
     .two-col-grid{display:grid;grid-template-columns:repeat(var(--cols,2),minmax(260px,1fr));gap:0.05px;align-items:start}
     .gt_table{width:100%%;max-width:100%%}", brand_green, brand_green)))),
  tabItems(
    tabItem("kbcc_al", fluidRow(
      box(width = 12, solidHeader = TRUE,
          title = uiOutput("title_kbcc_al"),
          downloadButton("dl_pdf_kbcc_al", "Download PDF"),
          downloadButton("dl_xlsx_kbcc_al", "Download Excel"),
          uiOutput("grid_kbcc_al"))
    )),
    tabItem("kbcc_fb", fluidRow(
      box(width = 12, solidHeader = TRUE,
          title = uiOutput("title_kbcc_fb"),
          downloadButton("dl_pdf_kbcc_fb", "Download PDF"),
          downloadButton("dl_xlsx_kbcc_fb", "Download Excel"),
          uiOutput("grid_kbcc_fb"))
    )),
    tabItem("mbcc_al", fluidRow(
      box(width = 12, solidHeader = TRUE,
          title = uiOutput("title_mbcc_al"),
          downloadButton("dl_pdf_mbcc_al", "Download PDF"),
          downloadButton("dl_xlsx_mbcc_al", "Download Excel"),
          uiOutput("grid_mbcc_al"))
    )),
    tabItem("mbcc_fb", fluidRow(
      box(width = 12, solidHeader = TRUE,
          title = uiOutput("title_mbcc_fb"),
          downloadButton("dl_pdf_mbcc_fb", "Download PDF"),
          downloadButton("dl_xlsx_mbcc_fb", "Download Excel"),
          uiOutput("grid_mbcc_fb"))
    ))
  )
)

ui <- dashboardPage(skin = "blue", header, sidebar, body)

# ---------- Server ----------
server <- function(input, output, session){
  strings_react <- reactive(strings_df())
  
  moves_raw <- reactive({
    req(input$moves_file)
    df <- read_xlsx(input$moves_file$datapath)
    require_cols(df, c(
      "Date & time of move:","Facility:","Studbook number:","Move type:",
      "ʻAlalā moved to aviary:","ʻAlalā moved to chamber(s):",
      "Forest bird moved to aviary:","Forest bird moved to chamber number(s):",
      "Forest bird moved to chamber letter(s):","Bird moved to enclosure ID:",
      "Corrected date & time for data export:"
    ), "BIRD_MOVES")
    df %>% mutate(SubsetDate = as.Date(substr(`Corrected date & time for data export:`, 1, 10))) %>%
      filter(SubsetDate <= as.Date(input$subset_date))
  })
  
  details_alive <- reactive({
    req(input$details_file)
    df <- read_xlsx(input$details_file$datapath)
    require_cols(df, c("Studbook number:","Name:","Sex:","Short color bands:"), "BIRD_DETAILS")
    df$`Short color bands:` <- gsub("(*)", "*", df$`Short color bands:`, fixed = TRUE)
    
    has_death <- "Died:" %in% names(df)
    has_hatched <- "Hatched:" %in% names(df)
    has_hatchdate <- "Hatch date:" %in% names(df)
    has_hatch_year <- "Hatch year:" %in% names(df)
    
    if(!has_death && !has_hatched && !has_hatchdate && !has_hatch_year){
      if("Status:" %in% names(df)) return(df %>% filter(!`Status:` %in% c("xDead","xReleased","xNeverExisted","PANAEWA")))
      return(df)
    }
    hatch_dt <- if(has_hatched) as.Date(as_posix_safe(df[["Hatched:"]])) else
      if(has_hatchdate) as.Date(as_posix_safe(df[["Hatch date:"]])) else
        if(has_hatch_year) suppressWarnings(as.Date(sprintf("%s-01-01", df[["Hatch year:"]]))) else as.Date(NA)
    death_dt <- if(has_death) as.Date(as_posix_safe(df[["Died:"]])) else as.Date(NA)
    cut <- as.Date(input$subset_date)
    df %>% mutate(`__hatch_dt` = hatch_dt, `__death_dt` = death_dt) %>%
      filter(!is.na(`__hatch_dt`) & `__hatch_dt` <= cut) %>%
      filter(is.na(`__death_dt`) | `__death_dt` > cut)
  })
  
  bm_core <- reactive({
    m <- moves_raw() %>% select(
      `Date & time of move:`,`Facility:`,`Studbook number:`,`Move type:`,
      `ʻAlalā moved to aviary:`,`ʻAlalā moved to chamber(s):`,
      `Forest bird moved to aviary:`,`Forest bird moved to chamber number(s):`,
      `Forest bird moved to chamber letter(s):`,`Bird moved to enclosure ID:`,
      `Corrected date & time for data export:`
    )
    d <- details_alive() %>% select(
      `Studbook number:`,`Name:`,`Sex:`,`Short color bands:`,`Hatch year:` = any_of("Hatch year:")
    )
    
    m2 <- m %>% group_by(`Studbook number:`, `Move type:`) %>%
      slice_max(as_posix_safe(`Date & time of move:`), n = 1, with_ties = FALSE) %>% ungroup()
    
    bm <- merge(d, m2, by = "Studbook number:", all.x = TRUE) %>%
      group_by(`Studbook number:`) %>%
      slice_max(as_posix_safe(`Date & time of move:`), n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(!is.na(`Move type:`) & `Move type:` != "to other") %>%
      mutate(
        Species = substr(`Studbook number:`, 1, 2),
        Aviary  = if_else(Species == "AL", `ʻAlalā moved to aviary:`, `Forest bird moved to aviary:`)
      ) %>%
      filter(!is.na(Aviary) & Aviary != "drop")
    
    s <- strings_react()
    if(!is.null(s) && nrow(s)) bm <- bm %>% left_join(s, by = c("Aviary" = "Aviary")) else bm$String <- NA_character_
    bm %>% arrange(Aviary)
  })
  
  # build list of mini-gt tables (alpha by Aviary)
  make_tables <- function(facility, species_is_al, compact = FALSE){
    bm <- bm_core() %>%
      filter(`Facility:` == facility,
             if (species_is_al) Species == "AL" else Species != "AL")
    if (!nrow(bm)) return(list())
    
    bm <- bm %>% mutate(Aviary = as.character(Aviary)) %>% arrange(Aviary)
    
    bm <- bm %>%
      mutate(
        Aviary = dplyr::case_when(
          facility == "MBCC" & grepl("^FBB1", Aviary, ignore.case = TRUE) ~ "FBB1",
          facility == "KBCC" & grepl("^FBB1", Aviary, ignore.case = TRUE) ~ "FBB1 (PIKO 1)",
          TRUE ~ Aviary
        )
      )
    
    splits <- split(bm, bm$Aviary, drop = TRUE)
    aviary_names <- sort(unique(bm$Aviary))
    
    purrr::map(aviary_names, function(av){
      df <- splits[[av]]
      
      hint_vec <- df$String[which(!is.na(df$String) & nzchar(df$String))] %>% unique()
      hint_vec <- chartr("\u00A0"," ", hint_vec)
      hint_vec <- trimws(hint_vec)
      
      av_norm <- trimws(chartr("\u00A0"," ", av))
      display_av <- av_norm
      
      if (identical(facility, "MBCC")) {
        # Strip any trailing PIKO / PIKO 1 / PIKO 2, with or without parentheses
        display_av <- gsub("[[:space:]]*\\(PIKO( [12])?\\)[[:space:]]*$", "", display_av, ignore.case = TRUE)
        display_av <- gsub("[[:space:]]+PIKO( [12])?[[:space:]]*$",       "", display_av, ignore.case = TRUE)
        display_av <- trimws(display_av)
      } else if (identical(facility, "KBCC")) {
        if (grepl("^FBB1\\b", display_av, ignore.case = TRUE)) {
          display_av <- gsub("[[:space:]]*\\(PIKO 1\\)[[:space:]]*$", "", display_av, ignore.case = TRUE)
          display_av <- "FBB1 (PIKO 1)"
        }
      }
      
      
      if (identical(facility, "MBCC") &&
          grepl("^FBB[12]\\b", display_av, ignore.case = TRUE)) {
        # Remove standalone "PIKO", "PIKO 1", "PIKO 2" from the hint text
        pikopat <- "\\(?[[:space:]]*PIKO([[:space:]]+[12])?[[:space:]]*\\)?"
        
        hint_vec <- hint_vec[ !grepl(pikopat, hint_vec, ignore.case = TRUE) ]
        hint_vec <- gsub(pikopat, "", hint_vec, ignore.case = TRUE)
        hint_vec <- trimws(hint_vec)
        hint_vec <- hint_vec[nzchar(hint_vec)]
      }
      
      
      
      hint <- if (length(hint_vec)) paste(sort(hint_vec), collapse="/") else NA_character_
      make_aviary_gt(df, aviary_name = display_av, string_hint = hint, compact = compact)
    })
  }
  
  
  # ---------- Excel helper: rbind all aviaries into one sheet ----------
  excel_df_for <- function(facility, species_is_al, compact = FALSE) {
    bm <- bm_core() %>%
      dplyr::filter(
        `Facility:` == facility,
        if (species_is_al) Species == "AL" else Species != "AL"
      )
    
    if (!nrow(bm)) {
      return(tibble::tibble())
    }
    
    # Match the aviary naming tweaks you use in make_tables()
    bm <- bm %>%
      dplyr::mutate(Aviary = as.character(Aviary)) %>%
      dplyr::arrange(Aviary) %>%
      dplyr::mutate(
        Aviary = dplyr::case_when(
          facility == "MBCC" & grepl("^FBB1", Aviary, ignore.case = TRUE) ~ "FBB1",
          facility == "KBCC" & grepl("^FBB1", Aviary, ignore.case = TRUE) ~ "FBB1 (PIKO 1)",
          TRUE ~ Aviary
        )
      )
    
    splits       <- split(bm, bm$Aviary, drop = TRUE)
    aviary_names <- sort(unique(bm$Aviary))
    
    # Helper to build a tidy data frame for one aviary
    build_aviary_df <- function(av) {
      df <- splits[[av]]
      
      # ----- STRING / hint, same logic as in make_tables() -----
      hint_vec <- df$String[which(!is.na(df$String) & nzchar(df$String))] %>% unique()
      hint_vec <- chartr("\u00A0"," ", hint_vec)
      hint_vec <- trimws(hint_vec)
      
      av_norm    <- trimws(chartr("\u00A0"," ", av))
      display_av <- av_norm
      
      if (identical(facility, "MBCC")) {
        display_av <- gsub("[[:space:]]*\\(PIKO 1\\)[[:space:]]*$", "", display_av, ignore.case = TRUE)
        display_av <- gsub("[[:space:]]+PIKO 1[[:space:]]*$",       "", display_av, ignore.case = TRUE)
        display_av <- gsub("[[:space:]]+PIKO 2[[:space:]]*$",       "", display_av, ignore.case = TRUE)
        display_av <- trimws(display_av)
      } else if (identical(facility, "KBCC")) {
        if (grepl("^FBB1\\b", display_av, ignore.case = TRUE)) {
          display_av <- gsub("[[:space:]]*\\(PIKO 1\\)[[:space:]]*$", "", display_av, ignore.case = TRUE)
          display_av <- "FBB1 (PIKO 1)"
        }
      }
      
      if (grepl("^FBB1\\b", display_av, ignore.case = TRUE)) {
        hint_vec <- hint_vec[ !grepl("^\\(?[[:space:]]*PIKO 1[[:space:]]*\\)?$", hint_vec, ignore.case = TRUE) ]
        hint_vec <- gsub("\\(?[[:space:]]*PIKO 1[[:space:]]*\\)?", "", hint_vec, ignore.case = TRUE)
        hint_vec <- trimws(hint_vec)
        hint_vec <- hint_vec[nzchar(hint_vec)]
      }
      
      hint <- if (length(hint_vec)) paste(sort(hint_vec), collapse = "/") else NA_character_
      
      # ----- PREP TABLE DATA (parallel to make_aviary_gt) -----
      df2 <- df %>%
        dplyr::mutate(
          Enclosure = purrr::map_chr(`Bird moved to enclosure ID:`, ~ normalize_enclosure(.x) %||% NA_character_),
          Enclosure = gsub("INNER CHAMBER,? ?OUTER CHAMBER", "IN/OUT", Enclosure, ignore.case = TRUE),
          Enclosure = gsub("INNER CHAMBER", "INNER", Enclosure, ignore.case = TRUE),
          Enclosure = gsub("OUTER CHAMBER", "OUTER", Enclosure, ignore.case = TRUE),
          Name      = ifelse(is.na(`Name:`), "", as.character(`Name:`)),
          Name      = substr(Name, 1, if (isTRUE(compact)) 10 else 12),
          Bands     = ifelse(is.na(`Short color bands:`), "", as.character(`Short color bands:`)),
          Bands     = gsub("[[:space:]]+", " ", Bands),
          Moved     = format_move_mmddyy(`Corrected date & time for data export:`)
        ) %>%
        dplyr::arrange(Enclosure, dplyr::desc(`Studbook number:`)) %>%
        dplyr::group_by(Enclosure) %>%
        dplyr::mutate(.last_in_encl = dplyr::row_number() == dplyr::n()) %>%
        dplyr::ungroup()
      
      # Same MBCC quirks / ordering as in make_aviary_gt()
      facility_here <- unique(as.character(df2$`Facility:`))[1] %||% NA_character_
      aviary_title  <- pretty_name(display_av)
      
      first_token <- function(x){
        rest <- sub("^[^/]+/?", "", x)
        if (identical(rest, x) || !nzchar(rest)) return(NA_character_)
        sub(",.*$", "", rest)
      }
      
      if (identical(facility_here, "MBCC") && grepl("^FBB1\\b", aviary_title, ignore.case = TRUE)) {
        tail <- sub("^[^/]+/", "", df2$Enclosure)
        letters_only <- toupper(gsub("[^A-Za-z]", "", tail))
        lett_key <- ifelse(
          nzchar(letters_only),
          substring(letters_only, nchar(letters_only), nchar(letters_only)), ""
        )
        get_min_num <- function(s) {
          m <- gregexpr("[0-9]+", s, perl = TRUE)
          v <- suppressWarnings(as.integer(unlist(regmatches(s, m))))
          v <- v[!is.na(v)]
          if (length(v)) min(v) else Inf
        }
        num_key <- vapply(tail, get_min_num, numeric(1))
        df2 <- df2 %>%
          dplyr::mutate(.lett_key = lett_key, .num_key = num_key) %>%
          dplyr::arrange(dplyr::desc(.lett_key), .num_key, Enclosure, dplyr::desc(`Studbook number:`)) %>%
          dplyr::select(-.lett_key, -.num_key)
      } else if (identical(facility_here, "MBCC") && grepl("^FBB(2|3)\\b", aviary_title, ignore.case = TRUE)) {
        num_key <- suppressWarnings(as.integer(sub("^[^/]+/(\\d+).*", "\\1", df2$Enclosure)))
        num_key[is.na(num_key)] <- Inf
        df2 <- df2 %>%
          dplyr::mutate(.num_key = num_key) %>%
          dplyr::arrange(.num_key, Enclosure, dplyr::desc(`Studbook number:`)) %>%
          dplyr::select(-.num_key)
      } else {
        tok   <- vapply(df2$Enclosure, first_token, character(1))
        num   <- suppressWarnings(as.integer(tok))
        isnum <- !is.na(num)
        df2 <- df2 %>%
          dplyr::mutate(.tok = tok, .num = ifelse(isnum, num, Inf), .isn = isnum) %>%
          dplyr::arrange(dplyr::desc(.isn), .num, .tok, dplyr::desc(`Studbook number:`)) %>%
          dplyr::select(-.tok, -.num, -.isn)
      }
      
      # Final tidy output for Excel; repeat display_av + hint on each row
      df2 %>%
        dplyr::transmute(
          Facility   = `Facility:`,
          Species    = Species,
          Aviary     = display_av,
          String     = hint,
          `Aviary/Ch`= Enclosure,
          `SB#`      = `Studbook number:`,
          Sex        = `Sex:`,
          Name       = Name,
          Bands      = Bands,
          Moved      = Moved
        )
    }
    
    # Build + rbind for all aviaries in this tab
    df_all <- purrr::map_dfr(aviary_names, build_aviary_df)
    df_all
  }
  
  
  ui_grid <- function(gt_list, n_cols = 2){
    if(!length(gt_list)) return(NULL)
    idx <- order_for_ui_grid(length(gt_list), n_cols)
    gt_list <- gt_list[idx]
    tagList(
      div(
        class = "two-col-grid",
        style = sprintf("--cols:%d;", max(1, n_cols)),
        lapply(gt_list, function(gtt) div(as.tags(gtt)))
      )
    )
  }
  
  # Live UI (KBCC forest birds = 1 column)
  output$grid_kbcc_al <- renderUI(ui_grid(make_tables("KBCC", TRUE,  compact = FALSE), n_cols = 2))
  output$grid_kbcc_fb <- renderUI(ui_grid(make_tables("KBCC", FALSE, compact = FALSE), n_cols = 1))
  output$grid_mbcc_al <- renderUI(ui_grid(make_tables("MBCC", TRUE,  compact = FALSE), n_cols = 2))
  output$grid_mbcc_fb <- renderUI(ui_grid(make_tables("MBCC", FALSE, compact = FALSE), n_cols = 2))
  
  fmt_date <- function(d) format(as.Date(d), "%B %d, %Y")
  
  output$title_kbcc_al <- renderText({
    paste0("KBCC — ‘Alalā locations as of ", fmt_date(input$subset_date))
  })
  output$title_kbcc_fb <- renderText({
    paste0("KBCC — Small forest bird locations as of ", fmt_date(input$subset_date))
  })
  output$title_mbcc_al <- renderText({
    paste0("MBCC — ‘Alalā locations as of ", fmt_date(input$subset_date))
  })
  output$title_mbcc_fb <- renderText({
    paste0("MBCC — Small forest bird locations as of ", fmt_date(input$subset_date))
  })
  
  # ---------- Helper: build HTML layout for PDF ----------
  # ---- HTML builder for a single-page A4 with explicit columns ----
  grid_html_for_pdf <- function(
    gt_list,
    doc_title,
    n_cols           = 2,
    col_align        = if (n_cols <= 1) "left" else "initial",
    margin_top_mm    = 2,
    margin_right_mm  = 2,
    margin_bottom_mm = 1,
    margin_left_mm   = 2,
    title_left_mm    = 11,
    col_gap_px       = 2,
    footer_text      = paste("Generated:", format(today_hst(), "%m/%d/%Y"))
  ) {
    n <- length(gt_list)
    if (n == 0) {
      stop("grid_html_for_pdf(): gt_list is empty.")
    }
    
    # IMPORTANT: keep original order of gt_list (no reordering)
    # If you had order_for_pdf_columns(), it is now intentionally *not* used.
    
    # Rough layout + scaling so tables fit on one A4 page
    rows       <- if (n_cols <= 1) n else ceiling(max(1, n) / n_cols)
    content_px <- rows * 118 + max(0, rows - 1) * 2
    page_h_px  <- 842 * 1.3333  # A4 @ ~96 dpi
    scale      <- min(1, page_h_px / max(1, content_px))
    scale      <- max(0.28, scale * 0.985)
    
    columns_style <- if (n_cols <= 1) {
      "display:inline-block; width:auto; text-align:left;"
    } else {
      "display:block; width:100%; text-align:initial;"
    }
    
    htmlTemplate(
      text_ = "<!DOCTYPE html>
<html>
<head>
<meta charset='utf-8'>
<style>
  @page {
    size: A4 portrait;
    margin-top: {{mt}}mm;
    margin-right: {{mr}}mm;
    margin-bottom: {{mb}}mm;
    margin-left: {{ml}}mm;
  }

  :root { --gap: {{gap}}px }

  body {
    font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial;
    margin:0;
  }

  @media print {
    * {
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
    }
    .gt_table, .gt_table * {
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
    }
  }

  header.title {
    position:fixed;
    top:3mm;
    left:{{title_left}}mm;
    right:8mm;
    text-align:left;
    font-weight:700;
    font-size:16px;
  }

  .content {
    position:relative;
    margin-top:8mm;
    margin-bottom:0mm;
  }

  .wrap {
    transform-origin: top left;
    text-align: left;
  }

  .columns {
    column-count: {{ncols}};
    column-gap: var(--gap);
    column-fill: auto;
    text-align: {{col_align}};
  }

  .gt_table {
    margin-top:0 !important;
    margin-bottom:0 !important;
    break-inside:avoid-column;
    page-break-inside:auto;
    width:auto;
    max-width:none;
    table-layout:fixed;
    border-collapse:collapse;
  }

  .gt_table td, .gt_table th {
    word-break:break-word;
    white-space:normal;
  }

  .gt_table .gt_col_headings {
    font-size:10.5px;
    line-height:1.05;
  }

  .gt_table .gt_row {
    font-size:10.5px;
    line-height:1.05;
  }

  .gt_table .gt_title {
    font-size:10.5px;
    line-height:1.05;
    margin:0;
    padding:0;
  }

  .gt_table .gt_col_headings,
  .gt_table .gt_row,
  .gt_table .gt_title,
  .gt_table .gt_heading {
    padding-top:0 !important;
    padding-bottom:0 !important;
  }

  .gt_table th,
  .gt_table td {
    padding-top:0 !important;
    padding-bottom:0 !important;
  }

  /* Footer at bottom of page */
  footer.footer {
    position: fixed;
    bottom: 4mm;
    left: {{ml}}mm;
    right: 8mm;
    font-size: 10px;
    color: #000000;
    text-align: right;
  }
</style>
</head>
<body>
  <header class='title'>{{doc_title}}</header>
  <div class='content'>
    <div class='wrap'
         style='transform: scale({{scale}}); width: calc(100%/{{scale}});'>
      <div class='columns' style='{{columns_style}}'>{{content}}</div>
    </div>
  </div>
  <footer class='footer'>{{footer_text}}</footer>
</body>
</html>",
      content       = tagList(lapply(gt_list, function(gtt) as.tags(gtt))),
      scale         = sprintf("%.3f", scale),
      doc_title     = doc_title,
      ncols         = max(1, n_cols),
      gap           = col_gap_px,
      col_align     = col_align,
      mt            = margin_top_mm,
      mr            = margin_right_mm,
      mb            = margin_bottom_mm,
      ml            = margin_left_mm,
      title_left    = title_left_mm,
      columns_style = columns_style,
      footer_text   = footer_text
    )
  }
  
  
  # ---------- HTML -> PDF (Docker = Chrome CLI, local = pagedown/webshot2) ----------
  do_pdf <- function(
    gt_list, file, doc_title, n_cols = 2,
    margin_top_mm = 2, margin_right_mm = 2, margin_bottom_mm = 2, margin_left_mm = 2,
    title_left_mm = 11, col_gap_px = 3
  ) {
    page <- grid_html_for_pdf(
      gt_list, doc_title,
      n_cols           = n_cols,
      col_align        = if (n_cols <= 1) "left" else "initial",
      margin_top_mm    = margin_top_mm,
      margin_right_mm  = margin_right_mm,
      margin_bottom_mm = margin_bottom_mm,
      margin_left_mm   = margin_left_mm,
      title_left_mm    = title_left_mm,
      col_gap_px       = col_gap_px
    )
    
    out_html <- tempfile(fileext = ".html")
    out_pdf  <- tempfile(fileext = ".pdf")
    htmltools::save_html(page, out_html)
    
    on.exit({
      if (file.exists(out_html)) unlink(out_html)
      if (file.exists(out_pdf))  unlink(out_pdf)
    }, add = TRUE)
    
    # Detect if we're inside Docker/Render
    in_docker <- file.exists("/.dockerenv")
    
    if (in_docker) {
      # ----- Docker / Render path: use Chrome CLI shim -----
      chrome <- "/usr/local/bin/google-chrome"  # shim from Dockerfile
      message("[do_pdf] (Docker) chrome path = ", chrome)
      
      if (!nzchar(chrome) || !file.exists(chrome)) {
        stop("In Docker but /usr/local/bin/google-chrome not found; cannot render PDF.")
      }
      
      args <- c(
        "--headless",
        "--disable-gpu",
        "--no-sandbox",
        "--disable-dev-shm-usage",
        "--disable-software-rasterizer",
        "--print-to-pdf-no-header",
        paste0("--print-to-pdf=", shQuote(out_pdf)),
        shQuote(out_html)
      )
      
      message("[do_pdf] (Docker) running: ", chrome, " ", paste(args, collapse = " "))
      res <- try(
        system2(chrome, args, stdout = TRUE, stderr = TRUE),
        silent = TRUE
      )
      message("[do_pdf] (Docker) chrome stdout/stderr:\n", paste(res, collapse = "\n"))
      
      if (!file.exists(out_pdf) || file.info(out_pdf)$size < 1024) {
        sz <- if (file.exists(out_pdf)) file.info(out_pdf)$size else NA
        stop(
          sprintf(
            "Chrome did not create a valid PDF in Docker (size = %s). See [do_pdf] logs above.",
            as.character(sz)
          )
        )
      }
      
    } else {
      # ----- Local (RStudio/mac) path: pagedown + webshot2 -----
      tried_pagedown <- FALSE
      ok <- FALSE
      
      if (requireNamespace("pagedown", quietly = TRUE)) {
        ch <- .find_chrome()
        if (nzchar(ch)) {
          options(pagedown.chrome = ch)
          Sys.setenv(PAGEDOWN_CHROMIUM = ch)
        }
        tried_pagedown <- TRUE
        message("[do_pdf] (local) trying pagedown::chrome_print() with ", ch)
        
        res <- try({
          pagedown::chrome_print(input = out_html, output = out_pdf, timeout = 180)
          file.exists(out_pdf) && file.info(out_pdf)$size > 1024
        }, silent = TRUE)
        
        ok <- isTRUE(res)
        if (!ok) {
          message("[do_pdf] (local) pagedown error: ", paste0(res, collapse = " "))
        }
      }
      
      if (!ok && requireNamespace("webshot2", quietly = TRUE)) {
        message("[do_pdf] (local) trying webshot2::webshot() fallback")
        
        res2 <- try({
          webshot2::webshot(
            url   = paste0("file://", normalizePath(out_html, winslash = "/")),
            file  = out_pdf,
            vwidth = 1200, vheight = 1600, zoom = 1
          )
          file.exists(out_pdf) && file.info(out_pdf)$size > 1024
        }, silent = TRUE)
        
        ok <- isTRUE(res2)
        if (!ok) {
          message("[do_pdf] (local) webshot2 error: ", paste0(res2, collapse = " "))
        }
      }
      
      if (!ok) {
        msg <- if (!tried_pagedown) {
          "Neither 'pagedown' nor 'webshot2' is installed. Install one of them to enable PDF export:\ninstall.packages(c('pagedown','webshot2'))"
        } else {
          paste(
            "Failed to render PDF with pagedown and webshot2.",
            "Check the logs for '[do_pdf] (local) pagedown error' or '[do_pdf] (local) webshot2 error' messages.",
            sep = "\n"
          )
        }
        stop(msg)
      }
    }
    
    # Copy temp PDF into the download target
    if (!file.copy(out_pdf, file, overwrite = TRUE)) {
      stop("Failed to write PDF to the download target.")
    }
    
    invisible(TRUE)
  }
  

  
  # ---------- Download handlers ----------
  output$dl_xlsx_kbcc_al <- downloadHandler(
    filename = function() {
      paste0("KBCC Alala Locations as of ",
             format(as.Date(input$subset_date), "%Y.%m.%d"),
             ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      df <- excel_df_for("KBCC", species_is_al = TRUE, compact = TRUE)
      writexl::write_xlsx(df, path = file)
    }
  )
  
  output$dl_xlsx_kbcc_fb <- downloadHandler(
    filename = function() {
      paste0("KBCC Forest Bird Locations as of ",
             format(as.Date(input$subset_date), "%Y.%m.%d"),
             ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      df <- excel_df_for("KBCC", species_is_al = FALSE, compact = TRUE)
      writexl::write_xlsx(df, path = file)
    }
  )
  
  output$dl_xlsx_mbcc_al <- downloadHandler(
    filename = function() {
      paste0("MBCC Alala Locations as of ",
             format(as.Date(input$subset_date), "%Y.%m.%d"),
             ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      df <- excel_df_for("MBCC", species_is_al = TRUE, compact = TRUE)
      writexl::write_xlsx(df, path = file)
    }
  )
  
  output$dl_xlsx_mbcc_fb <- downloadHandler(
    filename = function() {
      paste0("MBCC Forest Bird Locations as of ",
             format(as.Date(input$subset_date), "%Y.%m.%d"),
             ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      df <- excel_df_for("MBCC", species_is_al = FALSE, compact = TRUE)
      writexl::write_xlsx(df, path = file)
    }
  )
  
  
  output$dl_pdf_kbcc_al <- downloadHandler(
    filename = function() {
      paste0("KBCC Alala Locations as of ", format(as.Date(input$subset_date), "%Y.%m.%d"), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file){
      message("[downloadHandler dl_pdf_kbcc_al] Starting. file = ", file)
      do_pdf(
        make_tables("KBCC", TRUE, compact = TRUE),
        file,
        sprintf("KBCC — ‘Alalā locations as of %s", format(as.Date(input$subset_date), "%B %d, %Y")),
        n_cols = 2,
        margin_top_mm = 2, margin_right_mm = 2, margin_bottom_mm = 2, margin_left_mm = 2,
        title_left_mm = 9
      )
      message("[downloadHandler dl_pdf_kbcc_al] Finished.")
    }
  )
  
  output$dl_pdf_kbcc_fb <- downloadHandler(
    filename = function() {
      paste0("KBCC Forest Bird Locations as of ", format(as.Date(input$subset_date), "%Y.%m.%d"), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file){
      message("[downloadHandler dl_pdf_kbcc_fb] Starting. file = ", file)
      do_pdf(
        make_tables("KBCC", FALSE, compact = TRUE),
        file,
        sprintf("KBCC — Small forest bird locations as of %s", format(as.Date(input$subset_date), "%B %d, %Y")),
        n_cols = 1,
        margin_top_mm = 12, margin_right_mm = 14, margin_bottom_mm = 12, margin_left_mm = 25,
        title_left_mm = 0
      )
      message("[downloadHandler dl_pdf_kbcc_fb] Finished.")
    }
  )
  
  output$dl_pdf_mbcc_al <- downloadHandler(
    filename = function() {
      paste0("MBCC Alala Locations as of ", format(as.Date(input$subset_date), "%Y.%m.%d"), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file){
      message("[downloadHandler dl_pdf_mbcc_al] Starting. file = ", file)
      do_pdf(
        make_tables("MBCC", TRUE, compact = TRUE),
        file,
        sprintf("MBCC — ‘Alalā locations as of %s", format(as.Date(input$subset_date), "%B %d, %Y")),
        n_cols = 1,
        margin_top_mm = 12, margin_right_mm = 14, margin_bottom_mm = 12, margin_left_mm = 25,
        title_left_mm = 0
      )
      message("[downloadHandler dl_pdf_mbcc_al] Finished.")
    }
  )
  
  output$dl_pdf_mbcc_fb <- downloadHandler(
    filename = function() {
      paste0("MBCC Forest Bird Locations as of ", format(as.Date(input$subset_date), "%Y.%m.%d"), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file){
      message("[downloadHandler dl_pdf_mbcc_fb] Starting. file = ", file)
      do_pdf(
        make_tables("MBCC", FALSE, compact = TRUE),
        file,
        sprintf("MBCC — Small forest bird locations as of %s", format(as.Date(input$subset_date), "%B %d, %Y")),
        n_cols = 1,
        margin_top_mm = 12, margin_right_mm = 14, margin_bottom_mm = 12, margin_left_mm = 25,
        title_left_mm = 0
      )
      message("[downloadHandler dl_pdf_mbcc_fb] Finished.")
    }
  )
  
  # Jump to first tab when both files are present
  observe({
    if (!is.null(input$moves_file) && !is.null(input$details_file)) {
      updateTabItems(session, "tabs", selected = "kbcc_al")
    }
  })
}



shinyApp(ui, server)
