
remove_uniq_cols <- function(df) {
  df[,apply(df, 2, function(x) length(unique(x)) != 1)]
}

read_url_jh <- function(from = "2020-01-22",
                        to = as.character(Sys.Date())) {
  from <- as.Date(from)
  to <- as.Date(to)
  url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/"
  url <- paste0(url, "master/csse_covid_19_data/csse_covid_19_daily_reports/")
  url <- paste0(url, "%s.csv")
  df <- lapply(strftime(seq.Date(from, to, 1), format = "%m-%d-%Y"),
               function(dt) {
                 cat("read: ", dt, "\n")
                 tryCatch({read.csv(sprintf(url, dt))},
                          error = function(e){}, warning = function(w){})
               })
  df[sapply(df, is.null)] <- NULL
  if (length(df) == 0) return(NULL)
  df <- lapply(df, function(x) {
    fmt <- ifelse(all(grepl("/", x$Last.Update, fixed = TRUE)),
                  "%m/%d/%Y %H:%M", "%Y-%m-%dT%H:%M:%S")
    x$Last.Update <- as.POSIXct(as.POSIXlt(x$Last.Update, "UTC", fmt))
    return(x)
  })
  loc <- do.call(rbind, lapply(df[sapply(df, ncol) == 8],
                               function(x) x[,c(1:2, 7:8)]))
  loc <- loc[!duplicated(loc[,1:2]),]
  df <- do.call(rbind, lapply(df, function(x) x[,1:6]))
  for (i in 1:2) {
    df[,i] <- trimws(df[,i])
    loc[,i] <- trimws(loc[,i])
  }
  df <- merge(df, loc)
  df <- with(df, df[order(Country.Region, Province.State, Last.Update),])
  for (i in 1:2) {
    df[df[,i] == "", i] <- NA
    df[,i] <- factor(df[,i])
  }
  lubridate::year(df[lubridate::year(df[,3]) == 20, 3]) <- 2020
  df <- df[!duplicated(df),]
  colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)
  colnames(df) <- gsub("Last_Update", "date", colnames(df), fixed = TRUE)
  colnames(df) <- gsub("Latitude", "lat", colnames(df), fixed = TRUE)
  colnames(df) <- gsub("Longitude", "long", colnames(df), fixed = TRUE)
  df2 <- reshape2::melt(df,
                        measure.vars = c("Confirmed", "Deaths", "Recovered"),
                        variable.name = "type", value.name = "cases")
  colnames(df2) <- tolower(colnames(df2))
  df2 <- df2[, c("date", "country_region", "province_state", "type", "cases",
                 "lat", "long")]
  df2 <- df2[!duplicated(df2),]
  return(df2)
}

read_data <- function(from = c("dworld", "ramikrispin")) {
  from <- match.arg(from, c("dworld", "ramikrispin"))
  url <- switch(
    from,
    "dworld" = "https://query.data.world/s/igmopqfux3jq3omp6tl6fsabldvcnf",
    "ramikrispin" = "https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv")
  df <- read.csv(url, stringsAsFactors = TRUE)
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)
  colnames(df) <- gsub("case_type", "type", colnames(df), fixed = TRUE)
  i <- which(colnames(df) == "date")
  if (length(i) == 1 && i > 1) df <- cbind(df[,i, drop = FALSE], df[,-i])
  if (from == "data.world") {
    df$date <- as.Date(df$date, "%m/%d/%Y")
  } else {
    df$date <- as.Date(df$date)
  }
  substring(levels(df$type), 1) <- toupper(substring(levels(df$type), 1, 1))
  df <- df[order(df$country_region, df$date),]
  df <- remove_uniq_cols(df)
  df <- df[, c("date", "country_region", "province_state", "type", "cases",
               "lat", "long")]
  df$province_state <- as.character(df$province_state)
  df$province_state[df$province_state == "N/A"] <- ""
  df <- df[!duplicated(df),]
  df$province_state <- factor(df$province_state)
  rownames(df) <- NULL
  return(df)
}

#' Download Covid19 data
#'
#' @export
download.c19 <- function(from = c("dworld", "ramikrispin", "jh")) {
  from <- match.arg(from, c("dworld", "ramikrispin", "jh"))
  df <- if (from == "jh") read_url_jh() else read_data(from)
  return(df)
}

#' Update c19jh dataset
#'
#' If dataset is outdated, updates c19jh dataset.
#'
#' @usage update.c19jh()
#' @export update.c19jh
update.c19jh <- function() {
  data(c19jh)
  max_date <- as.Date(max(c19jh$date))
  today <- Sys.Date()
  if (max_date < today) {
    df <- read_url_jh(max_date, today)
    if (!is.null(df)) {
      c19jh <- rbind(c19jh, df)
    }
    c19jh <- c19jh[!duplicated(c19jh),]
    c19jh <- c19jh[order(c19jh$country_region, c19jh$province_state,
                         c19jh$date),]
  }
  invisible(NULL)
}