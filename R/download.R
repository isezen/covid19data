
remove_uniq_cols <- function(df) {
  df[,apply(df, 2, function(x) length(unique(x)) != 1)]
}

read_url_jh <- function(from = "2020-01-22",
                        to = as.character(Sys.Date())) {
  cn <- c("date", "fips", "country_region", "province_state", "admin2",
          "lat", "long", "confirmed", "deaths", "recovered", "active")
  from <- as.Date(from)
  to <- as.Date(to)
  url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/"
  url <- paste0(url, "master/csse_covid_19_data/csse_covid_19_daily_reports/")
  url <- paste0(url, "%s.csv")
  df <- lapply(strftime(seq.Date(from, to, 1), format = "%m-%d-%Y"),
               function(dt) {
                 cat("read: ", dt, "\n")
                 tryCatch({read.csv(sprintf(url, dt), stringsAsFactors = FALSE,
                                    strip.white = TRUE, na.strings = "")},
                          error = function(e){}, warning = function(w){})
               })
  df[sapply(df, is.null)] <- NULL
  if (length(df) == 0) return(NULL)
  df <- lapply(df, function(x) {
    colnames(x) <- tolower(colnames(x))
    colnames(x) <- gsub(".", "_", colnames(x), fixed = TRUE)
    colnames(x)[startsWith(colnames(x), "lat")] <- "lat"
    colnames(x)[startsWith(colnames(x), "long")] <- "long"
    colnames(x)[startsWith(colnames(x), "last")] <- "date"
    if (!("fips" %in% colnames(x))) x$fips <- NA
    if (!("admin2" %in% colnames(x))) x$admin2 <- NA
    if (!("active" %in% colnames(x))) x$active <- NA
    if (!("lat" %in% colnames(x))) x$lat <- NA
    if (!("long" %in% colnames(x))) x$long <- NA
    if ("combined_key" %in% colnames(x)) x <- subset(x, select = -combined_key)
    x <- x[,cn]
    if (all(grepl("/", x$date, fixed = TRUE))) {
      fmt <- "%m/%d/%Y %H:%M"
    } else if (all(grepl("T", x$date, fixed = TRUE))) {
      fmt <- "%Y-%m-%dT%H:%M:%S"
    } else {
      fmt <- "%Y-%m-%d %H:%M:%S"
    }
    x$date <- as.POSIXct(as.POSIXlt(x$date, "UTC", fmt))
    x$province_state[x$province_state == "None"] <- NA
    return(x)
  })
  loc <- do.call(rbind, lapply(df, function(x) x[c(2:7)]))
  loc <- loc[!duplicated(loc),]
  by <- lapply(as.list(loc[,1:4]), factor, exclude = NULL)
  a <- aggregate(1:nrow(loc), by, function(i) {
    x <- loc[i,, drop = FALSE]
    i <- which(!(is.na(x$lat) & is.na(x$long)))
    if (length(i) > 0) {
      x <- x[i,, drop = FALSE]
      x <- x[1,, drop = FALSE]
    }
    return(x)
  }, simplify = FALSE)
  loc <- do.call(rbind, a$x)

  df2 <- do.call(rbind, df)
  df2 <- merge(df2, loc)
  df2 <- df2[,cn]
  df2 <- with(df2, df2[order(country_region, province_state, admin2, date),])
  for (i in 3:5) df2[,i] <- factor(df2[,i])
  lubridate::year(df2[lubridate::year(df2$date) == 20, 1]) <- 2020
  df2 <- df2[!duplicated(df2),]
  by <- lapply(df2[,c(1:7)], factor, exclude = NULL)
  a <- aggregate(1:nrow(df2), by, function(i) {
    x <- df2[i, 8:11]
    if (length(i) > 1) {
      return(apply(x, 2, function(r) {
        if (all(is.na(r))) return(NA)
        return(max(r, na.rm = TRUE))
      }))
    }
    return(x)
  })
  b <- t(apply(a$x, 1, unlist))
  b <- cbind(a[,1:7], b)
  b$date <- as.POSIXct(b$date)
  b$fips <- as.integer(as.character(b$fips))
  b$lat <- as.numeric(as.character(b$lat))
  b$long <- as.numeric(as.character(b$long))
  b$province_state <- factor(b$province_state)
  b$admin2 <- factor(b$admin2)
  b <- with(b, b[order(country_region, province_state, admin2, date),])
  return(b)
}

read_data <- function(from = c("dworld", "ramikrispin")) {
  from <- match.arg(from, c("dworld", "ramikrispin"))
  url <- switch(
    from,
    "dworld" = "https://query.data.world/s/igmopqfux3jq3omp6tl6fsabldvcnf",
    "ramikrispin" = "https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv")
  df <- read.csv(url, stringsAsFactors = FALSE, strip.white = TRUE)
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)
  colnames(df) <- gsub("case_type", "type", colnames(df), fixed = TRUE)
  df <- remove_uniq_cols(df)
  df$province_state[df$province_state == "N/A"] <- ""
  df <- df[, c("date", "country_region", "province_state", "type", "cases",
               "lat", "long")]
  # handle duplicated records
  df <- df[!duplicated(df[,c("date", "country_region", "province_state", "type", "cases")]),]
  by <- df[, c("date", "country_region", "province_state", "type")]
  a <- aggregate(1:nrow(df), by, function(i) {
    df2 <- df[i,,drop = FALSE]
    if (nrow(df2) > 1) {
      df2[1,5] <- sum(df2[,5])
      return(df2[1,, drop = FALSE])
    } else  return(df2)
  }, simplify = FALSE)
  df <- do.call(rbind, a$x)

  i <- which(colnames(df) == "date")
  if (length(i) == 1 && i > 1) df <- cbind(df[,i, drop = FALSE], df[,-i])
  if (from == "dworld") {
    df$date <- as.Date(df$date, "%m/%d/%Y")
  } else {
    df$date <- as.Date(df$date)
  }
  substring(df$type, 1) <- toupper(substring(df$type, 1, 1))
  df$country_region <- factor(df$country_region)
  df$province_state <- factor(df$province_state)
  df$type <- factor(df$type)
  df <- df[order(df$country_region, df$province_state, df$date, df$type),]
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