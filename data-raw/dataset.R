library(usethis)
source("R/download.R")

for (d in c("dw", "ra", "jh")) {
  data_name <- paste0("c19", d)
  eval(parse(text = paste0(data_name, "<- download.c19('", d, "')")))
  cmd <- "write.csv2(%s, 'data-raw/%s.csv', row.names = FALSE, quote = FALSE, na = '')"
  eval(parse(text = sprintf(cmd, data_name, data_name)))
  eval(parse(text = sprintf("use_data(%s, overwrite = TRUE)", data_name)))
}
# -------------

cn <- c("date", "country_region", "province_state", "type", "cases",
        "lat", "long")
add.row <- function(date, type, cases, ntest) {
  df <- data.frame(date, "Turkey", NA, type, cases, ntest, 38.9637, 35.2433)
  colnames(df) <- cn
  c19tr <- rbind(c19tr, df)
  return(c19tr)
}

# c19tr <- c19jh[c19jh$country_region == "Turkey",]
# c19tr <- c19tr[order(c19tr$date, c19tr$type),]
# c19tr$ntest <- NA
# c19tr <- c19tr[,cn]
# rownames(c19tr) <- NULL
# c19tr[c19tr$type == "Confirmed", "ntest"][9:10] <- c(1981, 5637)
filename <- 'data-raw/c19trw.csv'
c19trw <- read.csv2(filename, na.strings = "",
                   colClasses = c("POSIXct", "integer", "integer",
                                "integer", "integer"))
c19trl <- reshape2::melt(c19trw, id.vars = c("date"), variable.name = "type", value.name = "cases")
c19trl$country_region <- "Turkey"
c19trl$province_state <- NA
c19trl$lat <- 38.9637
c19trl$long <- 35.2433
c19trl <- c19trl[,cn]
c19trl <- c19trl[order(c19trl$date, c19trl$type),]

write.csv2(c19trl, "data-raw/c19trl.csv", row.names = FALSE, quote = FALSE, na = '')
use_data(c19trw, c19trl, overwrite = TRUE)

