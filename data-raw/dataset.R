library(usethis)
source("R/download.R")

# download from scratch
for (d in c("dw", "ra", "jh")) {
  data_name <- paste0("c19", d)
  data_name <- paste0(data_name, ifelse(d == "jh", "_w", "_l"))
  eval(parse(text = paste0(data_name, "<- download.c19('", d, "')")))
  eval(parse(text = sprintf("use_data(%s, overwrite = TRUE)", data_name)))
}

# long format for John-Hopkins data
cn <- colnames(c19jh_w)[8:11]
substring(cn, 1) <- toupper(substring(cn, 1, 1))
colnames(c19jh_w)[8:11] <- cn
c19jh_l <- reshape2::melt(c19jh_w,
                      measure.vars = c("Confirmed", "Deaths", "Recovered", "Active"),
                      variable.name = "type", value.name = "cases")
c19jh_l <- with(c19jh_l, c19jh_l[order(country_region, province_state,
                                       admin2, date, type),])

# wide format for data.world
c19dw_w <- reshape2::dcast(c19dw_l, date + country_region + province_state +
                             lat + long ~ type, value.var = "cases")
c19dw_w <- with(c19dw_w, c19dw_w[order(country_region, province_state, date),])
c19ra_w <- reshape2::dcast(c19ra_l, date + country_region + province_state +
                             lat + long ~ type, value.var = "cases")
c19ra_w <- with(c19ra_w, c19ra_w[order(country_region, province_state, date),])
use_data(c19jh_l, c19dw_w, c19ra_w, overwrite = TRUE)
# -------------

cn <- c("date", "country_region", "province_state", "type", "cases",
        "lat", "long")
filename <- 'data-raw/c19tr_w.csv'
c19tr_w <- read.csv2(filename, na.strings = "",
                   colClasses = c("POSIXct", "integer", "integer",
                                "integer", "integer"))
c19tr_l <- reshape2::melt(c19tr_w, id.vars = c("date"), variable.name = "type", value.name = "cases")
c19tr_l$country_region <- "Turkey"
c19tr_l$province_state <- NA
c19tr_l$lat <- 38.9637
c19tr_l$long <- 35.2433
c19tr_l <- c19tr_l[,cn]
c19tr_l <- c19tr_l[order(c19tr_l$date, c19tr_l$type),]
c19tr_l$country_region <- factor(c19tr_l$country_region)
c19tr_l$province_state <- factor(c19tr_l$province_state)
c19tr_l$type <- factor(c19tr_l$type)

# write.csv2(c19tr_l, "data-raw/c19tr_l.csv", row.names = FALSE, quote = FALSE, na = '')
use_data(c19tr_w, c19tr_l, overwrite = TRUE)

cmd <- "write.csv2(%s, 'data-raw/%s.csv', row.names = FALSE, quote = FALSE, na = '')"
for (data_name in list("c19dw_w", "c19ra_w", "c19jh_w")) {
  eval(parse(text = sprintf(cmd, data_name, data_name)))
}

# dfu <- update.c19jh()
# dfu <- dfu[dfu$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]
#
# dfo <- c19jh_w[c19jh_w$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]

