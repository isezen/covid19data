library(usethis)
source("R/download.R")

c19raw <- c19dww <- c19jhw <- NULL
c19ral <- c19dwl <- c19jhl <- NULL
# download from scratch
for (d in c("dw", "ra", "jh")) {
  data_name <- paste0("c19", d)
  data_name <- paste0(data_name, ifelse(d == "jh", "w", "l"))
  eval(parse(text = paste0(data_name, "<- download.c19('", d, "')")))
  eval(parse(text = sprintf("use_data(%s, overwrite = TRUE)", data_name)))
}

# long format for John-Hopkins data
cn <- colnames(c19jhw)[8:11]
substring(cn, 1) <- toupper(substring(cn, 1, 1))
colnames(c19jhw)[8:11] <- cn
c19jhl <- reshape2::melt(c19jhw,
                      measure.vars = c("Confirmed", "Deaths", "Recovered", "Active"),
                      variable.name = "type", value.name = "cases")
c19jhl <- with(c19jhl, c19jhl[order(country_region, province_state,
                                       admin2, date, type),])

# wide format for data.world
c19dww <- reshape2::dcast(c19dwl, date + country_region + province_state +
                             lat + long ~ type, value.var = "cases")
c19dww <- with(c19dww, c19dww[order(country_region, province_state, date),])
c19raw <- reshape2::dcast(c19ral, date + country_region + province_state +
                             lat + long ~ type, value.var = "cases")
c19raw <- with(c19raw, c19raw[order(country_region, province_state, date),])
use_data(c19jhl, c19dww, c19raw, overwrite = TRUE)
# -------------

cmd <- "write.csv2(%s, 'data-raw/%s.csv', row.names = FALSE, quote = FALSE, na = '')"
for (data_name in list("c19dww", "c19raw", "c19jhw")) {
  eval(parse(text = sprintf(cmd, data_name, data_name)))
}

# dfu <- update.c19jh()
# dfu <- dfu[dfu$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]
#
# dfo <- c19jh_w[c19jh_w$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]

