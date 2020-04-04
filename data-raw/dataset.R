library(usethis)
source("R/download.R")

c19raw <- c19dww <- c19jhw <- NULL
c19ral <- c19dwl <- c19jhl <- NULL
# download from scratch
for (d in c("jh")) {
  data_name <- paste0("c19", d, "l")
  eval(parse(text = paste0(data_name, "<- download.c19('", d, "')")))
  eval(parse(text = sprintf("use_data(%s, overwrite = TRUE)", data_name)))
}

# Wide format for John-Hopkins data
c19jhw <- reshape2::dcast(c19jhl, date + country_region + province_state +
                            lat + long ~ type, value.var = "cases")
colnames(c19jhw) <- tolower(colnames(c19jhw))
use_data(c19jhw, overwrite = TRUE)
# -------------

cmd <- "write.csv2(%s, 'data-raw/%s.csv', row.names = FALSE, quote = FALSE, na = '')"
for (data_name in c("c19jhw")) {
  eval(parse(text = sprintf(cmd, data_name, data_name)))
}

# dfu <- update.c19jh()
# dfu <- dfu[dfu$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]
#
# dfo <- c19jh_w[c19jh_w$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]

