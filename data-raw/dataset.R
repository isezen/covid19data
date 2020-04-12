library(usethis)
source("R/download.R")

url_lookup <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"
col_colasses <- c("integer", "factor",  "factor", "factor", "factor",
                  "factor", "factor", "factor", "numeric", "numeric",
                  "factor", "integer")
lookup <- read.csv(url_lookup, colClasses = col_colasses)
use_data(lookup, overwrite = TRUE)

# c19jhw <- c19jhl <- NULL
df <- download.c19("jh")
df2 <- df[!duplicated(df[,2:3]), 2:3]
names <- unique(df2[duplicated(df2[,1]), 1])
i <- lookup[which(lookup$Country_Region %in% names & lookup$Province_State == ""),]
i <- i[order(i$Country_Region),]
snames <- tolower(i$iso3)

for (i in 1:length(names)) {
  data_name <- tolower(paste0("c19.", gsub("[[:space:]]", "", snames[i])))
  df3 <- df[df$country_region == names[i],]
  eval(parse(text = paste0(data_name, "<- df[df$country_region == names[i],]")))
  eval(parse(text = sprintf("use_data(%s, overwrite = TRUE)", data_name)))
}

c19l <- reshape2::melt(df, id.vars = 1:3, variable.name = "type", value.name = "cases")
c19l <- aggregate(cases ~ type + date + country_region, c19l, sum)
c19l <- c19l[,c("date", "country_region", "type", "cases")]
c19l <- with(c19l, c19l[order(date, country_region, type),])

# Wide format for John-Hopkins data
c19 <- reshape2::dcast(c19l, date + country_region ~ type, value.var = "cases")
colnames(c19) <- tolower(colnames(c19))
c19$recovered[which(c19$recovered == 0)] <- NA
c19[!sapply(c19, is.finite)] <- NA

use_data(c19, c19l, overwrite = TRUE)

for (i in 1:length(names)) {
  data_name <- tolower(paste0("c19.", gsub("[[:space:]]", "", snames[i])))
  data_name2 <- tolower(paste0("c19l.", gsub("[[:space:]]", "", snames[i])))
  eval(parse(text = paste0("df <- ", data_name)))
  df <- reshape2::melt(df, id.vars = 1:3, variable.name = "type", value.name = "cases")
  df <- with(df, df[order(date, country_region, province_state, type),])
  eval(parse(text = paste0(data_name2, " <- df")))
  df3 <- c19[c19$country_region == names[i],]
  eval(parse(text = sprintf("use_data(%s, overwrite = TRUE)", data_name2)))
}

# -------------

cmd <- "write.csv2(%s, 'data-raw/%s.csv', row.names = FALSE, quote = FALSE, na = '')"
for (data_name in c("c19")) {
  eval(parse(text = sprintf(cmd, data_name, data_name)))
}

# dfu <- update.c19jh()
# dfu <- dfu[dfu$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]
#
# dfo <- c19jh_w[c19jh_w$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]

