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

c19l <- aggregate(cases ~ type + date + country_region, df, sum)
c19l <- c19l[,colnames(df)[-c(3:5)]]

# Wide format for John-Hopkins data
c19 <- reshape2::dcast(c19l, date + country_region ~ type, value.var = "cases")
colnames(c19) <- tolower(colnames(c19))
use_data(c19l, c19, overwrite = TRUE)
# -------------

cmd <- "write.csv2(%s, 'data-raw/%s.csv', row.names = FALSE, quote = FALSE, na = '')"
for (data_name in c("c19")) {
  eval(parse(text = sprintf(cmd, data_name, data_name)))
}

# dfu <- update.c19jh()
# dfu <- dfu[dfu$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]
#
# dfo <- c19jh_w[c19jh_w$date > ISOdatetime(2020, 3, 24, 0, 0, 0),]

