library(tidyverse)
#saveRDS(divi_data, "diviregister.Rds")
#divi_data <- readRDS("diviregister.Rds")

### get urls
base_url <- "https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv"
divi_data <- tibble(CSVURL = character(0))
dummy <- tibble(CSVURL = character(0))
i <- 0

while(i == 0 || nrow(dummy) != 0){
  url = url(paste0(base_url, "?layout=table&start=", i), "rb")
  page_content <- xml2::read_html(url)
  close(url)
  dummy <- as_tibble(
    rvest::html_attr(rvest::html_nodes(page_content, "a"), "href")
  ) %>% 
    rename(CSVURL = value) %>%
    filter(
      str_starts(CSVURL, "/divi-intensivregister-tagesreport-archiv-csv/viewdocument/")
    )
  if (nrow(dummy) == 1){
    if (dummy[1,1] == "/divi-intensivregister-tagesreport-archiv-csv/viewdocument/3974/divi-intensivregister-2020-04-24-09-15"){
      dummy <- tibble(CSVURL = character(0))
    }
  }
  divi_data <- rbind(divi_data, dummy)
  i <- i + 20
}
rm(i, url, dummy, page_content)


divi_data <- divi_data %>%
  mutate(
    Datum = as.Date(substr(divi_data$CSVURL, 87,96)),
    CSVURL = paste0("https://www.divi.de", CSVURL)
  )

csvdata <- tibble()
for (i in 1:nrow(divi_data)){
  dummy <- read_csv(RCurl::getURL(divi_data[i,]$CSVURL))
  dummy$CSVDatum <- divi_data[i,]$Datum
  csvdata <- bind_rows(
    csvdata,
    dummy
  )
}
rm(i, dummy)

csvdata %>%
  group_by(
    CSVDatum
  ) %>%
  summarise(
    bedfree = sum(betten_frei),
    bedbelegt = sum(betten_belegt)
  ) %>%
  ggplot(
    aes (x=CSVDatum,)
  ) +
  geom_point(aes(y=bedfree), colour = "green")+
  geom_point(aes(y=bedbelegt), colour ="red")
