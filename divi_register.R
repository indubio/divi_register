library(tidyverse)
#library(RCurl)
#library(xml2)
#library(rvest)
#saveRDS(divi_data, "diviregister.Rds")
#divi_data <- readRDS("diviregister.Rds")
base_url <- "https://www.divi.de"
base_path <- "/divi-intensivregister-tagesreport-archiv-csv"
### get urls
divi_csvurls <- tibble(CSVURL = character(0))
i <- 0
repeat{
  url = url(paste0(base_url, base_path, "?layout=table&start=", i), "rb")
  page_content <- xml2::read_html(url)
  close(url)
  count_urls <- nrow(divi_csvurls)
  divi_csvurls <- divi_csvurls %>%
    bind_rows(
      as_tibble(
        rvest::html_attr(rvest::html_nodes(page_content, "a"), "href")
      ) %>% 
      filter(
        str_starts(value, paste0(base_path, "/viewdocument/"))
      ) 
    ) %>%
    distinct(value)
  ### break if no new urls
  if (count_urls == nrow(divi_csvurls)){break}
  i <- i + 20
}
rm(i, url, page_content, count_urls)

divi_csvurls <- divi_csvurls %>%
  mutate(
    value.date = as.Date(substr(value, 87, 96)),
    value.time = substr(value, 98, 102)
  ) %>%
  arrange(
    desc(value.date), desc(value.time)
  ) %>%
  distinct(
    value.date, .keep_all = TRUE
  )

### get csv data
divi_data <- tibble()
pb = txtProgressBar(min = 1, max = nrow(divi_csvurls), style = 3) 
for (i in 1:nrow(divi_csvurls)){
  setTxtProgressBar(pb, i)
  dummy <- tryCatch(
    {read_csv(RCurl::getURL(paste0(base_url, divi_csvurls[i,]$value)))},
    error = function(e) {tibble()},
    warning = function(w) {tibble()}
  )
  if (nrow(dummy) > 0){
    dummy$CSVDate <- divi_csvurls[i,]$value.date
    divi_data <- divi_data %>%
      bind_rows(
        dummy
      )
  }
}
close(pb)
rm(i, dummy, pb)

### simple plot
divi_data %>%
  group_by(
    CSVDate
  ) %>%
  summarise(
    bed.free = sum(betten_frei),
    bed.occupied = sum(betten_belegt),
    bed.sum = bed.free + bed.occupied
  ) %>%
  pivot_longer(
    cols = starts_with("bed."),
    names_to = "BedStatus",
    names_prefix = "bed."
  ) %>%
  ggplot(
    aes (x = CSVDate, y = value, colour = BedStatus)
  ) +
  geom_line()
