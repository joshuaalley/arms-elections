# Joshua Alley
# load and clean 655 reports on US export agreements

# key package
library(readxl)


# load each sheet 
excel_sheets("data/section-655-reports/data-2013-20.xlsx")


# function from 
multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- excel_sheets(fname)
  tibble <- lapply(sheets, function(x) read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

# specifying the path name
path <- "data/section-655-reports/data-2013-20.xlsx"
rep.655 <- multiplesheets(path)


# all data
rep.655.13 <- bind_rows(rep.655,
                     .id = "year")
colnames(rep.655.13) <- c("year", "country", "usml_cat", "quantity",
                       "authorized", "total_shipped")

# NA in usml cat is total: 
rep.655.13$usml_cat[is.na(rep.655.13$usml_cat)] <- "total"

rep.655.13 <- fill(rep.655.13, country, .direction = "down")

# ccode 
rep.655.13$country <- str_to_title(rep.655.13$country)

# remove IOs
rep.655.13 <- filter(rep.655.13, str_detect(rep.655.13$country, 
                             "International",
                             negate = TRUE))
# clean up further
rep.655.13$country <- trimws(rep.655.13$country, "right")
rep.655.13$country <- gsub("\r?\n|\r", " ", rep.655.13$country)

rep.655.13$ccode <- countrycode(sourcevar = rep.655.13$country,
                                origin = "country.name",
                                destination = "cown")

# manual ccode fix: mostly spelling 
rep.655.13$ccode[rep.655.13$country == "Cote D'voire"] <- 437
rep.655.13$ccode[rep.655.13$country == "Central Af Republic"] <- 482
rep.655.13$ccode[rep.655.13$country == "Hati"] <- 41
rep.655.13$ccode[rep.655.13$country == "United Arab"] <- 696
rep.655.13$ccode[rep.655.13$country == "Serbia"] <- 345

# countries only
rep.655.13$year <- as.numeric(rep.655.13$year)
rep.655.13 <- drop_na(rep.655.13, ccode) %>%
               left_join(us.trade.ally)

# authorized to numeric
rep.655.13$authorized <- as.numeric(gsub("[^[:alnum:]]", "", rep.655.13$authorized))
rep.655.13$total_shipped <- as.numeric(gsub("[^[:alnum:]]", "", rep.655.13$total_shipped))


# clear usml labels
table(rep.655.13$usml_cat)
rep.655.13$usml_det <- recode(rep.655.13$usml_cat,
                              "total" = "total",
                              "Agreements" = "agreements",
                              # consolidate w/ inconsistent reporting 
                              "I" = "firearms",
                              "I (a)" = "firearms",
                              "a" = "firearms",
                              "I (h)" = "firearms",
                              "I(a)" = "firearms",
                              "I(h)" = "firearms",
                              "II" = "armament",
                              "III" = "ammunition",
                              "IV" = "missile-bomb-mine",
                              "V" = "explosives",
                              "VI" = "ships",
                              "VII" = "ground-vehicles",
                              "VIII" = "aircraft",
                              "IX" = "mil-training",
                              "X" = "PPE",
                              "XI" = "electronics",
                              "XII" = "fire-control-sys", # maybe electronics too
                              "XIII" = "materials",
                              "XIV" = "chem-bio-agents",
                              "XV" = "spacecraft",
                              "XVI" = "nuclear",
                              "XVII" = "classified",
                              "XVIII" = "direct-energy",
                              "XIX" = "turbine",
                              "XX" = "submersible",
                              "XXI" = "other"
                              )
table(rep.655.13$usml_det)


# annual flow 
annual.655 <- rep.655.13 %>%
               drop_na(atop_defense) %>%
               group_by(year, usml_det, atop_defense) %>%
               summarize(
                 total_auth = sum(authorized, na.rm = TRUE),
                 total_shipped = sum(total_shipped, na.rm = TRUE),
                 .groups = "keep"
               ) %>%
              left_join(elections.data)


# plot
ggplot(annual.655, aes(x = year, y = total_auth,
                       group = factor(atop_defense),
                       color = factor(atop_defense))) +
  facet_wrap(~ usml_det, scales = "free_y") +
  geom_line()
