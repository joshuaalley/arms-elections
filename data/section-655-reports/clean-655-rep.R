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

# authorized to numeric and in millions
rep.655.13$authorized <- as.numeric(gsub("[^[:alnum:]]", "", rep.655.13$authorized)) /
                            1000000
rep.655.13$total_shipped <- as.numeric(gsub("[^[:alnum:]]", "", rep.655.13$total_shipped)) /
                             1000000


# clear usml labels
table(rep.655.13$usml_cat)
rep.655.13$usml_det <- recode(rep.655.13$usml_cat,
                              "total" = "total",
                              "Agreements" = "agreements",
                              # consolidate I w/ inconsistent reporting 
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

# consolidate to match contract sectors
rep.655.13$usml_cont <- NA
rep.655.13$usml_cont[rep.655.13$usml_det == "ships" |
                       rep.655.13$usml_det == "submersible"] <- "ships"

rep.655.13$usml_cont[rep.655.13$usml_det == "aircraft" |
                       rep.655.13$usml_det == "turbine"] <- "air"

rep.655.13$usml_cont[rep.655.13$usml_det == "missile-bomb-mine" |
                       rep.655.13$usml_det == "spacecraft"] <- "missile_space"

rep.655.13$usml_cont[rep.655.13$usml_det == "ground-vehicles"] <- "vehicles"

rep.655.13$usml_cont[rep.655.13$usml_det == "firearms" |
                       rep.655.13$usml_det == "ammunition" |
                       rep.655.13$usml_det == "armament" |
                       rep.655.13$usml_det == "explosives"] <- "arms"


rep.655.13$usml_cont[rep.655.13$usml_det == "electronics" |
                       rep.655.13$usml_det == "fire-control-sys"] <- "electronics"

rep.655.13$usml_cont[is.na(rep.655.13$usml_cont)] <- "other"

rep.655.13 <- drop_na(rep.655.13, ccode)


# summarize by full category
rep.655.cont <- rep.655.13 %>%
                 drop_na(usml_cont) %>%
                 group_by(year, ccode, usml_cont) %>%
                 summarize(
                   authorized = sum(authorized, na.rm = TRUE),
                   .groups = "keep"
                 ) 

# wide
rep.655.cont.wide <- rep.655.cont %>%
                     pivot_wider(id_cols = c("ccode", "year"),
                       names_from = "usml_cont",
                      values_from = "authorized")
rep.655.cont.wide[is.na(rep.655.cont.wide)] <- 0


# annual flow: ally/not by sector
annual.655.all <- rep.655.13 %>%
               left_join(select(us.trade.ally, ccode, year, atop_defense)) %>%
               drop_na(usml_cont) %>%
               group_by(year, usml_cont, atop_defense) %>%
               summarize(
                 total_auth = sum(authorized, na.rm = TRUE),
                 .groups = "keep"
               ) 


# annual flow: state partners
partner.655 <- rep.655.13 %>%
  group_by(year, ccode) %>%
  summarize( # summarize in billions (millions / 1000)
    total_auth = sum(authorized, na.rm = TRUE) / 1000,
    total_ship = sum(total_shipped, na.rm = TRUE) / 1000,
    .groups = "keep"
  ) 

# tack on to aggregates
us.trade.ally <- left_join(us.trade.ally, state.655)


# wide data by sector and trade
us.agg <- left_join(rep.655.cont.wide, us.trade.ally)


# summarize sector variables: wide
us.agg.all <- us.agg %>%
                group_by(atop_defense, year) %>% 
                drop_na(atop_defense) %>%
                summarise( # sum in billions
                  across(air:vehicles, ~ sum(.x, na.rm = TRUE) / 1000), 
                  .groups = "keep"
                  )
# rename wide cols for merging 
colnames(us.agg.all)[3:ncol(us.agg.all)] <- paste0(colnames(us.agg.all)[3:ncol(us.agg.all)], "_or")

us.agg.all <- us.agg.all %>%
            group_by(atop_defense) %>%
            mutate_at(vars(matches("_or")), 
              .funs = list(lag = lag,
              change = function(x) x - lag(x)))



# summarize sector variables: wide
us.agg.yr <- us.agg %>%
  group_by(year) %>% 
  summarise( # sum in billions
    across(air:vehicles, ~ sum(.x, na.rm = TRUE) / 1000), 
    .groups = "keep"
  )
# rename wide cols for merging 
colnames(us.agg.yr)[2:ncol(us.agg.yr)] <- paste0(colnames(us.agg.yr)[2:ncol(us.agg.yr)], "_or")

us.agg.yr <- us.agg.yr %>%
  ungroup() %>%
  mutate_at(vars(matches("_or")), 
            .funs = list(lag = lag,
            change = function(x) x - lag(x)))
