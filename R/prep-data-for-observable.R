# Function to convert ISO 3166-1 alpha-2 code to flag emoji
iso_to_flag <- function(iso_code) {
  # Convert each character to its corresponding regional indicator symbol letter
  intToUtf8(utf8ToInt("🇦") - utf8ToInt("A") + utf8ToInt(toupper(substr(iso_code, 1, 1)))) %>%
    paste0(intToUtf8(utf8ToInt("🇦") - utf8ToInt("A") + utf8ToInt(toupper(substr(iso_code, 2, 2)))))
}

# Function to map country names to flag emojis
country_to_flag <- function(iso_code) {
  # # Get the ISO 3166-1 alpha-2 code
  # iso_code <- countrycode::countrycode(country_name, "country.name", "iso2c")
  # Convert to flag emoji
  flag <- iso_to_flag(iso_code)
  return(flag)
}

# Vector of country names
countrycodes <- toupper(unique(df_plot$FID))
countrycodes <- countrycodes[order(countrycodes)]

# Convert country names to flag emojis
flag_emojis <- sapply(countrycodes, country_to_flag)


df_plot |> 
  st_drop_geometry() |> 
  select(countrycode = FID, everything(), 
         -c(geometry, centroid, PANEL, group, 
            xmin, ymin, xmax, ymax, alpha, fill, colour, linetype, stroke)) |> 
  arrange(countrycode) |> 
  bind_cols(country_flag = flag_emojis) |> 
  mutate(
    countrycode_upper = toupper(countrycode),
    countrycode_upper = ifelse(countrycode == "cn-tw", "TW", countrycode_upper)) |> 
  left_join(pop_latest, by = join_by(countrycode_upper == country_code_2)) |> 
  select(-c(year, country_code_3, countrycode_upper)) |> 
  mutate(
    population_fmt = scales::number(
      population, accuracy = 0.01,
      scale_cut = c("K" = 1e3, "M" = 1e6, "B" = 1e9))) |> 
  write_csv(file.path("data", "countries-dorling.csv"))
