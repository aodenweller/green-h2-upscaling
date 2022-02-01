ReadBPDB <- function(file.bp) {
  
  region.mapping.bp <- c(
    "Austria" = "EU",
    "Belgium" = "EU",
    "Bulgaria" = "EU",
    "Czech Republic" = "EU",
    "Denmark" = "EU",
    "Finland" = "EU",
    "France" = "EU",
    "Germany" = "EU",
    "Greece" = "EU",
    "Hungary" = "EU",
    "Ireland" = "EU",
    "Italy" = "EU",
    "Netherlands" = "EU",
    "Norway" = "EU",
    "Poland" = "EU",
    "Portugal" = "EU",
    "Romania" = "EU",
    "Slovakia" = "EU",
    "Spain" = "EU",
    "Sweden" = "EU",
    "Canada" = "North America",
    "Mexico" = "North America",
    "US" = "North America",
    "Argentina" = "Other",
    "Brazil" = "Other",
    "Chile" = "Other",
    "Costa Rica" = "Other",
    "Uruguay" = "Other",
    "Honduras" = "Other",
    "Other S. & Cent. America" = "Other",
    "Russian Federation" = "Asia",
    "Other CIS" = "Asia",
    "Iran" = "MENA",
    "Israel" = "MENA",
    "Jordan" = "MENA",
    "United Arab Emirates" = "MENA",
    "Other Middle East" = "MENA",
    "Algeria" = "MENA",
    "Egypt" = "MENA",
    "Morocco" = "MENA",
    "South Africa" = "Other",
    "Tunisia" = "MENA",
    "Other Africa" = "Other",
    "China" = "Asia",
    "India" = "Asia",
    "Japan" = "Asia",
    "Malaysia" = "Asia",
    "Pakistan" = "Asia",
    "Philippines" = "Asia",
    "South Korea" = "Asia",
    "Taiwan" = "Asia",
    "Thailand" = "Asia",
    "Viet Nam" = "Asia",
    "Other Asia Pacific" = "Asia",
    "Other Europe" = "Other",
    "Switzerland" = "Other",
    "Turkey" = "Asia",
    "Ukraine" = "Other",
    "New Zealand" = "Other",
    "United Kingdom" = "Other",
    "Total World" = "Global",
    NULL
  )
  
  # Read in solar sheet
  data.bp.solar <-
    read_excel(
      path = file.bp,
      sheet = "Solar Capacity",
      range = "A4:Z72",
      col_names = TRUE
    ) %>%
    filter(if_any(everything(), ~ !is.na(.))) %>%
    pivot_longer(cols = `1996`:`2020`,
                 names_to = "year",
                 values_to = "capacity") %>%
    rename(region = Megawatts) %>%
    mutate(technology = "solar") %>%
    # Delete total values except total world
    filter(!region %in% c("Total North America",
                          "Total S. & Cent. America",
                          "Total Europe",
                          "Total CIS",
                          "Total Middle East",
                          "Total Africa",
                          "Total Asia Pacific"))

  # Read in wind sheet
  data.bp.wind <-
    read_excel(
      path = file.bp,
      sheet = "Wind Capacity",
      range = "A4:AA70",
      col_names = TRUE
    ) %>%
    filter(if_any(everything(), ~ !is.na(.))) %>%
    pivot_longer(cols = `1995`:`2020`,
                 names_to = "year",
                 values_to = "capacity") %>%
    rename(region = Megawatts) %>%
    mutate(technology = "wind") %>%
    # Delete total values (use regional sub-values instead)
    filter(!region %in% c("Total North America",
                          "Total S. & Cent. America",
                          "Total Europe",
                          "Total CIS",
                          "Total Middle East",
                          "Total Africa",
                          "Total Asia Pacific"))
  
  data.bp <- bind_rows(data.bp.solar, data.bp.wind) %>%
    revalue.levels(region = region.mapping.bp) %>%
    group_by(region, year, technology) %>%
    summarise(capacity = sum(capacity)) %>%
    mutate(year = as.numeric(year))
  
  return(data.bp)
  
}