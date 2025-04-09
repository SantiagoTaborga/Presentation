library(tidyverse)
library(stargazer)
library(modelsummary)
library(gt)
library(kableExtra)

# graph settings
theme_classic2 = theme_classic() +
  theme(
    text = element_text(family = "Times"),
    title = element_text(size = 36, face = "bold"),
    axis.title.x = element_text(size = 28),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.y = element_text(size = 28),
    legend.text = element_text(size = 28),
    axis.line.y = element_blank()
  )
  

# loading the data
md <- readRDS("yg821jf8611_md_statewide_2020_04_01.rds") %>%
  mutate(
    state = "Maryland"
  )

va <- readRDS("yg821jf8611_va_statewide_2020_04_01.rds") %>%
  mutate(
    state = "Virginia"
  )

# combining and cleanup
dmv <- full_join(md, va) %>%
  relocate(state, .after = date) %>%
  filter(
    !is.na(subject_race),
    !is.na(date),
    subject_race != "unknown"
  ) %>%
  mutate( 
    search_conducted = if_else(search_conducted == "TRUE", 1, 0),
    subject_race = factor(subject_race),
    year = lubridate::year(date)
  )

# Graphs -----
## profiling graph
profiling <- dmv %>%
  group_by(subject_race) %>%
  summarize (
    search_conducted = mean(search_conducted, na.rm = TRUE)
    ) %>%
  ggplot(aes(x = search_conducted, y = subject_race, fill = subject_race)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "% Search Conducted by Race",
    x = "Mean Search Conducted",
    y = ""
    ) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 100),
    breaks = seq(0, 0.08, by = 0.02),
    limits = c(0, 0.08)) +
  theme_classic2 +
  theme(legend.title = element_blank(), legend.text = element_blank()) +
  scale_fill_manual(values = c("asian/pacific islander" = "#488f31", 
                               "black" = "#a5c796", 
                               "hispanic" = "#f3a3a4",
                               "white" = "#ffcc90",
                               "other" = "#de425b"),
                    guide = "none")
  
## regression models -----

### making data race-specific
overall <- dmv %>%
  mutate(
    subject_race = if_else(subject_race == "white", "white", "non-white")
  )

asian <- dmv %>%
  filter(subject_race == c("asian/pacific islander", "white"))

black <- dmv %>%
  filter(subject_race == c("black", "white"))

hispanic <- dmv %>%
  filter(subject_race == c("hispanic", "white"))

other <- dmv %>%
  filter(subject_race == c("other", "white"))

### estimates
mods <- 
  list(
    "Overall" = lm(search_conducted ~ subject_race, overall),
    "Asian/pacific islander" = lm(search_conducted ~ subject_race, asian),
    "Black" = lm(search_conducted ~ subject_race, black),
    "Hispanic" = lm(search_conducted ~ subject_race, hispanic),
    "Other" = lm(search_conducted ~ subject_race, other)
  )

### displaying all models
table <- modelsummary(
  mods,
  coef_rename = c(
    "subject_racewhite" = "White",
    "subject_raceother" = "Other"
  ),
  gof_map = c('nobs'),
  output = 'gt',
  star = TRUE,
  exclude = "(Intercept)"
)

### Add header above the table
table %>%
  tab_spanner(
    label = "Race",
    columns = c(`Overall`, `Asian/pacific islander`, `Black`, `Hispanic`, `Other`)
  )

## across years graph-----
profiling_year <- dmv %>%
  group_by(subject_race, year) %>%
  summarize(
    search_conducted = mean(search_conducted, na.rm = TRUE)
  ) %>%
  ggplot(aes(y = factor(year), x = search_conducted, fill = subject_race)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "% Search Conducted by Race and Year",
    y = "",
    x = "Mean Search Conducted",
    fill = "Race"
  ) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 100),
    breaks = seq(0, 0.08, by = 0.02),
    limits = c(0, 0.08)) +
  theme_classic2 +
  scale_fill_manual(values = c("asian/pacific islander" = "#488f31", 
                               "black" = "#a5c796", 
                               "hispanic" = "#f3a3a4",
                               "white" = "#ffcc90",
                               "other" = "#de425b"
  )) +
  theme(legend.key.size = unit(0.5, "cm"))


# across years table-----
profiling_year_data <- dmv %>%
  group_by(subject_race, year) %>%
  summarize(
    search_conducted = mean(search_conducted, na.rm = TRUE) * 100
  ) %>%
  group_by(year) %>%
  mutate(max_value = max(search_conducted)) %>%
  ungroup()

## Create the table
profiling_year <- profiling_year_data %>%
  select(subject_race, year, search_conducted) %>%
  kable(
    col.names = c("Race", "Year", "Mean Search Conducted"),
    caption = "% Search Conducted by Race and Year",
    format = "latex",
    digits = 3
  ) %>%
  row_spec(
    which(profiling_year_data$search_conducted == profiling_year_data$max_value),
    background = "yellow", color = "black", bold = TRUE
  ) %>%
  kable_styling(
    full_width = TRUE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

profiling_year

## Attempt at another table

profiling_year_data <- dmv %>%
  group_by(subject_race, year) %>%
  summarize(
    search_conducted = mean(search_conducted, na.rm = TRUE) * 100
  ) %>%
  group_by(year) %>%
  mutate(max_value = max(search_conducted)) %>%
  ungroup()

## Create the table
profiling_year <- profiling_year_data %>%
  gt() %>%
  cols_hide(columns = c(max_value)) %>%
  tab_header(
    title = "% Search Conducted by Race and Year"
  ) %>%
  cols_label(
    subject_race = "Race",
    year = "Year",
    search_conducted = "Mean Search Conducted"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "yellow"),
      cell_text(weight = "bold", color = "black")
    ),
    locations = cells_body(
      rows = search_conducted == max_value
    )
  ) %>%
  fmt_number(
    columns = c(search_conducted),
    decimals = 3,
    use_seps = TRUE
  )

profiling_year
