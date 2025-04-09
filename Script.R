library(tidyverse)

ct <- readRDS("yg821jf8611_ct_statewide_2020_04_01.rds") %>%
  mutate(
    state = "Connecticut"
  )

ma <- readRDS("yg821jf8611_ma_statewide_2020_04_01.rds")%>%
  mutate(
    state = "Massachussets"
  )
  

md <- readRDS("yg821jf8611_md_statewide_2020_04_01.rds") %>%
  mutate(
    state = "Maryland"
  )

nh <- readRDS("yg821jf8611_nh_statewide_2020_04_01.rds") %>%
  mutate(
    state = "New Hampshire"
  )

nj <- readRDS("yg821jf8611_nh_statewide_2020_04_01.rds") %>%
  mutate(
    state = "New Jersey"
  )

ny <- readRDS("yg821jf8611_ny_statewide_2020_04_01.rds") %>%
  mutate(
    state = "New York"
  )

ri <- readRDS("yg821jf8611_ri_statewide_2020_04_01.rds") %>%
  mutate(
    state = "Rhode Island"
  )

va <- readRDS("yg821jf8611_va_statewide_2020_04_01.rds") %>%
  mutate(
    state = "Virginia"
  )

vt <- readRDS("yg821jf8611_vt_statewide_2020_04_01.rds") %>%
  mutate(
    state = "Vermont"
  )


northeast <- ct %>%
  full_join(ma, by = "state") %>%
  full_join(md, by = "state") %>%
  full_join(nh, by = "state") %>%
  full_join(nj, by = "state") %>%
  full_join(ny, by = "state") %>%
  full_join(ri, by = "state") %>%
  full_join(va, by = "state") %>%
  full_join(vt, by = "state") %>%
  relocate(state, .after = date)

northeast <- northeast %>%
  
  
