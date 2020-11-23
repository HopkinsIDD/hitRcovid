#Admin1 code preparation

setwd("~/Boston University/COVID_Interventions/hitRcovid")

library(dplyr)
library(ISOcodes)
library(stringr)
load_all()

countries <- c("BEL", "DEU", "GBR", "ITA", "RUS", "BRA",
               "USA", "CAN", "COL", "AFG", "IND")

country_codes <- ISO_3166_1 %>% filter(Alpha_3 %in% countries)

admin_string <- paste0(paste(country_codes$Alpha_2, collapse = "-|"), "-")
russia_string <- ", Respublika| Respublika| oblast'| kray"
ending_string <- "skaya$|iya$|'$"
admin_codes1 <- ISO_3166_2 %>%
  filter(grepl(admin_string, Code), is.na(Parent), Type != "Outlying area") %>%
  mutate(Name = iconv(Name, "UTF-8", "ASCII//TRANSLIT"),
         alpha_2 = substr(Code, 1, 2),
         Name = gsub(russia_string, "", Name),
         Name = ifelse(alpha_2 == "RU", gsub(ending_string, "", Name), Name))

admin_codes2 <- geo_lookup %>%
  filter(country %in% countries) %>%
  select(alpha_2, admin1, admin1_name) %>%
  mutate(admin1_name = ifelse(alpha_2 == "RU", 
                              gsub("'$|sk$", "", admin1_name), admin1_name)) %>%
  unique()

#Reading in corrected names
names_correct <- read.csv("name_corrections.csv")

admin_codes2 <- admin_codes2 %>%
  full_join(names_correct, by = "admin1_name") %>%
  mutate(admin1_name = ifelse(!is.na(admin1_correction), admin1_correction, admin1_name)) %>%
  select(-admin1_correction)

#Combining codes
admin_codes <- full_join(admin_codes1, admin_codes2,
                         by = c("alpha_2", "Name" = "admin1_name")) %>%
  arrange(alpha_2, Name)

matched <- admin_codes %>% filter(!is.na(Code) & !is.na(admin1))
not_matched <- admin_codes %>% filter(is.na(Code) | is.na(admin1))

#write.csv(not_matched, "name_corrections_original.csv")

#Getting UK codes
uk <- covidregionaldata::get_regional_data(country = "UK")
uk <- uk %>%
  filter(region %in% c("England", "Northern Ireland", "Scotland", "Wales")) %>%
  select(region, ons_region_code) %>%
  unique()

admin_codes_final <- admin_codes %>%
  left_join(select(ISO_3166_2, Code, Name), by = "Code") %>%
  filter(!is.na(admin1)) %>%
  select(ISO_code = Code, GID_code = admin1, Name = Name.y, Type) %>%
  mutate(Name = ifelse(Name == "Wales; Cymru", "Wales", Name)) %>%
  full_join(uk, by = c("Name" = "region"))


write.csv(admin_codes_final, "ISO_to_GID_admin1_codes.csv", row.names = FALSE)


