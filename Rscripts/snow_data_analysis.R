library(tidyverse)

df <- read_csv("~/Desktop/Workspace/Parks/data/parks_snow_2006_2024.csv")

df <- df %>% 
  mutate(ecosystem = case_when(
    site %in% c("Roberge Lake: Tree Island","Roberge Island") ~ "Tree island",
    site %in% c("Mary Lake: Forest","Owl: Spruce Forest",
                "Broad: Riparian Forest","Broad: Spruce Forest",
                "Mary Lake Forest","Owl Spruce",
                "Broad Spruce","Mary Lake") ~ "Forest",
    site %in% c("Rupert Ck Riparian willow","Broad: Shrub","Broad Shrub","Broad Riparian") ~ "Shrub",
    site %in% c("Broad Sedge","Owl Sedge",
                "Owl: Coastal Fen","Broad: Sedge Fen") ~ "Sedge",
    site %in% c("Broad Ridge","Owl Ridge",
                "Broad: Coastal Beach Ridge","Owl: Coastal Beach Ridge") ~ "Beach ridge",
    site %in% c("Roberge Lake: Peat Plateau - Polygon Center","Roberge Crest") ~ "Polygon centre",
    site %in% c("Roberge Lake: Peat Plateau - Ice Wedge","Roberge Wedge" ) ~ "Ice wedge",
    site %in% c("South OF Nester 1 north 1km","Wetland N2Kmwest of nester 1") ~ "other",
    .default = NA
    ),
    period = case_when(
      year %in% 2006:2015 ~ "2006-2014",
      year %in% 2016:2024 ~ "2015-2024",
      .default = NA
    )
  )
                           


arrows_df <- df %>%
  filter(!is.na(ecosystem), ecosystem != "other") %>%
  group_by(period, ecosystem) %>%
  summarise(across(depth:htc, ~ mean(., na.rm = T)), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = c(depth, density, swe, htc)) %>%
  rename(swe_early = `swe_2006-2014`, swe_late = `swe_2015-2024`, htc_early = `htc_2006-2014`, htc_late = `htc_2015-2024`) %>%
  filter(!is.na(swe_early) & !is.na(swe_late))  # Ensure no missing data


df %>% 
  filter(!is.na(ecosystem), ecosystem != "other") %>% 
  group_by(period, ecosystem) %>%
  summarise(across(depth:htc, ~ mean(., na.rm = T)), .groups = "drop") %>%
  rename(Density = density) %>% 
  ggplot(aes(x = swe, y = htc, color = ecosystem, shape = period, size = Density)) + 
  geom_point(alpha = 0.6) +  # Ensure points are visible
  geom_segment(data = arrows_df, aes(x = swe_early, y = htc_early, 
                                     xend = swe_late, yend = htc_late, 
                                     color = ecosystem),
               arrow = arrow(length = unit(0.2, "cm")), 
               size = 1, 
               inherit.aes = FALSE) +  # Override inherited aesthetics
  # scale_x_log10() + 
  # scale_y_log10() +  # Log scales
  theme_bw() +
  labs(
    x = "Snow Water Equivalent (mm)",
    y = bquote(Heat~Transfer~Coefficient~"(W m"^-2~K^-1*")"),
    color = "Ecosystem",
    shape = "Period"
  ) +
  theme(legend.position = "right")
ggsave("./Parks/figures/snow_htc_swe.png", height = 5, width = 7, units = "in", dpi = 300)
