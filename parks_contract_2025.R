library(tidyverse)

df <- read_csv("/Users/sdmamet/Desktop/Workspace/Parks/data/parks_snow_2006_2024.csv")
df %>% problems()

unique(df$site)

             
              
                        
                           
                   
                      
"No Data"                                    
               
                     
                

                
                   
"Rupert Ck Riparian willow"                   
"South OF Nester 1 north 1km"                
"Wetland N2Kmwest of nester 1"               

df <- df %>% 
  mutate(site = case_when(
    site %in% c("Broad Ridge","Broad: Coastal Beach Ridge") ~ "broad_ridge",
    site %in% c("Broad Riparian","Broad: Riparian Forest") ~ "broad_forest",
    site %in% c("Broad Sedge","Broad: Sedge Fen") ~ "broad_fen",
    site %in% c("Broad Shrub","Broad: Shrub") ~ "broad_shrub",
    site %in% c("Broad Spruce","Broad: Spruce Forest") ~ "broad_forest",
    site %in% c("Mary Lake","Mary Lake Forest","Mary Lake: Forest") ~ "mary_forest",
    site %in% c("Owl Ridge","Owl: Coastal Beach Ridge") ~ "owl_ridge",
    site %in% c("Owl Sedge","Owl: Coastal Fen") ~ "owl_fen",
    site %in% c("Owl Spruce","Owl: Spruce Forest") ~ "owl_forest",
    site %in% c("Roberge Crest","Roberge Lake: Peat Plateau - Polygon Center") ~ "roberge_centre",
    site %in% c("Roberge Island","Roberge Lake: Tree Island") ~ "roberge_island",
    site %in% c("Roberge Lake: Peat Plateau - Ice Wedge","Roberge Wedge") ~ "broad_ridge"
    )
    ) %>% 
  separate(site, into = c("site", "type"), sep = "_")

  df %>% 
    group_by(site, type, year) %>% 
    summarise(
      depth = mean(depth, na.rm = T),
      density = mean(density, na.rm = T),
      swe = mean(swe, na.rm = T),
      htc = mean(htc, na.rm = T)
    ) %>% 
    ungroup() %>% 
    filter(!is.na(site)) %>%
    ggplot(aes(x = year, color = site, linetype = type)) + geom_line(aes(y = depth)) + geom_point(aes(y = depth)) +
    theme_bw()

df %>% 
  filter(site == "broad") %>% 
  ggplot(aes(x = factor(year), y = depth, fill = type)) +
  geom_boxplot()


ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot() +
  labs(x = "Number of Cylinders", y = "Miles per Gallon", title = "Boxplot of MPG by Cylinder Count")