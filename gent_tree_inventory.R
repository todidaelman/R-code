
library(sf)
library(ggplot2)
library(gtExtras)
library(svglite)
library(tidyr)
library(dplyr)

setwd("C:/Users/todaelma/OneDrive - UGent/PhD 2023-2027/QGIS/SOLWEIG/test-Gent")
gent_tree_inventory <- read_sf("input/InventoryGentCAVElab_20231011181854.shp")

gent_tree_inventory_df <- as.data.frame(st_drop_geometry(gent_tree_inventory))

gent_tree_inventory_df %>%
  select(SORTIMENT, METHOD, YEAR, SEASON, DBH, DBHDTM, HEIGHT, CROWNAREA, CROWNVOL, LOCATION, TYPE) %>%
  gt_plt_summary()

# histogrammen

ggplot(gent_tree_inventory, aes(x = DBHDTM)) +
  geom_histogram(color = "black") +
  theme_minimal()

#boxplots

gent_tree_inventory_df %>%
  filter(!is.na(DBHDTM)) %>%
  ggplot(aes(SORTIMENT, DBHDTM)) +
  geom_boxplot(aes(fill = SORTIMENT)) +
  theme_minimal()


gent_tree_inventory_df %>%
  filter(!is.na(HEIGHT)) %>%
  ggplot(aes(SORTIMENT, HEIGHT)) +
  geom_boxplot(aes(fill = SORTIMENT)) +
  theme_minimal()


gent_tree_inventory_df %>%
  filter(!is.na(CROWNAREA)) %>%
  ggplot(aes(SORTIMENT, CROWNAREA)) +
  geom_boxplot(aes(fill = SORTIMENT)) +
  theme_minimal()

# scatterplots

ggplot(gent_tree_inventory, aes(x = DBHDTM, y = HEIGHT)) +
  geom_point() +
  labs(title = "Scatter Plot of DBHDTM vs HEIGHT",
       x = "DBHDTM",
       y = "HEIGHT")

# Basic scatter plot for DBHDTM vs CROWNAREA
ggplot(gent_tree_inventory, aes(x = DBHDTM, y = CROWNAREA)) +
  geom_point() +
  labs(title = "Scatter Plot of DBHDTM vs CROWNAREA",
       x = "DBHDTM",
       y = "CROWNAREA")

# Basic scatter plot for HEIGHT vs CROWNAREA
ggplot(gent_tree_inventory, aes(x = HEIGHT, y = CROWNAREA)) +
  geom_point() +
  labs(title = "Scatter Plot of HEIGHT vs CROWNAREA",
       x = "HEIGHT",
       y = "CROWNAREA")
