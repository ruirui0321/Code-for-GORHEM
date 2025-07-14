
###############################################continents map
library(data.table)
library(dplyr)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
# 使用 mutate 和 ifelse 函数进行替换
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
wide_number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv")
total <- merge(wide_number,region,by=c("country"))
unique(total$country)
name_long <- unique(wide_number$country)
total_country <- unique(total$country)
missing_columns <- setdiff(name_long, total_country)
unique(region$country)
total
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

total
unique_combinations <- total %>%
  distinct(country, `Super regions`)
print(unique_combinations)
world_merged <- merge(world_new, unique_combinations, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)
colnames(world_merged)[which(names(world_merged) == 'Super regions')] <- "Super_regions"
library(randomcoloR)
regions_palette <- distinctColorPalette(8)
#regions_palette <- c("#D8D966","#7FDF80","#DF73AF","#D89A76","#75B9CD","#AADBCB","#AD59E0","#ADE35C","#D3D3D3")
regions_palette <- c("#D8D966","#7FDF80","#DF73AF","#D89A76","#AD59E0","#AADBCB","#75B9CD","#ADE35C","#D3D3D3")
regions_unique <- unique(world_merged$'Super_regions')
regions_unique <- c(regions_unique, "NA")
names(regions_palette) <- regions_unique

p <- ggplot(data = world_merged) +
  geom_sf(aes(fill = Super_regions), color = "#696969", size = 0.5) +  # 显示国家边界
  scale_fill_manual(
    values = regions_palette,
    na.value = "white", 
    name = "Geographic Regions",
    labels = setNames(regions_unique, c(unique(world_merged$Super_regions), "No Data"))
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 20),
  )+
  guides(
    fill = guide_legend(nrow = 1)  # 设置图例为3行显示
  )
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/continental_regions.png")
ggsave(filename, p, width = 15, height = 8, units = "in", dpi = 400)


###########################################A regional stacked bar chart of skin cancer and cataract
library(data.table)
library(dplyr)
#number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_1980_2100.csv")
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_de_1980_2100.csv")
#number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv")
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
unique(number$country)
number_region <- number %>%
  group_by(year, Super_regions) %>%
  summarise(
    dif_total_region_year = sum(dif, na.rm = TRUE), 
    .groups = "drop" 
  )

number_region <- data.frame(number_region)
region_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)
number_region$period <- ifelse(number_region$year <= 2030, "Before 2030", "After 2030")
number_region$scaled_year <- ifelse(number_region$period == "Before 2030",
                                    scales::rescale(number_region$year, from = c(min(number_region$year[number_region$period == "Before 2030"]), 2030), to = c(0, 1)),
                                    scales::rescale(number_region$year, from = c(2030, max(number_region$year[number_region$period == "After 2030"])), to = c(1, 8)))
width_before_2030 <- 0.02-0.001
width_after_2030 <- 0.1-0.01

number_region$dif_total_region_year_million <- number_region$dif_total_region_year/10^6


p <- ggplot(number_region, aes(x = scaled_year, y = dif_total_region_year_million, fill = Super_regions)) +
  geom_bar(stat = "identity", width = ifelse(number_region$period == "Before 2030", width_before_2030, width_after_2030)) +
  scale_fill_manual(values = region_colors, name = "Geographic Regions") + 
  labs(
    x = "Year",
    #y = "DALYs (million)") +
    y = "Numbers (million)") +
  scale_x_continuous(
    #breaks = c(seq(0, 1, by = 0.2), seq(1, 8, by = 0.5)),
    breaks = c(seq(0, 1, by = 0.5), seq(1, 8, by = 1)),
    labels = c(seq(1980, 2030, by = 25), seq(2030, 2100, by = 10))
  ) +scale_y_continuous(
    breaks = scales::pretty_breaks(n = 4)
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        #axis.text.x = element_text(angle = 45, hjust = 0.5,size = 22), 
        axis.text.x = element_text(angle = -90, hjust = 0.5,vjust=0.5,size = 24),
        axis.text.y = element_text(size = 24),  
        axis.title.x = element_text(size = 24),  
        axis.title.y = element_text(size = 24),  
        plot.title = element_text( size = 18,hjust = 0.5,),
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 24),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray", size = 0.5),  
        panel.grid.minor = element_line(color = "lightgray", size = 0.25) 
        )+
  #legend.position = c(0.45, 0.8))+
  #legend.position = "none"
  guides(fill = guide_legend(ncol = 1)) 
# 显示图形
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/number_de_avoided_by_continental_regions.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)


######################pie charts
library(ggplot2)
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    dif_total_region_year = sum(dif, na.rm = TRUE), 
    .groups = "drop" 
  )

custom_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)

number_type <- number_region %>%
  mutate(percentage = sprintf("%.1f", dif_total_region_year / sum(dif_total_region_year, na.rm = TRUE) * 100))

p <- ggplot(number_type, aes(x = "", y = dif_total_region_year, fill = Super_regions)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_manual(values = custom_colors) +  
  labs(x = NULL, y = NULL) +  
  theme_void() +  
  theme(legend.position = "none")  
print(p)

#filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/pin_daly_continent.png")
#filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/pin_number_continent.png")
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/pin_number_de_continent.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)



#################################################Avoid radiation distribution maps
library(data.table)
library(dplyr)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(Polychrome)
library(patchwork)
library(tidyr)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/radiation/pop_weighted_ra_m.csv")
ra_no <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/radiation/pop_weighted_ra_m_nocontrol.csv")
colnames(ra_no)[which(names(ra_no) == "ra_mj_weighted_total")] <- "ra_mj_weighted_total_nocontrol"
#total <- merge(ra,ra_no,by=c("country","age_name","sex_name","year"))
total <- merge(ra,ra_no,by=c("country","year"))
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
total <- merge(total,region,by=c("country"))
colnames(total)[which(names(total) == 'Super regions')] <- "Super_regions"
total$avoided_ra <- total$ra_mj_weighted_total_nocontrol-total$ra_mj_weighted_total
avoided_ra_mean <- total %>%
  group_by(country) %>%
  summarise(mean_avoided_ra = mean(avoided_ra, na.rm = TRUE))
print(avoided_ra_mean)
avoided_ra_mean <- data.frame(avoided_ra_mean)

super_region_avg_overall <- total %>%
  group_by(Super_regions) %>%
  summarise(avg_avoided_ra = mean(avoided_ra, na.rm = TRUE))

world_merged_1 <- merge(world_new, avoided_ra_mean, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)
boundary <- st_boundary(world_merged_1)
min_value <- min(c(min(world_merged_1$mean_avoided_ra, na.rm = TRUE)))
max_value <- max(c(max(world_merged_1$mean_avoided_ra, na.rm = TRUE)))
quantiles <- quantile(world_merged_1$mean_avoided_ra, probs = seq(0, 1, 0.01), na.rm = TRUE)
custom_colors <- c("#A8D4D5","white","#f1dbfa","#c7a5d5","#9c72b2","#73418f","#480a6d")

custom_values <- c(min_value, 0, 0.5, 1,1.5, 2, max_value)
p1 <- ggplot(data = world_merged_1) +
  geom_sf(aes(fill = mean_avoided_ra), color = NA) +
  geom_sf(data = boundary, color = "#696969", fill = NA, linewidth = 0.5) +
  scale_fill_gradientn(
    colors = custom_colors,
    values = scales::rescale(custom_values, from = c(min_value, max_value)),
    limits = c(min_value, max_value),
    oob = scales::squish
  ) +
  labs(fill = expression("Population-weighted UV Radiation (MJ m"^-2*")"))+
  #  ggtitle("(a) 2030") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text( size = 18),
    legend.title = element_text(size = 28),
    legend.text = element_text(size = 28), 
  ) +
  guides(
    fill = guide_colorbar(
      barheight = unit(0.5, "cm"),
      barwidth = unit(14, "cm")
    )
  )
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/mean_avoided_ra_m.png")
ggsave(filename, p1, width = 18, height = 10, units = "in", dpi = 400)


###########################################################gbd_90_21
library(data.table)
library(dplyr)
library(tidyr)
#gbd <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_1980_2021.csv")
gbd <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/GBD/gbd_cataract_1990_2021.csv")

pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/population data/pop_2000_2021_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

unique(gbd$cause_name)
#gbd_ <- gbd[gbd$measure_name=="Incidence"&gbd$age_name!="All ages",]
#gbd_ <- gbd[gbd$measure_name=="Deaths"&gbd$age_name!="All ages",]
gbd_ <- gbd[gbd$age_name!="All ages",]
unique(gbd_$cause_name)
colnames(gbd_)[which(names(gbd_) == "location_name")] <- "country"
total <- gbd_

total <- merge(total,pop_per_country,by=c("country"))
total$pop_country_permillion <- total$pop_country/10^6
total$total_incidence_country <- total$val/total$pop_country_permillion
total <- total[,c("country","total_incidence_country","total_upper_country","total_lower_country")]


library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

world_merged_1 <- merge(world_new, total, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)
boundary <- st_boundary(world_merged_1)

min_value <- min(c(min(world_merged_1$total_incidence_country, na.rm = TRUE)))
max_value <- max(c(max(world_merged_1$total_incidence_country, na.rm = TRUE)))
quantiles <- quantile(world_merged_1$total_incidence_country, probs = seq(0, 1, 0.01), na.rm = TRUE)

world_merged_1$centroid <- st_point_on_surface(world_merged_1$geometry)
world_merged_1$lon <- st_coordinates(world_merged_1$centroid)[, 1]
world_merged_1$lat <- st_coordinates(world_merged_1$centroid)[, 2]
centroid_coordinates <- st_coordinates(world_merged_1$centroid)
centroid_data <- data.frame(
  NAME_LONG = world_merged_1$NAME_LONG,  
  dif_total_country = world_merged_1$total_incidence_country,  
  lon = centroid_coordinates[, "X"], 
  lat = centroid_coordinates[, "Y"]  
)


world_merged_1 <- world_merged_1 %>%
  mutate(dif_total_category = cut(
    total_incidence_country,
    breaks = c(-500, 50, 200, 300, 400, 500, 600, 1000, 2000, max_value),
    labels = c("<50", "50~200", "200~300", "300~400", "400~500", "500~600", "600~1,000","1,000~2,000", ">2,000"),
    include.lowest = TRUE
  ))


# world_merged_1 <- world_merged_1 %>%
#   mutate(dif_total_category = cut(
#     total_incidence_country,
#     breaks = c(-10, 10, 50, 100, 200, 500, 1000,2000,5000, max_value),
#     labels = c("<10", "10~50", "50~100", "100~200", "200~500", "500~1,000", "1,000~2,000","2,000~5,000", ">5,000"),
#     include.lowest = TRUE
#   ))


# world_merged_1 <- world_merged_1 %>%
#   mutate(dif_total_category = cut(
#     total_incidence_country,
#     breaks = c(-5, 1, 2, 5, 10, 20, 30, 50, 100, max_value),
#     labels = c("<1", "1~2", "2~5", "5~10", "10~20", "20~30", "30~50","50~100", ">100"),
#     include.lowest = TRUE
#   ))


# custom_colors <- c(
#   "<1" = "#C8D6E7",
#   "1~2" = "#E8EDF1",
#   "2~5" = "#F2EBE5",
#   "5~10" = "#ECD0B4",
#   "10~20" = "#F27F69",
#   "20~30" = "#C26B57",
#   "30~50" = "#A13D3B",
#   "50~100" = "#831A21",
#   ">100" = "#4A1416"
# )

# custom_colors <- c(
#   "<10" = "#C8D6E7",
#   "10~50" = "#E8EDF1",
#   "50~100" = "#F2EBE5",
#   "100~200" = "#ECD0B4",
#   "200~500" = "#F27F69",
#   "500~1,000" = "#C26B57",
#   "1,000~2,000" = "#A13D3B",
#   "2,000~5,000" = "#831A21",
#   ">5,000" = "#4A1416"
# )


custom_colors <- c(
  "<50" = "#C8D6E7",
  "50~200" = "#E8EDF1",
  "200~300" = "#F2EBE5",
  "300~400" = "#ECD0B4",
  "400~500" = "#F27F69",
  "500~600" = "#C26B57",
  "600~1,000" = "#A13D3B",
  "1,000~2,000" = "#831A21",
  ">2,000" = "#4A1416"
)


filtered_world_merged_1 <- world_merged_1 %>%
  filter(!(near(lon, 114.0559975, tol = 1e-3) & near(lat, 22.41120026, tol = 1e-3)) & 
           !(near(lon, 113.5594336, tol = 1e-3) & near(lat, 22.13617585, tol = 1e-3))) 
p1 <- ggplot(data = filtered_world_merged_1) +
  geom_sf(aes(fill = dif_total_category), color = NA) +
  geom_sf(data = boundary, color = "#696969", fill = NA, linewidth = 0.5) +
  scale_fill_manual(
    values = custom_colors,
    name = "DALYs Per Million",
    #name = "Numbers Per Million",
    na.translate = FALSE  
  ) +
  labs(fill = "DALYs Per Million") +
  #labs(fill = "Numbers Per Million") +
  #  ggtitle("(a) 2030") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text( size = 18),
    legend.title = element_text(size = 22), 
    legend.text = element_text(size = 22), 
  ) +
  guides(
    #fill = guide_colorbar(
    fill = guide_legend(
      barheight = unit(0.5, "cm"),
      barwidth = unit(0.5, "cm")
    )
  )

filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/DALY_90_21.png")
ggsave(filename, p1, width = 16, height = 8, units = "in", dpi = 400)


###########################################A stacked bar chart of different costs for skin cancer
library(dplyr)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
library(data.table)
library(patchwork)
library(tidyr)

total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
total <- data.frame(total)

total$cost_vsly_total <- total$cost_sc
total_long <- total %>%
  pivot_longer(
    cols = c(cost_treat_total, cost_vsly_total),
    names_to = "cost_type",
    values_to = "cost"
  ) %>%
  mutate(cost_type = case_when(
    cost_type == "cost_treat_total" ~ "Direct Costs",
    cost_type == "cost_vsly_total" ~ "Indirect Costs"
  ))

head(total_long)
total_long <- data.frame(total_long)
tail(total_long)

number_type <- total_long %>%
  group_by(cost_type,year) %>%
  summarise(cost_year = sum(cost, na.rm = TRUE))
number_type <- data.frame(number_type)
number_type_test <- number_type[number_type$cost_type=="Indirect Costs",]
sum(number_type_test$cost_year)
custom_colors <- c("#629433", "#FEFF99")


number_type$period <- ifelse(number_type$year <= 2030, "Before 2030", "After 2030")
number_type$scaled_year <- ifelse(number_type$period == "Before 2030",
                                  scales::rescale(number_type$year, from = c(min(number_type$year[number_type$period == "Before 2030"]), 2030), to = c(0, 1)),
                                  scales::rescale(number_type$year, from = c(2030, max(number_type$year[number_type$period == "After 2030"])), to = c(1, 8)))
number_type_di <- number_type[number_type$cost_type=="Direct Costs",]
sum(number_type_di$cost_year)
number_type_in <- number_type[number_type$cost_type=="Indirect Costs",]
sum(number_type_in$cost_year)
width_before_2030 <- 0.02-0.001
width_after_2030 <- 0.1-0.01

number_type$cost_year_billion <- number_type$cost_year/10^9

p <- ggplot(number_type, aes(x = scaled_year, y = cost_year_billion, fill = cost_type)) +
  geom_bar(stat = "identity", width = ifelse(number_type$period == "Before 2030", width_before_2030, width_after_2030)) +
  scale_fill_manual(values = custom_colors, name = "Types") + 
  labs(
    #title = "Costs of Skin Cancer Avoided by Montreal Protocol in Different Types",
    x = "Year",
    y = "Costs (in billion US$)") +
  scale_x_continuous(
    #breaks = c(seq(0, 1, by = 0.2), seq(1, 8, by = 0.5)),
    #labels = c(seq(1980, 2030, by = 10), seq(2030, 2100, by = 5))
    breaks = c(seq(0, 1, by = 0.5), seq(1, 8, by = 1)),
    labels = c(seq(1980, 2030, by = 25), seq(2030, 2100, by = 10))
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_text(angle = -90, hjust = 0.5, vjust=0.5, size = 22),  
        axis.text.y = element_text(size = 22),  
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22), 
        plot.title = element_text( size = 18,hjust = 0.5,),
        legend.title = element_text(size = 22), 
        legend.text = element_text(size = 22),
        legend.position = c(0.2, 0.85),
        panel.grid.major = element_line(color = "gray", size = 0.5),  
        panel.grid.minor = element_line(color = "lightgray", size = 0.25)  
        )
# 显示图形
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/costs_avoided_by_type_skin.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)


total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
total <- total %>%
  #group_by(year) %>%
  summarise(cost_treat_total = sum(cost_treat_total, na.rm = TRUE),
            cost_vsly_total=sum(cost_sc, na.rm = TRUE))
total_long <- total %>%
  pivot_longer(
    cols = c(cost_treat_total, cost_vsly_total),
    names_to = "cost_type",
    values_to = "cost"
  ) %>%
  mutate(cost_type = case_when(
    cost_type == "cost_treat_total" ~ "Direct Costs",
    cost_type == "cost_vsly_total" ~ "Indirect Costs"
  ))
number_type <- total_long
number_type <- data.frame(number_type)
custom_colors <- c("Direct Costs"="#629433", "Indirect Costs"="#FEFF99")

number_type$cost_type <- factor(number_type$cost_type, 
                                levels = names(custom_colors))
class(number_type$cost)
sum(number_type$cost,na.rm=TRUE)
number_type <- number_type %>%
  mutate(percentage = sprintf("%.1f", cost / sum(cost, na.rm = TRUE) * 100))

p <- ggplot(number_type, aes(x = "", y = cost, fill = cost_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_manual(values = custom_colors) +  
  labs(x = NULL, y = NULL) +  
  theme_void() + 
  theme(legend.position = "none")  
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/pin_cost_type.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)


###########################################Stacked bar chart of different costs for cataracts
library(dplyr)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
library(data.table)
library(patchwork)
library(tidyr)

total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
total <- data.frame(total)

total$cost_vsly_total <- total$cost_ca
total$cost_treat_ca_total <- total$treat_ca
total_long <- total %>%
  pivot_longer(
    cols = c(cost_treat_ca_total, cost_vsly_total),
    names_to = "cost_type",
    values_to = "cost"
  ) %>%
  mutate(cost_type = case_when(
    cost_type == "cost_treat_ca_total" ~ "Direct Costs",
    cost_type == "cost_vsly_total" ~ "Indirect Costs"
  ))
head(total_long)
total_long <- data.frame(total_long)
tail(total_long)

number_type <- total_long %>%
  group_by(cost_type,year) %>%
  summarise(cost_year = sum(cost, na.rm = TRUE))
number_type <- data.frame(number_type)
number_type_test <- number_type[number_type$cost_type=="Indirect Costs",]
sum(number_type_test$cost_year)
custom_colors <- c("#8E7FB8", "#A2C9AE")

number_type$period <- ifelse(number_type$year <= 2030, "Before 2030", "After 2030")
number_type$scaled_year <- ifelse(number_type$period == "Before 2030",
                                  scales::rescale(number_type$year, from = c(min(number_type$year[number_type$period == "Before 2030"]), 2030), to = c(0, 1)),
                                  scales::rescale(number_type$year, from = c(2030, max(number_type$year[number_type$period == "After 2030"])), to = c(1, 8)))
number_type_di <- number_type[number_type$cost_type=="Direct Costs",]
sum(number_type_di$cost_year)
number_type_in <- number_type[number_type$cost_type=="Indirect Costs",]
sum(number_type_in$cost_year)
width_before_2030 <- 0.02-0.001
width_after_2030 <- 0.1-0.01

number_type$cost_year_billion <- number_type$cost_year/10^9


p <- ggplot(number_type, aes(x = scaled_year, y = cost_year_billion, fill = cost_type)) +
  geom_bar(stat = "identity", width = ifelse(number_type$period == "Before 2030", width_before_2030, width_after_2030)) +
  scale_fill_manual(values = custom_colors, name = "Types") + 
  labs(
    #title = "Costs of Skin Cancer Avoided by Montreal Protocol in Different Types",
    x = "Year",
    y = "Costs (in billion US$)") +
  scale_x_continuous(
    #breaks = c(seq(0, 1, by = 0.2), seq(1, 8, by = 0.5)),
    #labels = c(seq(1980, 2030, by = 10), seq(2030, 2100, by = 5))
    breaks = c(seq(0, 1, by = 0.5), seq(1, 8, by = 1)),
    labels = c(seq(1980, 2030, by = 25), seq(2030, 2100, by = 10))
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_text(angle = -90, hjust = 0.5, vjust=0.5, size = 22),
        axis.text.y = element_text(size = 22),  
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),  
        plot.title = element_text( size = 18,hjust = 0.5,),
        legend.title = element_text(size = 22),  
        legend.text = element_text(size = 22),
        legend.position = c(0.2, 0.85),
        panel.grid.major = element_line(color = "gray", size = 0.5), 
        panel.grid.minor = element_line(color = "lightgray", size = 0.25)
        )
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/costs_avoided_by_type_ca.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)


total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
total <- total %>%
  #group_by(year) %>%
  summarise(cost_treat_ca_total = sum(treat_ca, na.rm = TRUE),
            cost_vsly_total=sum(cost_ca, na.rm = TRUE))
total_long <- total %>%
  pivot_longer(
    cols = c(cost_treat_ca_total, cost_vsly_total),
    names_to = "cost_type",
    values_to = "cost"
  ) %>%
  mutate(cost_type = case_when(
    cost_type == "cost_treat_ca_total" ~ "Direct Costs",
    cost_type == "cost_vsly_total" ~ "Indirect Costs"
  ))
number_type <- total_long
number_type <- data.frame(number_type)
custom_colors <- c("Direct Costs"="#8E7FB8", "Indirect Costs"="#A2C9AE")

number_type$cost_type <- factor(number_type$cost_type, 
                                levels = names(custom_colors))
class(number_type$cost)
sum(number_type$cost,na.rm=TRUE)
number_type <- number_type %>%
  mutate(percentage = sprintf("%.1f", cost / sum(cost, na.rm = TRUE) * 100))

p <- ggplot(number_type, aes(x = "", y = cost, fill = cost_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_manual(values = custom_colors) + 
  labs(x = NULL, y = NULL) +  
  theme_void() +  
  theme(legend.position = "none") 
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/pin_cost_type_ca.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)



#A stacked bar chart of skin cancer costs and cataract costs
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
library(data.table)
library(patchwork)
library(tidyr)

total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
total$cost_skin_total <- rowSums(cbind(total$cost_treat_total, total$cost_sc), na.rm = TRUE)
total$cost_cataract <- rowSums(cbind(total$treat_ca, total$cost_ca), na.rm = TRUE)

total_long <- total %>%
  pivot_longer(
    cols = c(cost_skin_total, cost_cataract),
    names_to = "cost_type",
    values_to = "cost"
  ) %>%
  mutate(cost_type = case_when(
    cost_type == "cost_skin_total" ~ "Total Costs of Skin Cancers",
    cost_type == "cost_cataract" ~ "Total Costs of Cataract"
  ))
head(total_long)
total_long <- data.frame(total_long)
tail(total_long)

number_type <- total_long %>%
  group_by(cost_type,year) %>%
  summarise(cost_year = sum(cost, na.rm = TRUE))
number_type <- data.frame(number_type)
custom_colors <- c("#A24D56", "#516770")

number_type$period <- ifelse(number_type$year <= 2030, "Before 2030", "After 2030")
number_type$scaled_year <- ifelse(number_type$period == "Before 2030",
                                  scales::rescale(number_type$year, from = c(min(number_type$year[number_type$period == "Before 2030"]), 2030), to = c(0, 1)),
                                  scales::rescale(number_type$year, from = c(2030, max(number_type$year[number_type$period == "After 2030"])), to = c(1, 8)))
width_before_2030 <- 0.02-0.001
width_after_2030 <- 0.1-0.01

number_type$cost_year_billion <- number_type$cost_year/10^9


p <- ggplot(number_type, aes(x = scaled_year, y = cost_year_billion, fill = cost_type)) +
  geom_bar(stat = "identity", width = ifelse(number_type$period == "Before 2030", width_before_2030, width_after_2030)) +
  scale_fill_manual(values = custom_colors, name = "Types") + 
    #title = "Costs of Skin Cancer Avoided by Montreal Protocol in Different Types",
    x = "Year",
    y = "Costs (in billion US$)") +
  scale_x_continuous(
    #breaks = c(seq(0, 1, by = 0.2), seq(1, 8, by = 0.5)),
    #labels = c(seq(1980, 2030, by = 10), seq(2030, 2100, by = 5))
    breaks = c(seq(0, 1, by = 0.5), seq(1, 8, by = 1)),
    labels = c(seq(1980, 2030, by = 25), seq(2030, 2100, by = 10))
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        axis.text.x = element_text(angle = -90, hjust = 0.5, vjust=0.5, size = 22),
        axis.text.y = element_text(size = 22), 
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text( size = 18,hjust = 0.5,),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.position = c(0.3, 0.8),
        panel.grid.major = element_line(color = "gray", size = 0.5), 
        panel.grid.minor = element_line(color = "lightgray", size = 0.25)
        )

print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/total_costs_avoided_by_desease_type.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)

library(ggpattern)
library(ggrepel)
total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
total$cost_skin_total <- rowSums(cbind(total$cost_treat_total, total$cost_sc), na.rm = TRUE)
total$cost_cataract <- rowSums(cbind(total$treat_ca, total$cost_ca), na.rm = TRUE)
total <- total %>%
  summarise(cost_skin_total = sum(cost_skin_total, na.rm = TRUE),
            cost_cataract=sum(cost_cataract, na.rm = TRUE))

total_long <- total %>%
  pivot_longer(
    cols = c(cost_skin_total, cost_cataract),
    names_to = "cost_type",
    values_to = "cost"
  ) %>%
  mutate(cost_type = case_when(
    cost_type == "cost_skin_total" ~ "Total Costs of Skin Cancers",
    cost_type == "cost_cataract" ~ "Total Costs of Cataract"
  ))
number_type <- total_long
number_type <- data.frame(number_type)

custom_colors <- c("Total Costs of Cataract"="#A24D56", "Total Costs of Skin Cancers"="#516770")

number_type$cost_type <- factor(number_type$cost_type, 
                                levels = names(custom_colors))
number_type <- number_type %>%
  mutate(percentage = sprintf("%.0f", cost / sum(cost) * 100))
sum(number_type$cost)

p <- ggplot(number_type, aes(x = "", y = cost, fill = cost_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_manual(values = custom_colors) +  
  labs(x = NULL, y = NULL) +  
  theme_void() + 
  theme(legend.position = "none")   # 不显示图例

print(p)

filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/pin_desease_cost_type.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)



########################################################################Draw the distribution of A5 countries and non-A5 countries
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
library(data.table)
library(patchwork)
library(tidyr)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)
type <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/Montreal_parties.csv")
world_merged_1 <- merge(world_new, type, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)

p1 <- ggplot(data = world_merged_1) +
  geom_sf(aes(fill = type), color = NA) +
  geom_sf(data = st_boundary(world_merged_1), color = "#696969", fill = NA, linewidth = 0.5) +
  scale_fill_manual(
    values = c("A5" = "#F8766D", "Non-A5" = "#00BFC4"), 
    na.value = "white"  
  ) +
  labs(fill = "Parties") +
  #ggtitle("Distribution of A5 Counties and Non-A5 Countries") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 22,hjust = 0.5),
    legend.title = element_text(size = 22),  
    legend.text = element_text(size = 22)  
  )
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/distribution_a5_nona5.png")
ggsave(filename, p1, width = 18, height = 9, units = "in", dpi = 400)


###############################distribution of VSLY
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
library(data.table)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)
VSLY <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/life expectancy/VSLY_final.csv")
VSLY
unique(VSLY$Country)
colnames(VSLY)[which(names(VSLY) == "Country")] <- "country"
m_total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_1980_2100.csv")
unique(m_total$country)
VSLY$country[VSLY$country == "C?te d'Ivoire"] <- "Côte d'Ivoire"
VSLY$VSLY_thou <- VSLY$VSLY/1000
dif <- setdiff(VSLY$country,m_total$country)
dif <- setdiff(m_total$country,VSLY$country)

world_merged <- merge(world_new, VSLY, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)
boundary <- st_boundary(world_merged)
p <- 
  ggplot(data = world_merged) +
  geom_sf(aes(fill = VSLY_thou), color = NA) +
  geom_sf(data = boundary,color = "#696969", fill = NA, linewidth = 0.5) +
  #scale_fill_viridis_c(name = "Cases of Incidence") + 
  scale_fill_distiller(palette = "Spectral", name = "Values of VSLY (in thousand US$)",
                       na.value = "white") +
  labs(fill = "Values of VSLY (in thousand US$)", family = "Arial") + 
  #ggtitle(paste("Values of VSLY")) +
  theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(size = 20,hjust = 0.5),legend.title = element_text(size = 16),legend.text = element_text(size = 14))+
  guides(fill = guide_colorbar(barheight = unit(0.5, "cm"), barwidth = unit(10, "cm"))) 
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/VSLY.png")
ggsave(filename, p, width = 12, height = 8, units = "in", dpi = 400)


##################################fig 1
#############################################################################The total number of people to be avoided by 2100
library(data.table)
library(dplyr)
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv")
#number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_de_1980_2100.csv")
#number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_1980_2100.csv")
wide_per_country <- number %>%
  group_by(country) %>%
  summarise(
    dif_total_country = sum(dif, na.rm=TRUE),
    .groups = "drop" 
  )
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
library(data.table)
library(patchwork)
library(tidyr)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

wide_per_country <- data.frame(wide_per_country)

world_merged_1 <- merge(world_new, wide_per_country, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)
min_value <- min(c(min(world_merged_1$dif_total_country, na.rm = TRUE)))
max_value <- max(c(max(world_merged_1$dif_total_country, na.rm = TRUE)))

world_merged_1 <- world_merged_1 %>%
  mutate(dif_total_category = cut(
    dif_total_country,
    breaks = c(-40000, -10000,0,100, 1000, 5000, 10000, 50000, 100000, 1000000, 5000000,10000000,100000000, max_value),
    labels = c("<-10,000","-10,000~0","0~100", "100~1,000", "1,000~5,000", "5,000~10,000", "10,000~50,000", "50,000~100,000", "100,000~1,000,000", "1,000,000~5,000,000", "5,000,000~10,000,000","10,000,000~100,000,000",">100,000,000"),
    include.lowest = TRUE
  ))

custom_colors <- c(
  "<-10,000" = "#C8D6E7",
  "-10,000~0" = "#E8EDF1",
  "0~100" = "#F1F1A2",
  "100~1,000" = "#EBE264",
  "1,000~5,000" = "#FDBC36",
  "5,000~10,000" = "#F7972C",
  "10,000~50,000" = "#E7623B",
  "50,000~100,000" = "#CB4B51",
  "100,000~1,000,000" = "#AB3960",
  "1,000,000~5,000,000" = "#8E286D",
  "5,000,000~10,000,000" = "#732676",
  "10,000,000~100,000,000" = "#512870",
  ">100,000,000" = "#322361"
)

p1 <- ggplot(data = world_merged_1) +
  geom_sf(aes(fill = dif_total_category), color = NA) +
  geom_sf(data = boundary, color = "#696969", fill = NA, linewidth = 0.3) +
  scale_fill_manual(
    values = custom_colors,
    na.value = "white"  
  ) +
  labs(fill = "Numbers") +
  #ggtitle("Total Numbers of Skin Cancer Cases Avoided by Montreal Protocol") +
  theme_minimal() +
  theme(
    #legend.position = "right",
    legend.position = "bottom",
    plot.title = element_text(size = 18,hjust = 0.5),
    legend.title = element_text(size = 22), 
    legend.text = element_text(size = 22) 
  ) +
  guides(
    fill = guide_legend(
      barheight = unit(0.5, "cm"),
      barwidth = unit(0.5, "cm")
    )
  )
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/total_numbers_avoided.png")
ggsave(filename, p1, width = 16, height = 8, units = "in", dpi = 400)


wide_per_country <- data.frame(wide_per_country)
world_merged_1 <- merge(world_new, wide_per_country, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)
min_value <- min(c(min(world_merged_1$dif_total_country, na.rm = TRUE)))
max_value <- max(c(max(world_merged_1$dif_total_country, na.rm = TRUE)))

# world_merged_1 <- world_merged_1 %>%
#   mutate(dif_total_category = cut(
#     dif_total_country,
#     breaks = c(-10000, -1000,0,50,100,500, 1000, 5000, 10000, 50000, 100000,500000, 1000000, max_value),
#     labels = c("<-1,000","-1,000~0","0~50", "50~100", "100~500", "500~1,000", "1,000~5,000", "5,000~10,000", "10,000~50,000", "50,000~100,000","100,000~500,000", "500,000~1,000,000",">1,000,000"),
#     include.lowest = TRUE
#   ))

# custom_colors <- c(
#   "<-1,000" = "#C8D6E7",
#   "-1,000~0" = "#E8EDF1",
#   "0~50" = "#F1F1A2",
#   "50~100" = "#EBE264",
#   "100~500" = "#FDBC36",
#   "500~1,000" = "#F7972C",
#   "1,000~5,000" = "#E7623B",
#   "5,000~10,000" = "#CB4B51",
#   "10,000~50,000" = "#AB3960",
#   "50,000~100,000" = "#8E286D",
#   "100,000~500,000" = "#732676",
#   "500,000~1,000,000" = "#512870",
#   ">1,000,000" = "#322361"
# )

world_merged_1 <- world_merged_1 %>%
  mutate(dif_total_category = cut(
    dif_total_country,
    breaks = c(-5000, -500,0,50,100,500, 1000, 5000, 10000, 20000, 50000,100000, 500000, max_value),
    labels = c("<-500","-500~0","0~50", "50~100", "100~500", "500~1,000", "1,000~5,000", "5,000~10,000", "10,000~20,000", "20,000~50,000","50,000~100,000", "100,000~500,000",">500,000"),
    include.lowest = TRUE
  ))

custom_colors <- c(
  "<-500" = "#C8D6E7",
  "-500~0" = "#E8EDF1",
  "0~50" = "#F1F1A2",
  "50~100" = "#EBE264",
  "100~500" = "#FDBC36",
  "500~1,000" = "#F7972C",
  "1,000~5,000" = "#E7623B",
  "5,000~10,000" = "#CB4B51",
  "10,000~20,000" = "#AB3960",
  "20,000~50,000" = "#8E286D",
  "50,000~100,000" = "#732676",
  "100,000~500,000" = "#512870",
  ">500,000" = "#322361"
)

p1 <- ggplot(data = world_merged_1) +
  geom_sf(aes(fill = dif_total_category), color = NA) +
  geom_sf(data = boundary, color = "#696969", fill = NA, linewidth = 0.3) +
  scale_fill_manual(
    values = custom_colors,
    na.value = "white"  
  ) +
  labs(fill = "Numbers") +
  #ggtitle("Total Numbers of Skin Cancer Deaths Avoided by Montreal Protocol") +
  theme_minimal() +
  theme(
    #legend.position = "right",
    legend.position = "bottom",
    plot.title = element_text(size = 18,hjust = 0.5),
    legend.title = element_text(size = 22), 
    legend.text = element_text(size = 22) 
  ) +
  guides(
    fill = guide_legend(
      barheight = unit(0.5, "cm"),
      barwidth = unit(0.5, "cm")
    )
  )
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/total_numbers_de_avoided.png")
ggsave(filename, p1, width = 16, height = 8, units = "in", dpi = 400)


wide_per_country <- data.frame(wide_per_country)
world_merged_1 <- merge(world_new, wide_per_country, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)
min_value <- min(c(min(world_merged_1$dif_total_country, na.rm = TRUE)))
max_value <- max(c(max(world_merged_1$dif_total_country, na.rm = TRUE)))
quantiles <- quantile(world_merged_1$dif_total_country, probs = seq(0, 1, 0.01), na.rm = TRUE)

# world_merged_1 <- world_merged_1 %>%
#   mutate(dif_total_category = cut(
#     dif_total_country,
#     breaks = c(-1200000, -20000,0,10000, 50000, 100000, 200000, 300000, 400000, 500000, 1000000 , 3000000 ,50000000, max_value),
#     labels = c("<-20,000","-20,000~0","0~10,000", "10,000~50,000", "50,000~100,000", "100,000~200,000", "200,000~300,000", "300,000~400,000", "400,000~500,000", "500,000~1,000,000", "1,000,000~3,000,000","3,000,000~50,000,000",">50,000,000"),
#     include.lowest = TRUE
#   ))

# custom_colors <- c(
#   "<-20,000" = "#C8D6E7",
#   "-20,000~0" = "#E8EDF1",
#   "0~10,000" = "#F1F1A2",
#   "10,000~50,000" = "#EBE264",
#   "50,000~100,000" = "#FDBC36",
#   "100,000~200,000" = "#F7972C",
#   "200,000~300,000" = "#E7623B",
#   "300,000~400,000" = "#CB4B51",
#   "400,000~500,000" = "#AB3960",
#   "500,000~1,000,000" = "#8E286D",
#   "1,000,000~3,000,000" = "#732676",
#   "3,000,000~50,000,000" = "#512870",
#   ">50,000,000" = "#322361"
# )

world_merged_1 <- world_merged_1 %>%
  mutate(dif_total_category = cut(
    dif_total_country,
    breaks = c(-300000, 0,5000,10000, 30000, 50000, 100000, 200000, 300000, 500000, 1000000 , 5000000 ,10000000, max_value),
    labels = c("<-0","0~5,000","5,000~10,000", "10,000~30,000", "30,000~50,000", "50,000~100,000", "100,000~200,000", "200,000~300,000", "300,000~500,000", "500,000~1,000,000", "1,000,000~5,000,000","5,000,000~10,000,000",">10,000,000"),
    include.lowest = TRUE
  ))

custom_colors <- c(
  "<-0" = "#C8D6E7",
  "0~5,000" = "#E8EDF1",
  "5,000~10,000" = "#F1F1A2",
  "10,000~30,000" = "#EBE264",
  "30,000~50,000" = "#FDBC36",
  "50,000~100,000" = "#F7972C",
  "100,000~200,000" = "#E7623B",
  "200,000~300,000" = "#CB4B51",
  "300,000~500,000" = "#AB3960",
  "500,000~1,000,000" = "#8E286D",
  "1,000,000~5,000,000" = "#732676",
  "5,000,000~10,000,000" = "#512870",
  ">10,000,000" = "#322361"
)

p1 <- ggplot(data = world_merged_1) +
  geom_sf(aes(fill = dif_total_category), color = NA) +
  geom_sf(data = boundary, color = "#696969", fill = NA, linewidth = 0.3) +
  scale_fill_manual(
    values = custom_colors,
    na.value = "white" 
  ) +
  labs(fill = "DALYs") +
  #ggtitle("Total Numbers of Skin Cancer Cases Avoided by Montreal Protocol") +
  theme_minimal() +
  theme(
    #legend.position = "right",
    legend.position = "bottom",
    plot.title = element_text(size = 18,hjust = 0.5),
    legend.title = element_text(size = 22), 
    legend.text = element_text(size = 22) 
  ) +
  guides(
    fill = guide_legend(
      barheight = unit(0.5, "cm"),
      barwidth = unit(0.5, "cm")
    )
  )
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/total_cataract_daly_avoided.png")
ggsave(filename, p1, width = 16, height = 8, units = "in", dpi = 400)



###################right figures
library(dplyr)
library(data.table)
library(ggplot2)

#number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_1980_2100.csv")
#number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_de_1980_2100.csv")
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv")
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
unique(number$country)
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions,age_name,sex_name) %>%
  summarise(
    dif_total_region_year = sum(dif, na.rm=TRUE),
    .groups = "drop"
  )


# For cataracts, add empty 15 to 19 years of data
##############################################################################
number_region <- number_region %>%
  bind_rows(
    data.frame(
      Super_regions = unique(number_region$Super_regions), 
      age_name = "15-19 years",                                     
      sex_name = "Male",                                            
      dif_total_region_year = 0                                     
    )
  ) %>%
  bind_rows(
    data.frame(
      Super_regions = unique(number_region$Super_regions), 
      age_name = "15-19 years",                                     
      sex_name = "Female",                                          
      dif_total_region_year = 0                                     
    )
  ) %>%
  mutate(age_name = factor(age_name, 
                           levels = c("15-19 years", sort(unique(age_name)[unique(age_name) != "15-19 years"]))))

#####################################################################################################################
library(ggplot2)
library(dplyr)

custom_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)
number_region$age_name <- gsub(" years", "", number_region$age_name)
number_region_modified <- number_region %>%
  mutate(dif_total_region_year = ifelse(sex_name == "Male", 
                                        -dif_total_region_year, 
                                        dif_total_region_year)) %>%
  arrange(age_name, desc(dif_total_region_year)) %>%
  mutate(age_name = factor(age_name, 
                           levels = rev(unique(age_name)))) 

number_region_modified$dif_total_region_year_million <- number_region_modified$dif_total_region_year/10^6



p<-ggplot(number_region_modified, aes(x = age_name, 
                                      y = dif_total_region_year_million, 
                                      fill = Super_regions)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +  
  #scale_fill_manual(values = custom_colors, name = "Geographic Regions") +
  scale_fill_manual(values = custom_colors, name = NULL) +
  #scale_y_continuous(labels = abs) + 
  #####cataracts
  scale_y_continuous(limits = c(-6.5, 6.5),,
                     breaks = c(-6, -4, -2, 0, 2, 4, 6),labels = abs) +  
  # ####incidence
  # scale_y_continuous(limits = c(-75, 75),,
  #                    breaks = c(-75, -50, -25, 0, 25, 50, 75),labels = abs) + 
  # ####deaths
  # scale_y_continuous(limits = c(-0.6, 0.6),,
  #                    breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6),labels = abs) +  
  labs(x = "Age Groups (years)", 
       #y = "Numbers (million)",
       y = "DALYs (million)",
       fill = "Super Regions") +
  # annotate("text", x = max(as.numeric(number_region_modified$age_name)), 
  #          y = -100000, label = "Male", hjust = 1.5, size = 10) + 
  # annotate("text", x = max(as.numeric(number_region_modified$age_name)), 
  #          y = 100000, label = "Female", hjust = -0.5, size = 10) + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20),  
        #axis.title.x = element_text(size = 22,  hjust = 0.435),  
        #axis.title.x = element_text(size = 22,  hjust = 0.65),  
        #axis.title.x = element_text(size = 22,  hjust = 0.66), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),  
        panel.grid.major = element_line(color = "gray80", size = 0.8),  
        #panel.grid.major = element_line(color = "gray90"),
        #legend.position = c(0.05, 0.9),  
        legend.position = "none",  
        legend.justification = c(0, 1), 
        #legend.position = "top",
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_text(size = 22),  
        legend.text = element_text(size = 26))+ 
  guides(fill = guide_legend(nrow = 7))  
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/daly_cataract_avoided_by_super_regions_byage_bysex_new.png")
ggsave(filename, p, width = 14, height = 8, units = "in", dpi = 400)



############################fig 2
######treat

library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
#install.packages("ggpattern")
library(ggpattern)

number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_treat_accumu = sum(cost_treat_total, na.rm=TRUE),
    .groups = "drop" 
  )

number_region <- data.frame(number_region)

total_accumu_cost_with_ratio <- number_region %>%
  group_by (Super_regions) %>%
  mutate(ratio = total_treat_accumu / sum(number_region$total_treat_accumu))

number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_1980_2100.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
# 使用 mutate 和 ifelse 函数进行替换
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_number_accumu = sum(dif, na.rm=TRUE),
    .groups = "drop" 
  )

total_accumu_with_ratio <- number_region %>%
  group_by (Super_regions) %>%
  mutate(ratio = total_number_accumu / sum(number_region$total_number_accumu))

colnames(total_accumu_with_ratio)[which(names(total_accumu_with_ratio) == 'ratio')] <- "ratio_number_region"
colnames(total_accumu_cost_with_ratio)[which(names(total_accumu_cost_with_ratio) == 'ratio')] <- "ratio_cost_region"

total <- merge(total_accumu_with_ratio,total_accumu_cost_with_ratio,by=c("Super_regions"))

custom_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)
total <- data.frame(total)
library(dplyr)
library(patchwork)

total_long <- total %>%
  dplyr::select(Super_regions, ratio_number_region, ratio_cost_region) %>%
  pivot_longer(cols = c(ratio_number_region, ratio_cost_region), names_to = "ratio_type", values_to = "ratio_value") %>%
  mutate(ratio_type = factor(ratio_type, levels = c("ratio_number_region", "ratio_cost_region")))


total_number <- total_long %>%
  filter(ratio_type == "ratio_number_region") %>%
  arrange(desc(ratio_value)) %>%
  mutate(country = factor(Super_regions, levels = Super_regions))
total_cost <- total_long %>%
  filter(ratio_type == "ratio_cost_region") %>%
  arrange(desc(ratio_value)) %>%
  mutate(country = factor(Super_regions, levels = Super_regions))

#p1 <- ggplot(total_long, aes(x = ratio_type, y = ratio_value, fill = country_sorted)) +
p1 <- ggplot(total_number, aes(x = ratio_type, y = ratio_value, fill = Super_regions)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_text(aes(label = ifelse(ratio_value >= 0.04, scales::percent(ratio_value, accuracy = 0.1, suffix = ""), "")),
            position = position_fill(vjust = 0.5), size = 11) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("Numbers")) +
  scale_fill_manual(values = custom_colors, guide = guide_legend(title = "Geographic Regions", nrow = 3)) +  
  labs(x = NULL, y = "Percentage") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26), 
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    plot.margin = margin(r = -20),  
    legend.position = "none"  
  )

#p2 <- ggplot(total_sorted_cost, aes(x = ratio_type, y = ratio_value, fill = country_sorted)) +
p2 <- ggplot(total_cost, aes(x = ratio_type, y = ratio_value, fill = Super_regions)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_bar_pattern(
    stat = "identity", 
    position = "fill", 
    width = 0.5, 
    pattern = "stripe",  
    pattern_density = 0.1,  
    pattern_angle = 45,  
    pattern_color = "gray",  
    alpha = 0.3 
  ) +
  geom_text(aes(label = ifelse(ratio_value >= 0.04, scales::percent(ratio_value, accuracy = 0.1, suffix = ""), "")),
            position = position_fill(vjust = 0.5), size = 11) +
  scale_y_continuous(labels = NULL) +  
  scale_x_discrete(labels = c("Costs")) +
  scale_fill_manual(values = custom_colors, guide = "none") +  
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26), 
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    plot.margin = margin(l = -20)  
  )

combined_plot <- (p1 + p2 + plot_layout(ncol = 2, guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 26)
  )

print(combined_plot)

filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/treat_cost_noordered.png")
ggsave(filename, combined_plot, width = 6, height = 10, units = "in", dpi = 400)


##################################treatment costs of cataracts
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
#install.packages("ggpattern")
library(ggpattern)

number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_treat_accumu = sum(treat_ca, na.rm=TRUE),
    .groups = "drop" 
  )

number_region <- data.frame(number_region)

total_accumu_cost_with_ratio <- number_region %>%
  group_by (Super_regions) %>%
  mutate(ratio = total_treat_accumu / sum(number_region$total_treat_accumu))

number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_number_accumu = sum(dif, na.rm=TRUE),
    .groups = "drop" 
  )

total_accumu_with_ratio <- number_region %>%
  group_by (Super_regions) %>%
  mutate(ratio = total_number_accumu / sum(number_region$total_number_accumu))

colnames(total_accumu_with_ratio)[which(names(total_accumu_with_ratio) == 'ratio')] <- "ratio_number_region"
colnames(total_accumu_cost_with_ratio)[which(names(total_accumu_cost_with_ratio) == 'ratio')] <- "ratio_cost_region"

total <- merge(total_accumu_with_ratio,total_accumu_cost_with_ratio,by=c("Super_regions"))

custom_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)
total <- data.frame(total)
library(dplyr)
library(patchwork)

total_long <- total %>%
  dplyr::select(Super_regions, ratio_number_region, ratio_cost_region) %>%
  pivot_longer(cols = c(ratio_number_region, ratio_cost_region), names_to = "ratio_type", values_to = "ratio_value") %>%
  mutate(ratio_type = factor(ratio_type, levels = c("ratio_number_region", "ratio_cost_region")))


total_number <- total_long %>%
  filter(ratio_type == "ratio_number_region") %>%
  arrange(desc(ratio_value)) %>%
  mutate(country = factor(Super_regions, levels = Super_regions))
total_cost <- total_long %>%
  filter(ratio_type == "ratio_cost_region") %>%
  arrange(desc(ratio_value)) %>%
  mutate(country = factor(Super_regions, levels = Super_regions))

#p1 <- ggplot(total_long, aes(x = ratio_type, y = ratio_value, fill = country_sorted)) +
p1 <- ggplot(total_number, aes(x = ratio_type, y = ratio_value, fill = Super_regions)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_text(aes(label = ifelse(ratio_value >= 0.04, scales::percent(ratio_value, accuracy = 0.1, suffix = ""), "")),
            position = position_fill(vjust = 0.5), size = 11) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("DALYs")) +
  scale_fill_manual(values = custom_colors, guide = guide_legend(title = "Geographic Regions", nrow = 3)) + 
  labs(x = NULL, y = "Percentage") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26), 
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    plot.margin = margin(r = -20),  
    legend.position = "none"  
  )

#p2 <- ggplot(total_sorted_cost, aes(x = ratio_type, y = ratio_value, fill = country_sorted)) +
p2 <- ggplot(total_cost, aes(x = ratio_type, y = ratio_value, fill = Super_regions)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_bar_pattern(
    stat = "identity", 
    position = "fill", 
    width = 0.5, 
    pattern = "stripe",  
    pattern_density = 0.1, 
    pattern_angle = 45, 
    pattern_color = "gray",  
    alpha = 0.3  
  ) +
  geom_text(aes(label = ifelse(ratio_value >= 0.04, scales::percent(ratio_value, accuracy = 0.1, suffix = ""), "")),
            position = position_fill(vjust = 0.5), size = 11) +
  scale_y_continuous(labels = NULL) +  
  scale_x_discrete(labels = c("Costs")) +
  scale_fill_manual(values = custom_colors, guide = "none") +  
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26), 
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    plot.margin = margin(l = -20) 
  )

combined_plot <- (p1 + p2 + plot_layout(ncol = 2, guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 26)
  )

print(combined_plot)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/treat_cost_ca_noordered.png")
ggsave(filename, combined_plot, width = 6, height = 10, units = "in", dpi = 400)


######death and vsly

number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_cost_accumu = sum(cost_sc, na.rm=TRUE),
    .groups = "drop" 
  )

number_region <- data.frame(number_region)

total_accumu_cost_with_ratio <- number_region %>%
  group_by (Super_regions) %>%
  mutate(ratio = total_cost_accumu / sum(number_region$total_cost_accumu))

number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_de_1980_2100.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_number_accumu = sum(dif, na.rm=TRUE),
    .groups = "drop" 
  )
total_accumu_with_ratio <- number_region %>%
  group_by (Super_regions) %>%
  mutate(ratio = total_number_accumu / sum(number_region$total_number_accumu))

colnames(total_accumu_with_ratio)[which(names(total_accumu_with_ratio) == 'ratio')] <- "ratio_number_region"
colnames(total_accumu_cost_with_ratio)[which(names(total_accumu_cost_with_ratio) == 'ratio')] <- "ratio_cost_region"

total <- merge(total_accumu_with_ratio,total_accumu_cost_with_ratio,by=c("Super_regions"))


custom_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)
total <- data.frame(total)
library(dplyr)
total_long <- total %>%
  dplyr::select(Super_regions, ratio_number_region, ratio_cost_region) %>%
  pivot_longer(cols = c(ratio_number_region, ratio_cost_region), names_to = "ratio_type", values_to = "ratio_value") %>%
  mutate(ratio_type = factor(ratio_type, levels = c("ratio_number_region", "ratio_cost_region")))


total_number <- total_long %>%
  filter(ratio_type == "ratio_number_region") %>%
  arrange(desc(ratio_value)) %>%
  mutate(country = factor(Super_regions, levels = Super_regions))
total_cost <- total_long %>%
  filter(ratio_type == "ratio_cost_region") %>%
  arrange(desc(ratio_value)) %>%
  mutate(country = factor(Super_regions, levels = Super_regions))

#p1 <- ggplot(total_sorted_number, aes(x = ratio_type, y = ratio_value, fill = country_sorted)) +
p1 <- ggplot(total_number, aes(x = ratio_type, y = ratio_value, fill = Super_regions)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_text(aes(label = ifelse(ratio_value >= 0.04, scales::percent(ratio_value, accuracy = 0.1, suffix = ""), "")),
            position = position_fill(vjust = 0.5), size = 11) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("DALYs")) +
  scale_fill_manual(values = custom_colors, guide = guide_legend(title = "Geographic Regions", nrow = 3)) + 
  labs(x = NULL, y = "Percentage") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26), 
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    plot.margin = margin(r = -20),  
    legend.position = "none"  
  )

#p2 <- ggplot(total_sorted_cost, aes(x = ratio_type, y = ratio_value, fill = country_sorted)) +
p2 <- ggplot(total_cost, aes(x = ratio_type, y = ratio_value, fill = Super_regions)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_bar_pattern(
    stat = "identity", 
    position = "fill", 
    width = 0.5, 
    pattern = "stripe",  
    pattern_density = 0.1,   
    pattern_angle = 45,
    pattern_color = "gray",  
    alpha = 0.3 
  ) +
  geom_text(aes(label = ifelse(ratio_value >= 0.04, scales::percent(ratio_value, accuracy = 0.1, suffix = ""), "")),
            position = position_fill(vjust = 0.5), size = 11) +
  scale_y_continuous(labels = NULL) +  
  scale_x_discrete(labels = c("Costs")) +
  scale_fill_manual(values = custom_colors, guide = "none") +  
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26), 
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    plot.margin = margin(l = -20) 
  )
combined_plot <- (p1 + p2 + plot_layout(ncol = 2, guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 26)
  )

print(combined_plot)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/vsly_cost_noordered.png")
ggsave(filename, combined_plot, width = 6, height = 10, units = "in", dpi = 400)


######cataract
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_cost_accumu = sum(cost_ca, na.rm=TRUE),
    .groups = "drop" 
  )

number_region <- data.frame(number_region)

total_accumu_cost_with_ratio <- number_region %>%
  group_by (Super_regions) %>%
  mutate(ratio = total_cost_accumu / sum(number_region$total_cost_accumu))

number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_number_accumu = sum(dif, na.rm=TRUE),
    .groups = "drop" 
  )
total_accumu_with_ratio <- number_region %>%
  group_by (Super_regions) %>%
  mutate(ratio = total_number_accumu / sum(number_region$total_number_accumu))

colnames(total_accumu_with_ratio)[which(names(total_accumu_with_ratio) == 'ratio')] <- "ratio_number_region"
colnames(total_accumu_cost_with_ratio)[which(names(total_accumu_cost_with_ratio) == 'ratio')] <- "ratio_cost_region"

total <- merge(total_accumu_with_ratio,total_accumu_cost_with_ratio,by=c("Super_regions"))

custom_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)
total <- data.frame(total)
library(dplyr)
total_long <- total %>%
  dplyr::select(Super_regions, ratio_number_region, ratio_cost_region) %>%
  pivot_longer(cols = c(ratio_number_region, ratio_cost_region), names_to = "ratio_type", values_to = "ratio_value") %>%
  mutate(ratio_type = factor(ratio_type, levels = c("ratio_number_region", "ratio_cost_region")))

total_number <- total_long %>%
  filter(ratio_type == "ratio_number_region") %>%
  arrange(desc(ratio_value)) %>%
  mutate(country = factor(Super_regions, levels = Super_regions))
total_cost <- total_long %>%
  filter(ratio_type == "ratio_cost_region") %>%
  arrange(desc(ratio_value)) %>%
  mutate(country = factor(Super_regions, levels = Super_regions))

library(patchwork)

#p1 <- ggplot(total_sorted_number, aes(x = ratio_type, y = ratio_value, fill = country_sorted)) +
p1 <- ggplot(total_number, aes(x = ratio_type, y = ratio_value, fill = Super_regions)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_text(aes(label = ifelse(ratio_value >= 0.04, scales::percent(ratio_value, accuracy = 0.1, suffix = ""), "")),
            position = position_fill(vjust = 0.5), size = 11) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("DALYs")) +
  scale_fill_manual(values = custom_colors, guide = guide_legend(title = "Geographic Regions", nrow = 3)) + 
  labs(x = NULL, y = "Percentage") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26), 
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    plot.margin = margin(r = -20),  
    legend.position = "none"  
  )

#p2 <- ggplot(total_sorted_cost, aes(x = ratio_type, y = ratio_value, fill = country_sorted)) +
p2 <- ggplot(total_cost, aes(x = ratio_type, y = ratio_value, fill = Super_regions)) +

  geom_bar(stat = "identity", position = "fill", width = 0.5) +

  geom_bar_pattern(
    stat = "identity", 
    position = "fill", 
    width = 0.5, 
    pattern = "stripe",
    pattern_density = 0.1,  
    pattern_angle = 45,
    pattern_color = "gray", 
    alpha = 0.3 
  ) +
  geom_text(aes(label = ifelse(ratio_value >= 0.04, scales::percent(ratio_value, accuracy = 0.1, suffix = ""), "")),
            position = position_fill(vjust = 0.5), size = 11) +
  scale_y_continuous(labels = NULL) + 
  scale_x_discrete(labels = c("Costs")) +
  scale_fill_manual(values = custom_colors, guide = "none") + 
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26), 
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    plot.margin = margin(l = -20)  
  )

combined_plot <- (p1 + p2 + plot_layout(ncol = 2, guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 26)
  )

print(combined_plot)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/cataract_vsly_cost_noordered.png")
ggsave(filename, combined_plot, width = 6, height = 10, units = "in", dpi = 400)


#####total cost
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
library(data.table)
library(patchwork)
library(tidyr)
library(ggpattern)

number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_cost_accumu = sum(cost_total, na.rm=TRUE),
    .groups = "drop" 
  )

number_region <- data.frame(number_region)

total_accumu_cost_with_ratio <- number_region %>%
  group_by (Super_regions) %>%
  mutate(ratio_cost_region = total_cost_accumu / sum(number_region$total_cost_accumu))
region_cost <- total_accumu_cost_with_ratio


total_ <- number_region %>%
  mutate(percentage = sprintf("%.1f", ratio_cost_region = total_cost_accumu / sum(number_region$total_cost_accumu) * 100))
custom_colors <- c("Africa" = "#D89A76", "North America" = "#75B9CD", "South America" = "#AD59E0", 
                   "Asia" = "#D8D966", "Europe" = "#DF73AF", "Oceania" = "#AADBCB")
total_ <- data.frame(total_)
p <- ggplot(total_, aes(x = "", y = total_cost_accumu, fill = Super_regions)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_manual(values = custom_colors) +
  labs(x = NULL, y = NULL) +
  theme_void() + 
  theme(legend.position = "none")  
print(p)

filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/pin_cost_noordered.png")
ggsave(filename, p, width = 6, height = 6, units = "in", dpi = 400)



total_sorted_cost <- region_cost %>%
  arrange((ratio_cost_region)) %>%
  mutate(country_sorted = factor(Super_regions, levels = Super_regions))
total_sorted_cost <- total_sorted_cost %>%
  group_by(country_sorted) %>%
  mutate(cumulative_position = cumsum(ratio_cost_region) - ratio_cost_region / 2) %>%
  ungroup()
class(total_sorted_cost)
total_sorted_cost <- data.frame(total_sorted_cost)
temp <- total_sorted_cost

cost_treat <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_treat_3000.csv")
cost_vsly_mm <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/m_vsly_cost_3000.csv")
cost_vsly_mm <- cost_vsly_mm[,c("country","year","age_name","sex_name","cost")]
cost_vsly_scc <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/scc_vsly_cost_3000.csv")
cost_vsly_scc <- cost_vsly_scc[,c("country","year","age_name","sex_name","cost")]
colnames(cost_vsly_mm)[which(names(cost_vsly_mm) == "cost")] <- "cost_vsly_mm"
colnames(cost_vsly_scc)[which(names(cost_vsly_scc) == "cost")] <- "cost_vsly_kc"
cost_vsly <- merge(cost_vsly_mm,cost_vsly_scc, by = c("country","year","age_name","sex_name"), all = TRUE)
total <- merge(cost_treat,cost_vsly,by=c("country","year","age_name","sex_name"), all = TRUE)
total$cost_treat_total <- rowSums(cbind(total$cost_mm, total$cost_bcc, total$cost_scc), na.rm = TRUE)
total$cost_vsly_total <- rowSums(cbind(total$cost_vsly_mm, total$cost_vsly_kc), na.rm = TRUE)
cataract_cost <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_daly_cost.csv")
cataract_cost <- cataract_cost[,c("country","year","age_name","sex_name","pop_country_age_sex_permillion","cost")]
colnames(cataract_cost)[which(names(cataract_cost) == "cost")] <- "cost_cataract"
total <- merge(total,cataract_cost,by=c("country","year","age_name","sex_name","pop_country_age_sex_permillion"))
total$cost_skin_total <- rowSums(cbind(total$cost_treat_total, total$cost_vsly_total), na.rm = TRUE)
total$cost_total <- total$cost_cataract+total$cost_skin_total
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
total <- merge(total,region,by=c("country"))
total_accumu <- total
total_accumu$total_cost <- sum(total_accumu$cost_total)
total_accumu$ratio_cost <- total_accumu$cost_total/total_accumu$total_cost
colnames(total_accumu)[which(names(total_accumu) == 'Super regions')] <- "Super_regions"
region_cost <- total_accumu %>%
  group_by(Super_regions) %>%
  summarise(ratio_cost_region = sum(ratio_cost, na.rm = TRUE))
total_sorted_cost <- region_cost %>%
  arrange((ratio_cost_region)) %>%
  mutate(country_sorted = factor(Super_regions, levels = Super_regions))
total_sorted_cost <- total_sorted_cost %>%
  group_by(country_sorted) %>%
  mutate(cumulative_position = cumsum(ratio_cost_region) - ratio_cost_region / 2) %>%
  ungroup()
temp

library(dplyr)
total_sorted_cost <- total_sorted_cost %>%
  mutate(ratio_cost_region = temp %>% pull(ratio_cost_region))
print(total_sorted_cost)

custom_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)

library(patchwork)
p2 <- ggplot(total_sorted_cost, aes(x = factor(1), y = ratio_cost_region, fill = country_sorted)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_bar_pattern(
    stat = "identity", 
    position = "fill", 
    width = 0.5, 
    pattern = "stripe",  
    pattern_density = 0.1,  
    pattern_angle = 45,  
    pattern_color = "gray",  
    alpha = 0.3  
  ) +

  geom_text(aes(label = ifelse(ratio_cost_region >= 0.03, 
                               #scales::percent(ratio_cost_region, accuracy = 0.1, suffix = ""), "")), 
                               scales::percent(ratio_cost_region, accuracy = 0.1), "")), 
            position = position_fill(vjust = 0.5), size = 11) +

  scale_y_continuous(labels = NULL) +  

  scale_x_discrete(labels = c("Costs")) +

  scale_fill_manual(values = custom_colors, guide = guide_legend(title = "Locations")) +
  labs(x = NULL, y = NULL) +

  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 24), 
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    plot.margin = margin(l = -20), 
    #legend.position = "right",
    legend.position = "bottom",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 22)
  )
print(p2)


p2 <- ggplot(total_sorted_cost, aes(x = factor(1), y = ratio_cost_region, fill = country_sorted)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_bar_pattern(
    stat = "identity", 
    position = "fill", 
    width = 0.5, 
    pattern = "stripe",  
    pattern_density = 0.1,  
    pattern_angle = 45,  
    pattern_color = "gray",  
    alpha = 0.3  
  ) +
  #geom_text(aes(label = ifelse(ratio_cost_region >= 0.03, 
  geom_text(aes(label = ifelse(ratio_cost_region >= 0.06, 
                               #scales::percent(ratio_cost_region, accuracy = 0.1, suffix = ""), "")), 
                               scales::percent(ratio_cost_region, accuracy = 0.1), "")),
            position = position_fill(vjust = 0.5), size = 14) +
  scale_y_continuous(labels = NULL) +  
  scale_x_discrete(labels = c("Costs")) +
  scale_fill_manual(values = custom_colors, guide = guide_legend(title = "Locations")) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 28), 
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    plot.margin = margin(l = -20),
    legend.position = "bottom",
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 26),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()  
  )
print(p2)

filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/total_cost_ordered_heng.png")
ggsave(filename, p2, width = 12, height = 4, units = "in", dpi = 400)


###############################################################Bar charts of three economic avoidances in each continent

library(data.table)
library(dplyr)
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
head(number)
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(number,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
number_region <- number %>%
  group_by(Super_regions) %>%
  summarise(
    total_treat = sum(cost_treat_total, na.rm=TRUE),
    total_treat_ca = sum(treat_ca,na.rm=TRUE),
    total_sc = sum(cost_sc,na.rm=TRUE),
    total_ca = sum(cost_ca,na.rm=TRUE),
    .groups = "drop" 
  )

number_region <- data.frame(number_region)
library(ggplot2)
library(dplyr)
library(tidyr)
number_region <- tibble::tibble(
  Super_regions = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
  total_treat = c(1947814629, 34929990225, 304023659646, 763209094092, 21570849523, 11915515854),
  total_treat_ca = c(638995136, 9822371549, 8099098369, 2496179262, 813427377, 861747569),
  total_sc = c(7.546487e+09, 3.627341e+11, 2.076697e+12, 1.982148e+12, 3.463500e+11, 8.638533e+10),
  total_ca = c(26122170345, 583787103141, 378192266201, 271385773730, 20099213847, 65227735131)
)

number_region_long <- number_region %>%
  pivot_longer(cols = c(total_treat, total_sc,total_treat_ca ,total_ca),
               names_to = "Cost_Type", values_to = "Cost_Value") %>%
  mutate(
    Cost_Type = factor(Cost_Type, 
                       levels = c("total_treat", "total_sc","total_treat_ca", "total_ca"),
                       labels = c("Direct Costs of Skin Cancer", 
                                  "Indirect Costs of Skin Cancer",
                                  "Direct Costs of Cataracts",
                                  "Indirect Costs of Cataracts")),
    Super_regions = factor(Super_regions, levels = c(
      "Africa", "Asia", "Europe", "North America", "Oceania", "South America"
    )),
    Super_regions_label = as.character(Super_regions),  
    # Super_regions_label = ifelse(Super_regions_label == "North America", "North\nAmerica",
    #                              ifelse(Super_regions_label == "South America", "South\nAmerica",
    #                                     Super_regions_label))
  )


custom_colors <- c("Direct Costs of Skin Cancer" = "#FF9A9B", "Indirect Costs of Skin Cancer" = "#A4E048","Direct Costs of Cataracts" = "#FFC107" , "Indirect Costs of Cataracts" = "#397FC7")

label_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)

p <- ggplot(number_region_long, aes(x = Super_regions_label, y = Cost_Value / 1e12, fill = Cost_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors, name = NULL) +
  labs(x = NULL, y = "Cost (in trillion US$)") +
  theme_minimal() +
  theme(
    #legend.position = "right",                      
    legend.position = "none",
    legend.direction = "horizontal",            
    axis.text.x = element_text(size = 28,color = label_colors[levels(number_region_long$Super_regions)]),
    axis.text.y = element_text(size = 28),
    axis.title = element_text(size = 28),
    legend.title = element_text(size = 28),
    legend.text = element_text(size = 28),
    panel.grid.major.x = element_blank()            
  )+
  guides(fill = guide_legend(nrow = 4))  
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/super_region_type_of_cost.png")
ggsave(filename, p, width = 20, height = 6, units = "in", dpi = 400)


############uncertainty range
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
cl <- makeCluster(12)
registerDoParallel(cl)
years <- 1980:2100
input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/cost_total_3000_origin_norandom_"
output_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_continental_"
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))

foreach(year = years, .packages = c("data.table", "dplyr")) %dopar% {
  input_file <- paste0(input_path_template, year, ".csv")
  inc <- fread(input_file)
  
  total <- merge(inc,region,by=c("country"))
  colnames(total)[which(names(total) == 'Super regions')] <- "Super_regions"
  result <- total %>%
    group_by(year, Super_regions, simulation) %>%
    summarize(
      total_treat = sum(cost_treat_total, na.rm = TRUE), 
      total_treat_ca = sum(treat_ca, na.rm = TRUE), 
      total_vsly_sc = sum(cost_sc, na.rm = TRUE), 
      total_vsly_ca = sum(cost_ca, na.rm = TRUE)
    )
  output_file <- paste0(output_path_template, year, ".csv")
  fwrite(result, file = output_file)
}
stopCluster(cl)


library(data.table)
years <- 1980:2100
input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_continental_"
output_file <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_continental_cost.csv"

year_sum_death <- data.table()
for (year in years) {
  input_file <- paste0(input_path_template, year, ".csv")
  if (file.exists(input_file)) {
    temp_data <- fread(input_file)
    year_sum_death <- rbind(year_sum_death, temp_data, fill = TRUE)
  } else {
    message(paste("File not found:", input_file))
  }
}
fwrite(year_sum_death, file = output_file)

library(data.table)
library(dplyr)
inc <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_continental_cost.csv")
result <- inc %>%
  group_by( Super_regions, simulation) %>%
  summarize(
    treat = sum(total_treat, na.rm = TRUE), 
    treat_ca = sum(total_treat_ca, na.rm = TRUE),
    vsly_sc = sum(total_vsly_sc, na.rm = TRUE),
    vsly_ca = sum(total_vsly_ca, na.rm = TRUE)
  )
total <- result %>%
  group_by(Super_regions) %>%
  summarise(
    treat_upper = quantile(treat, 0.975),
    treat_lower = quantile(treat, 0.025),
    treat_ca_upper = quantile(treat_ca, 0.975),
    treat_ca_lower = quantile(treat_ca, 0.025),
    ind_sc_upper = quantile(vsly_sc, 0.975),
    ind_sc_lower = quantile(vsly_sc, 0.025),
    ind_ca_upper = quantile(vsly_ca, 0.975),
    ind_ca_lower = quantile(vsly_ca, 0.025),
    .groups = "drop" 
  )
total <- data.frame(total)

####################################################################scatter plot

library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(ggpattern)
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
number_region <- number %>%
  group_by(country) %>%
  summarise(
    total_cost_accumu = sum(cost_total, na.rm=TRUE),
    .groups = "drop" 
  )
number_region <- data.frame(number_region)

pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_daly_cost.csv")
pop <- pop[,c("country","year","age_name","sex_name","pop_country_age_sex_permillion")]
pop$pop_country_age_sex <- pop$pop_country_age_sex_permillion*10^6
pop_per <- pop %>%
  group_by(country) %>%
  summarise(pop_year = sum(pop_country_age_sex, na.rm = TRUE))
total <- merge(number_region,pop_per,by=("country"))
total_per <- total
total_per$cost_per <- total_per$total_cost_accumu/total_per$pop_year

type <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/Montreal_parties.csv")
unique(type$country)
type <- type %>%
  mutate(country = ifelse(country == "C么te d'Ivoire", "Côte d'Ivoire", country))
total <- merge(total_per,type,by=c("country"))
total$GDP_per_thou <- total$GDP_per/1000
total$cost_year_billion <- total$total_cost_accumu/10^9
head(total)

library(ggplot2)
#install.packages("ggrepel")
library(ggrepel)
library(scales)



custom_log_trans <- trans_new(
  name = "custom_log",
  transform = function(x) sign(x) * log(abs(x) + 1),  
  inverse = function(x) sign(x) * (exp(abs(x)) - 1)  
)

library(ggpmisc) 


p <- ggplot(total, aes(x = GDP_per_thou, y = cost_per, color = type, size = cost_year_billion, label = country)) +
  geom_point(alpha = 0.6) + 
  geom_text_repel(aes(size = cost_year_billion), show.legend = FALSE, max.overlaps = 10) + 
  scale_size_continuous(range = c(5, 10)) +  
  scale_x_log10() + 
  scale_y_log10() +  
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") + 
  labs(x = "Log Scale of GDP Per Capita (Thousand US$)", 
       y = "Log Scale of Costs Avoided Per Capita (US$)") +  
  theme_minimal() +  
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),  
    axis.title.x = element_text(size = 24),  
    axis.title.y = element_text(size = 24),  
    legend.position = "none",  
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10, unit = "pt")) 

print(p)


model <- lm(log10(cost_per) ~ log10(GDP_per_thou), data = total)
summary(model)  
model_data <- model.frame(log10(cost_per) ~ log10(GDP_per_thou), data = total)
nrow(model_data)  
nan_rows <- total[is.nan(log10(total$cost_per)) | is.nan(log10(total$GDP_per_thou)), ]
nan_rows 

print(p)

filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/cost_percapita_GDP_total.png")
ggsave(filename, p, width = 16, height = 16, units = "in", dpi = 400)



library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
cl <- makeCluster(12) 
registerDoParallel(cl)
years <- 1980:2100

input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/cost_total_3000_origin_"
output_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_party_type_"
type <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/Montreal_parties.csv")
unique(type$country)
type <- type %>%
  mutate(country = ifelse(country == "C么te d'Ivoire", "Côte d'Ivoire", country))

foreach(year = years, .packages = c("data.table", "dplyr")) %dopar% {
  input_file <- paste0(input_path_template, year, ".csv")
  inc <- fread(input_file)
  total <- merge(inc,type,by=c("country"))
  
  result <- total %>%
    group_by(year, type, random_order) %>%
    summarize(
      total_treat = sum(cost_treat_total, na.rm = TRUE), 
      total_vsly_sc = sum(cost_sc, na.rm = TRUE),
      total_vsly_ca = sum(cost_ca, na.rm = TRUE)
    )
  output_file <- paste0(output_path_template, year, ".csv")
  fwrite(result, file = output_file)
}
stopCluster(cl)


library(data.table)
years <- 1980:2100
input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_party_type_"
output_file <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_party_cost_type.csv"
year_sum_death <- data.table()
for (year in years) {
  input_file <- paste0(input_path_template, year, ".csv")
  if (file.exists(input_file)) {
    temp_data <- fread(input_file)
    year_sum_death <- rbind(year_sum_death, temp_data, fill = TRUE)
  } else {
    message(paste("File not found:", input_file))
  }
}
fwrite(year_sum_death, file = output_file)


#########pie chart
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
type <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/Montreal_parties.csv")
number <- merge(type,number,by=c("country"))
unique(number$country)
number_region <- number %>%
  group_by(type) %>%
  summarise(
    total_cost_accumu = sum(cost_total, na.rm=TRUE),
    .groups = "drop" 
  )

total_ <- number_region %>%
  mutate(percentage = sprintf("%.1f", total_cost_accumu / sum(number_region$total_cost_accumu) * 100))

#sum(number_type$cost_year)
custom_colors <- c("A5" = "#F8766D", "Non-A5" = "#00BFC4")

total_ <- data.frame(total_)
p <- ggplot(total_, aes(x = "", y = total_cost_accumu, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_manual(values = custom_colors) +  
  labs(x = NULL, y = NULL) +  
  theme_void() +  
  theme(legend.position = "none") 
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/pin_cost_a5_nona5_accumu.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)


###########cost and benefits
library(ggplot2)
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
number <- number[number$year <= 2024,]
type <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/Montreal_parties.csv")
number <- merge(type,number,by=c("country"))
unique(number$country)
number_region <- number %>%
  group_by(type) %>%
  summarise(
    total_cost_accumu = sum(cost_total, na.rm=TRUE),
    .groups = "drop"
  )
number_region <- data.frame(number_region)
sum(number_region$total_cost_accumu)
data <- data.frame(
  category = c("Costs", "Benefits"),
  value = c(4.7e9, 60681663515),  
  A5_value = c(NA, 6258756949),   
  Non_A5_value = c(NA, 54422906565) 
)

data$A5_percent <- data$A5_value / data$value * 100
data$Non_A5_percent <- data$Non_A5_value / data$value * 100

p <- ggplot(data) +
  geom_bar(aes(x = category, y = value), stat = "identity", fill = "#00BFC4", width = 0.5) + 
  geom_bar(aes(x = category, y = A5_value), stat = "identity", fill = "#F8766D", width = 0.5) + 
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        title = element_text(size = 22),
        legend.position = "none")  

print(p)
ggsave("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/costs_benefits_barchart.png", p, width = 10, height = 8, dpi = 300)



#####################################蒙约避免的总费用占据各国GDP以及卫生支出的比例
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
cl <- makeCluster(12)
registerDoParallel(cl)
years <- 1980:2100

input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/cost_total_3000_origin_norandom_"
output_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_country_type_norandom_"
foreach(year = years, .packages = c("data.table", "dplyr")) %dopar% {
  input_file <- paste0(input_path_template, year, ".csv")
  inc <- fread(input_file)
  
  result <- inc %>%
    #group_by(year, country, random_order) %>%
    group_by(year, country, simulation) %>%
    summarize(
      total_treat = sum(cost_treat_total, na.rm = TRUE), 
      total_treat_ca = sum(treat_ca,na.rm = TRUE),
      total_vsly_sc = sum(cost_sc, na.rm = TRUE), 
      total_vsly_ca = sum(cost_ca, na.rm = TRUE)
    )
  output_file <- paste0(output_path_template, year, ".csv")
  fwrite(result, file = output_file)
}
stopCluster(cl)


library(data.table)
years <- 1980:2100
input_path_template <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_country_type_norandom_"
output_file <- "C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_country_cost_type_norandom.csv"
year_sum_death <- data.table()
for (year in years) {
  input_file <- paste0(input_path_template, year, ".csv")
  if (file.exists(input_file)) {
    temp_data <- fread(input_file)
    year_sum_death <- rbind(year_sum_death, temp_data, fill = TRUE)
  } else {
    message(paste("File not found:", input_file))
  }
}
fwrite(year_sum_death, file = output_file)

number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/year_sum_country_cost_type_norandom.csv")
number$total_cost = rowSums(cbind(number$total_treat,number$total_treat_ca, number$total_vsly_sc , number$total_vsly_ca), na.rm = TRUE)
head(number)
number_region <- number %>%
  #group_by(country,random_order) %>%
  group_by(country,simulation) %>%
  summarise(
    total_cost_accumu = sum(total_cost, na.rm=TRUE),
    .groups = "drop"
  )
head(number_region)
fwrite(number_region,file="C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/cost_total/sum_country_cost_type_norandom.csv",row.names = FALSE)



##############################################################################Circular stacked bar chart
library(data.table)
library(dplyr)
library(ggplot2)
number <-fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
number$total_sk = rowSums(cbind(number$cost_treat_total, number$cost_sc), na.rm = TRUE)
number$total_ca = rowSums(cbind(number$treat_ca, number$cost_ca), na.rm = TRUE)

number_region <- number %>%
  group_by(country) %>%
  summarise(
    cost_total_ = sum(cost_total, na.rm=TRUE),
    sk_total = sum(total_sk,na.rm=TRUE),
    ca_total = sum(total_ca,na.rm=TRUE),
    .groups = "drop"
  )
number_region <- data.frame(number_region)
pop <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cataract_daly_cost.csv")
pop <- pop[,c("country","year","age_name","sex_name","pop_country_age_sex_permillion")]
pop$pop_country_age_sex <- pop$pop_country_age_sex_permillion*10^6
pop_per <- pop %>%
  group_by(country) %>%
  summarise(pop_year = sum(pop_country_age_sex, na.rm = TRUE))
total <- merge(number_region,pop_per,by=("country"))

type <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/Montreal_parties.csv")
unique(type$country)
type <- type %>%
  mutate(country = ifelse(country == "C么te d'Ivoire", "Côte d'Ivoire", country))
total <- merge(total,type,by=c("country"))
head(total)

total$ratio_total <- total$cost_total_/total$GDP_2020*100
total$ratio_sk <- total$sk_total/total$GDP_2020*100
total$ratio_ca <- total$ca_total/total$GDP_2020*100
total_sorted <- total %>%
  arrange(desc(ratio_total)) %>%
  head(15)

library(ggplot2)
library(dplyr)
library(tidyr)

df_long <- total_sorted %>%
  select(country, type, ratio_sk, ratio_ca) %>%
  pivot_longer(cols = c(ratio_sk, ratio_ca), names_to = "variable", values_to = "value")

total_sorted <- total_sorted %>%
  mutate(total = ratio_sk + ratio_ca) %>%
  arrange(desc(total)) %>%
  mutate(country = factor(country, levels = country)) 

df_long <- df_long %>%
  mutate(country = factor(country, levels = total_sorted$country),
         id = as.numeric(country)) 

n_country <- nlevels(df_long$country)
angle_per_country <- 360 / n_country

label_df <- tibble(
  country = levels(df_long$country),
  id = 1:n_country,
  angle = ((0:(n_country - 1)) * angle_per_country + angle_per_country / 2) %% 360
) %>%
  left_join(dplyr::select(total_sorted, country, type), by = "country")

label_df <- label_df %>%
  mutate(country = ifelse(country == "United Kingdom", "United\nKingdom", country)) %>%
  mutate(country = ifelse(country == "New Zealand", "New\nZealand", country)) %>%
  mutate(country = ifelse(country == "United States", "United\nStates", country))

type_colors <- c("A5" = "#F8766D", "Non-A5" = "#00BFC4")

max_y <- max(df_long$value)

p <- ggplot(df_long, aes(x = id, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_x_continuous(limits = c(0.5, n_country + 0.5)) +
  
  geom_segment(data = label_df,
               aes(x = id, xend = id, y = 0, yend = max_y * 1.15),
               inherit.aes = FALSE,
               color = "grey80", linetype = "dashed", linewidth = 0.4) +
  
  geom_hline(yintercept = c(10, 20, 30),
             color = "grey70", linetype = "dashed", linewidth = 0.6) +
  
  coord_polar(start = 0) +
  
  scale_fill_manual(values = c("ratio_sk" = "#516770", "ratio_ca" = "#A24D56"),
                    labels = c(
                    "Avoided Costs of Cataracts", 
                    "Avoided Costs of Skin Cancer")) +
  
  geom_text(data = label_df,
            aes(x = id, y = max_y * 1.2, label = country, color = type),
            inherit.aes = FALSE,
            hjust = 0.5, vjust = 0.5,
            size = 9, fontface = "bold") +
  
  geom_text(aes(label = ifelse(value > 0, as.integer(round(value)), "")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 7,
            fontface = "bold") +
  
  scale_color_manual(values = type_colors,
                     labels = c("A5 Countries", "Non-A5 Countries")) +
  
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 26))+  
guides(
  fill = guide_legend(nrow = 3),  
  color = guide_legend(nrow = 2)  
) 

print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/meigui_gdp.png")
ggsave(filename, p, width = 12, height = 14, units = "in", dpi = 400)




total <- merge(number_region,pop_per,by=("country"))
type <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/Montreal_parties.csv")
unique(type$country)
type <- type %>%
  mutate(country = ifelse(country == "C么te d'Ivoire", "Côte d'Ivoire", country))
total <- merge(total,type,by=c("country"))
head(total)

total$ratio_total <- total$cost_total_/total$health_spend*100
total$ratio_sk <- total$sk_total/total$health_spend*100
total$ratio_ca <- total$ca_total/total$health_spend*100
total_sorted <- total %>%
  arrange(desc(ratio_total)) %>%
  head(15)

library(ggplot2)
library(dplyr)
library(tidyr)

df_long <- total_sorted %>%
  select(country, type, ratio_sk, ratio_ca) %>%
  pivot_longer(cols = c(ratio_sk, ratio_ca), names_to = "variable", values_to = "value")

total_sorted <- total_sorted %>%
  mutate(total = ratio_sk + ratio_ca) %>%
  arrange(desc(total)) %>%
  mutate(country = factor(country, levels = country)) 

df_long <- df_long %>%
  mutate(country = factor(country, levels = total_sorted$country),
         id = as.numeric(country)) 

n_country <- nlevels(df_long$country)
angle_per_country <- 360 / n_country

label_df <- tibble(
  country = levels(df_long$country),
  id = 1:n_country,
  angle = ((0:(n_country - 1)) * angle_per_country + angle_per_country / 2) %% 360
) %>%
  left_join(dplyr::select(total_sorted, country, type), by = "country")

label_df <- label_df %>%
  mutate(country = ifelse(country == "United Kingdom", "United\nKingdom", country)) %>%
  mutate(country = ifelse(country == "New Zealand", "New\nZealand", country)) %>%
  mutate(country = ifelse(country == "United States", "United\nStates", country))

type_colors <- c("A5" = "#F8766D", "Non-A5" = "#00BFC4")


max_y <- max(df_long$value)

p <- ggplot(df_long, aes(x = id, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_x_continuous(limits = c(0.5, n_country + 0.5)) +
  
  geom_segment(data = label_df,
               aes(x = id, xend = id, y = 0, yend = max_y * 1.15),
               inherit.aes = FALSE,
               color = "grey80", linetype = "dashed", linewidth = 0.4) +
  
  geom_hline(yintercept = c(100, 200, 300),
             color = "grey70", linetype = "dashed", linewidth = 0.6) +
  
  coord_polar(start = 0) +
  
  scale_fill_manual(values = c("ratio_sk" = "#516770", "ratio_ca" = "#A24D56"),
                    labels = c(
                      "Avoided Costs of Cataracts", 
                      "Avoided Costs of Skin Cancer")) +
  
  geom_text(data = label_df,
            aes(x = id, y = max_y * 1.2, label = country, color = type),
            inherit.aes = FALSE,
            hjust = 0.5, vjust = 0.5,
            size = 9, fontface = "bold") +
  
  geom_text(aes(label = ifelse(value > 13, as.integer(round(value)), "")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 6,
            fontface = "bold") +
  
  scale_color_manual(values = type_colors,
                     labels = c("A5 Countries", "Non-A5 Countries")) +
  
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Arial"),
        legend.text = element_text(size = 26))+
  guides(
    fill = guide_legend(nrow = 3), 
    color = guide_legend(nrow = 2)
  ) 

print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/meigui_health.png")
ggsave(filename, p, width = 12, height = 14, units = "in", dpi = 400)


###################################################Draw the average result as a bar chart
library(data.table)
library(dplyr)
total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
type <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/Montreal_parties.csv")
type <- type %>%
  mutate(country = ifelse(country == "C么te d'Ivoire", "Côte d'Ivoire", country))
total <- merge(total,type,by=("country"))
total$sum_cost_sc = rowSums(cbind(total$cost_treat_total,total$cost_sc), na.rm = TRUE)
total$sum_cost_ca = rowSums(cbind(total$cost_ca,total$treat_ca), na.rm = TRUE)
total <- total %>%
  group_by(type) %>%
  summarise(
    total_cost = sum(cost_total, na.rm=TRUE),
    total_sc = sum(sum_cost_sc,na.rm=TRUE),
    total_ca = sum(sum_cost_ca,na.rm=TRUE),
    .groups = "drop" 
  )
GDP <- type %>%
  group_by(type) %>%
  summarise(
    GDP_v = sum(GDP_2020, na.rm=TRUE),
    .groups = "drop"
  )
total <- merge(total,GDP,by=("type"))
total$ratio_sc <- total$total_sc/total$GDP_v*100
total$ratio_ca <- total$total_ca/total$GDP_v*100

library(tidyr)
library(ggplot2)
data_long <- total %>%
  gather(key = "ratio_type", value = "value", ratio_sc, ratio_ca)
p <- ggplot(data_long, aes(x = type, y = value, fill = ratio_type)) +
  geom_bar(stat = "identity", position = "stack", show.legend = FALSE, width = 0.8) +
  scale_fill_manual(values = c('ratio_sc' = '#516770', 'ratio_ca' = '#A24D56')) +
  labs(x = NULL, y = 'Percentage (%)') +
  scale_x_discrete(labels = c("A5" = "A5 Countries", "Non-A5" = "Non-A5 Countries")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26, color = c('#F8766D', '#00BFC4')), 
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 28, hjust = 0.5)
  )
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/bar_gdp.png")
ggsave(filename, p, width = 7, height = 10, units = "in", dpi = 400)


library(data.table)
library(dplyr)
total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
type <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/Montreal_parties.csv")
type <- type %>%
  mutate(country = ifelse(country == "C么te d'Ivoire", "Côte d'Ivoire", country))
total <- merge(total,type,by=("country"))
total$sum_cost_sc = rowSums(cbind(total$cost_treat_total,total$cost_sc), na.rm = TRUE)
total$sum_cost_ca = rowSums(cbind(total$cost_ca,total$treat_ca), na.rm = TRUE)
total <- total %>%
  group_by(type) %>%
  summarise(
    total_cost = sum(cost_total, na.rm=TRUE),
    total_sc = sum(sum_cost_sc,na.rm=TRUE),
    total_ca = sum(sum_cost_ca,na.rm=TRUE),
    .groups = "drop"
  )
health <- type %>%
  group_by(type) %>%
  summarise(
    health_v = sum(health_spend, na.rm=TRUE),
    .groups = "drop" 
  )
total <- merge(total,health,by=("type"))
total$ratio_sc <- total$total_sc/total$health_v*100
total$ratio_ca <- total$total_ca/total$health_v*100

library(tidyr)
library(ggplot2)
data_long <- total %>%
  gather(key = "ratio_type", value = "value", ratio_sc, ratio_ca)
p <- ggplot(data_long, aes(x = type, y = value, fill = ratio_type)) +
  geom_bar(stat = "identity", position = "stack", show.legend = FALSE, width = 0.8) +
  scale_fill_manual(values = c('ratio_sc' = '#516770', 'ratio_ca' = '#A24D56')) +
  labs(x = NULL, y = 'Percentage (%)') +
  scale_x_discrete(labels = c("A5" = "A5 Countries", "Non-A5" = "Non-A5 Countries")) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 26, color = c('#F8766D', '#00BFC4')),  
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 28, hjust = 0.5)
  )
print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/bar_health.png")
ggsave(filename, p, width = 7, height = 10, units = "in", dpi = 400)



#########################################################################
###########################################A bar chart of the continental accumulation of avoided costs

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
library(data.table)
library(patchwork)
library(tidyr)

total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(total,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
unique(number$country)
number_region <- number %>%
  group_by(year, Super_regions) %>%
  summarise(
    treat = sum(cost_treat_total, na.rm = TRUE), 
    treat_ca_total =sum(treat_ca, na.rm = TRUE),
    sc = sum(cost_sc, na.rm = TRUE),
    ca = sum(cost_ca, na.rm = TRUE),
    .groups = "drop" 
  )

number_region <- data.frame(number_region)

region_colors <- c(
  "Africa" = "#D89A76",
  "North America" = "#75B9CD",
  "South America" = "#AD59E0",
  "Asia" = "#D8D966",
  "Europe" = "#DF73AF",
  "Oceania" = "#AADBCB"
)
number_region$period <- ifelse(number_region$year <= 2030, "Before 2030", "After 2030")
number_region$scaled_year <- ifelse(number_region$period == "Before 2030",
                                    scales::rescale(number_region$year, from = c(min(number_region$year[number_region$period == "Before 2030"]), 2030), to = c(0, 1)),
                                    scales::rescale(number_region$year, from = c(2030, max(number_region$year[number_region$period == "After 2030"])), to = c(1, 8)))

width_before_2030 <- 0.02-0.001
width_after_2030 <- 0.1-0.01

number_region$cost_treat_total_billion <- number_region$treat/10^9
number_region$cost_treat_ca_total_billion <- number_region$treat_ca_total/10^9
number_region$cost_vsly_sc_billion <- number_region$sc/10^9
number_region$cost_vsly_ca_billion <- number_region$ca/10^9


p <- ggplot(number_region, aes(x = scaled_year, y = cost_vsly_ca_billion, fill = Super_regions)) +
  geom_bar(stat = "identity", width = ifelse(number_region$period == "Before 2030", width_before_2030, width_after_2030)) +
  #scale_fill_manual(values = regions_palette, name = "Geographic Regions") +  
  scale_fill_manual(values = region_colors, name = "Geographic Regions") + 
  labs(
    #title = "Numbers of Skin Cancer Deaths Avoided by Montreal Protocol in Different Geographic Super Regions",
    x = "Year",
    y = "Costs (in billion US$)") +
  scale_x_continuous(
    #breaks = c(seq(0, 1, by = 0.2), seq(1, 8, by = 0.5)),
    breaks = c(seq(0, 1, by = 0.5), seq(1, 8, by = 1)),
    labels = c(seq(1980, 2030, by = 25), seq(2030, 2100, by = 10))
  ) +scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5) 
    #breaks = seq(0, 0.75, by = 0.25)  
    #breaks = seq(0, 125, by = 25)  
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        #axis.text.x = element_text(angle = 45, hjust = 0.5,size = 24),  
        axis.text.x = element_text(angle = -90, hjust = 0.5,vjust=0.5,size = 24),
        axis.text.y = element_text(size = 26),  
        axis.title.x = element_text(size = 28),  
        axis.title.y = element_text(size = 28), 
        plot.title = element_text( size = 18,hjust = 0.5,),
        legend.title = element_text(size = 22), 
        legend.text = element_text(size = 22),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray", size = 0.5),  
        panel.grid.minor = element_line(color = "lightgray", size = 0.25) 
        )+
  #legend.position = c(0.45, 0.8))+
  #legend.position = "none"
  guides(fill = guide_legend(ncol = 1)) 
print(p)
#filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/treat_cost_year_avoided_by_continental_regions.png")
#filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/vsly_sc_cost_year_avoided_by_continental_regions.png")
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/vsly_ca_cost_year_avoided_by_continental_regions.png")
#filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/treat_ca_cost_year_avoided_by_continental_regions.png")
ggsave(filename, p, width = 8, height = 8, units = "in", dpi = 400)



library(ggpattern)
library(ggrepel)
library(dplyr)
library(tidyr)
library(ggplot2)
total <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/cost_total_oe.csv")
region <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/continental_regions_2025.csv")
unique(region$`Super regions`)
unique(region$Regions)
colnames(region)[which(names(region) == "NAME_LONG")] <- "country"
region <- region %>%
  mutate(country = ifelse(country == "C?te d'Ivoire", "Côte d'Ivoire", country))
number <- merge(total,region,by=c("country"))
colnames(number)[which(names(number) == 'Super regions')] <- "Super_regions"
unique(number$country)
number_region <- number %>%
  #group_by(Super_regions) %>%
  summarise(
    treat = sum(cost_treat_total, na.rm = TRUE), 
    vsly_sc = sum(cost_sc, na.rm = TRUE), 
    vsly_ca = sum(cost_ca, na.rm = TRUE), 
    .groups = "drop" 
  )
total_long <- number_region %>%
  pivot_longer(
    cols = c(treat,vsly_sc,vsly_ca),
    names_to = "cost_type",
    values_to = "cost"
  )
number_type <- total_long
number_type <- data.frame(number_type)

number_type <- number_type %>%
  mutate(percentage = sprintf("%.0f", cost / sum(cost) * 100))
sum(number_type$cost)

custom_colors <- c("#FF9A9B", "#397FC7","#A4E048")

p <- ggplot(number_type, aes(x = "", y = cost, fill = cost_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(
    values = custom_colors,
    labels = c("Treatment Costs of Skin Cancer", "Indirect Costs of Cataracts", "Indirect Costs of Skin Cancer") 
  ) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(), 
    legend.text = element_text(size = 28) 
  ) +
  guides(
    fill = guide_legend(nrow = 3)  
  ) 

print(p)
filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/pin_cost_type_3_new.png")
ggsave(filename, p, width = 10, height = 6, units = "in", dpi = 400)



############################distribution of avoided incidence
library(data.table)
library(dplyr)
number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_cataract_daly_1980_2100.csv")
#number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_de_1980_2100.csv")
#number <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/optimal_estimation/total_number_1980_2100.csv")
wide_per_country <- number %>%
  group_by(country) %>%
  summarise(
    dif_total_country = sum(dif, na.rm=TRUE),
    .groups = "drop"
  )
pop <- number %>%
  group_by(country) %>%
  summarise(
    pop_total_country = sum(pop_country_age_sex_permillion, na.rm=TRUE),
    .groups = "drop" 
  )
total <- merge(wide_per_country,pop,by=c("country"))
total$inc <- total$dif_total_country/total$pop_total_country

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

world_merged_1 <- merge(world_new, total, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)
boundary <- st_boundary(world_merged_1)

min_value <- min(c(min(world_merged_1$inc, na.rm = TRUE)))
max_value <- max(c(max(world_merged_1$inc, na.rm = TRUE)))
quantiles <- quantile(world_merged_1$inc, probs = seq(0, 1, 0.01), na.rm = TRUE)

world_merged_1$centroid <- st_point_on_surface(world_merged_1$geometry)
world_merged_1$lon <- st_coordinates(world_merged_1$centroid)[, 1]
world_merged_1$lat <- st_coordinates(world_merged_1$centroid)[, 2]
centroid_coordinates <- st_coordinates(world_merged_1$centroid)
centroid_data <- data.frame(
  NAME_LONG = world_merged_1$NAME_LONG,  
  dif_total_country = world_merged_1$inc,  
  lon = centroid_coordinates[, "X"],  
  lat = centroid_coordinates[, "Y"]  
)


# world_merged_1 <- world_merged_1 %>%
#   mutate(dif_total_category = cut(
#     inc,
#     breaks = c(-10, 0, 10, 50, 100, 300, 500, 1000, 5000, max_value),
#     labels = c("<0", "0~10", "10~50", "50~100", "100~300", "300~500", "500~1,000","1,000~5,000", ">5,000"),
#     include.lowest = TRUE
#   ))


# world_merged_1 <- world_merged_1 %>%
#   mutate(dif_total_category = cut(
#     inc,
#     breaks = c(-2, 0, 0.5,1, 2, 5, 10, 20,50,max_value),
#     labels = c("<0", "0~0.5", "0.5~1", "1~2", "2~5", "5~10", "10~20","20~50", ">50"),
#     include.lowest = TRUE
#   ))


world_merged_1 <- world_merged_1 %>%
  mutate(dif_total_category = cut(
    inc,
    breaks = c(-70, 0, 10, 30, 50, 70, 100, 150, 200, max_value),
    labels = c("<0", "0~10", "10~30", "30~50", "50~70", "70~100", "100~150","150~200", ">200"),
    include.lowest = TRUE
  ))


custom_colors <- c(
  "<0" = "#C8D6E7",
  "0~10" = "#E8EDF1",
  "10~30" = "#F2EBE5",
  "30~50" = "#ECD0B4",
  "50~70" = "#F27F69",
  "70~100" = "#C26B57",
  "100~150" = "#A13D3B",
  "150~200" = "#831A21",
  ">200" = "#4A1416"
)

# custom_colors <- c(
#   "<0" = "#C8D6E7",
#   "0~0.5" = "#E8EDF1",
#   "0.5~1" = "#F2EBE5",
#   "1~2" = "#ECD0B4",
#   "2~5" = "#F27F69",
#   "5~10" = "#C26B57",
#   "10~20" = "#A13D3B",
#   "20~50" = "#831A21",
#   ">50" = "#4A1416"
# )


# custom_colors <- c(
#   "<0" = "#C8D6E7",
#   "0~10" = "#E8EDF1",
#   "10~50" = "#F2EBE5",
#   "50~100" = "#ECD0B4",
#   "100~300" = "#F27F69",
#   "300~500" = "#C26B57",
#   "500~1,000" = "#A13D3B",
#   "1,000~5,000" = "#831A21",
#   ">5,000" = "#4A1416"
# )


filtered_world_merged_1 <- world_merged_1 %>%
  filter(!(near(lon, 114.0559975, tol = 1e-3) & near(lat, 22.41120026, tol = 1e-3)) &  
           !(near(lon, 113.5594336, tol = 1e-3) & near(lat, 22.13617585, tol = 1e-3)))   
p1 <- ggplot(data = filtered_world_merged_1) +
  geom_sf(aes(fill = dif_total_category), color = NA) +
  geom_sf(data = boundary, color = "#696969", fill = NA, linewidth = 0.5) +
  scale_fill_manual(
    values = custom_colors,
    name = "DALYs Per Million",
    #name = "Numbers Per Million",
    na.translate = FALSE  
  ) +
  labs(fill = "DALYs Per Million") +
  #labs(fill = "Numbers Per Million") +
  #  ggtitle("(a) 2030") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text( size = 18),
    legend.title = element_text(size = 22),  
    legend.text = element_text(size = 22), 
  ) +
  guides(
    #fill = guide_colorbar(
    fill = guide_legend(
      barheight = unit(0.5, "cm"),
      barwidth = unit(0.5, "cm")
    )
  )

filename <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/DALY_rates_avoided.png")
ggsave(filename, p1, width = 16, height = 8, units = "in", dpi = 400)


#################################A grid diagram of avoided radiation
library(data.table)
library(dplyr)
library(tidyr)
ra <- fread("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/results of TUV/ra_annual_NMSC.csv")
head(ra)
ra$ra_mj <- ra$sum_y*3600/10^6
unique(ra$year)
ra <- ra[ra$year >= 1979,]
result <- ra %>%
  group_by(lat, lon) %>%
  summarise(mean_ra_mj = mean(ra_mj))
result <- data.frame(result)
library(data.table)
library(raster)
gen_raster <- function(value_cells, r.ext, r.res_row, r.res_col) {
  # Calculate the number of rows and columns based on resolutions
  n.rows <- ceiling((r.ext[4] - r.ext[3]) / r.res_row)
  n.cols <- ceiling((r.ext[2] - r.ext[1]) / r.res_col)
  
  # Create a raster with specified rows, columns, extent, and CRS
  value_raster <- raster(matrix(NA, nrow = n.rows, ncol = n.cols),
                         xmn = r.ext[1], xmx = r.ext[2], ymn = r.ext[3], ymx = r.ext[4],
                         crs = "+proj=longlat +datum=WGS84")
  
  # Calculate the row and column indices for each value cell
  row_indices <- ceiling((r.ext[4] - value_cells[, 1]) / r.res_row)
  col_indices <- ceiling((value_cells[, 2] - r.ext[1]) / r.res_col)
  
  # Assign values to the corresponding cells in the raster
  for (i in 1:nrow(value_cells)) {
    value_raster[row_indices[i], col_indices[i]] <- value_cells[i, 3]
  }
  return(value_raster)
}
r.ext <- c(-180, 180, -90, 90)
r.res_row <- 1
r.res_col <- 1.25
library(sp)
library(sf)
# load the map
world_new <- st_read("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/map/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)
world_new <- as(world_new,"Spatial")

df <- result[,c("lat","lon","mean_ra_mj")]
quantile(df$mean_ra_mj,na.rm=TRUE)
library(colorRamps)
breaks <- seq(0, 8, 1)
cols <- matlab.like(length(breaks))
min_value = min(breaks)
max_value = max(breaks)
breaks_label = seq(0, 8, 1)
df <- data.frame(df)
r <- gen_raster(df, r.ext, r.res_row,r.res_col)
r[r<min_value] <- min_value
r[r>max_value] <- max_value
output_file <- paste("C:/Users/Administrator/Desktop/health benefits and economic welfare of MP/drawings/", "ra_mean_nmsc_wmo", ".tiff", sep = "")
tiff(output_file, width = 640*4, height = 400*4, res = 99*4, compression = "lzw")
tmp <- spplot(r,
              col.regions=cols, at=breaks, maxpixels=500000,
              #colorkey=list(labels=list(labels=c(expression(paste("0 (",  "MJ/m"^2,")")),"1", "2", "3", "4","5","6","7","8","9","10","11"
              colorkey=list(labels=list(labels=c(expression(paste("0 (",  "MJ/m"^2,")")),"1", "2", "3", "4","5","6","7","8"
              ), at=breaks_label), space="right", width=1.2),
              xlim=c(-180,180),ylim=c(-90,90),
              panel=function(...) {
                panel.gridplot(...)
                sp.polygons(world_new, col="black",cex=1)
              },
             # main = sprintf("%d/%02d", i, j)
              )
print(tmp)
dev.off()
