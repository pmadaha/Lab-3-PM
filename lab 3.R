install.packages("tidyverse")
install.packages("spdep")
install.packages("spData")
install.packages("sf")
install.packages("tmap")

library (tidyverse)
library (spdep)
library (spData)
library (sf)
library(tmap)


d.all <- sf::read_sf ("./ACS_2020_5YR_COUNTY.gdb/ACS_2020_5YR_COUNTY.gdb", layer = "ACS_2020_5YR_COUNTY")
glimpse (d.all)

d.xl <- sf::read_sf ("./ACS_2020_5YR_COUNTY.gdb/ACS_2020_5YR_COUNTY.gdb", layer = "X01_AGE_AND_SEX") %>% mutate (fixed_geoid = str_sub (GEOID, start = 8, end = -1))

d.joined <- d.all %>% left_join(., d.xl, by = c("GEOID" = "fixed_geoid"))




Task1<- d.all %>% dplyr :: filter(STATEFP %in% c("26", "39","18", "21", "47"))
tmap::tm_shape(Task1) + tm_polygons()

Task_join <- Task1 %>% left_join(., d.xl, by = c("GEOID" = "fixed_geoid"))

# you can ignore the top bit, I created the subset after completing my lab instead of prior. 

sf::write_sf(Task_join, "Task_join.shp")
Task_join<- sf::read_sf("./Task_join.shp")


summary (Task_join$B01001e34)

Task2<- Task_join %>% group_by(STATEFP) %>% summarise (Avg_Fem_22_24 = sum (B01001e34, na.rm = TRUE))

#Task Variable chosen: B01001e34 Total Population, Female: 22 to 24 years

Task3<- ggplot (Task2, aes(x=STATEFP, y=Avg_Fem_22_24)) + geom_col(fill = "pink") + labs (title = "Female Population Between the Age of 22 - 24", x = "State", y = "Total Population") + theme_minimal()
plot(Task3)

#Task 4: I removed the title of the map and placed it on the legend because it kept on coming on top of the map rather to the top of it. I did not know what to do, so i just have two options one with and one without the title.

cols4all::c4a_palettes()

tm_shape (Task2) + tm_polygons (fill  = "Avg_Fem_22_24", fill.scale = tm_scale(values = "carto.sunset"), fill.legend = tm_legend (title = "Total Female Population (22-24)")) + tm_layout(title = "Choropleth Map: Female Population (22-24)", title.position = c("center", "top"), frame = TRUE, legend.outside = TRUE, component.autoscale = TRUE)

tm_shape (Task2) + tm_polygons (fill  = "Avg_Fem_22_24", fill.scale = tm_scale(values = "carto.sunset"), fill.legend = tm_legend (title = "Total Female Population (22-24)")) + tm_layout(frame = FALSE, legend.outside = TRUE, component.autoscale = TRUE) 

sf::st_crs(Task2)
AOI.projected <- Task2 %>% sf::st_transform (., "ESRI: 102010")
tmap:: tm_shape(AOI.projected) + tm_polygons()

Task5<- spdep::poly2nb(AOI.projected, queen = FALSE) 
Task5
Task5[[1]] %>% AOI.projected$STATEFP[.]

Task5.1<- nb2listw(Task5, style = "W", zero.policy = TRUE)
Task5.1 $ weights [1]

Task5.2 <- attr (Task5.1$weights, "comp") $d
hist(Task5.2)

Task5.3 <- lag.listw(Task5.1, AOI.projected$Avg_Fem_22_24) 
Task5.3

#Task 5.4 

moran.test(AOI.projected$Avg_Fem_22_24, Task5.1)
moran.plot(AOI.projected$Avg_Fem_22_24, Task5.1, zero.policy=TRUE, plot = TRUE)

sf::st_crs(AOI.projected)

Task6 <- spdep::dnearneigh(st_centroid(AOI.projected), d1=0, d2=600000)
Task6
Task6 [[1]] %>% AOI.projected$STATEFP[.]

Task6.1 <- nb2listw(Task6, style = "W", zero.policy = TRUE)

Task6.2 <- attr(Task6.1$weights, "comp") $d
hist (Task6.2)

Task6.3 <- lag.listw(Task6.1, AOI.projected$Avg_Fem_22_24)
Task6.3

#Task 6.4

moran.test(AOI.projected$Avg_Fem_22_24, Task6.1)
moran.plot(AOI.projected$Avg_Fem_22_24, Task6.1, zero.policy = TRUE, plot = TRUE) 

#Q1: Moran's I measures spatial autocorrelation, by checking for similar or randomly distributed values.The Moran's I tend to be positive when nearby areas share similar values meaning that the variables are spatially clustered together, where as if the Moran's I is negative means the nearby areas have values that aren't similar, expressing the variables are spatially dispersed.
#Q2: The way I understood it, a spatially lagged variable is transforming the weight of the existing variable, by accounting for a weighted average based of the neighbour's values. Basically, how neighboring values influences the variable in question.
#Q3: When using contiguity you see how the neighbours accounted for was much lower than using the IDW, Contiguity would be greater for a local interpretation where as the IDW would be more valued for a regional analysis. However, when looking at the map parts of Ohio would be much closer to Tennessee in comparison to areas that are further north in Michigan, so one would consider the IDW rather than the contiguity. So, one would need to make a judgement based on the variables being analyzed. 
#Q4: The HL would entail that the area has high value but it is surrounded by points representing low values (mainly showcasing an outlier within the dataset). This is crucial when identifying sudden changes towards an area, let's say water accessibility declined due to drying lakes, however, the neighborhood still has working water piped systems. so this scenario entails two factors attributing to one variable (water accessibility) could result to a high value area, however, the declination of one factor may result to low value occurences within the same area.

#BONUS

AOI.mean<- mean(AOI.projected$Avg_Fem_22_24, na.rm = TRUE)
Lag.mean<- mean(Task5.3, na.rm = TRUE)


moran.test(AOI.projected$Avg_Fem_22_24, Task5.3)

Bonus<- AOI.projected %>% mutate(Quadrant = case_when(Avg_Fem_22_24 >= AOI.mean & Task5.3 >= Lag.mean ~ "HH", Avg_Fem_22_24 >= AOI.mean & Task5.3 < Lag.mean ~ "HL", Avg_Fem_22_24 < AOI.mean & Task5.3 < Lag.mean ~ "LL", Avg_Fem_22_24 < AOI.mean & Task5.3 >= Lag.mean ~ "LH"))
Bonus<- Bonus %>% mutate(Task5.3)
aoilag.lm<- lm(Bonus$Avg_Fem_22_24 ~ Bonus$Task5.3)
coef(aoilag.lm)
inf<- influence.measures(aoilag.lm)
infmtrx<- as.data.frame(inf$is.inf)
infrw<- apply(infmtrx, 1, any)
Bonus<- Bonus %>% mutate (is.inf=infrw)
significant<- Bonus [Bonus$is.inf,]

#the significant variable has a different shape in comparison to the rest.

ggplot(Bonus, aes( x = Avg_Fem_22_24, y = Task5.3, colour = STATEFP)) + geom_point() + geom_point(data = significant, aes(x = Avg_Fem_22_24, y = Task5.3), shape = 17) + geom_abline(slope = 0.1278789, intercept = 133456.4) + geom_hline(yintercept = Lag.mean) + geom_vline (xintercept = AOI.mean) + annotate ("text", x=max(Bonus$Avg_Fem_22_24), y= max (Bonus$Task5.3), label="HH") + annotate ( "text" , x=max(Bonus$Avg_Fem_22_24), y= min(Bonus$Task5.3), label= "HL") + annotate ("text", x=min(Bonus$Avg_Fem_22_24), y= min(Bonus$Task5.3), label = "LL") + annotate ("text", x=min(Bonus$Avg_Fem_22_24), y= max(Bonus$Task5.3), label = "LH")  + labs (title = "DIY Moran's I Plot", x = "Observed Value", y = "Spatially Lagged Value") + theme_minimal()

tm_shape (Bonus) + tm_polygons (fill  = "Quadrant", fill.scale = tm_scale(values = "carto.sunset"), fill.legend = tm_legend (title = "Moran's I Quadrant")) + tm_layout(title = "Bonus Moran Plot Quadrant Map", title.position = c("center", "top"), frame = TRUE, legend.outside = TRUE, component.autoscale = TRUE)
