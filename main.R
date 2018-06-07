library(tidyverse)
library(skimr)
library(highcharter)


weed <- read_csv("input/cannabis.csv")

weed %>% skim()
weed %>% glimpse() 
weed %>% head()

by_type <- weed %>% 
  count(Type)

highchart() %>% 
  hc_xAxis(categories = by_type$Type) %>% 
  hc_add_series(name = 'number of cannabis', data = by_type$n, colorByPoint = 1) %>% 
  hc_title(text = "Cannabis by Types")  %>%
  hc_chart(type = 'bar', options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% 
  hc_add_theme(hc_theme_google())

rm(by_type)






















weed_effects <- weed %>% 
  mutate(Effects = str_split(Effects,',')) %>% 
  unnest(Effects)

df1 <- weed %>% 
  group_by(name = Type, drilldown = Type) %>% 
  summarise(y = n()) %>% 
  arrange(desc(y))

df2 <- weed_effects %>% 
  group_by(Type, Effects) %>% 
  mutate(y = n(), colorByPoint =  1) %>% 
  arrange(desc(y)) %>%
  group_by(name = Type, id = Type, colorByPoint) %>% 
  do(data = list_parse(
    mutate(.,name = Effects, drilldown = tolower(paste(Type, Effects,sep=": "))) %>% 
      group_by(name, drilldown) %>% 
      summarise(y=n()) %>% 
      select(name, y, drilldown) %>%
      arrange(desc(y)))) 


# df2$data %>% head(1)

a <- highchart() %>% 
  hc_chart(type = 'bar') %>% 
  hc_xAxis(type = "category") %>% 
  hc_add_series(name = 'number of cannabis', data = df1, colorByPoint = 1) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>%
  hc_legend(enabled = F) %>% 
  hc_title(text = "Type of Cannbis vs Effects") %>% 
  hc_add_theme(hc_theme_darkunica())

rm(df1, df2, weed_effects)



















weed_flavor <- weed %>% 
  filter(Flavor != 'none') %>% 
  mutate(Flavor = str_split(Flavor, ',')) %>% 
  unnest(Flavor)

df1 <- weed %>% 
  group_by(name = Type, drilldown = Type) %>% 
  summarise(y = n()) %>% 
  arrange(desc(y))

df2 <- weed_flavor %>% 
  group_by(Type, Flavor) %>% 
  mutate(y = n(), colorByPoint = 1) %>% 
  arrange(desc(y)) %>%
  group_by(name = Type, id = Type, colorByPoint) %>% 
  do(data = list_parse(
    mutate(.,name = Flavor, drilldown = tolower(paste(Type, Flavor,sep=": "))) %>% 
      group_by(name, drilldown) %>% 
      summarise(y=n()) %>% 
      select(name, y, drilldown) %>%
      arrange(desc(y)))) 

b <- highchart() %>% 
  hc_chart(type = 'bar') %>% 
  hc_xAxis(type = "category") %>% 
  hc_add_series(name = 'number of cannabis', data = df1, colorByPoint = 1) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>%
  hc_legend(enabled = F) %>% 
  hc_title(text = "Type of Cannbis vs Flavor") %>% 
  hc_add_theme(hc_theme_darkunica())
rm(df1, df2, weed_flavor)


weed %>% 
  filter(Type == 'indica') %>% 
  select(Flavor)

lst <- list(
  a,
  b
)

hw_grid(lst, rowheight = 400)
rm(a, b, lst)

























