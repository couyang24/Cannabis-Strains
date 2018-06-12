library(tidyverse)
library(skimr)
library(highcharter)
library(qdap)
library(tm)
library(plotly)
library(viridis)
library(wordcloud)
library(plotrix)
library(DescTools)
library(tidytext)

weed <- read_csv("input/cannabis.csv")

weed %>% skim()
weed %>% glimpse() 
weed %>% head()



weed$Rating %>% PlotFdist("Cannabis Ranking Distribution")




by_type <- weed %>% 
  count(Type)

highchart() %>% 
  hc_xAxis(categories = by_type$Type) %>% 
  hc_add_series(name = 'number of cannabis', data = by_type$n, colorByPoint = 1) %>% 
  hc_title(text = "Cannabis by Types")  %>%
  hc_chart(type = 'bar', options3d = list(enabled = TRUE, beta = 1, alpha = 1)) %>% 
  hc_add_theme(hc_theme_google())



hchart(by_type, type = 'treemap', hcaes(x = 'Type', value = 'n', color = 'n'))

rm(by_type)
















weed_effects <- weed %>% 
  mutate(Effects = str_split(Effects,',')) %>% 
  unnest(Effects)


weed_effects %>% 
  count(Effects) %>% 
  hchart(type = 'treemap', hcaes(x = 'Effects', value = 'n', color = 'n'))


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

(a <- highchart() %>% 
  hc_chart(type = 'pie') %>% 
  hc_xAxis(type = "category") %>% 
  hc_add_series(name = 'number of cannabis', data = df1, colorByPoint = 1) %>% 
  # hc_drilldown(
  #   allowPointDrilldown = TRUE,
  #   series =list_parse(df2)
  # ) %>%
  hc_legend(enabled = F) %>% 
  hc_title(text = "Type of Cannbis vs Effects") %>% 
  hc_add_theme(hc_theme_darkunica()))

rm(df1, df2)



















weed_flavor <- weed %>% 
  filter(Flavor != 'none') %>% 
  mutate(Flavor = str_split(Flavor, ',')) %>% 
  unnest(Flavor)


weed_flavor %>% 
  count(Flavor) %>% 
  hchart(type = 'treemap', hcaes(x = 'Flavor', value = 'n', color = 'n'))



df1 <- weed %>% 
  group_by(name = Type, drilldown = Type) %>% 
  summarise(y = n()) %>% 
  arrange(desc(y))

df2 <- weed_flavor %>% 
  group_by(Type, Flavor) %>% 
  mutate(y = n()
         # , colorByPoint = 1
         ) %>% 
  arrange(desc(y)) %>%
  group_by(name = Type, id = Type
           # , colorByPoint
           ) %>% 
  do(data = list_parse(
    mutate(.,name = Flavor, drilldown = tolower(paste(Type, Flavor,sep=": "))) %>% 
      group_by(name, drilldown) %>% 
      summarise(y=n()) %>% 
      select(name, y, drilldown) %>%
      arrange(desc(y)))) 

(b <- highchart() %>% 
  hc_chart(type = 'bar') %>% 
  hc_xAxis(type = "category") %>% 
  hc_add_series(name = 'number of cannabis', data = df1, colorByPoint = 1) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>%
  hc_legend(enabled = F) %>% 
  hc_title(text = "Type of Cannbis vs Flavor") %>% 
  hc_add_theme(hc_theme_darkunica()))
rm(df1, df2, weed_flavor)



lst <- list(
  a,
  b
)

hw_grid(lst, rowheight = 400)
rm(a, b, lst)







































# clean corpus
cleanCorpus <- function(corpus){
  
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  v_stopwords <- c(stopwords("en"), c("thats","weve","hes","theres","ive","im",
                                      "will","can","cant","dont","youve","us",
                                      "youre","youll","theyre","whats","didnt"))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, v_stopwords)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  return(corpus.tmp)
}

# frequent terms 
frequentTerms <- function(text){
  
  s.cor <- Corpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}


# clean by each Type
clean_top_char <- function(dataset){
  all_dialogue <- list()
  namelist <- list()
  
  for (i in 1:3){
    top <- dataset %>% count(Type) %>% arrange(desc(n)) %>% head(20)
    name <- top$Type[i]
    Description <- paste(dataset$Description[dataset$Type == name], collapse = " ")
    all_dialogue <- c(all_dialogue, Description)
    namelist <- c(namelist, name)
    
  }
  
  
  
  all_clean <- all_dialogue %>% 
    VectorSource() %>% 
    Corpus() %>% 
    cleanCorpus() %>% 
    TermDocumentMatrix() %>%
    as.matrix()
  
  colnames(all_clean) <- namelist
  
  assign("all_clean",all_clean,.GlobalEnv)
  all_clean %>% head()
}

weed %>% clean_top_char()






weed$Description %>% 
  frequentTerms() %>% 
  # dim()
  head(30) %>% 
  mutate(word = factor(word))%>% 
  plot_ly(x = ~reorder(word,-freq), y = ~freq, colors = viridis(10)) %>%
  add_bars(color = ~word) %>%
  layout(title = "Top 30 Words", 
         yaxis = list(title = " "), 
         xaxis = list(title = ""), 
         margin = list(l = 100))
















df <- all_clean %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  gather(type, value, -1)

df1 <- df %>% 
  group_by(name = type, drilldown = type) %>% 
  summarise(y = sum(value)) %>% 
  arrange(desc(y))


df2 <- df %>% 
  group_by(type, rowname) %>% 
  summarise(total = sum(value)) %>% 
  arrange(desc(total)) %>% 
  group_by(name = type, id = type) %>% 
  do(data = list_parse(
    mutate(., name = rowname, drilldown = tolower(paste(type, rowname, sep=": "))) %>% 
      group_by(name, drilldown) %>% 
      summarise(y = sum(total)) %>% 
      select(name, y, drilldown) %>%
      arrange(desc(y))) %>% 
      head(30))

  

highchart() %>% 
  hc_chart(type = 'column') %>% 
  hc_xAxis(type = "category") %>% 
  hc_add_series(name = 'number of words in comments', data = df1, colorByPoint = 1) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>%
  hc_legend(enabled = F) %>% 
  hc_title(text = "Top 30 Words by Type of Cannbis") %>% 
  hc_add_theme(hc_theme_darkunica())


rm(df2)


df2 <- df %>%
  inner_join(get_sentiments("loughran"), by = c('rowname' = 'word')) %>% 
  group_by(type,sentiment) %>% 
  summarise(total = sum(value)) %>% 
  arrange(desc(total)) %>% 
  group_by(name = type, id = type) %>% 
  do(data = list_parse(
    mutate(., name = sentiment, drilldown = tolower(paste(type, sentiment, sep = ": "))) %>% 
      group_by(name, drilldown) %>% 
      summarise(y = sum(total)) %>% 
      select(name, y , drilldown) %>% 
      arrange(desc(y))
  ))



highchart() %>% 
  hc_chart(type = 'column') %>% 
  hc_xAxis(type = "category") %>% 
  hc_add_series(name = 'number of words in comments', data = df1, colorByPoint = 1) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series =list_parse(df2)
  ) %>%
  hc_legend(enabled = F) %>% 
  hc_title(text = "Sentiment Analysis by Type of Cannbis") %>% 
  hc_add_theme(hc_theme_darkunica())


rm(df2)
















commonality.cloud(all_clean[,c("sativa","indica")], colors = "steelblue1", at.least = 2, max.words = 100)
comparison.cloud(all_clean[,c("sativa","indica")], colors = c("#F8766D", "#00BFC4"), max.words=50)

common_words <- all_clean %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(sativa>0, indica>0) %>% 
  # select(sativa, indica)
  mutate(difference = abs(sativa - indica)) %>% 
  arrange(desc(difference)) 

common_words_25 <- common_words%>%
  head(25)

pyramid.plot(common_words_25$sativa, common_words_25$indica,
             labels = common_words_25$rowname, gap = 200,
             top.labels = c("sativa", "Words", "indica"),
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)
rm(common_words, common_words_25)

effects <- weed_effects$Effects %>% unique() %>% tolower()
rm(weed_effects)


effectByType <- all_clean %>%
  as.data.frame() %>% 
  rownames_to_column('word') %>% 
  filter(word %in% effects) %>% 
  mutate(word=factor(word))
  

effectByType %>% 
  plot_ly(x=~hybrid,y=~sativa,z= ~indica, color=~word, hoverinfo = 'text', colors = viridis(15),
          text = ~paste('Effects:', word,
                        '<br>hybrid:', hybrid,
                        '<br>sativa:', sativa,
                        '<br>indica:', indica)) %>% 
  add_markers(opacity = 0.8) %>%
  layout(title = "Effects by Different Cannabis",
         annotations=list(yref='paper',xref="paper",y=1.05,x=1.1, text="Effects",showarrow=F),
         scene = list(xaxis = list(title = 'hybrid'),
                      yaxis = list(title = 'sativa'),
                      zaxis = list(title = 'indica')))


library(ggraph)
library(igraph)
library(ggthemes)

effectByType %>%
  mutate(type = 'hybrid') %>%
  select(word, type, hybrid) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = hybrid), show.legend = FALSE) +
  geom_node_point(color = "firebrick", size = 20, alpha = .5) +
  geom_node_text(aes(label = name), col = "white") +
  theme_solarized(light = F)


library(visNetwork)
network3_edg <- effectByType %>% 
  mutate(type = 'hybrid') %>%
  select(word, type, hybrid) %>% 
  filter(hybrid>0) %>% 
  rename(from = type, to = word, weight = hybrid, width = hybrid)

network3_node <- effectByType %>% 
  mutate(type = 'hybrid') %>%
  filter(hybrid>0) %>% 
  select(word, hybrid) %>% 
  rename(id = word, size = hybrid)
network3_node$label <- network3_node$id # Node label

network3_node <- bind_rows(network3_node,data.frame(id = 'hybrid',  size = 24,  label = 'hybrid'))

visNetwork(network3_node, network3_edg, height = "500px", width = "100%") %>% 
  visIgraphLayout(layout = "layout_with_lgl") %>% 
  visEdges(shadow = TRUE,
           color = list(color = "gray", highlight = "orange"))




# # Word association
# word_associate(weed$Description, match.string = c("happy"), 
#                stopwords = c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
#                                                      "will","can","cant","dont","youve","us",
#                                                      "youre","youll","theyre","whats","didnt")), 
#                network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
# # Add title
# title(main = "Master Yoda")