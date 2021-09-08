library(dplyr)
library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)

format.graph.basic <- function(graph, color.scheme = three.color.extremes, background.col = "black", text.col = "white"){
  result <- graph +
    theme(panel.background = element_rect(fill = background.col, color = background.col),
          legend.key.width = unit(10, "mm"),
          plot.caption = element_text(hjust = 0, color = text.col, size = 8), 
          plot.background = element_rect(fill = background.col, color = background.col),
          legend.background = element_rect(fill=background.col),
          legend.key = element_rect(fill = background.col),
          text = element_text(color = text.col, size = 14),
          legend.text = element_text(color = text.col, size = 12),
          legend.title = element_text(color = text.col, size = 14),
          panel.spacing = unit(5, "cm"),
          plot.margin=grid::unit(c(10,10,10,10), "mm"),
          strip.background = element_rect(color = "black", fill = "black"),
          strip.text = element_text(color = text.col)) 
  return(result)
}


data_clean <- readRDS("board_interlock/data_clean.R")

  
graph_companies <- data_clean %>% 
  mutate(from = name, to = company) %>%
  as_tbl_graph() %>%
  mutate(type = name %in% data_clean$name) %>%
  bipartite_projection(which = "false") %>%
  as_tbl_graph()


graph_individuals <- data_clean %>% 
  mutate(from = name, to = company) %>%
  as_tbl_graph() %>%
  mutate(type = name %in% data_clean$name) %>%
  bipartite_projection(which = "true") %>%
  as_tbl_graph() %>%
  mutate(name = str_replace(name, "\\ ", "_"))

ggraph(graph_companies) +
  geom_edge_link() +
  geom_node_point()

ggraph(graph_individuals, layout = "nicely") +
  geom_edge_link() +
  geom_node_point()


