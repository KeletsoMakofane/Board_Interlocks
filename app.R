#### LIBRARIES #######
library(shiny)
library(dplyr)
library(tidyverse)
library(tidygraph)
library(igraph)
library(visNetwork)
library(shinydashboard)


#### FUNCTIONS #######
format.graph.basic <- function(graph, color.scheme = three.color.extremes, background.col = "white", text.col = "black"){
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

make_ego_net <- function(network, names = c("TFG"), order = 1){
    network %>%
        make_ego_graph(nodes = names, order = order) %>%
        lapply(as_tbl_graph) %>%
        purrr::reduce(graph_join)
    
}

name_query <- function(name = c("TFG"), reference_names = c(TFG =  "Hello")){
    ifelse(name %in% reference_names, "Focus", "Other")
}

make_graph <- function(which_graph, focus_nodes, order = 1){
    
    graph <- list_graphs[[which_graph]] %>%
        make_ego_net(names = focus_nodes, order = order) %>%
        mutate(focus_names = name_query(name = name, reference_names = focus_nodes)) 
    
    nodelist <- graph %>%
        mutate(id = 1:gorder(.)) %>%
        mutate(title = company_info[name,2]) %>%
        data.frame %>%
        mutate(color.background = ifelse(focus_names == "Other", "blue", "orange"),
               size = ifelse(focus_names == "Other", 10, 30), 
               color.border = ifelse(focus_names == "Other", "blue", "orange"))
    
    edgelist <- graph %>%
        activate(edges) %>%
        data.frame() %>%
        data_frame() %>%
        mutate(color = "grey")
    
    visNetwork(nodelist, edgelist, width="100%", height="2000px", background = "white") %>%
    #visPhysics(minVelocity = 20, maxVelocity = 20, stabilization = list(iterations = 5000)) %>%
    visInteraction(hover = TRUE)
}

#### DATA #######
data_clean <- readRDS("data_clean.R")
company_info <- readRDS("company_info.R")

rownames(company_info) <- company_info[,1]

company_info_vec <- company_info[,1]
names(company_info_vec) <- company_info[,2]

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

list_graphs <- list()

list_graphs[[1]] <- graph_companies
list_graphs[[2]] <- graph_individuals





# Define UI for application that draws a histogram
ui <- fluidPage(

    
    fluidRow(        box( width = 12, solidHeader = TRUE, status = "primary", fill = "white",
                         
                         
                         visNetworkOutput("network", width = "100%", height = "1000px"),
                         
                         absolutePanel(id = "controls", fixed = FALSE, 
                                       draggable = TRUE, top = 50, left = "50", right = "auto", bottom = "auto",
                                       width = "auto", height = "auto",
                                       box( solidHeader = TRUE, color = "black", width = "auto", height = "auto", background = "black",
                                            title = "Which JSE Listed Companies Have Directors in Common?",
                                            br(),
                                            br(),
                                            "Hover over circles to see company names,", br(), " drag them to change layout, and", br(),
                                            "scroll to zoom in and out", br(),
                                           selectizeInput('names_list', label = "", choices = NULL, multiple = TRUE, selected = c("TKG", "VOD"))
                                       )
                         )
    )

                       )
    



)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    


    output$network <- renderVisNetwork({
        req(input$names_list)
        make_graph(which_graph = 1, focus_nodes = input$names_list, order = 1)
        })
    
    updateSelectizeInput(session, 'names_list', choices = company_info_vec, server = TRUE, selected = c("TKG", "VOD", "MTN"))#, options = list(background = "black"))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
