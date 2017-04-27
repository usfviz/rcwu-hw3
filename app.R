# devtools::install_github("ggobi/ggally")
# rm(list = ls())
# cat('\014')

library(tidyr)
library(dplyr)
library(ggplot2)
library(GGally)
library(shiny)

# Read and clean data
# setwd('~/Desktop/MSAN622/Homework/HW3/')
dat <- read.table('dataset_Facebook.csv', header = T, sep = ';')
dat$Post.Weekday <- factor(dat$Post.Weekday, levels = 1:7, 
                           labels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
dat$Post.Month <- factor(dat$Post.Month, levels = 1:12,
                         labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
dat$Post.Hour <- factor(dat$Post.Hour)
dat[is.na(dat$Paid), 'Paid'] <- 0
dat <- dat[complete.cases(dat), ]
dat <- dat %>%
  rename_('Likes' = 'like', 'Shares' = 'share', 'Comments' = 'comment')


ui <- navbarPage(
  "Facebook Metrics", fluid = T,
  tabPanel("Heatmap",
    fluidRow(column(2, selectInput("heatVal", 
                                   label = 'Heat Value',
                                   choices = list('Total Interactions' = 'Total.Interactions', 
                                                  'Likes' = 'Likes', 
                                                  'Comments' = 'Comments', 
                                                  'Shares' = 'Shares'),
                                   selected = 'Total Interactions'),
                    selectInput("paid",
                                label = 'Paid Filter',
                                choices = list('All', 'Free', 'Paid'),
                                selected = 'All')
                    ),
             column(10, plotOutput("heatmap"))
    )
  ),
  tabPanel("Scatterplot Matrix",
    fluidRow(column(8,
                    checkboxGroupInput("scatterCols", inline = T,
                                       label = 'Plot Columns',
                                       choices = list('Lifetime Post Total Reach' = 'Lifetime.Post.Total.Reach',
                                                      'Lifetime Post Total Impressions' = 'Lifetime.Post.Total.Impressions',
                                                      'Lifetime Engaged Users' = 'Lifetime.Engaged.Users',
                                                      'Lifetime Post Consumptions' = 'Lifetime.Post.Consumptions',
                                                      'Paid' = 'Paid',
                                                      'Likes' = 'Likes',
                                                      'Comments' = 'Comments',
                                                      'Shares' = 'Shares',
                                                      'Total Interactions' = 'Total.Interactions'),
                                       selected = c('Likes', 'Comments', 'Shares', 'Total.Interactions'))
                    )
             ),
    fluidRow(column(12, plotOutput("scatterplot")))
  ),
  tabPanel("Parallel Coordinates",
    fluidRow(column(8,
                    selectInput("parallelCols",
                                label = 'Group By',
                                choices = list('Post Month' = 'Post.Month',
                                               'Post Weekday' = 'Post.Weekday',
                                               'Post Hour' = 'Post.Hour'),
                                selected = c('Post.Weekday'))
                    )
             ),
    fluidRow(column(12, plotOutput("parallel")))
  )
)


server <- function(input, output) {
  ################## Heatmap ##################
  heatVal <- reactive({input$heatVal})
  heatValLabel <- reactive({
    ifelse(input$heatVal == 'Total.Interactions', 
           'Total Interactions', 
           input$heatVal)
  })
  legendLimits <- reactive({
    switch(input$heatVal,
           'Total.Interactions' = c(100, 2500),
           'Likes' = c(0, 3000),
           'Comments' = c(0, 100),
           'Shares' = c(0, 250))
  })
  paidFilter <- reactive({
    switch(input$paid,
           'All' = 1:nrow(dat),
           'Paid' = dat$Paid == 1,
           'Free' = dat$Paid == 0)
  })
  output$heatmap <- renderPlot({
    dat[paidFilter(), ] %>%
      group_by(Post.Month, Post.Weekday) %>%
      summarise(count = n(),
                Likes = sum(Likes, na.rm = T),
                Comments = sum(Comments, na.rm = T),
                Shares = sum(Shares, na.rm = T),
                Total.Interactions = sum(Total.Interactions, na.rm = T)) %>%
      ggplot(aes(factor(Post.Month), factor(Post.Weekday))) +
      geom_tile(aes_string(fill = heatVal())) +
      scale_fill_gradient(name = heatValLabel(), low = '#edf8fb', 
                          high = '#006d2c', na.value = "#006d2c",
                          limits = legendLimits()) +
      scale_x_discrete(name = '\nPost Month', expand = c(0,0)) +
      scale_y_discrete(name = 'Post Weekday', expand = c(0,0)) +
      ggtitle(paste0('Heatmap Using ', heatVal())) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'))
  })
  
  
  ################## Scatterplot matrix ##################
  scatterCols <- reactive({input$scatterCols})
  output$scatterplot <- renderPlot({
    dat %>%
      select_(.dots = scatterCols()) %>%
      ggpairs() +
        theme_bw()
  }, height = 550)
  
  ################## Parallel coordinates ##################
  groupCol <- reactive({input$parallelCols})
  groupColLabel <- reactive({
    switch(input$parallelCols,
           'Post.Month' = 'Post Month',
           'Post.Weekday' = 'Post Weekday',
           'Post.Hour' = 'Post Hour')
  })
  output$parallel <- renderPlot({
    grp <- dat %>%
      filter(Total.Interactions <= 500) %>%
      group_by_(.dots = groupCol()) %>%
      summarise(Comments = median(Comments, na.rm = T),
                Likes = median(Likes, na.rm = T),
                Shares = median(Shares, na.rm = T),
                Total.Interactions = median(Total.Interactions, na.rm = T)) 
  
    plt <- grp %>%
      ggparcoord(columns = 2:5, groupColumn = 1, showPoints = TRUE, 
                 title = paste0("Interactions by ", groupColLabel()), 
                 scale = 'uniminmax', alphaLines = 0.3) +
      scale_y_continuous(expand = c(0.01, 0.01)) +
      scale_x_discrete(expand = c(0.01, 0.01)) +
      theme_bw() +
      theme(axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#bbbbbb"),
            plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'))
    
    # Figure out y-axis range after GGally scales the data
    min_y <- min(plt$data$value)
    max_y <- max(plt$data$value)
    pad_y <- (max_y - min_y) * 0.1

    # Calculate label positions for each veritcal bar
    lab_x <- rep(1:4, times = 2) # 2 times, 1 for min 1 for max
    lab_y <- rep(c(min_y - pad_y, max_y + pad_y), each = 4)

    # Get min and max values from original dataset
    lab_z <- as.character(c(sapply(grp[, 2:5], min, na.rm = T), 
                            sapply(grp[, 2:5], max, na.rm = T)))

    # Add labels to plot
    plt <- plt + annotate("text", x = lab_x, y = lab_y, label = lab_z, size = 3)
    
    return(plt)
  }, height = 550)
 
  output$txt <- renderText({input$parallelCols}) 
}

shinyApp(ui = ui, server = server)
