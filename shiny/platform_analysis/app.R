rm(list = ls())
##################
# packages 
##################
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','readxl',
              'scales','dplyr','mlbench','caTools',
              'forecast','lubridate','tidytext',
              'SnowballC','wordcloud', 'RColorBrewer')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

##################
# data
##################
df <- read.csv("clean.csv")


##################
# UI
##################

#----------------
# UI dropdown
#----------------
category_info <- c(sort(unique(df$Category)))
sub_category_info <- c(sort(unique(df$Subcat)))


ui <- dashboardPage(skin = "yellow",
    dashboardHeader(title = "Platform analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summary", tabName = "summary", icon = icon("clock")),
            menuItem("Category", tabName = "category", icon = icon("th")),
            menuItem("Sub Category", tabName = "subcategory", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
        #----------------
        # Summary
        #----------------
            tabItem(tabName = "summary",
                    fluidRow(
                        h2("Summary Insights",style="text-align: center;"),
                        tabBox(
                            title="Categories",
                            id = "tabset1",
                            width = "10%",
                            selected = "Price",
                            tabPanel("Price", plotOutput("categoryPricePlot",height = 250)),
                            tabPanel("Ratings", plotOutput("categoryRatingPlot",height = 250)),
                            tabPanel("Sales", plotOutput("categorySalesPlot",height = 250))
                        ),
                         tabBox(
                             title="Sub-Categories",
                             id = "tabset2",
                             width = "10%",
                             selected = "Price",
                             tabPanel("Price", plotOutput("subcategoryPricePlot", height = 250)),
                             tabPanel("Ratings", plotOutput("subcategoryRatingPlot", height = 250)),
                             tabPanel("Sales", plotOutput("subcategorySalesPlot", height = 250))
                             
                         )
                    )
            ),
        #----------------
        # Category
        #----------------       
            tabItem(tabName = "category",
                    h2("Category Insights",style="text-align: center;"),
                    fluidRow(
                        column(width = 3,
                               selectInput("CategoryInput", "Category",
                                           selected = "All",choices = category_info))
                        ),
                    fluidRow(
                        infoBoxOutput("priceCategoryBox"),
                        infoBoxOutput("salesCategoryBox"),
                        infoBoxOutput("ratingCategoryBox")
                    ),
                    fluidRow(
                        box(title = "Price & Sales", status = "primary", 
                            plotOutput("priceSalesCategoryPlot", height = 250)),
                        box(title = "Price & Ratings", status = "primary", 
                            plotOutput("priceRatingsCategoryPlot", height = 250)),
                        box(title = "Sales & Ratings", status = "primary", 
                            plotOutput("salesRatingsCategoryPlot", height = 250))
                    )
        ),
        #----------------
        # Sub Category
        #----------------         
            tabItem(tabName = "subcategory",
                    h2("Sub-Category Insights",style="text-align: center;"),
                    fluidRow(
                        column(width = 3,
                               selectInput("SubCategoryInput", "Sub Category",choices = sub_category_info))
                    ),
                    fluidRow(
                        infoBoxOutput("priceSubCategoryBox"),
                        infoBoxOutput("salesSubCategoryBox"),
                        infoBoxOutput("ratingSubCategoryBox")
                    ), 
                    fluidRow(
                        #box(title = "Histogram", status = "primary", plotOutput("plot2", height = 250)),
                        #box(title = "Histogram", status = "primary", plotOutput("plot2", height = 250)),
                        #box(title = "Histogram", status = "primary", plotOutput("plot2", height = 250))
                    )
            )
        
    )
  )
)
##################
# Server
##################
server <- function(input, output, session) {
    
    #----------------
    # Category Summary Plot
    #---------------- 
    output$categoryPricePlot <- renderPlot({
        
        category_price_df <- df %>%
            group_by(Category) %>%
            dplyr::summarise(`Average Price` = mean(price)) %>%
            arrange(desc(`Average Price`)) %>%
            top_n(10) %>%
            dplyr::select(Category, `Average Price`)
        
        ggplot(category_price_df, aes(reorder(Category,`Average Price`),`Average Price`),color = Category) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, aes(fill = Category)) + coord_flip() +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Category", y = "Average Price") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    })
    
    output$categoryRatingPlot <- renderPlot({
        category_rating_df <- df %>%
            group_by(Category) %>%
            dplyr::summarise(`Average Rating` = mean(stars)) %>%
            arrange(desc(`Average Rating`)) %>%
            top_n(10) %>%
            dplyr::select(Category, `Average Rating`)
        
        ggplot(category_rating_df, aes(reorder(Category,`Average Rating`),`Average Rating`),color = Category) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, aes(fill = Category)) + coord_flip() +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Category", y = "Average Rating") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    })
    
    output$categorySalesPlot <- renderPlot({
        
        category_sales_df <- df %>%
            group_by(Category) %>%
            dplyr::summarise(`Average Sales` = mean(sales)) %>%
            arrange(desc(`Average Sales`)) %>%
            top_n(10) %>%
            dplyr::select(Category, `Average Sales`)
        
        ggplot(category_sales_df, aes(reorder(Category,`Average Sales`),`Average Sales`),color = Category) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, aes(fill = Category)) + coord_flip() +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Category", y = "Average Sales") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    })
    
    #----------------
    # Sub Category Summary Plot
    #---------------- 
    output$subcategoryPricePlot <- renderPlot({
        
        subcategory_price_df <- df %>%
            group_by(Subcat) %>%
            dplyr::summarise(`Average Price` = mean(price)) %>%
            arrange(desc(`Average Price`)) %>%
            top_n(10) %>%
            dplyr::select(Subcat, `Average Price`)
        
        ggplot(subcategory_price_df, aes(reorder(Subcat,`Average Price`),`Average Price`),color = Subcat) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, aes(fill = Subcat)) + coord_flip() +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Sub Category", y = "Average Price", fill = "Sub Category") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    })
    
    output$subcategoryRatingPlot <- renderPlot({
        subcategory_rating_df <- df %>%
            group_by(Subcat) %>%
            dplyr::summarise(`Average Rating` = mean(stars)) %>%
            arrange(desc(`Average Rating`)) %>%
            top_n(10) %>%
            dplyr::select(Subcat, `Average Rating`)
        
        ggplot(subcategory_rating_df, aes(reorder(Subcat,`Average Rating`),`Average Rating`),color = Subcat) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, aes(fill = Subcat)) + coord_flip() +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Sub Category", y = "Average Rating", fill="Sub Category") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    })
    
    output$subcategorySalesPlot <- renderPlot({
        
        subcategory_sales_df <- df %>%
            group_by(Subcat) %>%
            dplyr::summarise(`Average Sales` = mean(sales)) %>%
            arrange(desc(`Average Sales`)) %>%
            top_n(10) %>%
            dplyr::select(Subcat, `Average Sales`)
        
        ggplot(subcategory_sales_df, aes(reorder(Subcat,`Average Sales`),`Average Sales`),color = Subcat) + 
            geom_bar(stat="identity",position="dodge",size=2 ,width = 0.4, aes(fill = Subcat)) + coord_flip() +
            theme_minimal() + scale_y_continuous(labels = comma) +
            labs(x = "Sub Category", y = "Average Sales", fill="Sub Category") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
    })
    
    
    
    
    
    
    #----------------
    # Category Info boxes
    #----------------
    
    # Price Infobox
    output$priceCategoryBox <- renderInfoBox({
        
        price_info <- df %>%
            filter(Category == input$CategoryInput) %>%
            summarize(average_price = mean(price)) %>%
            select(average_price)
        
        infoBox(
            "Average Price", paste0("$" , round(price_info$average_price,2) ), icon = icon("list"),
            color = "blue"
        )
    })
    
    # Sales Infobox
    output$salesCategoryBox <- renderInfoBox({
        
        sales_info <- df %>%
            filter(Category == input$CategoryInput) %>%
            summarize(average_sales = mean(sales)) %>%
            select(average_sales)
        
        infoBox(
            "Average Sales", paste0(round(sales_info$average_sales,2)), icon = icon("credit-card"),
            color = "blue"
        )
    })
    
    # Rating Infobox
    output$ratingCategoryBox <- renderInfoBox({
        
        rating_info <- df %>%
            filter(Category == input$CategoryInput) %>%
            summarize(average_ratings = mean(stars)) %>%
            select(average_ratings)
        
        infoBox(
            "Average Ratings", paste0( round(rating_info$average_ratings,2) ), icon = icon("star"),
            color = "blue"
        )
    })
    
    #----------------
    # Sub-Category Info boxes
    #----------------
    
    # Price Infobox
    output$priceSubCategoryBox <- renderInfoBox({
        
        price_info <- df %>%
            filter(Subcat == input$SubCategoryInput) %>%
            summarize(average_price = mean(price)) %>%
            select(average_price)
        
        infoBox(
            "Average Price", paste0("$" , round(price_info$average_price,2) ), icon = icon("list"),
            color = "green"
        )
    })
    
    # Sales Infobox
    output$salesSubCategoryBox <- renderInfoBox({
        
        sales_info <- df %>%
            filter(Subcat == input$SubCategoryInput) %>%
            summarize(average_sales = mean(sales)) %>%
            select(average_sales)
        
        infoBox(
            "Average Sales", paste0(round(sales_info$average_sales,2)), icon = icon("credit-card"),
            color = "green"
        )
    })
    
    # Rating Infobox
    output$ratingSubCategoryBox <- renderInfoBox({
        
        rating_info <- df %>%
            filter(Subcat == input$SubCategoryInput) %>%
            summarize(average_ratings = mean(stars)) %>%
            select(average_ratings)
        
        infoBox(
            "Average Ratings", paste0( round(rating_info$average_ratings,2) ), icon = icon("star"),
            color = "green"
        )
    })
    
    
    
    
    
    
    #----------------
    # Category Plots
    #----------------
    
    # Sales & Price
    output$priceSalesCategoryPlot <- renderPlot({
        price_sales_df <- df %>%
            filter(Category == input$CategoryInput) %>%
            select(price, sales)
        
        ggplot(price_sales_df, aes(x=price, y=sales)) + geom_point(size=2, shape=23) + 
            theme_minimal() + 
            labs(x = "Price", y = "Sales") + 
            theme(legend.text = element_text(size = 12),
                  legend.title = element_text(size = 15),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 00, hjust = 1))
        
     
        
        
    })
    
    # Price & Ratings
    output$priceRatingsCategoryPlot <- renderPlot({
        
    })
    
    # Sales & Ratings
    output$salesRatingsCategoryPlot<- renderPlot({
        
    })
    
    #----------------
    # Sub Category Plots
    #----------------
}


shinyApp(ui, server)