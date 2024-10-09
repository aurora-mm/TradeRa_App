# A Shiny app that allows users to search for items on Tradera using a query.
# The app retrieves search results and displays them in a data table format.

# Install prerequisites in the following way:

# install.packages("shiny")
# install.packages("data.table")
# install.packages("DT")
# devtools::install_github("aurora-mm/TradeRa")

library(shiny)
library(data.table)
library(DT)
library(TradeRa)

# Define the UI
ui <- fluidPage(
  titlePanel("Tradera Search"),

  # Text input for the user query
  textInput("query", "Search Query:", value = ""),

  # Button to trigger search
  actionButton("searchBtn", "Find"),

  # Display the Data Table
  DTOutput("searchResults")
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store search data
  results <- reactiveValues(df = NULL)

  # Event handler for search button
  observeEvent(input$searchBtn, {
    # Call the Search function with the specified AppId, AppKey, etc.
    searchResult <- Search(
      AppId = 5201,
      AppKey = "458a01fd-c949-4ff0-bef3-740181491793",
      pageNumber = 1,
      orderBy = "Relevance",
      query = input$query
    )
    # Get all pages if total_pages is larger than 1
    if (searchResult$total_pages > 1) {
      final_df <- data.frame()
      for (i in 1:searchResult$total_pages)
      {
        searchResult <- Search(
          AppId = 5201,
          AppKey = "458a01fd-c949-4ff0-bef3-740181491793",
          pageNumber = i,
          orderBy = "Relevance",
          query = input$query
        )
        final_df <- rbind(final_df, searchResult$df)
      }
      searchResult$df <- final_df
      searchResult$df <- searchResult$df[searchResult$df$ShortDescription != "Nothing found!", ]
    }


    # Extract the data frame from the search result
    if (!is.null(searchResult$df)) {
      # Select relevant columns for display
      results$df <- data.table(
        ShortDescription = searchResult$df$ShortDescription,
        LongDescription = searchResult$df$LongDescription,
        Price = searchResult$df$Price,
        ItemUrl = searchResult$df$ItemUrl
      )
    } else {
      results$df <- data.table(
        ShortDescription = "Cannot fetch search results!",
        LongDescription = "",
        Price = "",
        ItemUrl = ""
      )
    }
  })

  # Render the data table with the search results
  output$searchResults <- renderDataTable({
    results$df
  })
}

# Run the app
shinyApp(ui = ui, server = server)
