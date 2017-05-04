if(!exists("foo", mode="function")) source("Portfolio.R")
usePackage('shiny')

internet_data = get_public_data()
# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Microsoft Data"),

  sidebarPanel(

    sliderInput("integer", "Integer:", 
      min=12, max=length(unique(internet_data$Path)), value=20),

    selectInput("Methodology","ClusteringMethodology:",c("kmeans","Hierarchical",'SOM')),
    sliderInput("clusternumber","Number of Clusters:", min=2, max =8, value = 3), 
  # Only show this panel if the clustering type is hierarchical
    conditionalPanel(
      condition = "input.Methodology == 'Hierarchical'",
      selectInput(
         "Algorithm", "Algorithm",
         c("SingleLinkage",
           "CompleteLinkage",
           "AverageLinkage",
           "Ward",
           "Centroid"))
     )
  
   ##only show when clustering type is SOM: select network
  ),

  mainPanel(
    h3(textOutput("caption")),
    plotOutput("Popular_Pages"),
    plotOutput("ClusterModel")

  )
))
