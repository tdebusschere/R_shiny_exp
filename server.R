###R server

# Define server logic required to plot various variables against mpg
if(!exists("foo", mode="function")) source("Helper.R")
usePackage('shiny')
usePackage('datasets')
usePackage('data.table')
usePackage('reshape')
usePackage('kohonen')
usePackage('dendextend')
usePackage('factoextra')

options(shiny.reactlog=TRUE) 
options(shiny.trace=TRUE)
# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
internet_data =  get_public_data()
sumdat = aggregate(internet_data$User_id, by = list(Category = internet_data$Path), FUN = length)
sumdat = sumdat[order(sumdat$x,decreasing=TRUE),]
Int2 = processing(internet_data,sumdat) 


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  #print($input)
 output$Popular_Pages <- renderPlot({ 
	barCenters <- barplot(sumdat$x[0:input$integer],  las = 2, col = rainbow(input$integer),xlab='products',ylab='# visited',main = 'Microsoft Webpages')
	text(barCenters, par("usr")[3] - 0.8, srt = 90, adj = 1, labels =sumdat$Category[0:input$integer], xpd = TRUE, cex =0.7)
  })

   
   
   ## use hierarchical clustering to cluster the codebook vectors
  #plot
   output$ClusterModel <- renderPlot({
    pops <- sumdat$Category[0:input$integer] 
    Int2 =  Int2[ Int2$page1 %in% pops, ] 
    Int2 = Int2[ Int2$page2 %in% pops, ]
    repurposed =  cast( melt(Int2[,c('page1','page2','jqd_dist')]), page1 ~  page2) 
    repurposed[ is.na(repurposed) ]  = 1.0 
    trainingdata <- as.matrix(repurposed[0:(input$integer),  1:(input$integer)+1])
    dimnames(trainingdata) = list(repurposed[,1],repurposed[,1])
    groups<-input$clusternumber

    ###SOM
    if (input$Methodology == 'SOM') {
     if (input$integer > 20) {     dimnames(trainingdata) = list() }
     xdim =4
     ydim =3
     if (((input$xdim * input$ydim) > groups) & ((input$xdim * input$ydim) < input$integer)) { 
      xdim = input$xdim
      ydim = input$ydim
     }
     som_model = som( trainingdata[0:input$integer,] ,grid = somgrid(xdim, ydim , "hexagonal")) 
     som.hc <- cutree(hclust(dist(som_model$codes[[1]])), groups)
     plot(som_model, type="codes", bgcol=terrain.colors(groups)[som.hc], codeRendering='segments',main = 'SOM Clustering') 
    }
    if (input$Methodology == 'Hierarchical') {
    ###Hierarchical Clustering
  
     dist_trainingdata = as.dist(trainingdata)
     hc_trainingdata <- hclust(dist_trainingdata, method =hrclustmethod(input$Algorithm))
     dend <- as.dendrogram(hc_trainingdata)
     # order it the closest we can to the order of the observations
     dend <- rotate(dend, 1:input$integer)
     # Color the branches based on the clusters:
     dend <- color_branches(dend, k=groups) 
     # We hang the dendrogram a bit:
     #dend <- hang.dendrogram(dend,hang_height=0.1)
     # reduce the size of the labels:
     # dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
     dend <- set(dend, "labels_cex", 0.5)
     # And plot:
     par(mar = c(3,3,3,7))
     plot(dend, 
     main = "Hierarchical clustered Microsoft Data", 
     horiz =  TRUE,  nodePar = list(cex = .007))
     #legend("topleft", legend = lev_groups, fill = rainbow_hcl(3))
    }
     ###KM
    if (input$Methodology == 'kmeans'){ 
     kmeanstraining = kmeans(trainingdata,groups)
     plot( fviz_cluster(kmeanstraining, data = trainingdata, ellipse =TRUE ) )
     }
   })


})
#cluster boundaries

#  if (input$Methodology == 'kmeans'):


  # Return the formula text for printing as a caption

  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  #output$mpgPlot <- renderPlot({
  #  boxplot(as.formula(formulaText()), 
  #          data = mpgData,
  #          )
  #})


