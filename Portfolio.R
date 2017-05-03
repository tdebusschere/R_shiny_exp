library(Matrix)
library(arules)

get_public_data = function()
{
temp <- tempfile()
download.file("http://kdd.ics.uci.edu/databases/msweb/anonymous-msweb.data.gz",temp)

##irregular input

##in principle need to add error handling for temporal inconsistencies,
##but ignored here because of ideal dataset

con  <- file(temp, open = "r")
maxlines = 250000 
users = c()
products = c()
categories = c()
users[1:maxlines] = NA 
products[1:maxlines] = NA
categories =  data.frame(  ID =integer(5000),Description =character(5000), Path = character(5000), stringsAsFactors=FALSE) 
counter = 0
categors = 0

while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
	templine = strsplit(oneLine,',')
	##new user
	if (templine[[1]][1] == 'A'){
		categors= categors + 1
		categories[categors,] = c( templine[[1]][2], templine[[1]][4] , templine[[1]][5])
	}
	if (templine[[1]][1] == 'C'){
		user = templine[[1]][3]
	} else if (templine[[1]][1] == 'V') {
                counter = counter + 1 
                product = templine[[1]][2]
                users[counter] =user
		products[counter] =product
	}
} 

close(con)
unlink(temp)

products = products[!is.na(products)]
users = users[!is.na(users)]
filtered_categories = categories[!apply(categories['ID'] == "0",1, all),]
###dataset
DS = data.frame(User_id = users , Product_id = products,stringsAsFactors = FALSE)
annotated_data = merge(filtered_categories, DS, by.x = 'ID',by.y = 'Product_id')
return(annotated_data)
}

processing = function(internet_data, sumdat) {
   ## data processing
    tmp = internet_data[,c('Path','User_id')] 
    dt1 <- data.table(tmp, key = "User_id")
    dt2 <- data.table(tmp, key = "User_id")
    intself <- dt1[dt2,allow.cartesian = TRUE]
    Int <- aggregate( intself$User_id,by = list(page1 = intself$Path, page2 = intself$i.Path),length) 
    Int1 = merge(Int,sumdat,by.x = 'page1', by.y = 'Category')
    Int2 = merge(Int1,sumdat,by.x = 'page2', by.y = 'Category')
    colnames(Int2) = c('page2','page1','intersect','tot_pag_1','tot_pag_2')
    ###jqd simularity = |AB| / ((A) + (B)- (AB))
    Int2['jqd_dist'] =1 -  Int2['intersect'] / ( Int2['tot_pag_1'] + Int2['tot_pag_2'] - Int2['intersect'])
    return(Int2) } 
  

hrclustmethod = function(Algorithm){
return( switch (Algorithm ,
   "SingleLinkage" = 'single',
   "CompleteLinkage" = 'complete',
   "AverageLinkage" = 'average',
   "Ward" ='ward',
   "Centroid" = 'centroid',
   "Otherwise" = 'ward')
)}

#
#rows = sort(unique(annotated_data$Path))
#columns = sort(unique(annotated_data$User_id))
#dimnamesM = list(Products = rows,User_id = columns)
#
#UIMatrix <- sparseMatrix(i = match(annotated_data$Path,rows),j = match(annotated_data$User_id,columns),dimnames = dimnamesM)
#trans1 <- as(UIMatrix, "transactions")
#rules_set = apriori(trans1, parameter = list(conf = 0.05, sup=0.04,minval = 0.05, minlen =2), appearance = NULL, control = NULL)




#Load the packages & the Data Stream Data for ffdf objects
#
#require(devtools)
#require(stream)
#require(ff)
#source_gist("5239198")
#
#    Set up a data stream
#
#myffdf <- as.ffdf(iris)
#myffdf <- myffdf[c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]
#mydatastream <- DSD_FFDFstream(x = myffdf, k = 100, loop=TRUE) 
#mydatastream
#
#    Build the streaming clustering model
#
#### Get some points from the data stream
#get_points(mydatastream, n=5)
#mydatastream

#### Cluster (first part)
#myclusteringmodel <- DSC_CluStream(k = 100)
#cluster(myclusteringmodel, mydatastream, 1000)
#myclusteringmodel
#plot(myclusteringmodel)

#### Cluster (second part)
#kmeans <- DSC_Kmeans(3)
#recluster(kmeans, myclusteringmodel)
#plot(kmeans, mydatastream, n = 150, main = "Streaming model - with 3 clusters")


