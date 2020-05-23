# This file contains relevant functions for this application

# function to read and transform the data
getData <- function(fname) {
  require(dplyr)
  df <- read.csv("sales.csv", header = TRUE, stringsAsFactors = FALSE)
  # aggregate numeric data by sales rep
  df <- df %>% group_by(sales.rep) %>% summarise_if(is.numeric, funs(mean)) %>% as.data.frame
  rownames(df) <- df$sales.rep
  # add gender column
  df$gender = ifelse(df$rep.sex == 1, "male", "female")
  # exclude certain columns
  df <- df[, !names(df) %in% c('sales.rep','year','rep.sex')]
  # rename columns
  colnames(df)[1:6] <- paste("mean", colnames(df)[1:6])
  return(df)
}

# Function to round all numeric variables in a data frame
roundDat <- function(x) {
  dat <- x
  i <- sapply(dat, is.numeric)
  dat[i] <- lapply(dat[i], round, digits = 2)
  return(dat)
}

##################################################
# Visualization
##################################################

# Function to create interactive D3 heat map
getHeatMap <- function(df) {
  require(d3heatmap)
  num.cols <- which(sapply(df, is.numeric))
  d3heatmap(df[,num.cols], scale = 'column', cexRow = 0.8
            , cexCol = 0.8, colors = 'Blues'
            , dendrogram = 'row', height = "1000px", width = "1000px")
}

# Function to create chernoff faces
getChernoff <- function(df) {
  require(aplpack)
  num.cols <- which(sapply(df, is.numeric))
  dat <- df[,num.cols]
  #par(mar = c(5,4,4,2))
  faces(dat, ncolors=dim(dat)[2], print.info = TRUE)
}

# Function to create star plot
getStar <- function(df) {
  require(RColorBrewer)
  num.cols <- which(sapply(df, is.numeric))
  df <- df[,num.cols]
  stars(df, flip.labels = TRUE, cex = 0.8, labels = rownames(df)
        , radius = FALSE, draw.segments = TRUE
        , col.segments = brewer.pal(dim(df)[2], 'Paired')
        , xpd = FALSE, mar = c(0,0,0,0)
        )
}

# Function to create parallel coordinates plot
getParallelCoord <- function(df) {
  require(GGally); require(ggplot2)
  num.cols <- which(sapply(df, is.numeric))
  p <- ggparcoord(df, columns = num.cols
                  , scale = c(
                    'std','robust','uniminmax'
                    ,'globalminmax','center','centerObs'
                  )[1]
                  , groupColumn = 7)
  p
}

# Function to create multidimensional scaling plot
getMDS <- function(df) {
  require(ggplot2)
  num.cols <- which(sapply(df, is.numeric))
  d <- dist(df[,num.cols])
  mds <- cmdscale(d)
  df.mds <- data.frame(
    name = rownames(mds)
    , x = mds[,1]
    , y = mds[,2]
    , gender = df$gender
  )
  p <- ggplot(df.mds, aes(x = x, y = y, label = name, color = gender)) + 
    geom_point()
  if (dim(df.mds)[1] <= 100) {
    p + geom_text(size = 3.5, vjust=1)
  } else {
    p
  }
}

# Function to plot dendrogram
getDendro <- function(df, k = NULL) {
  require(dendextend)
  num.cols <- which(sapply(df, is.numeric))
  datScaledSub <- scale(df[,num.cols])
  distEnorm <- dist(datScaledSub, method = 'euclidean')
  fit.hac <- hclust(distEnorm, method = 'ward.D2')
  hcd <- as.dendrogram(fit.hac)
  dend_parts <- color_branches(hcd, k = k)
  dend_parts <- assign_values_to_leaves_nodePar(dend_parts, 0.8, "lab.cex")
  par(mar = c(5, 4, 4, 2) + 0.1)
  if (dim(df)[1] <= 25) {
    plot(dend_parts)
  } else {
    plot(dend_parts, leaflab = 'none')
  }
}

# Define function to create HTML table widget for data table in main panel
# input: data frame
# output: HTML table widget
GetDataTable <- function(data, caption = NULL, n = 25) {
  require(DT)
  data$name <- rownames(data)
  data <- data[,c(ncol(data), 1:ncol(data)-1)]
  data <- roundDat(data)
  data$gender <- as.factor(data$gender)
  dat.tmp <- datatable(data = data, extensions = 'Buttons', caption = caption,
                       options = list(
                         paging = TRUE,
                         searching = TRUE,
                         fixedColumns = TRUE,
                         autoWidth = TRUE,
                         ordering = TRUE,
                         dom = 'Bfrtip',
                         buttons = list('copy', 
                                        list(extend = 'csv', filename = 'data'),
                                        I('colvis')),
                         columnDefs = list(list(targets = 0, searchable = FALSE)),
                         pageLength = n
                       ),
                       class = 'display',
                       rownames = FALSE,
                       filter = 'top',
                       escape = FALSE,
                       selection = 'single'
  )
  return(dat.tmp)
}

##################################################
# Text
##################################################
shortdesc.hm <- "Compare sales reps by their feature heat signatures"
shortdesc.ch <- "Compare sales reps by mapping features to facial features"
shortdesc.st <- "Compare sales reps by mapping features to polar coordinates"
shortdesc.pc <- "Observe correlations between features"
shortdesc.md <- "Observe multiple features in two-dimensions"
shortdesc.dd <- "View hierarchical groupings of sales reps"
shortdesc.df <- "A data table of feature measurements"

# Testing
# df <- getData()
# getHeatMap(df)
# getChernoff(df)
# getStar(df)
# getParallelCoord(df)
# getMDS(df)
# getDendro(df)

