#### Stable Marriage Problem ####

# Numberphile Video: https://www.youtube.com/watch?v=Qcv1IqHWAzg

# Cool ideas:
# - unequal number of men and women
# - fluid spectrum of homosexual <-> heterosexual
# - globally attractive traits that determine the ranking
# - individual empheses on global traits that determine the ranking
# - increasing cost of breaking ties the longer the connection lasted


# loading libraries

if("igraph" %in% installed.packages() != TRUE) {
  install.packages("igraph")
}

if("magick" %in% installed.packages() != TRUE) {
  install.packages("magick")
}

if("ggplot2" %in% installed.packages() != TRUE) {
  install.packages("ggplot2")
}

library(igraph)
library(magick)
library(ggplot2)





############## Generating graph object #############

StartingPoint <- function(Netsize = 30, col1 = "skyblue", col2 = "lightcoral"){
  
  # creating nodes with gender attributes
  graph <- make_empty_graph() + vertices(as.character(1:Netsize))
  
  # assigning gender attribute to nodes 50/50
  V(graph)[1:(Netsize/2)]$gender <- "male"
  V(graph)[((Netsize/2)+1):max(Netsize)]$gender <- "female"
  
  # assigning color to nodes based on gender
  V(graph)[V(graph)$gender == "male"]$color <- col1
  V(graph)[V(graph)$gender == "female"]$color <- col2
  
  # assigning preferences function
  AssignPrefList <- function(Netsize){
    
    if(V(graph)[Netsize]$gender == "male"){
      
      PrefList <- as.character(sample(V(graph)[V(graph)$gender == "female"]))
      
    }
    
    if(V(graph)[Netsize]$gender == "female"){
      
      PrefList <- as.character(sample(V(graph)[V(graph)$gender == "male"]))
      
    }
    
    return(PrefList)
    
  }
  
  # Creating matrix of preferences
  PrefMat <- t(sapply(as.character(V(graph)),AssignPrefList))
  
  # Attaching PrefList as a node attribute to each node
  for (i in 1:Netsize){
    
    V(graph)[as.character(i)]$PrefList <- list(PrefMat[i,])
    
  }
  
  # return graph object
  return(list(graph,PrefMat))
  
}





############################## Proposals & Selection ######################

# function to create edges for all unpaired women towards their nth choice
WomenProposing <- function(graph,n){
  
  # selecting all unpaired women with zero edges
  Proposers <- V(graph)[V(graph)$gender == "female" & degree(graph,V(graph)$gender == "female") == 0]
  
  # creating edgelist for all unpaired women with their nth preference
  NthProposals <- c(rbind(as.character(V(graph)[Proposers]), sapply(V(graph)[Proposers]$PrefList,"[[",n)))
  
  # Adding proposals as edges to the graph
  NthPropGraph <- add_edges(graph, NthProposals)
  
  # Return Graph object
  return(NthPropGraph)
  
}



# We need a function that selects all male nodes with more than one edge
# picks the one towards the female node that is highest in their preference list and deletes all the other ones

MaleSelection <- function(graph){
  
  # Selecting male nodes with more than one edge
  Selectors <- V(graph)[V(graph)$gender == "male" & degree(graph,V(graph)$gender == "male", mode = "all") > 1]
  
  # Preference Ranking for Selectors
  V(graph)[Selectors]$PrefList
  
  # Selecting the women highest in the preference ranking
  adjacent_vertices(graph,V(graph)[Selectors], mode = "all")
  
  # function for chosing the highest ranking proposal for one man
  BestChoice <- function(edge,graph){
    
    # Getting the PrefList ranks of all proposing women to one man
    PrefRanking <- sapply(unlist(adjacent_vertices(graph,edge, mode = "all")), function(x){which(x == unlist(V(graph)[edge]$PrefList))})
    
    # Determining the index of the highest ranking proposal
    BestIndex <- which(PrefRanking == min(PrefRanking))
    
    # Determining the node number of the best proposal
    BestProposal <- unlist(adjacent_vertices(graph,edge, mode = "all"))[BestIndex]
    
    # return result
    return(BestProposal)
    
  }
  
  # applying the function on all selectors
  SelectedWomen <- sapply(as.numeric(Selectors),BestChoice, graph)
  
  # defining new edge list
  MaleChoiceEdgeList <- c(rbind(SelectedWomen,as.numeric(Selectors)))
  
  # Deleting all edges of Selectors
  DeletedEdgesGraph <- delete_edges(graph, E(graph)[to(Selectors)])
  
  # Readding the best option as an edge 
  MaleChoiceGraph <-add_edges(DeletedEdgesGraph,MaleChoiceEdgeList)
  
}






################## Wrapper Function to do everything automatically ################


StableMarriage <- function(Netsize = 30, col1 = "skyblue", col2 = "lightcoral"){
  
  # generating empty network list for saving results
  Networks <- list()
  Networks2 <- list()
  
  # generating starting point
  ListReturn <- StartingPoint()
  graph <- ListReturn[[1]]
  PrefMat <- ListReturn[[2]]
  rm(ListReturn)
  
  # saving first network
  StartingGraph <- graph
  
  # iterating through women proposing and men selecting
  for (i in seq(from = 1, to = Netsize/2, by = 1)){
    
    graph <- WomenProposing(graph,i)
    Networks[[i]] <- graph
    graph <- MaleSelection(graph)
    Networks2[[i]] <- graph
    
  }

  # pasting networks back together
  Newlist <- vector(mode = "list", length = (length(Networks) + length(Networks2)))
  Newlist[c(TRUE,FALSE)] <- Networks
  Newlist[c(FALSE,TRUE)] <- Networks2
  
  # adding starting network
  Netlist <- c(list(StartingGraph),Newlist)

  # return network list
  return(list(PrefMat, Netlist))
  
}

# Testing function -> works!
Results <- StableMarriage(Netsize = 50)
SMN <- Results[[2]]
PrefMat <- Results[[1]]









#################### Plotting ####################

# Creating new directory if it doesn't exist already
if ("Animations" %in% list.files()){}else {dir.create("Animations")}
  
# going into animation folder
setwd(paste0(getwd(),"/Animations"))
  
# setting fixed layout
layout <- layout_nicely(SMN[[1]])
  
for (i in 1:length(SMN)){
    
  # ensuring each iteration has the same layout by including a seed
  set.seed(123)
    
  # set up picture device
  jpeg(paste("network",as.character(i),".jpeg", sep = ""),quality = 100)
    
  # creating network plot   
  plot(SMN[[i]],
       edge.color = "black",
       layout = layout,
       main = paste("Stable Marriage Problem Iteration",i))
    
  # closing picture device
  dev.off()
    
}


############### Animating Plots ###################

# this takes some time
GIFAnimator <- function(){
  
  # list all image files in the subdir
  Plots <- list.files(full.names=TRUE, recursive=TRUE, pattern = ".jpeg$")
  
  # order them correctly
  Plots <- Plots[order(nchar(Plots),Plots)]
  
  # creating frames for animation (we can use read_plots() directly because it is vectorized)
  frames <- image_morph(image_read(Plots), frames = 2)
  
  #creating animation
  animation <- image_animate(frames)
  
  # saving animation
  image_write(animation,"Network.gif")
  
  #going up to the main dir again
  setwd("..")
}


# Applying function
GIFAnimator()





##### Analyzing Happiness ################

# initializng vectors
node <- vector()
gender <- vector()
partner <- vector()
rank <- vector()

# extracting information from final network
for (i in 1:dim(PrefMat)[1]){
  
  # List of all vertices
  node[i] <- i
  
  # gender of all vertices
  gender[i] <- V(SMN[[length(SMN)]])[i]$gender
  
  # matched partner
  partner[i] <- unlist(adjacent_vertices(SMN[[length(SMN)]],i, mode= "all"))
  
  # Rank of partner
  rank[i] <- which(unlist(adjacent_vertices(SMN[[length(SMN)]],i, mode= "all")) == PrefMat[i,])
  
}

# combining info in dataframe
data <- cbind.data.frame(node,
                         gender,
                         partner,
                         rank,
                         stringsAsFactors = FALSE)

# comparing distribution of partner ranks by gender
# we can see this as a proxy for satisfaction

ggplot(data, aes(x = gender, y = rank, color = gender)) +
      geom_violin() +
      ggtitle("Distribution of Partner's Preference Rank by Gender") +
      xlab("Gender") +
      ylab("Partner Rank")
  
