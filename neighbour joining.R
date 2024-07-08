# In this algorithm we alternate between the distance matrix and matrixM.
# Distance matrix is updated with each node added and matrixM is used the find
# the nearest nodes each time.

# This is a helper method to find mij distances. Used in matrixM.
mij <- function(mat, x, y) {
  if(x == y) return(0)
  mij <- (nrow(mat) - 2)*mat[x,y] - sum(mat[x,]) - sum(mat[,y])
  return(mij)
}

# computes the matrix M.
matrixM <- function(mat) {
     new <- matrix(0, nrow(mat), ncol(mat))
     for(i in seq(nrow(mat))) {
         for(j in seq(ncol(mat))) {
             new[i,j] = mij(mat, i, j)
         }
     }
     return(new)
}

# find the nearest nodes in a matrixM.
findNearestNodes <- function(mat) {
  index <- which.min(m) - 1
  indX <- index %/% nrow(mat) + 1
  indY <- index %% nrow(mat) + 1
  return(c(indX, indY))
}

# Find distance of two nearest nodes to the new node to be added
branchToNew <- function(mat, nrst1, nrst2) {
  if(ncol(mat) == 2) return(mat[nrst1,nrst2])
  oneToNew = mat[nrst1, nrst2]*0.5 +
    0.5/(ncol(mat)-2)*(sum(mat[nrst1,]) - sum(mat[nrst2, ]))
  twoToNew = mat[nrst1, nrst2] - oneToNew
  return(c(oneToNew, twoToNew))
}

# Update distance matrix based on new node added. Nodes denote the nodes
# that were combined to find new node, in other words they are the children
# and correspond to nrst1 and nrst2 in the previous function.
# THIS USES THE DISTANCE MATRIX.
updateDistances <- function(mat, nodes) {
  new <- mat[-nodes,-nodes]
  newVec <- c()
  ind = 1
  for(i in setdiff(seq(ncol(mat)), nodes)) # !! We should take note of which
                                          # nodes got deleted.
  {
    newVec[ind] = 0.5*(mat[nodes[1],i] + mat[nodes[2],i] - mat[nodes[1],nodes[2]])
    ind <- ind+1
  }
  new <- cbind(new, newVec)
  newVec[ind] = 0
  new <- rbind(new, newVec)
  dimnames(new) <- NULL
  return(new)
}

library(igraph)


# These are example matrices: matex from slides, matBlatt from exercise.
matex <- matrix(c(0,5,4,9,8,5,0,5,10,9,4,5,0,7,6,9,10,7,0,7,8,9,6,7,0), 5, 5)
matBlatt <- matrix(c(0,3,5,9, 3,0,6,3, 5,6,0,4, 9,3,4,0), 4,4, byrow = T)

dist = matBlatt
g = make_empty_graph(n = ncol(dist))

n = ncol(dist)
usedNodes = c()
while(ncol(dist) - 2 >= 0) {
  print(dist)
  m = matrixM(dist)
  print(m)
  nodes = findNearestNodes(m)
  usables = nodes
  if(nodes[1] %in% usedNodes) {
    usables[1] = setdiff(V(g), usedNodes)[1]
  }
  usedNodes = c(usedNodes, usables[1])
  if(nodes[2] %in% usedNodes) {
    usables[2] = setdiff(V(g), usedNodes)[1]
  }
  usedNodes = c(usedNodes, usables[2])
  print(usables)
  lens = branchToNew(dist, nodes[1], nodes[2])
  print(lens)
  if(ncol(dist) == 2) break # At the last step, we do not add another vertex...
  g <- add_vertices(g, 1)
  g <- add_edges(g, c(usables[1], length(g)), label = lens[1])
  g <- add_edges(g, c(usables[2], length(g)), label = lens[2])
  dist = updateDistances(dist, nodes)
}
# ... and finish by adding the last edge.
g <- add_edges(g, c(usables[1], usables[2]), label=lens[1])

g <- as.undirected(g, mode = "each")
plot(g)
