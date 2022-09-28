#' dijkstra alhorithum
#'
#'@description This algorithum used for to find shortest path from one point to another point.
#'
#' @param graph a graph
#' @param init_node a number
#'
#' @return a vector
#' @export
#'
#' @examples
#' wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'          w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#'
#'@references \url{https://en.wikipedia.org/wiki/Euclideanalgorithm.}

dijkstra<-function(graph, init_node){
  vec <- names(graph)
  stopifnot("v1" %in% vec, "v2" %in% vec, "w" %in% vec , init_node %in% graph$v1, init_node %in% graph$v2  )

    if(!(names(graph)[1] == "v1"&& names(graph)[2] == "v2" && names(graph)[3] =="w")){
      stop("column names of the input graph must be v1, v2 and w")
    }
    else if (!(init_node %in% unlist(graph[1], graph[2]))){
      stop("init_node must be in the input graph")
    }
    else{
      verset_set_M <- c()
      dist <- c()
      prev <- c()
      for (vertex in unique(unlist(list(graph$v1, graph$v2)))) {
        dist[vertex] <- Inf
        prev[vertex] <- 'NULL'
        verset_set_M<- append(verset_set_M, vertex)
      }
      names(prev) <- verset_set_M
      names(dist) <- verset_set_M
      dist[names(dist) == init_node] <- 0
      while(length(verset_set_M) != 0){
        vertex_k <- as.numeric(names(dist)[dist == min(unlist(dist[names(dist) %in% verset_set_M]))])
        verset_set_M <- verset_set_M[!(verset_set_M %in% vertex_k)]
        closest_of_k <- unique(unlist(list(graph$v2[graph$v1 %in% vertex_k], graph$v1[graph$v2 %in% vertex_k])))
        closest_of_k_in_M <- closest_of_k[closest_of_k %in% verset_set_M]
        for (closest_v in closest_of_k_in_M) {
          alt <- dist[names(dist) == vertex_k] + graph$w[(graph$v1 == vertex_k & graph$v2 == closest_v)]
          if(alt < dist[names(dist) == closest_v]){
            dist[names(dist) == closest_v] <- alt
            prev[names(dist) == closest_v] <- vertex_k
          }
        }
      }
      return(as.numeric(dist))
    }
  }

