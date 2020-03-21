getShape <- function(edges, nodes){
  parents <- sapply(nodes$id, function(i){
    j <- i
    p <- 0
    while(any(j %in% edges$to) & !p > 10){
      p <- p + 1
      j <- edges$from[edges$to %in% j]
    }
    p
  })
  roots <- which(parents == 0)
  for(i in roots){
    node_id <- nodes$id[i]
    first_child_generation <- min(parents[nodes$id %in% edges$to[edges$from == node_id]])
    parents[i] <- first_child_generation - 1
  }
  shape <- as.vector(table(parents))
  return(list(shape = shape,
              generation = parents))
}

# nodes <- data.frame(id = 1:10,
#                     color = "green3",
#                     label = 1:10,
#                     stringsAsFactors = FALSE)
# edges = data.frame(to = c(4, 4, 6, 7, 7,  8, 9, 10, 9),
#                    from = c(1, 2, 3, 4, 5, 6, 7, 8, 8))
# plotFlow(nodes = nodes, edges = edges, base64 = FALSE)
