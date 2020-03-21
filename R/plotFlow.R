plotFlow <- function(nodes, edges, base64 = FALSE){
  tmp <- getShape(nodes = nodes, edges = edges)
  shape <- tmp$shape
  generation <- tmp$generation
  size_x <- max(shape)
  size_y <- length(shape)
  pos_x <- seq(0, 1, by = 1 / (size_x * 2))[seq(2, 2 * size_x, 2)]
  pos_y <- seq(0, 1, by = 1 / (size_y * 2))[seq(2, 2 * size_y, 2)]
  rank_within_generation <- ave(as.numeric(nodes$id), generation, FUN = function(x) rev(rank(x)))
  elpos <- cbind(
    pos_x[rank_within_generation],
    rev(pos_y)[generation + 1]
  )
  if(base64){
    tmp_file <- tempfile(fileext = ".png")
    png(tmp_file, width = 50 + 150 * max(shape), height = 120 * length(shape))
  }else{
    windows()
  }
  par(cex = 0.8)
  par(mar = c(1, 1, 1, 1))
  par(oma = c(1, 1, 1, 1))
  box_width <- 0.45
  if(max(generation[edges$to] - generation[edges$from]) > 1){
    elpos[, 1] <- elpos[, 1] * 0.8 + 0.1
    box_width <- 0.45 * 0.8
  }
  openplotmat()
  for(i in 1:nrow(edges)){
    if(generation[edges$to[i]] - generation[edges$from[i]] == 1){
      straightarrow(from = elpos[edges$from[i], ],
                    to = elpos[edges$to[i], ],
                    lwd = 1,
                    arr.pos = 0.6,
                    arr.length = 0.5)
    }else{
      curvedarrow(from = elpos[edges$from[i], ],
                  to = elpos[edges$to[i], ],
                  lwd = 1,
                  arr.pos = 0.6,
                  arr.length = 0.5,
                  curve = 0.3)
    }

  }
  for(i in 1:nrow(elpos)){
    textrect(elpos[i, ], box_width / max(shape), 0.04, shadow.size = 0, box.col = nodes$color[i], lab = nodes$label[i])
  }
  if(base64){
    dev.off()
    uri <- image_uri(tmp_file)
    return(sprintf("<img src=\"%s\" />\n", uri))
  }
}

# nodes <- data.frame(id = 1:10,
#                     color = "green3",
#                     label = 1:10,
#                     stringsAsFactors = FALSE)
# edges = data.frame(to = c(4, 4, 5, 6, 7, 7,  8, 9, 10),
#                    from = c(1, 2, 3, 4, 5, 6, 7, 8, 8))
# plotFlow(nodes = nodes, edges = edges)
