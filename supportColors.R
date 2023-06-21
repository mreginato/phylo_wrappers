supportColors <- function(support=tree$node.label, breaks=c(50,70,90), colors="gray") {
  if (length(colors) == 1) {
    colors <- gray.colors(length(breaks)-1, start=1, end=0)
  } else {
    colors -> colors
  }
  cut(support, breaks, include.lowest=T) -> c.sup
  levels(c.sup) -> cuts
  as.character(c.sup) -> node.colors
  for (i in 1:length(cuts)) {
    sub(cuts[i], colors[i], node.colors, fixed=T) -> node.colors
  }
  return(list(support=node.colors, cols=colors, cuts=cuts))
}