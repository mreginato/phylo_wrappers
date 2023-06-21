checkPhylo <- function(tree = tree, data = data) {
  tree$tip.label -> tips
  if (length(nrow(data)) == 0) {
    data.frame(data) -> data 
  }
  rownames(data) -> spp
  tips[is.na(match(tips, spp))] -> missing.tree
  spp[is.na(match(spp, tips))] -> missing.data
  if (length(missing.tree) > 0) {
    drop.tip(tree, missing.tree) -> ntree
  } else {
    missing.tree <- "none"
    tree -> ntree
  }
  if (length(missing.data) > 0) {
    as.vector(missing.data) -> rem.data
    match(rem.data, spp) -> rem.rows
    as.matrix(data) -> data
    data[-rem.rows,] -> data
    as.data.frame(data) -> data
    Data<-data[ntree$tip.label,]
  } else {
    missing.data <- "none"
    Data<-data[ntree$tip.label,]
  }
  cat("Removed taxa from tree:", fill=T)
  cat("", fill=T)
  for (i in 1:length(missing.tree)) {
    cat(missing.tree[i], fill=T)
  }
  cat("", fill=T)
  cat("Removed taxa from data:", fill=T)
  cat("", fill=T)
  for (i in 1:length(missing.data)) {
    cat(missing.data[i], fill=T)
  }
  return(list(tree=ntree, data=Data, dropped_data=missing.data, dropped_tree=missing.tree)) 
}