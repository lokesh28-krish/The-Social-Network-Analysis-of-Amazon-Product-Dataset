

BookRecommendation = function(productNodeId) {
  
  
  communityMembership = as.numeric(rec_raw[rec_raw$NodeId == productNodeId, "Community"])
  subv <- rec_raw[rec_raw$Community == communityMembership, 1]
  
  
  communityGraph <- induced.subgraph(coPurchaseNetworkGraph, vids = as.character(subv))
  
  
  CliqueVertexList = maximal.cliques(communityGraph, min = 2, max = 5)
  
  
  CliqueList <- lapply(CliqueVertexList, function (x) { induced_subgraph(communityGraph, x) })
  
  ProductBasedCliqueFlagList = sapply(CliqueList, function(x) { as.vector(table(V(x)$name == productNodeId))[2] })
  ProductBasedCliqueFlagList <- ifelse(is.na(ProductBasedCliqueFlagList), 0, 1)
  
  ProductBasedCliqueList <- CliqueList[ProductBasedCliqueFlagList == 1]
  
  cliquesDataFrameList <- lapply(ProductBasedCliqueList,
                                 function(y) {
                                   as.data.frame(
                                     list(
                                       NodeId = as.numeric(V(y)$name)
                                     ) 
                                     ,stringsAsFactors = FALSE
                                   )
                                 })
  cliqueDataFrame <- data.frame() 
  tempCliqueFrame <- data.frame()
  
  
  cliqueDataFrame <- unique(cliqueDataFrame[cliqueDataFrame$NodeId != productNodeId, ])
  cliqueDataFrame <- filter(rec_raw, NodeId %in% cliqueDataFrame)
  cliqueDataFrame <- subset (cliqueDataFrame, select = -c(Community,strength))
  
  book_cat <- rec_raw[rec_raw$NodeId == productNodeId, "Category"]
  cat_match <- cliqueDataFrame[cliqueDataFrame$Category == book_cat, ]
  cat_match <-cat_match[order(cat_match$AverageRating * cat_match$Downloads, cat_match$Reviews, decreasing = T), ]
  
  if (nrow(cat_match) < 2) {
    rest <- cliqueDataFrame[cliqueDataFrame$Category != book_cat, ]
    rest <-rest[order(rest$AverageRating * rest$Downloads, rest$Reviews, decreasing = T), ]
    other_cat <- rest[1:(2 - nrow(cat_match)), ]
    Rec <- rbind(cat_match, other_cat)
  } else{
    Rec <- cat_match
  }
  Rec <- Rec[complete.cases(Rec),]
  
  if (nrow(Rec) < 2) {
    extra <- rec_raw[rec_raw$Category == book_cat & !(rec_raw$NodeId %in% Rec$NodeId) & rec_raw$NodeId != productNodeId, ]
    extra <- extra[order(extra$AverageRating * extra$Downloads, extra$Reviews, decreasing = T), ]
    rec_extra <- extra[1:(2 - nrow(Rec)), c(1:7)]
    Rec1 <- rbind(Rec, rec_extra)
  } else{
    Rec1 <- Rec
  }
  Rec1 <- Rec1[complete.cases(Rec1),]
  
  Rec2 <- rbind(rec_raw[rec_raw$NodeId == productNodeId, c(1:7)], Rec1)
  Recommendation<-Rec2[c(1:3),]
  
  return(Recommendation)
}
