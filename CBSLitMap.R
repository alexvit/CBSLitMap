
# Setting up --------------------------------------------------------------
 
library(tidyverse)
library(microdemic)
library(visNetwork)
library(tidygraph)
library(shiny)
library(plotly)
library(igraph)
library(RColorBrewer)
# library(tm)
# library(tidytext)
# library(textreuse)

Sys.setenv(MICROSOFT_ACADEMIC_KEY = "04edc778fe3444878795798cd59a8e2d")
paperAttrs <- c("RId", "Id", "AA.AuN", "J.JN", "Ti", "Y", "CC", "AW", "F.DFN", "DN", "DJN")


# Importing Zotero library
dfZotero <- read_csv("My Library.csv")
colnames(dfZotero) <- make.names(colnames(dfZotero))
dfZotero <- dfZotero %>%
  filter(str_detect(Manual.Tags, "Course: Corporate")) %>%
  filter(!is.na(Extra))


dfCourse <- tibble()


# Gather data Tri-citation and reference mapping  ---------------------------------

# Simply request with Id from MA. Requires adding paper's microsoft ID to "Extra" field in Zotero
for(i in 1 : nrow(dfZotero)){
  tmp <- ma_search(query = paste0("Id = ", dfZotero$Extra[i]), atts = paperAttrs)
  dfCourse <- dfCourse %>%
    bind_rows(tmp)
}

# # Search by title (in development)
# for(i in 1 : nrow(dfZotero)){
#   tmp <- ma_interpret(str_sub(paste0(str_remove(dfZotero$Author[i], ";.*"),
#                              "; ", dfZotero$Publication.Year[i], "; ",
#                              dfZotero$Title[i]), 1, 50), count = 1,
#                       opts = list(
#                         timeout = 0.5
#                       ))
#  
#   if(is.null(tmp$interpretations$rules[[1]]$output.value)) next()
#   tmp2 <- ma_search(query = tmp$interpretations$rules[[1]]$output.value, atts = paperAttrs)
#   if(nrow(tmp2) > 1){
#     tmp2 <- tmp2[1,]
#   } else if(is.null(tmp2)){
#     next()
#   } 
#   tmp2$Key <- dfZotero$Key[i]
#   dfCourse <- dfCourse %>%
#     bind_rows(tmp2)
#   print(i)
# }
# 
# dfCourse <- dfCourse %>% 
#   distinct(Id, .keep_all = T)
# 
# dfZotero <- dfZotero %>% 
#   left_join(select(dfCourse, Key, Id), by = "Key") %>% 
#   mutate(Extra = Id)



# Prepare Microsoft Academic topics to visualise on the hovers
dfAssist <- dfCourse %>% 
  select(Id, F) %>% 
  unnest_longer(F) %>% mutate(DFN = unlist(F), .keep = "unused")

dfAssist <- dfAssist %>% 
  group_by(DFN) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Id, DFN) %>% 
  group_by(Id) %>% 
  mutate(tag = paste0("tag", 1:n())) %>% 
  pivot_wider(values_from = DFN, names_from = "tag")

dfCourse <- dfCourse %>% 
  left_join(dfAssist)


# Gather papers that cite course's papers from MA
dfCite <- tibble()
  
for(i in 1 : nrow(dfCourse)){
  tmp <- ma_search(query = paste0("RId = ", dfCourse$Id[i]), count = 20000)
  tmp$idEgo <- dfCourse$Id[i]
  dfCite <- dfCite %>%
    bind_rows(tmp)
}

 
# # add xy axes based on abstract and topics similarity -------------------------------------------------------------------------
# # Sources:
# # https://medium.com/@adriensieg/text-similarities-da019229c894
# # https://cran.r-project.org/web/packages/textreuse/vignettes/textreuse-pairwise.html
# 
# # Postponed for now
# 
# dfAssist5 <- dfCourse %>% 
#   select(Id, AW) %>% 
#   unnest_longer(AW) 
# 
# dfAssist6 <- dfCourse %>% 
#   select(Id, F) %>% 
#   unnest_longer(F) %>% 
#   rename(AW = `F`) %>% 
#   mutate(AW = unlist(AW),
#          AW = str_replace_all(AW, " ", "_")) %>% 
#   bind_rows(dfAssist5)
# 
# dfAssist6 <- dfAssist6 %>% 
#   group_by(Id) %>% 
#   summarise(text = str_flatten(AW, collapse = " ")) %>% 
#   rename(doc_id = Id) %>% 
#   filter(nchar(text) > 100)
# 
# tr <- TextReuseCorpus(text = dfAssist6$text, meta = list(id = dfAssist6$doc_id))
# 
# mtDist <- pairwise_compare(tr, jaccard_similarity)
# 
# 
# rownames(mtDist) <- dfAssist6$doc_id
# colnames(mtDist) <- dfAssist6$doc_id
# mtDist[is.na(mtDist)] <- 0
# 
# txtSimScore <- as_tibble(mtDist)
# txtSimScore$Var1 <- rownames(mtDist)
# txtSimScore %>% 
#   pivot_longer(cols = matches("^Var1"), names_to = "Var2", values_to = "value") %>% 
#   select(Var1, Var2, txtSimScore = value)
# 
# # mtDist <- mtDist[,rownames(mtDist)]
# # mtDist <- as.dist(mtDist)
# 
# mds <- cmdscale(mtDist, k = 2)
# mdsTxtSim <- as_tibble(mds, rownames = "id") %>% 
#   rename(xTxtSim = V1, yTxtSim = V2) %>% 
#   mutate(id = as.numeric(id))
 
# Calculate similarity based on references and future citation similarity ---------------------------------------
# And calculate mapping based on that

# Create 3-citation proximity score

dfAssist <- as_tibble(expand.grid(dfCourse$Id, dfCourse$Id)) %>%
  filter(Var1 != Var2) %>%
  distinct(Var1, Var2, .keep_all = T)

dfAssist2 <- dfCite %>%
  count(Id) %>%
  filter(n > 1)

dfCite <- dfCite %>%
  semi_join(dfAssist2)

out <- numeric()
for(i in 1:nrow(dfAssist)){
  dfTmp <- dfCite %>%
    filter(idEgo == dfAssist$Var1[i] | idEgo == dfAssist$Var2[i])
tmp <- dfTmp %>%
    count(Id) %>%
    filter(n == 2) %>%
    nrow()
tmpTotal1 <- dfCourse$CC[dfCourse$Id == dfAssist$Var1[i]]
tmpTotal2 <- dfCourse$CC[dfCourse$Id == dfAssist$Var2[i]]

  out <- c(out, mean(c(coalesce(tmp/tmpTotal1, 0), coalesce(tmp/tmpTotal2, 0))))
}

dfAssist$propFComCit <- 1 - out


# Turning this into matrix.
mtDist <- dfAssist %>%
  pivot_wider(names_from = Var2, values_from = propFComCit, names_prefix = "")

rownames(mtDist) <- mtDist$Var1
mtDist <- as.matrix(mtDist, rownames.force = T)[,-1]

mtDist <- mtDist[,rownames(mtDist)]
mtDist <- as.dist(mtDist)


mds <- cmdscale(mtDist, k = 2)
mdsF <- as_tibble(mds, rownames = "id") %>%
  rename(xFCom = V1, yFCom = V2) %>%
  mutate(id = as.numeric(id))


# Create citation proximity score

dfAssist4 <- dfAssist %>%
  left_join(select(dfCourse, Id, RId1 = RId), by = c("Var1" = "Id")) %>%
  left_join(select(dfCourse, Id, RId2 = RId), by = c("Var2" = "Id")) %>%
  mutate(propBComCit = map2_dbl(RId1, RId2, ~length(intersect(.y, .x)) / min(c(length(.x), length(.y)))),
         propBComCit = coalesce(propBComCit, 0),
         propBComCit = 1 - propBComCit)

# Create average forward and backward citation score
dfAssist4 <- dfAssist4 %>%
  mutate(propFBComCit = (propFComCit+ propBComCit) / 2)

# Turning this into matrix.
mtDist <- dfAssist4 %>%
  select(Var1, Var2, propBComCit) %>%
  pivot_wider(names_from = Var2, values_from = propBComCit, names_prefix = "")

rownames(mtDist) <- mtDist$Var1
mtDist <- as.matrix(mtDist, rownames.force = T)[,-1]

mtDist <- mtDist[,rownames(mtDist)]
mtDist <- as.dist(mtDist)


mds <- cmdscale(mtDist, k = 2)
mdsB <- as_tibble(mds, rownames = "id") %>%
  rename(xBCom = V1, yBCom = V2) %>%
  mutate(id = as.numeric(id))


# Now average

# Turning this into matrix.
mtDist <- dfAssist4 %>%
  select(Var1, Var2, propFBComCit) %>%
  pivot_wider(names_from = Var2, values_from = propFBComCit, names_prefix = "")

rownames(mtDist) <- mtDist$Var1
# mtDist$Var1 <- NULL
mtDist <- as.matrix(mtDist, rownames.force = T)[,-1]
mtDist[is.na(mtDist)] <- 0

mtDist <- mtDist[,rownames(mtDist)]
mtDist <- as.dist(mtDist)
# mtDist <- scale(mtDist)


mds <- cmdscale(mtDist, k = 2)
mdsFB <- as_tibble(mds, rownames = "id") %>%
  rename(xFBCom = V1, yFBCom = V2) %>%
  mutate(id = as.numeric(id))

# # For future development. Try to do mapping with correspondence analysis.
# It might make mapping more interpretable.

# caFit <- ca(mtDist)
# caFit <- FactoMineR::CA(mtDist)
# factoextra::fviz_ca_row(caFit)




# Prepare visualization with PlotLy ------------------------------------------------
dfAssist3 <- dfZotero %>% 
  select(Extra, Manual.Tags) %>% 
  mutate(session = str_remove_all(str_extract(Manual.Tags, "Session.*?;|Session.*$"), ";|Session: "),
         type = str_extract(Manual.Tags, "Type*?;|Type.*$"),
         Extra = as.numeric(Extra))

nodes <- dfCourse %>% 
  rename(id = Id) %>% 
  left_join(mdsF, by = "id") %>% 
  left_join(mdsB, by = "id") %>% 
  left_join(mdsFB, by = "id") 

nodes <- nodes %>% 
  unnest_wider(AA) %>% 
  unnest_wider(AuN) 

nodes <- nodes %>% 
  left_join(dfAssist3, by = c("id" = "Extra"))

nodes <- nodes %>% 
  mutate(title = paste0(DN, "<br>", "Author1: ", ...1, "<br>", "Author2: ", ...2, "<br>", 
                        "<br>", "Year:  ", Y, "<br>", 
                        "Journal = ", J.JN, "<br>",
                        "Session:", session, "<br>",
                        type, "<br>",
                        "# Ciations = ", CC, "<br>",
                        "Topic1 = ", tag1, "<br>",
                        "Topic2 = ", tag2, "<br>",
                        "Topic3 = ", tag3, "<br>",
                        "Topic4 = ", tag4, "<br>",
                        "Topic5 = ", tag5, "<br>"))

nbCols <- n_distinct(nodes$session)
dfAssist2 <- nodes %>% 
  distinct(session) %>% 
  # mutate(color = RColorBrewer::brewer.pal(n_distinct(nodes$session), name = "Set3")) 
  mutate(color = colorRampPalette(brewer.pal(8, "Set2"))(nbCols))
  
nodes <- nodes %>% 
  left_join(dfAssist2, by = "session")

# write_rds(nodes, "CBSNodes.rds")



# Preparing the citation network with NetVis ------------------------------

nodes <- nodes %>% 
  bind_rows(nodesExtend)

edges <- nodes %>% 
  select(id, RId) %>% 
  unnest_longer(RId) %>% 
  distinct(id, RId)

edges4Net <- edges %>% 
  filter(RId %in% dfCourse$Id)

edges4Net <- edges4Net %>% 
  rename(to = RId, from = id)

dfAssist <- as_tbl_graph(edges4Net) %>% 
  to_undirected() %>% 
  mutate(community = tidygraph::group_leading_eigen(),
         betweenness = centrality_betweenness(),
         degreeIn = centrality_degree(mode = "out"),
         degreeOut = centrality_degree(mode = "out")) %>% 
  as_tibble() %>% 
  rename(id = name) %>% 
  mutate(id = as.numeric(id))

nodes4Net <- nodes %>% 
  left_join(dfAssist, by = "id")

dfAssist <- nodes4Net %>% 
  count(typeGen) %>% 
  mutate(shape = c("square", "triangle", "box", "circle", "star",
                   "ellipse", "diamond")[1:n_distinct(typeGen)])

nodes4Net <- nodes %>% 
  left_join(dfAssist, by = "type")

# Test visualization
lgd1 <- distinct(nodes4Net, shape, label = type) %>% 
  mutate(label = str_remove(label, "Type: "))
lgd2 <- distinct(nodes4Net, color, label = session)
lgd <- bind_rows(lgd1, lgd2)

visNetwork(nodes4Net, edges4Net) %>%
  visNodes(scaling = list(label = list(enabled = T)),
           mass = 1) %>%
  visEdges(smooth = F, arrows = "to") %>%
  visPhysics(stabilization = F) %>%
  visLayout(randomSeed = 12) %>%
  visLegend(addNodes = lgd, useGroups = F, width = 0.2, position = "right", main = "Type") %>% 
  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"))%>%
  visIgraphLayout(layout = "layout_nicely")


# Gather papers to improve transitivity -----------------------------------


# Gather papers that improve BACKWARD transitivity the most

gr <- graph_from_data_frame(edges4Net)
gr2 <- edges %>% 
  anti_join(dfCourse, by = c("RId" = "Id")) %>% 
  filter(!is.na(RId), !is.na(id)) %>% 
  rename(from = id, to = RId) 

out <- numeric(length = nrow(gr2))
for(i in 1:nrow(gr2)){
  tmp <- edges %>% 
    filter(RId %in% dfCourse$Id | RId == gr2$to[i]) %>% 
    filter(!is.na(RId), !is.na(id))
  tmp <- graph_from_data_frame(tmp)
  # gr3 <- union(gr, subgraph.edges(gr2, i))
  out[i] <- transitivity(tmp)
}

dfAssist <- tibble(Id = gr2$to, transit = out) %>% 
  distinct(Id, .keep_all = T) %>% 
  slice_max(transit, n = 30)

dfAssist2 <- tibble()
for(i in 1 : nrow(dfAssist)){
  tmp <- ma_search(query = paste0("Id = ", dfAssist$Id[i]), atts = paperAttrs)
  dfAssist2 <- dfAssist2 %>%
    bind_rows(tmp)
}

dfAssist <- dfAssist %>% 
  left_join(dfAssist2, by = "Id") %>% 
  unnest_wider(AA) %>% 
  separate(AuN, ",", into = paste0("author_", 1:5), remove = F) %>% 
  mutate(title = paste0("<p>", DN, "<br>", "author1: ", author_1, "<br>", "author2: ", author_2, "<br>", 
                        "<br>", "Year:  ", Y, "<br>", 
                        "Journal = ", J.JN, "<br>",
                        "MA_Ciations = ", CC, "<br>",
                        "</p>")) %>% 
  mutate(isTransit = 1)

dfAssist <- dfAssist %>% 
  rename(id = Id)

nodes <- nodes %>% 
  bind_rows(dfAssist)


# Same for forward transitivity



# Writing data for CBS course
write_rds(nodes, "CBSNodes.rds")
write_rds(dfCourse, "CBSCourse.rds")

# # Writing data for FoM course
# write_rds(nodes, "FoMNodes15_06_2021.rds")
# write_rds(dfCourse, "FoMCourse15_06_2021.rds")

# Preparing the network to visualize (supplanted by Plotly) --------------------------------------

# dfAssist %>%
#   filter(nCoCites > 0)

edges <- dfAssist %>% 
  rename(from = Var1, to = Var2, value = propFComCit, weight = propFComCit)

# Select only necessary data from exported Zotero library
dfAssist3 <- dfZotero %>% 
  select(Extra, Manual.Tags) %>% 
  mutate(session = str_remove_all(str_extract(Manual.Tags, "Session.*?;|Session.*$"), ";|Session: "),
         Extra = as.numeric(Extra))

# dfCourse %>% 
#   unnest_wider(F) %>% 
#   unnest_wider(DFN)

# dfCourse %>% 
#   unnest_wider(AW)  
#   unnest_wider(DFN)

nodes <- dfCourse %>% 
  rename(id = Id) %>% 
  left_join(mds, by = "id") 

nodes <- nodes %>% 
  unnest_wider(AA) %>% 
  separate(AuN, ",", into = paste0("author_", 1:5), remove = F)

nodes <- nodes %>% 
  left_join(dfAssist3, by = c("id" = "Extra"))

nodes <- nodes %>% 
  mutate(title = paste0("<p>", DN, "<br>", "author1: ", author_1, "<br>", "author2: ", author_2, "<br>", 
                        "<br>", "Year:  ", Y, "<br>", 
                        "Journal = ", J.JN, "<br>",
                        "MA_Ciations = ", CC, "<br>",
                        "Session:", session,
                        "</p>"))

dfAssist2 <- nodes %>% 
  distinct(session) %>% 
  # select(session, id) %>% 
  mutate(color = RColorBrewer::brewer.pal(n_distinct(nodes$session), name = "Set1")) 

nodes <- nodes %>% 
  left_join(dfAssist2, by = "session")

# visNetwork(nodes, edges) %>% 
#   visNodes(shape = "square",
#            scaling = list(label = list(enabled = T))) %>% 
#   visEdges(hidden = T) %>% 
#   visPhysics(stabilization = F) %>%
#   visLayout(randomSeed = 12) %>%
#   # visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"))%>%
#   visIgraphLayout(layout = "layout.norm", layoutMatrix = as.matrix(dplyr::select(nodes, x, y)))

write_rds(nodes, "CBSNodes.rds")
# write_rds(edges, "CBSEdges.rds")


# References --------------------------------------------------------------

# Guide interactive ggplot to shiny apps: https://www.youtube.com/watch?v=Q6RswNU2xLQ
# Vignette for Shiny visNetwork apps: shiny::runApp(system.file("shiny", package = "visNetwork"))
# Flexdashboards homepage: https://pkgs.rstudio.com/flexdashboard/?_ga=2.82959062.1799990613.1623311790-1234995206.1615229242



# Cellar ------------------------------------------------------------------


nodes <- nodes %>% 
  mutate(typeGen = ifelse(str_detect(type, "theoretical"), "Type: theoretical", type),
         typeGen = ifelse(str_detect(typeGen, "meta"), "Type: quantitative", typeGen))

dfAssist <- nodes %>% 
  count(type) %>% 
  mutate(shape = 1:3)

nodes <- nodes %>% 
  left_join(dfAssist)

nodes <- nodes %>% 
  mutate(session = as.factor(session))

nodes <- nodes %>% 
  mutate(session = factor(session, c(levels(nodes$session)[-2], levels(nodes$session)[2])))

nodes %>% 
  count(session)

g <- nodes %>% 
  ggplot(aes(x = xFBCom, y = yFBCom, 
             text = title,
             fill = session)) +
  geom_point(aes(shape = typeGen)) +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        # axis.text = element_blank(),
        # axis.ticks = element_blank()
        # panel.background = element_blank()
  ) 

g
ggplotly(g, tooltip = c("title"))


