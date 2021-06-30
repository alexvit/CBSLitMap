This is an interactive map and report for Corporate and Business strategy literature. It presents all papers from the course as a scatterplot based on citation similarity (tab1 of the dashboard) and as a citation graph (tab2 of the dashboard). The tabs also contain my reflections on what these visualizations tell about the literature.  
Below are my notes on motivation, technology used, and plans for further development. You can also find these notes in tab3 of the dashboard. The Shiny app is available via the link: https://alextyulyupo.shinyapps.io/CBSLitMapShinyDash/  

**Motivation**  
*I choose this form for the mindmap assignment because I wanted to:*  

1. Automatically obtain readily available information: publication year, citations, authors, journal;  
2. Effortlessly import all information on the papers I've previously added to Zotero (e.g., methodology used, custom topics, notes);  
3. Avoid exposing my awful sense of aesthetics by trying to manually draw and chart things (although the design of the app is also showing, I'm afraid);  
4. Obtain coordinates for the papers based on some objective metric of their relationships and build my mindmaps on that;  
5. Get an insight on what to read next;  
6. Being able to compactly represent all this and conveniently share it;    
7. Have a framework that will do the same thing for other sets of literature at zero additional efforts.  

**Implementation**  
*This app is implemented in R. Here is the list of key technologies it relies on:*  

1. Zotero reference manager.  
2. Microsoft Academic Knowledge Graph API. Google Scholar doesn't have any convenient way to access its data. Microsoft is unlimited and open access, allows complex queries. All additional information on the papers obtained from there;   
3. Igraph package to calculate network metrics;  
4. Viznetwork package for interactive visualization of network data (tab2 - Citation network);  
5. MDS package for multidimensional scaling. This allowed me to map similarity matrix of the papers onto X_Y plane;  
6. Plotly for interactively plotting the non-network data (tab1 - custom mapping);  
7. Shiny and FlexDashboard to design and deploy the web-application.  

*This is the pipeline to get from course syllabus to the app:*  

1. Import CBS literature to Zotero and create bibliography items for each paper;    
2. Manually add tags that mark the CBS course, session, and type of the paper;  
3. Export the whole library in .csv file into R and filter only the CBS papers;   
4. Request information on the course papers from Microsoft Academic (MA);  
5. From MA, request information on the papers that were referenced by the course papers and the papers that cited them;
6. Calculate reference and future citations similarity scores. For instance, if paper A referenced half of the papers that were referenced by paper B, their reference similarity will be 0.5. If these two papers were cited together in 20 future papers, while paper A has 100 citations and paper B 100+ citations, then their future citation similarity score = 20/100 = 0.2. This is the tricitation score implemented from (McCain, 2009)* Their overall similarity score (the default on tab1) will be (0.2+0.5)/2 = 0.35. Performing this operation for all pairs of the papers yielded 3 citation similarity matrices;  
7. On each citation similarity matrix, perform multidimensional scaling, transforming similarity score into coordinates on XY plane. Similar papers will be mapped close together. This resulted in 3 sets of XY coordinates to choose from in the first tab;  
8. The data for citation similarity contained subset of cross references between the course literature to create the citation graph on tab2;  
9. To locate theoretical background of the papers, I first considered just taking the most commonly cited classics in the course paper. But this would be boring. I would probably just get the most commonly cited papers in OT, and this you can do in Microsoft Academic interface. Instead, I iteratively added to the citation graph each paper that was at least twice cited by the course's papers and measured how this improves transitivity of the graph. The top 10 papers seem relevant. Some of them are pivotal for strategy as a discipline and unite multiple papers in the course. Others are only important for small subsets of papers. My comments on that are available on hovers in tab2 after adding the transitivity-improving papers to the graph.    
10. Deploy the results as a web application.  

*McCain, Katherine W. 2009. “Using Tricitation to Dissect the Citation Image: Conrad Hal Waddington and the Rise of Evolutionary Developmental Biology.” Journal of the American Society for Information Science and Technology 60(7): 1301–19.  


**Plans for improvement**  
*These are just things that can be done better with more efforts than I could afford right now. Most of them I'm going to implement for future personal use.*

1. On tab2, fix node coordinates to original values, so that the papers stay in place when adding transitivity-improving papers. A mindmap shouldn't be too fluid;
2. Add hovers on reference links at tab2. This can be done manually (but this is too much work), or automatically (parsing the pdf's and taking taking the sentences with the cite to hover. But this would be quite challenging too). Now that I think about it, this could be a useful collaborative exercise for a group of PhD students;  
3. To tab1, add mapping based on abstract similarity (based on topic modelling, maybe with embeddings);
4. Tie choices of XY axes in tab1 to only appropriate combinations (e.g., x = future citations & y = common references - makes no sense);  
5. To tab2, I could do the same transitivity search for future works. I.e., what papers, if added to the graph of the literature, will most improve its transitivity  
6. Instead of multidimensional scaling, I am going to try correspondence analysis for tab1. I believe it can provide more meaningful interpretation of the dimension space.
7. It is also feasible to allow a user to upload personal zotero library in the interface and get the same analysis for this. But for my personal use, I'm satisfied with doing it with code as it takes only a couple of minutes.
