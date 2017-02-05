# AdvElectroNetwork.R
# R version 3.3.1 (2016-06-21)
# February 3, 2017. Mallory B. Lai.
# Reviewed by: TODO (Mallory B. Lai) : Find reviewer to proofread
# Creating pheno network for Brassica data using 
# bnlearn package. Data taken from Brassica control
# and droughted conditions. 

#-----------------------------------------------------------------------
library(bnlearn)
#-----------------------------------------------------------------------

#### Preprocessing: 

  # Read in middayCO file. 
  middayCO <- read.csv(file = "middayCO.csv", row.names = 1)
  
  # Read in middayDR file. 
  middayDR <- read.csv(file = "middayDR.csv", row.names = 1)
  
  # Read in predawnCO file. 
  predawnCO <- read.csv(file = "predawnCO.csv", row.names = 1)
  
  # Read in predawnCO file. 
  predawnDR <- read.csv(file = "predawnDR.csv", row.names = 1)
  
  # Remove CO and DR classifiers from column names on dataframes. 
  colnames(middayCO)[1:3] <- c("Stem", "Petiole", "Blade")
  colnames(middayDR)[1:3] <- c("Stem", "Petiole", "Blade")
  colnames(predawnCO)[1:3] <- c("Stem", "Petiole", "Blade")
  colnames(predawnDR)[1:3] <- c("Stem", "Petiole", "Blade")
  
  # Combine into one midday dataframe. 
  midday <- rbind(middayCO, middayDR)
  
  # Combine into one predawn dataframe. 
  predawn <- rbind(predawnCO, predawnDR)
  
  # Remove Fv.Fm.
  predawn <- predawn[, -11]
  
  # Create vector of FvFm for predawn. Values should be zero.
  predawn$FvP.FmP. <- rep(0, nrow(predawn))
  
  # Combine into one dataframe. 
  Brassica <- rbind(midday, predawn)
  
  # Discretize the data. 
  discBrassica <- discretize(Brassica, method = "interval",
                          breaks = c(3, 3, 3, 2, 2, 2, 2, 3, 5, 5, 2))
  
  # Create blacklist. 
  NameList <- colnames(discBrassica)
  BlackList <- data.frame(From = NameList[-4], 
                          To = rep("tempNorm", 10))
  BlackList <- rbind(BlackList, cbind(From = NameList[-6], 
                                      To = rep("VpdNorm", 10)))
  BlackList <- rbind(BlackList, cbind(From = NameList[-7], 
                                      To = rep("lightNorm", 10)))
  
#### Bootstrap approach: 

  # Apply a bootstrap resambling to learn a set of 1000 
  # network structures. 
  boot <- boot.strength(data = discBrassica, R = 1000, algorithm = "hc", 
                        m = nrow(discBrassica), 
                        algorithm.args = list(score = "bde", 
                        blacklist = BlackList, iss = 10))
  
  # Find the averaged network.
  avg.boot <- averaged.network(boot, threshold = 0.8)
  
  # Plot the network. 
  plot(avg.boot, main = "Averaged Boot bde")
  
  # Apply a bootstrap resambling to learn a set of 1000 network 
  # structures with AIC. 
  boot <- boot.strength(data = discBrassica, R = 1000, algorithm = "hc", 
                        m = nrow(discBrassica), 
                        algorithm.args = list(score = "aic",
                        blacklist = BlackList, iss = 10))
  
  # Find the averaged network.
  avg.boot2 <- averaged.network(boot, threshold = 0.8)
  
  # Plot the network. 
  plot(avg.boot2, main = "Averaged Boot AIC")
  
  # NOTE: Can use above approach with different algorithms. 

#### Average several hill-climbing searches, each starting from a 
  # different network. 
  
  # Create nodes for each variable. 
  nodes <- names(discBrassica)
  
  # Generate random graphs from uniform distribution over space
  # of connected graphs. 
  
      # Initialize. 
      start <- random.graph(nodes = nodes, method = "ic-dag", 
                            num = 500)
    
      # Loop.
      netlist <- lapply(start, function(net){
        hc(discBrassica, score = "bde", blacklist = BlackList,
           iss = 10, start = net)
      })

      # Measure strength of prob. relationship and use model averaging
      # to build network on sig. arcs. 
      rnd <- custom.strength(netlist, nodes = nodes)
      
      # Select average network with a strength greater than 0.7.
      avg.start <- averaged.network(rnd, threshold = .8)

      # Plot network. 
      plot(avg.start, main = "Averaged Hill-Climbing")

#### Analyze as interventional data: droughted vs control. 
   
    # Whitelisted intervention.   
         
        # Add intervention column. 
        discBrassica$INT <- as.factor(rep(c("CO", "DR"), 
                                          each = 4, times = 2))
        
        # Create whitelist. 
        wh <- matrix(c(rep("INT", 11), names(discBrassica)[1:11]), 
                     ncol = 2)
        
        # Perform tabu search.
        bn.wh <- tabu(discBrassica, whitelist = wh, 
                      blacklist = BlackList, 
                      score = "bde", iss = 10, tabu = 50)
        
        plot(bn.wh, main = "Whitelisted Interventions tabu")
        
    # Let structure learning algorithm decide which arcs 
    # connect to INT.  
      
      # Create list for blacklist.
      tiers <- list("INT", names(discBrassica)[1:11])
      
      # Create blacklist.
      bl <- tiers2blacklist(nodes = tiers)
      
      # Merge bl blacklist with BlackList.
      colnames(bl) <- c("From", "To")
      bl <- rbind(bl, BlackList)
      
      # Search for network.
      bn.tiers <- tabu(discBrassica, blacklist = bl, score = "bde",
                       iss = 10, tabu = 50)
      
      # Plot network. 
      plot(bn.tiers, main = "Learned Interventions CO v DR")
      
    
# Plot networks. 
  pdf("BNplots.pdf")
  plot(avg.boot, main = "Averaged Boot bde")
  plot(avg.boot2, main = "Averaged Boot AIC")
  plot(avg.start, main = "Averaged Hill-Climbing")
  plot(bn.wh, main = "Whitelisted Interventions tabu")
  plot(bn.tiers, main = "Learned Interventions CO v DR")
  dev.off()
       
#### Create interventions for midday and predawn only. 
  
  # Add intervention column. 
  midday$INT <- as.factor(rep(c("CO", "DR"), each = 4))
  
  # Discretize the data. 
  discMidday <- discretize(midday, method = "interval",
                    breaks = c(3, 3, 3, 2, 2, 2, 2, 3, 5, 5, 2, 2))
  
  # Let structure learning algorithm decide which arcs 
  # connect to INT.  
  
    # Create list for blacklist.
    tiers <- list("INT", names(midday)[1:11])
    
    # Create blacklist.
    bl <- tiers2blacklist(nodes = tiers)
    
    # Merge bl blacklist with BlackList.
    colnames(bl) <- c("From", "To")
    bl <- rbind(bl, BlackList)
    
    # Search for network.
    M.bn.tiers <- tabu(discMidday, blacklist = bl, score = "bde",
                     iss = 10, tabu = 50)
    
    # Plot network. 
    plot(M.bn.tiers, main = "Midday Learned Interventions CO v DR")
  
  # Repeat for predawn. 
    
    # Combine into one predawn dataframe. 
    predawn <- rbind(predawnCO, predawnDR)
    
    # Create vector of FvFm for predawn. Values should be zero.
    predawn$FvP.FmP. <- rep(0, nrow(predawn))
    
    # Add intervention column. 
    predawn$INT <- as.factor(rep(c("CO", "DR"), each = 4))
    
    # Discretize the data. 
    discPredawn <- discretize(predawn, method = "interval",
                        breaks = c(3, 3, 3, 2, 2, 2, 2, 3, 5, 5, 2, 2))
    
    # Let structure learning algorithm decide which arcs 
    # connect to INT.  
    
    # Create list for blacklist.
    tiers <- list("INT", names(predawn)[1:11])
    
    # Create blacklist.
    bl <- tiers2blacklist(nodes = tiers)
    
    # Create blacklist. 
    NameList <- colnames(predawnCO)
    BlackList <- data.frame(From = NameList[-4], 
                            To = rep("tempNorm", 10))
    BlackList <- rbind(BlackList, cbind(From = NameList[-6], 
                                        To = rep("VpdNorm", 10)))
    BlackList <- rbind(BlackList, cbind(From = NameList[-7], 
                                        To = rep("lightNorm", 10)))
    
    # Merge bl blacklist with BlackList.
    colnames(bl) <- c("From", "To")
    bl <- rbind(bl, BlackList)
    
    # Search for network.
    P.bn.tiers <- tabu(discPredawn, blacklist = bl, score = "bde",
                     iss = 10, tabu = 50)
    
    # Plot network. 
    plot(P.bn.tiers, main = "Predawn Learned Interventions CO v DR")

# Create pdf of predawn and midday networks. 
    # Plot networks. 
    pdf("BN_pd_md_plots.pdf")
    plot(M.bn.tiers, main = "Midday Learned Interventions CO v DR")
    plot(P.bn.tiers, main = "Predawn Learned Interventions CO v DR")
    dev.off()
    
# Write csv files for predawn and midday learned interventions
    # to import to Neo4j. 
    write.csv(P.bn.tiers$arcs, file = "PredawnLearnedArcs.csv")
    write.csv(M.bn.tiers$arcs, file = "MiddayLearnedArcs.csv")

    # Import into Neo4j:
    
    #LOAD CSV WITH HEADERS FROM 'file:///prob_above08.csv' AS line
    #MERGE (a:Gene {name:line.From})
    #MERGE (b:Gene {name:line.To})
    #MERGE (a)-[:RELATED_TO {method: "GRENITS", 
    # Prob: TOFLOAT( line.Probability), 
    # Strength: TOFLOAT(line.Strength)}]->(b);
    
library(igraph)
library(visNetwork)
library(RNeo4j)
    
graph = startGraph("http://localhost:7474/db/data/", username = "neo4j",
                       password = "plantanalytics")

query = "
MATCH (n)-[]-(m)
WHERE n.name < {x}
RETURN n, m
"

edges <- cypher(graph, query, x = "A")

head(edges)




