################################
# 2.6 Resampling study R script
################################
# Performed in R
	# Table 2.1 extracted as file "2.1-COG_table.txt"
	# Table 2.2 extracted as file "2.2-deCOG_table.txt"

R

library(ade4) # v.1.7-22
library(ggplot2) # v.3.4.4
library(purrr) # v.1.0.2

nsim <- 10000
deCOG <- read.delim("2.2-deCOG_table.txt", row.names = 1, skip = 4)
numDE <- colSums(deCOG)
COG <- read.delim("2.1-COG_table.txt", row.names = 1, skip = 4)
allCOGs <- rownames(COG)
COGsets <- apply(COG, 2, function(x){
    present.COGs <- rownames(COG)[x == 1]
    return(present.COGs)
})

# # Run resampling study (drawn from full set of 16,600 COGs)
# num.overlap <- sapply(1:nsim, function(i){
#     set.seed(i)
#     # Sample number of COGs equal to number of significantly DE COGs for each organism
#     sig.genes <- sapply(numDE, function(n)sample(allCOGs, size = n))
#     overlaps <- reduce(sig.genes, intersect)
#     return(length(overlaps))
# })

num.overlap <- sapply(1:nsim, function(i){
    set.seed(i)
    # Sample number of COGs equal to number of significantly DE COGs for each organism from COGsets
    organisms <- names(COGsets)
    sig.genes <- sapply(organisms, function(org){
        sample(COGsets[[org]], size = numDE[org])
    })
    overlaps <- reduce(sig.genes, intersect)
    return(length(overlaps))
})

plotdat <- data.frame(`Number of Overlapping COGs` = num.overlap, check.names = FALSE)
ggplot(plotdat, aes(x = `Number of Overlapping COGs`)) + geom_histogram(color = "black", fill = "cyan") + geom_vline(xintercept = 160, color = "red", linetype = "dashed") + labs(y = "Frequency") + theme_bw()
