#################################################################
#                                                               #
#                                                               #
#                      SHINY APP Global                         #
#                                                               #
#    Shiny App must consits of either a "ui" & "server" file or #
#    an "app" file. Our files can be found in the "ShinyApp"    #
#    folder.                                                    #
#    This file will prepare the data for the network plot and   #
#    run the app at the end.                                    #
#                                                               #
#################################################################


### visnetwork

## Build the nodes for the network

# Take the unique set of creditor and debtor bank IDs given by the interbank exposure
label <- sort(unique(c(data_interbank$BankCode, data_interbank$DebtorBankCode)))
id <- 1:length(label)
# Initiate the contagion group
group <- rep("A", length(label))
# Here you would match your real bank names with the bank IDs from label
title <- paste0("Bank ", label)
# visNetwork needs exactly this data frame
nodes <- data.frame(id, label, group, title)


## Build the edges for the network

from <- match(data_interbank$DebtorBankCode, label)
to <- match(data_interbank$BankCode, label)
value <- data_interbank$EAD # / 1000000
arrows <- rep("to", length(from))

edges <- data.frame(from, to, arrows, value)
edges <- unique(edges)
edges$id <- 1:nrow(edges)

# Get rid of connections between the same banks
edges <- subset(edges, from != to)


## The app will have a reset button for the network plot

# Initialize empty data frame and original nodes
newedges_reset <- data.frame(from = numeric(0), to = numeric(0), arrows = numeric(0), value = numeric(0))
nodes_init <- nodes


## The network needs to know which banks causes contagious defaults

# Take the saved total defaults per bank
defaulted <- cbind("BANK" = c(1:nobanks), as.data.frame(data_furfine_total[,1,]))
# Reshape the data to a long-format for easier handling
defaulted <- melt(data = defaulted, id.vars = "BANK")
defaulted$BANK <- as.numeric(defaulted$BANK)
defaulted$variable <- as.numeric(defaulted$variable)
# Keep only the rows which indicate a default and remove the diagonal
defaulted <- subset(defaulted, value != 0 & BANK != variable)
defaulted$value <- NULL
names(defaulted) <- c("BANK","COUNTERPARTY")
defaulted <- defaulted[order(defaulted$BANK),] 


## Rank the banks by their caused contagion losses

contagionlosses <- data.frame("BANK" = c(1:nobanks), "VALUE" = rowSums(data_furfine_total[,5,]))
contagionlosses$RANK <- rank(-contagionlosses$VALUE, ties.method="min") 


## Legend
legendNodes <- data.frame(
  label = c("Stable", "Defaulted","Contagious"),
  color.background = c("#c6dbff", "#f9f9f9","#ffc1c1"),
  color.border = c("blue", "black", "#FF3838")
)
