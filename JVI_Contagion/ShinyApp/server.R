#################################################################
#                                                               #
#                                                               #
#                      SHINY APP Server                         #
#                                                               #
#    Shiny App must consits of either a "ui" & "server" file or #
#    an "app" file. Our files can be found in the "ShinyApp"    #
#    folder.                                                    #
#    This file implements the mechanics behind the interface.   #
#    Within output$netplot we set all configs for the network   #
#    plot, such as groups and physics. The "observe" part waits #
#    for a user interaction, e.g. click in the plot, and        #
#    redraws the network, prints the text and tables. The       #
#    checkbox and buttons are controlled wihin observeEvent     #
#                                                               #
#################################################################


shinyServer(function(input, output) {
  
  ### network chart
  output$netplot <- renderVisNetwork({
    
    visNetwork(nodes,edges[edges$value > input$slider1,])%>% 
      visGroups(
        groupname = "Contagious", color = list(
          background = "#FF8080",
          border = "#FF3838",
          highlight = list(
            background = "#FF8F8F",
            border = "#FFF8F8"
          ),
          hover = list(
            background = "#FF8F8F",
            border = "#FFF8F8"
          )
        )
      ) %>%
      visGroups(
        groupname = "Default", color = list(
          background = "black",
          border = "#FF3838",
          highlight = list(
            background = "black",
            border = "#FFF8F8"
          ),
          hover = list(
            background = "black",
            border = "#FFF8F8"
          )
        )
      ) %>%
      visPhysics(minVelocity = 0.1,
                 maxVelocity = 50,
                 timestep = 0.3,
                 adaptiveTimestep = TRUE,
                 solver = "barnesHut",
                 stabilization=list(
                   iterations = 50,
                   updateInterval = 10,
                   fit = TRUE
                 )
      ) %>%
      # visIgraphLayout(#physics = T,
      #                 #smooth = T,
      #                 #type = "square",
      #                 randomSeed = 123) %>%
      visLayout(improvedLayout = FALSE, 
                randomSeed = 123
      ) %>%
      visOptions(
        nodesIdSelection = list(
          enabled = TRUE
        ),
        highlightNearest = FALSE
      ) %>%
      visLegend(addNodes = legendNodes, useGroups = FALSE, width = 0.1)
  })
  
  
  ### color the defaulted nodes if one node is selected and add edges of the defaulted node
  
  observe({
    newgroup <- rep("A",length(label))

    if (!is.null(input$netplot_selected)) {
      if(input$netplot_selected!="") {
        
        idxdef <- as.numeric(input$netplot_selected)
        codedef <- label[idxdef]
        
        newgroup <- rep("A",length(label))
        newgroup[idxdef] <- "Default"
        
        thisdefaults <- subset(defaulted, BANK == codedef)
        
        if (nrow(thisdefaults)>0) {
          newgroup[match(thisdefaults$COUNTERPARTY,label)] <- "Contagious"
        }
        
        # we want to display all connections when a node is clicked
        # get all from and to connections and add only those below the slider value
        posedges <- edges$from == idxdef | edges$to == idxdef
        posedges <- posedges & !(edges$value > input$slider1)
        newedges <- edges[posedges,]
        newedges_reset <<- rbind(newedges_reset, newedges)
  
        visNetworkProxy("netplot") %>% visUpdateEdges(newedges)
      }
    }
    
    nodes$group <- newgroup
    visNetworkProxy("netplot") %>% visUpdateNodes(nodes)
  })
  
  ### bank name for display
  output$bank_name <- renderText({
    
    if (!is.null(input$netplot_selected)) {
      if(input$netplot_selected!="") {
        idxdef <- as.numeric(input$netplot_selected)
        codedef <- label[idxdef]
        bankname <- as.character(nodes$title[nodes$label == codedef])
      }
    }
  })
  
  ### additional display info on number of defaults and rank
  output$additional_info<- renderText({
    if (!is.null(input$netplot_selected)) {
      if(input$netplot_selected!="") {
        
        idxdef <- as.numeric(input$netplot_selected)
        codedef <- label[idxdef]
        
        thisdefaults <- subset(defaulted, BANK == codedef)
        
        numDefs = nrow(thisdefaults)
        
        percrank <- contagionlosses$RANK[contagionlosses$BANK == codedef]
        
        if (numDefs > 1) {
          definfo <- paste("This bank caused",numDefs," defaults. ")
        } else if (numDefs == 1) {
          definfo <- paste("This bank caused",numDefs," default. ")
        } else if (numDefs == 0) {
          definfo <- "This bank caused no defaults."
        }
        
        definfo <- paste0(definfo,"Rank #", percrank," (of ",
                          max(contagionlosses$RANK),") for contagion losses.")
        
      }
    }
  })
  
  ### table with contagious defaults
  output$contagion_list <- DT::renderDataTable({
    if (!is.null(input$netplot_selected)) {
      if(input$netplot_selected!="") {

        idxdef <- as.numeric(input$netplot_selected)
        codedef <- label[idxdef]
        
        thisdefaults <- subset(defaulted, BANK==codedef)
        
        if (nrow(thisdefaults) > 0) {
          thisdefaults$defaults <- nodes$title[match(thisdefaults$COUNTERPARTY, nodes$label)]
          
          thisdefaults <- as.data.frame(thisdefaults[,"defaults"])
          names(thisdefaults) <- "Following banks suffered contagious defaults:"
          thisdefaults
        }
      }
    } 
  }, options = list(lengthChange = FALSE,bFilter=0)
  )
  
  
  ### Display network with or without physics
  observeEvent(input$physics, {
    
    if (!is.null(input$netplot_selected)) {
      if(input$physics == T) {
        visNetworkProxy("netplot") %>% visPhysics(enabled = TRUE)
      }else if(input$physics == F) {
        visNetworkProxy("netplot") %>% visPhysics(enabled = FALSE)
      }
    }
  })
  
  ### Redraw/Reset the network
  observeEvent(input$redraw, {
    
    if (!is.null(input$netplot_selected)) {
      if(nrow(newedges_reset) > 0)
      visNetworkProxy("netplot") %>%
        visRemoveEdges(id = newedges_reset$id) %>%
        visUpdateNodes(nodes_init)
    }
  })
})
