#################################################################
#                                                               #
#                                                               #
#                      SHINY APP Ui                             #
#                                                               #
#    Shiny App must consits of either a "ui" & "server" file or #
#    an "app" file. Our files can be found in the "ShinyApp"    #
#    folder.                                                    #
#    This file creates the interface elements, header and some  #
#    texts. In the sidebar panel is a slider input to adjust    #
#    minimum threshold for the edges in € millions, a checkbox  #
#    to (de)activate the physics animations and reset/redraw    #
#    button. Below that you'll find text and tables regarding   #
#    the defaulted banks.                                       #
#                                                               #
#################################################################


shinyUI(fluidPage(

  tags$head(
    tags$style(type = "text/css", ".col-sm-4 { width: 25%; }"),
    tags$style(type = "text/css", ".col-sm-8 { width: 75%; }"),
    tags$style(type = "text/css", ".alignleft{ float: left;}"),
    tags$style(type = "text/css", ".alignright{ float: right;}")
  ),
  
  tags$div(id="textbox",
           h1(class="alignleft", style = "color: #434a7e;", "JVI Case Study - Contagion"),
           h6(class="alignright", "v1.0")
  ),
  
  tags$div(style="clear: both;"),
  
  h3(style = "margin-top: -5px; float: left;", "Perspective of Systemic Threat"),
  tags$div(style="clear: both;"),
  tags$em(h4("How severe is the default of a bank for the system?")),
  tags$div(style="clear: both;"),
  h4(style = "margin-bottom: 20px;", "The arrows point in the direction of the exposures und thereby in the direction of the contagion losses (i.e. arrow heads are creditors!)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider1", label = "Minimum Threshold of Edges (m. EUR):", min = min(data_interbank$EAD), 
                  max = max(data_interbank$EAD), value = round(mean(data_interbank$EAD), digits = 0)),
      checkboxInput("physics", label = "Network with Physics?", value = T),
      actionButton("redraw", "Redraw Network?"),
      
      tags$hr(style = "display: block; height: 1px; border: 0; border-top: 1px solid #ccc; margin: 1em 0; padding: 0; "),
      
      h4(textOutput("bank_name")),
      textOutput("additional_info"),
      DT::dataTableOutput("contagion_list")
    ),

    mainPanel(
      h4("Choose which bank should default:"),
      visNetworkOutput("netplot", width="100%", height="1000px")
      
    )
  )
))
