library(shiny)
library(shinyBS)
library(plotly)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("sandstone"),
                useShinydashboard(), #This is important to enable you to use shiny dashboard functions
           navbarPage("Shiny Tutorial", #Creates a page with a top navigation bar
                 tabPanel("About", #Used to create individual tabs
                          box( #Creates a box enclosing UI elements
                            width = 12, solidHeader = TRUE,
                            title = "Introduction",
                            #HTML codes for text on the first tab
                            span(textOutput("desc"), style="font-size:20px"),
                            h4(p(div("Today's session will cover"))),
                            uiOutput("bullet_list"),
                            br(),
                            h4(p(div("Other useful resources"))),
                            tags$ul( # Unordered list for bullet points
                              tags$li(tags$a(href = "https://shiny.posit.co/", "Shiny Official Website")),
                              tags$li(tags$a(href = "https://shiny.posit.co/r/gallery/widgets/widget-gallery/", "Shiny widget gallery")),
                              tags$li(tags$a(href = "https://rstudio.github.io/shinydashboard/", "Shiny Dashboard")),
                              tags$li(tags$a(href = "https://rstudio.github.io/shinythemes/","Shiny Themes")),
                            ),
                            br()
                          )),
                 #Second tab
                          tabPanel("Bulk RNA-Seq data",
                                   box(width = 12, status = "primary",solidHeader = TRUE,title = "Controls",
                                       fluidRow(
                                       column(6,radioButtons("filetype2", label = h4("Select file input type"),inline=F,choices = list( "Select from list" = 'list',"Upload RData" = 'upload'),selected = 'list')),
                                       column(6,conditionalPanel(
                                         condition = "input.filetype2 == 'list'",
                                         uiOutput("projectlist2")
                                       ),
                                       conditionalPanel(
                                         condition = "input.filetype2 == 'upload'",
                                         fileInput('rdatafileupload2', 'Upload File')
                                       ))),
                                       fluidRow(
                                       column(6,uiOutput("contrastlist")),
                                       column(6,sliderInput("volcslider", label = h4("Select top number of genes"), min = 0,max = 25, value = 5))),
                                       fluidRow(
                                       column(6,downloadButton('dwld','Download results table')),
                                       column(6,downloadButton('downloaddotplot', 'Download Dot plot'))
                                   )),
                                   box(width = 12, status = "primary",solidHeader = TRUE,title = "Data",
                                       fluidRow(
                                        column(6,plotOutput('dotplot')),
                                        column(6,plotlyOutput("volcanoplot",height=500))
                                        ),
                                       DT::dataTableOutput('table')
                                   )
                           ),
                 #third tab
                 tabPanel("single cell RNA-Seq data",
                          box(width = 4, status = "primary",solidHeader = TRUE,title = "Controls",
                              radioButtons("filetype", label = h4("Select file input type"),inline=F,choices = list( "Select from list" = 'list',"Upload RData" = 'upload'),selected = 'list'),
                              conditionalPanel(
                                condition = "input.filetype == 'list'",
                                uiOutput("projectlist")
                              ),
                              conditionalPanel(
                                condition = "input.filetype == 'upload'",
                                fileInput('rdatafileupload', 'Upload RDS File')
                              ),
                              uiOutput("groupby"),
                              uiOutput("gene1aui"),
                              sliderInput("point", "Violin Point Size:",min = 0, max = 5, value = 1,step=.25),
                              checkboxInput("checklabel1", label = "Check for cell  group labelling", value = TRUE)
                              
                          ),
                          box(width = 8, status = "primary",solidHeader = TRUE,title = "Plots",
                                  plotOutput("comptsne2", height = 800),
                                  downloadButton('downloadtsneplot', 'Download Dimension Reduction plot')
                              
                          )
                 )
                           )
              )
                
                           