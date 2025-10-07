library(shiny)
library(shinyBS)
library(RColorBrewer)
library(ggplot2)
library(png)
library(dplyr)
library(tidyr)
library(plotly)
library(shinyjs)
library(htmlwidgets)
library(DT)
library(readxl)
library(data.table)
library(tibble)
library(cowplot)
#Specify user-ids and passwords
auth=read.csv("data/authentication.csv")
my_username <- auth$user
my_password <- auth$pwd

server <- function(input, output, session) {
  
  #### Authentication ####
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed'
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal.
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
      }
    }
  })
  
  #### Page 1 dashboard ###
  output$desc <- renderText({
    text="Hi ! Thank you for attending today's hands-on Shiny tutorial"
    return(text)
  })
  
  output$bullet_list <- renderUI({
    HTML("<ul>
           <li>Creating a static shiny page</li>
           <li>Adding texts using HTML </li>
           <li>Creating tabs</li>
           <li>Reading input files</li>
           <li>Shiny widgets (drop-down menu's, radio button, checkboxes)</li>
           <li>Adding authentication to app</li>
           <li>Displaying plots</li>
           <li>Downloading data</li>
         </ul>")
  })
  
  #### Read data ####
  #### Tab 1 for RNA data plot ####
  #Generate project list
  output$projectlist2 = renderUI({
    excel= read.csv("data/param2.csv")
    prj=as.character(excel$projects)
    selectInput("projects2","Select a project",as.list(sort(as.character(prj))))
  })
  
  #Load file
  fileload_bulk <- reactive({
    if(input$filetype2 == 'list'){
      inFile = paste('data/',as.character(input$projects2),'.rds',sep = '')
      results=readRDS(inFile)
    }else{
      file=input$rdatafileupload
      results=readRDS(file$datapath)
    }
    return(results)
  })
   
  #Generate the contrast list
    output$contrastlist = renderUI({
      results=fileload_bulk()
      contrasts=names(results$DEG)
      selectInput("contrast","Select a comparison",as.list(sort(as.character(contrasts))))
    })
  
    #Based on selected contrast, return the right DEG table
  datasetInput= reactive({
    results=fileload_bulk()
    k=paste('results$DEG$',input$contrast,sep='')
    degdata=eval(parse(text = k))
    return(degdata)
  })
  
  #print DEG results in data table
  output$table = DT::renderDataTable({
    input$projects2
    input$contrast
    DT::datatable(datasetInput(),
                  extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = list()),
                  rownames=FALSE,selection = list(mode = 'single', selected =1),escape=FALSE)
  })
  
  #Create function to select gene from table and make dotplot of that gene
  dotplot_out = reactive({
    s = input$table_rows_selected #select rows from table
    dt = datasetInput() #load deg data
    dt1 = dt[s, , drop=FALSE]
    id = dt1$geneSymbol #Get selected gene
    results=fileload_bulk()
    tpm <- results$TPM #get TPM
    pData=results$metadata #get metadata
    signal=as.data.frame(t(tpm[tpm$geneSymbol == id,])) %>% rownames_to_column("USI") 
    signal <- signal[-c(1, 2), ]
    colnames(signal)=c("USI","gene")
    e=left_join(pData,signal,by="USI")
    e$gene=as.numeric(e$gene)
    gg=ggplot(e,aes(x=subtype,y=gene,col=subtype))+
        labs(title=id, x="Condition", y="Expression Value") + geom_point(size=5,position=position_jitter(w = 0.1))
        
    gg
  })
  
  # plot dotplot
  output$dotplot = renderPlot({
    dotplot_out()
  })
  
  #function to download dot plot
  output$downloaddotplot <- downloadHandler(
    filename = function() {
      paste0(input$projects, '_dotplot.jpg', sep='') 
    },
    content = function(file){
      jpeg(file, quality = 100, width = 800, height = 800)
      plot(dotplot_out())
      dev.off()
    })
  
  #Function to draw the volcano plot
  volcanoplot_out = reactive({
    diff_df=datasetInput()
    diff_df$group <- "NotSignificant"
    diff_df[which(diff_df['padj'] < 0.05 & diff_df['log2FoldChange'] < -2 ),"group"] <- "Downregulated genes"
    diff_df[which(diff_df['padj'] < 0.05 & diff_df['log2FoldChange'] > 2 ),"group"] <- "Upregulated genes"

      # Find and label the top peaks..
      n=input$volcslider
      if(n>0){
        top_genes <- diff_df %>% filter(log2FoldChange > 2 & padj < 0.05) %>% drop_na(any_of('SYMBOL'))%>%arrange(-log2FoldChange) %>%head(n)
        bottom_genes <- diff_df %>% filter(log2FoldChange < -2 & padj < 0.05) %>% drop_na(any_of('SYMBOL'))%>%arrange(log2FoldChange) %>%head(n)
        top_peaks <- rbind(top_genes, bottom_genes)
        
        a <- list()
        for (i in seq_len(nrow(top_peaks))) {
          m <- top_peaks[i, ]
          a[[i]] <- list(x = m[["log2FoldChange"]],y = -log10(m[["padj"]]),text = m[["geneSymbol"]],xref = "x",yref = "y",showarrow = FALSE,arrowhead = 0.5,ax = 20,ay = -40)
        }
        p <- plot_ly(data = diff_df, x = diff_df$log2FoldChange, y = -log10(diff_df$padj),text = diff_df$geneSymbol, 
                     mode = "markers", color = diff_df$group) %>% layout(title ="Volcano Plot",xaxis=list(title="Log Fold Change"),yaxis=list(title="-log10(FDR)")) %>%
          layout(annotations = a)
      }
      else{
        p <- plot_ly(data = diff_df, x = diff_df$logFC, y = -log10(diff_df$adj.P.Val),text = diff_df$SYMBOL, mode = "markers", color = diff_df$group) %>% layout(title ="Volcano Plot",xaxis=list(title="Log Fold Change"),yaxis=list(title="-log10(FDR)"))
      }
    
    p
  })
  
  output$volcanoplot = renderPlotly({
    input$radio
    input$lfc
    input$apval
    input$volcslider
    input$volcdrop
    volcanoplot_out()
  })
  

  #### Tab 2 for single cell data ####
  
  #Generate project list
  output$projectlist = renderUI({
    excel= read.csv("data/param.csv")
    prj=as.character(excel$projects)
    selectInput("projects","Select a project",as.list(sort(as.character(prj))))
  })
  
  #Load file
  fileload <- reactive({
    if(input$filetype == 'list'){
      inFile = paste('data/',as.character(input$projects),'.RDS',sep = '')
        scrna=readRDS(inFile)
    }else{
      file=input$rdatafileupload
      scrna=readRDS(file$datapath)
    }
    return(scrna)
  })
  
  #generate variable list for left plot
  output$groupby = renderUI({
    scrna=fileload()
    metadata=as.data.frame(scrna@meta.data) 
    var=colnames(metadata)
    selectInput("groupby","Select a variable to group by",var,"Select one")
  })
  
  #Generate genelist for Violin and featureplot
  output$gene1aui = renderUI({
    scrna=fileload()
    options=sort(rownames(GetAssayData(object = scrna)))
    withProgress(session = session, message = 'Generating gene list...',detail = 'Please Wait...',{
      selectInput('gene', label='Gene Name',options,multiple=FALSE, selectize=TRUE,selected=options[1])})
  })
  
  #generate plot
  comptsne2 = reactive({
    scrna=fileload()
    
    plot1=DimPlot(object = scrna,group.by = input$groupby,label = input$checklabel1)
    plot2=FeaturePlot(object = scrna, features = input$gene)
    plot3=VlnPlot(object = scrna,features = input$gene,pt.size = input$point)
    
    p=(plot1+plot2)/plot3
    ggdraw(p)
  })
  
  #render final plot
  output$comptsne2 = renderPlot({
    input$load
    withProgress(session = session, message = 'Generating...',detail = 'Please Wait...',{
      comptsne2()
    })
  })
  
  #Handler to download plot
  output$downloadtsneplot <- downloadHandler(
    filename = function() {
      paste0(input$projects,"_DRplot.pdf",sep="")
    },
    content = function(file){
      pdf(file,width=14,height = 8,useDingbats=FALSE)
      plot(comptsne2())
      dev.off()
    })
}#end of server
