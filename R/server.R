#' @import rmarkdown


server <- function(input, output, session){

  K <- reactive({
    input$NoVars
  })

  LID <- reactive({
    input$LID
  })

  M = reactive({
    inFile <- input$file1
    M = tagList()

    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    D = readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    D = as.data.frame(D)

    ## Upating file names
    colnames(D) <- labs <- gsub("\r\n"," ", colnames(D))
    nums = labs[which(sapply(D, is.numeric) == TRUE)]

    M[[1]] = D
    M[[2]] = nums

    M
  })

  j <- reactive({
    ceiling(dim(M()[[1]])[1]/2)
  })

  output$VarsInput <- renderUI({
    C = sapply(1:K(), function(i){paste0("cols",i)})
    W = sapply(1:K(), function(i){paste0("weight",i)})
    S = sapply(1:K(), function(i){paste0("slider",i)})

    output = tagList()

    for(i in seq_along(1:K())){
      output[[i]] = tagList()
      output[[i]][[1]] = br()
      output[[i]][[2]] = hr(style="height:5px;background-color:blue")
      output[[i]][[3]] = helpText("Input information for a variable below:")
      output[[i]][[4]] = selectInput(C[i], "Variable to randomize:",
                                     M()[[2]], selected = M()[[2]][i])
      output[[i]][[6]] = textInput(W[i], "Weight for variable:",
                                   value = "1")
      output[[i]][[7]] = textInput(S[i], "Max for variable",
                                   value = "5")
    } ## for loop

    output
  })

  output$NVs <- renderUI({
    numericInput("NoVars","No. of matching variables",
                 value = 3, min = 2, max = length(M()[[2]]))
  })

  output$RanInput <- renderUI({
    if(Dat()[[6]] == TRUE){
      G = M()[[1]][Dat()[[4]][,1], LID()]
    }else{
      G = c(M()[[1]][Dat()[[5]], LID()], M()[[1]][Dat()[[4]][,1], LID()])
    }

    output = tagList()

    for(i in seq_along(1:j())){
      output[[i]] = tagList()
      output[[i]][[1]] = hr(style="height:5px;background-color:blue")
      output[[i]][[2]] = G[i]
      output[[i]][[3]] = br()

    } ## for loop

    output
  })

  output$ID <- renderUI({
    selectInput("LID", "Label:",
                colnames(M()[[1]]), selected = 1)
  })

  output$IS <- renderUI({
    textInput("S","Seed for this trial",
              value = "Insert seed here")
  })

  A <- reactive({
    random::randomNumbers(1, col = 1, min = 1, max = 1000000000)
  })

  output$DS <- renderPrint({
    ifelse(!is.numeric(input$S), paste0("The seed is ", A()),
           paste0("The seed is ", input$S))
  })

  Rs <- eventReactive(input$rand, {
    ## Finding the seed
    N = ifelse(!is.numeric(input$S), A(), input$S)
    set.seed(N)

    ## Collecting the data
    D = Dat()[[4]]
    npairs = nrow(D)

    ## KEN YOU WANTED rbinom EARLIER!!!
    rand = runif(npairs)
    Group = ifelse(rand > 0.5,  input$Arm1,  input$Arm2)
    Trt2 = ifelse(Group == input$Arm1, input$Arm2, input$Arm1)

    if(Dat()[[6]] == TRUE){
      ## Even number of units to randomize
      rbind(cbind(M()[[1]][D[,1], 1], Group), cbind(M()[[1]][D[,2], 1], Trt2))
    }else{
      ## Odd number of units to randomize

      ## KEN RANDOMIZES TO WHATEVER THEY CALL THE FIRST ARM, IS THIS OKAY???
      rbind(c(M()[[1]][Dat()[[5]], 1], input$Arm1),
            (cbind(M()[[1]][D[,1], 1], Group)), cbind(M()[[1]][D[,2], 1], Trt2))
    }
  })

  output$Rands <- renderDataTable(Rs())

  Dat <- eventReactive(input$go, {
    C = sapply(1:K(), function(i){input[[paste0("cols",i)]]})
    W = sapply(1:K(), function(i){input[[paste0("weight",i)]]})
    S = sapply(1:K(), function(i){input[[paste0("slider",i)]]})

    V = list()
    for(i in 1:K()){
      V[[i]] = list()
      V[[i]][[1]] = C[i]
      V[[i]][[2]] = as.numeric(W[i])
      V[[i]][[3]] = C[i]
      V[[i]][[4]] = 0
      V[[i]][[5]] = as.numeric(S[i])
    }

    make.Ks(M = input$Times, D = M()[[1]], vars = V,
            ToC = as.numeric(isolate({input$ToC})), S = "glpk")
  }) ## eventReactive

  observeEvent(input$Tab2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })

  even_or_odd <- eventReactive(input$go, {
    ifelse(dim(M()[[1]])[1]%%2 == 1, FALSE, TRUE)
  })

  observeEvent(input$Tab3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })

  ## The first column is selected upon initiation for
  ## color of graph.
  C <- reactiveVal(1)

  ## Updating to the column of choice.
  observeEvent(input$plot_dblclick$x, {

    ## Converting the double click output to the scale of No. of Variables
    ## Unless the double click hasn't happened yet
    newC = ifelse(is.null(input$plot_dblclick$x), 1,
                  Round1(DC = input$plot_dblclick$x, K = K(), Min = input$brush$xmin,
                         Max = input$brush$xmax))
    ## Exporting to local environment
    C(newC)
  })

  ## Making the plot by ordering it and then drawing the plot.
  output$plot <- renderPlot({
    ## Ordering it so the color comes out right
    E = make.order(column = C(), data = Dat()[[3]])

    ## Plotting
    make.plot(data = E, I = Dat()[[2]])
  })

  output$zoom <- renderPlot({
    ## If there's no brush done don't return a thing
    if (is.null(input$brush)) return()

    ## If there is color it right
    E = make.order(column = C(), data = Dat()[[3]])

    ## Now zoom it baby!
    make.zoom.plot(data = E, I = Dat()[[2]], Min = input$brush$xmin,
                   Max = input$brush$xmax)
  })

  ## Summary of data
  output$summary <- renderPrint({
    C <- sapply(1:K(), function(i) {input[[paste0("cols",i)]]})
    summary(M()[[1]][, C])
  })

  output$NM <- renderPrint({
    if(Dat()[[6]] == TRUE){
      paste0("There were ", floor(dim(M()[[1]])[1]/2),
             " matched pairs")
    }else{
      paste0("There were ", floor(dim(M()[[1]])[1]/2),
             " matched pairs and one unit leftover.")
    }
  })

  output$MA <- renderPrint({
    Dat()[[4]]
  })

  output$MAL <- renderPrint({
    if(Dat()[[6]] == TRUE){
      cbind(M()[[1]][Dat()[[4]][,1], LID()],
            M()[[1]][Dat()[[4]][,2], LID()])
    }else{
      rbind(c(M()[[1]][Dat()[[5]], LID()], "NA"),
            cbind(M()[[1]][Dat()[[4]][,1], LID()],
                  M()[[1]][Dat()[[4]][,2], LID()]))
    }
  })

  observeEvent(input$bookmark1, {
    session$doBookmark()
  })

  output$downloadReport1 <-  downloadHandler(
    ## Making the filename for the report here. Using two different
    ## extensions for the file
    filename = function() {
      paste(input$FN1, sep = '.', switch(input$format, PDF = 'pdf',
                                         Word = 'docx'))
    },

    content = function(file) {
      ## Copy the report file to a temporary directory before processing it, in
      ## case we don't have write permissions to the current working dir (which
      ## can happen when deployed).
      tempReport1 <- file.path(tempdir(), "report1.Rmd")
      file.copy(system.file("reports/report1.Rmd", package = "GoldilocksPackage"),
                tempReport1, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(I1 = Dat()[[1]], # name
                     I2 = Dat()[[2]], # Labels and maxs for plot
                     I3 = Dat()[[3]], # Ks things to plot
                     j = even_or_odd(), # even or odd no. of units
                     M = M()[[1]], # Orig data
                     Dt = Dat()[[7]], # Data for each
                     Rs = Rs()) # Randomization


      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport1,
                        switch(input$format,
                               PDF = pdf_document(), Word = word_document()),
                        output_file = file, params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$downloadReport <-  downloadHandler(
    ## Making the filename for the report here. Using two different
    ## extensions for the file
    filename = function() {
      paste(input$FN, sep = '.', switch(input$format, PDF = 'pdf',
                                        Word = 'docx'))
    },

    content = function(file) {
      ## Copy the report file to a temporary directory before processing it, in
      ## case we don't have write permissions to the current working dir (which
      ## can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(system.file("reports/report.Rmd",
                            package = "GoldilocksPackage"),
                tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(I1 = Dat()[[1]],
                     I2 = Dat()[[2]],
                     I3 = Dat()[[3]],
                     N = input$YN)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        switch(input$format,
                               PDF = pdf_document(), Word = word_document()),
                        output_file = file, params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}
