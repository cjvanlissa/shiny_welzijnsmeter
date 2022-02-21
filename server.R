library(shiny)
library(shinyjs)
if (FALSE)
  require("V8")
library(ggplot2)
library(DT)
library(OpenMx)
library(tidySEM)
plot_biv <- readRDS("shiny_plot.RData")
mixmod <- readRDS("shiny_mixmod.RData")
newdat <- structure(list(belasting = c(3.2, 3, 3.5, 4.5),
                         vrijheid = c(2, 1.5, 1.3, 3),
                         gedachten = c(2.5, 2.3, 2.5, 4.2),
                         eenzaamheid = c(2.7, 3.2, 2.8, 3)),
                    row.names = 1:4, class = "data.frame")
#mixmod <- mxModel(mixmod, mxData(newdat, type = "raw"))

shinyServer(function(input, output, session) {
  js$disableTab("Resultaat")
  js$disableTab("Explore")
  js$disableTab("Exploratory")
  shinyjs::disable("analyze")
  reactives <- reactiveValues()
  reactives$mplusmodellist <- list()
  
  output$file_input = renderUI({
    fileInput(
      "file",
      NULL,
      accept = c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        '.csv',
        '.tsv'
      )#,
      #width = "300px"
    )
  })
  
  output$mplus_file_input = renderUI({
    fileInput(
      "mplus_file",
      NULL,
      accept = c(
        'text/plain')#,
      #width = "300px"
    )
  })
  observeEvent(input$submit, {
    survdat <- getSurveyData()
    survdat[[1]] <- NULL
    survdat$response <- c("Helemaal niet" = 1,
                          "Niet" = 2,
                          "Gemiddeld" = 3,
                          "Wel" = 4,
                          "Heel erg" = 5)[survdat$response]
    reactives[["response_data"]] <- survdat
    # enable Resultaat when clicking the button
    js$enableTab("Resultaat")
    # switch to Resultaat
    updateTabsetPanel(session, "navbar", "Resultaat")
    js$disableTab("Upload")
  })

  observeEvent(input$load, {
    if (is.data.frame(input$file)) {
      data <- read.csv(input$file$datapath)
      reactives[["response_data"]] <- data
      # enable Resultaat when clicking the button
      js$enableTab("Resultaat")
      # switch to Resultaat
      updateTabsetPanel(session, "navbar", "Resultaat")
      js$disableTab("Upload")
    }
  })
  
  
  output$welzijnsmeter.csv <- downloadHandler(
    filename = "welzijnsmeter.csv",
    content = function(file) {
      write.csv(structure(list(Label = structure(c(1L, 3L, 2L, 4L, 1L, 3L, 2L, 
                                                   4L, 1L, 3L, 2L, 4L), .Label = c("a2.ON.a1", "a2.ON.b1", "b2.ON.a1", 
                                                                                   "b2.ON.b1"), class = "factor"), Estimate = c(0.3, 0.2, 0.4, 0.642, 
                                                                                                                                0.5, 0.3, 0.3, 0.6, 0.67, 0.43, 0.32, 0.53), SE = c(0.004, 0.003, 
                                                                                                                                                                                    0.01, 0.04, 0.042, 0.04115, 0.074, 0.003, 0.012, 0.03, 0.002, 
                                                                                                                                                                                    0.005), Sample = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 
                                                                                                                                                                                                       3L)), class = "data.frame", row.names = c(NA, -12L)), 
                file, row.names = FALSE)
    },
    contentType = "text/csv"
  )


  output$diagram_header <- renderUI({
    #h5(strong(ifelse(is.null(reactives[["table_estimates"]]), "", "Table of meta-analytic estimates: ")))
    h5(strong("Class plot"))
  })
  output$report <- renderUI({
    
    
    #cp <- class_prob(mixmod, type = "individual")$individual
    #indiv_classprob <- cp[5, , drop = TRUE]
    #reactives <- list(indiv_classprob = indiv_classprob)
    cpind <- reactives$indiv_classprob
    cpind <- sort(cpind, decreasing = TRUE)
    classtext <- paste0("<ul", 
                        paste0("<li>", names(cpind), ": ", round(100*cpind), "%</li>"),
                        "</ul>", collapse = "")
    HTML(paste0(
      "Volgens uw antwoorden op de Welzijnsmeter past u het beste bij de volgende typen naasten:",
      classtext,
      collapse = ""
      ))
  })
  
  output$classplot <- renderImage({
    outfile <- tempfile(fileext = '.png')
    #event.data <-
    #  event_data("plotly_click", source = "path_selected")
    # if(is.null(input$meta_table_rows_selected)) {
    #   png(outfile, width = 600, height = 450)
    #   plot(
    #     5,
    #     5,
    #     type = "n",
    #     axes = FALSE,
    #     ann = FALSE,
    #     xlim = c(0, 10),
    #     ylim = c(0, 10)
    #   )
    #   dev.off()
    # } else {
      newdat <- rbind(mixmod$data$observed,
                      reactives$response_data$response)
      newmod <- mxModel(mixmod, mxData(newdat, type = "raw"))
      out <- mxRun(newmod)
      cp <- class_prob(out, type = "individual")$individual
      indiv_classprob <- cp[5, , drop = TRUE]
      reactives$indiv_classprob <- indiv_classprob

      colr <- c("red", "blue", "green", "purple")[which.max(indiv_classprob)]
      newdat <- newdat[5, , drop = FALSE]
      plot_biv[[1]] <- plot_biv[[1]]+geom_vline(xintercept = newdat[[plot_biv[[1]]$labels$y]], colour = colr, size = 2)
      plot_biv[[2]] <- plot_biv[[2]] + geom_point(aes(x = newdat[[plot_biv[[2]]$labels$x]], y = newdat[[plot_biv[[2]]$labels$y]]), colour = colr, inherit.aes = FALSE, size = 2)
      plot_biv[[3]] <- plot_biv[[3]] + geom_point(aes(x = newdat[[plot_biv[[3]]$labels$x]], y = newdat[[plot_biv[[3]]$labels$y]]), colour = colr, inherit.aes = FALSE, size = 2)
      plot_biv[[4]] <- plot_biv[[4]] + geom_point(aes(x = newdat[[plot_biv[[4]]$labels$x]], y = newdat[[plot_biv[[4]]$labels$y]]), colour = colr, inherit.aes = FALSE, size = 2)
      plot_biv[[6]] <- plot_biv[[6]]+geom_vline(xintercept = newdat[[plot_biv[[6]]$labels$y]], colour = colr, size = 2)
      plot_biv[[7]] <- plot_biv[[7]] + geom_point(aes(x = newdat[[plot_biv[[7]]$labels$x]], y = newdat[[plot_biv[[7]]$labels$y]]), colour = colr, inherit.aes = FALSE, size = 2)
      plot_biv[[8]] <- plot_biv[[8]] + geom_point(aes(x = newdat[[plot_biv[[8]]$labels$x]], y = newdat[[plot_biv[[8]]$labels$y]]), colour = colr, inherit.aes = FALSE, size = 2)
      plot_biv[[11]] <- plot_biv[[11]]+geom_vline(xintercept = newdat[[plot_biv[[11]]$labels$y]], colour = colr, size = 2)
      plot_biv[[12]] <- plot_biv[[12]] + geom_point(aes(x = newdat[[plot_biv[[12]]$labels$x]], y = newdat[[plot_biv[[12]]$labels$y]]), colour = colr, inherit.aes = FALSE, size = 2)
      plot_biv[[16]] <- plot_biv[[16]]+geom_vline(xintercept = newdat[[plot_biv[[16]]$labels$y]], colour = colr, size = 2)
      
      png(outfile, width = 600, height = 450)
      tidySEM:::merge_corplots(plot_biv)
      dev.off()
    #}
    
    list(
      src = outfile,
      width = 600,
      height = 450
    )
  }, deleteFile = TRUE)
  
  
})
