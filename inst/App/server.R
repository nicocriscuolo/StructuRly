################################# SERVER ######################################
server <- function(input, output, session) {



### Commands for conditional Panel - Display Action Button of Start
output$fileUploaded_PER <- reactive({

    return(!is.null(Data_PER_Str()))

})
outputOptions(output, name = "fileUploaded_PER", suspendWhenHidden = FALSE)



output$fileUploaded_DA <- reactive({

    return(!is.null(Data_DA_Str()))

})
outputOptions(output, name = "fileUploaded_DA", suspendWhenHidden = FALSE)



### Commands shinyjs to display and hide panel with actionButtons

# Show and hide instructions and results buttons
observeEvent(input$back_instructions, {
  shinyjs::showElement(id = "back_results")
  shinyjs::hideElement(id = "back_instructions")
})

observeEvent(input$back_results, {
  shinyjs::showElement(id = "back_instructions")
  shinyjs::hideElement(id = "back_results")
})



# Show and hide instructions and results panels
observeEvent(input$back_instructions, {
  shinyjs::showElement(id = "instructions")
  shinyjs::hideElement(id = "wallpaper")
  shinyjs::hideElement(id = "table_panel")
  shinyjs::hideElement(id = "plot_panel")
})

observeEvent(input$back_results, {
  shinyjs::hideElement(id = "instructions")
  shinyjs::showElement(id = "wallpaper")
  shinyjs::showElement(id = "table_panel")
  shinyjs::showElement(id = "plot_panel")
})



# Show and hide check_table and customize_plot buttons
observeEvent(input$check_table, {
  shinyjs::hideElement(id = "check_table")
})

observeEvent(input$customize_plot, {
  shinyjs::hideElement(id = "customize_plot")
})



# Show and hide comparison_plot and comparison_table buttons
observeEvent(input$show_comparison_plot, {
  shinyjs::showElement(id = "show_comparison_table")
  shinyjs::hideElement(id = "show_comparison_plot")
})

observeEvent(input$show_comparison_table, {
  shinyjs::showElement(id = "show_comparison_plot")
  shinyjs::hideElement(id = "show_comparison_table")
})



# Show and hide comparison plot and comparison table panel
observeEvent(input$show_comparison_plot, {
  shinyjs::showElement(id = "panel_comparison_plot")
  shinyjs::hideElement(id = "panel_comparison_table")
})

observeEvent(input$show_comparison_table, {
  shinyjs::showElement(id = "panel_comparison_table")
  shinyjs::hideElement(id = "panel_comparison_plot")
})



### Open STRUCTURE
observeEvent(input$open_structure, {
  system("open -a structure")
})



############################### TABLE SECTION ####

### Reactive dataset FOR STRUCTURE input
Data_PER_Str <- reactive({

  req(input$Data_PER_Str)

  File <- input$Data_PER_Str

  df <- read.csv(File$datapath,
                 header = TRUE,
                 sep = input$separator,
                 quote = input$quote)

  updateSelectInput(session,
                    inputId = "subset_variable",
                    choices = names(df),
                    selected = NULL)

  return(df)

})



### Output table imported
output$table_import <- renderDataTable({

  Data_PER_Str()

}, options = list(pageLength = 10))



### Download subset
output$download_subset <- downloadHandler(

  filename = function() {
    paste0("Subset_", input$Data_PER_Str)
  },

  content = function(file) {
    write.table(subset(Data_PER_Str(),
                       select = c(input$subset_variable)),
                file,
                row.names = FALSE,
                quote = FALSE,
                sep = "\t")
  }
)



### Reactive dataset FOR STRUCTURE to export
Data_export <- reactive({

  Dataset <- Data_PER_Str()

  Dataset[is.na(Dataset)] <- -9 # To allow STRUCTURE read "NA" values

  if (all(c("Sample_ID", "Pop_ID", "Loc_ID") %in% colnames(Data_PER_Str()))) {

    # Definito l'ordine della colonna Sample_ID uguale a quella che si trova nel dataset originale importato dall'utente
    Dataset$Sample_ID <- factor(Dataset$Sample_ID, levels = Dataset$Sample_ID)

    COLNAMES <- c(colnames(Dataset[-c(1, 2, 3)]))

    Dataset_reshape <- reshape(Dataset, # must takes all variables of dataset
                               direction = "long",
                               varying = COLNAMES,
                               timevar = NULL,
                               # drop = c("UDO6.1", "UDO6.2", "UDO6.3"), # con drop specificare quali variabili lasciare indietro
                               # times = c("UDO36"),
                               idvar = "Sample_ID"
                               # new.row.names = seq(from = 1, to = nrow(Dataset)*length(COLNAMES)/input$ploidy, by = 1) # 2 è il numero di loci, il tutto è la ploidia
                       )

    Dataset_reshape$Sample_ID <- factor(Dataset_reshape$Sample_ID,
                                        levels = Dataset$Sample_ID)

    Dataset_reshape <- Dataset_reshape[order(Dataset_reshape$Sample_ID), ]

    if (input$remove_ID == TRUE &&
        input$remove_POP == FALSE &&
        input$remove_LOC == FALSE) {

      Dataset_reshape$Sample_ID <- NULL

    } else if (input$remove_ID == FALSE &&
               input$remove_POP == TRUE &&
               input$remove_LOC == FALSE) {

      Dataset_reshape$Pop_ID <- NULL

    } else if (input$remove_ID == FALSE &&
               input$remove_POP == FALSE &&
               input$remove_LOC == TRUE) {

      Dataset_reshape$Loc_ID <- NULL

    } else if (input$remove_ID == TRUE &&
               input$remove_POP == TRUE &&
               input$remove_LOC == FALSE) {

      Dataset_reshape$Sample_ID <- NULL

      Dataset_reshape$Pop_ID <- NULL

    } else if (input$remove_ID == TRUE &&
               input$remove_POP == FALSE &&
               input$remove_LOC == TRUE) {

      Dataset_reshape$Sample_ID <- NULL

      Dataset_reshape$Loc_ID <- NULL

    } else if (input$remove_ID == FALSE &&
               input$remove_POP == TRUE &&
               input$remove_LOC == TRUE) {

      Dataset_reshape$Pop_ID <- NULL

      Dataset_reshape$Loc_ID <- NULL

    } else if (input$remove_ID == TRUE &&
               input$remove_POP == TRUE &&
               input$remove_LOC == TRUE) {

      Dataset_reshape$Sample_ID <- NULL

      Dataset_reshape$Pop_ID <- NULL

      Dataset_reshape$Loc_ID <- NULL

    }

  } else if (("Sample_ID" %in% colnames(Data_PER_Str())) &&
             (!"Pop_ID" %in% colnames(Data_PER_Str())) &&
             (!"Loc_ID" %in% colnames(Data_PER_Str()))) {

    Dataset$Sample_ID <- factor(Dataset$Sample_ID,
                                levels = Dataset$Sample_ID)

    COLNAMES <- c(colnames(Dataset[-1]))

    Dataset_reshape <- reshape(Dataset,
                               direction = "long",
                               varying = COLNAMES,
                               timevar = NULL,
                               idvar = "Sample_ID"
    )

    Dataset_reshape$Sample_ID <- factor(Dataset_reshape$Sample_ID,
                                        levels = Dataset$Sample_ID)

    Dataset_reshape <- Dataset_reshape[order(Dataset_reshape$Sample_ID), ]

    if (input$remove_ID == TRUE) {

      Dataset_reshape$Sample_ID <- NULL

    }

  } else if ((!"Sample_ID" %in% colnames(Data_PER_Str())) &&
             ("Pop_ID" %in% colnames(Data_PER_Str())) &&
             (!"Loc_ID" %in% colnames(Data_PER_Str()))) {

    COLNAMES <- c(colnames(Dataset[-1]))

    ID <- factor(seq(from = 1, to = nrow(Dataset), by = 1))

    Dataset_reshape <- reshape(Dataset,
                               direction = "long",
                               varying = COLNAMES,
                               timevar = NULL
    )

    Dataset_reshape$id <- factor(Dataset_reshape$id, levels = ID)

    Dataset_reshape <- Dataset_reshape[order(Dataset_reshape$id), ]

    Dataset_reshape$id <- NULL

    if (input$remove_POP == TRUE) {

      Dataset_reshape$Pop_ID <- NULL

    }

  } else if ((!"Sample_ID" %in% colnames(Data_PER_Str())) &&
             (!"Pop_ID" %in% colnames(Data_PER_Str())) &&
             ("Loc_ID" %in% colnames(Data_PER_Str()))) {

    COLNAMES <- c(colnames(Dataset[-1]))

    ID <- factor(seq(from = 1, to = nrow(Dataset), by = 1))

    Dataset_reshape <- reshape(Dataset,
                               direction = "long",
                               varying = COLNAMES,
                               timevar = NULL
    )

    Dataset_reshape$id <- factor(Dataset_reshape$id, levels = ID)

    Dataset_reshape <- Dataset_reshape[order(Dataset_reshape$id), ]

    Dataset_reshape$id <- NULL

    if (input$remove_LOC == TRUE) {

      Dataset_reshape$Loc_ID <- NULL

    }

  } else if (("Sample_ID" %in% colnames(Data_PER_Str())) &&
             ("Pop_ID" %in% colnames(Data_PER_Str())) &&
             (!"Loc_ID" %in% colnames(Data_PER_Str()))) {

    Dataset$Sample_ID <- factor(Dataset$Sample_ID, levels = Dataset$Sample_ID)

    COLNAMES <- c(colnames(Dataset[-c(1, 2)]))

    Dataset_reshape <- reshape(Dataset,
                               direction = "long",
                               varying = COLNAMES,
                               timevar = NULL,
                               idvar = "Sample_ID"
    )

    Dataset_reshape$Sample_ID <- factor(Dataset_reshape$Sample_ID,
                                        levels = Dataset$Sample_ID)

    Dataset_reshape <- Dataset_reshape[order(Dataset_reshape$Sample_ID), ]

    if (input$remove_ID == TRUE &&
        input$remove_POP == TRUE) {

      Dataset_reshape$Sample_ID <- NULL

      Dataset_reshape$Pop_ID <- NULL

    } else if (input$remove_ID ==  TRUE &&
               input$remove_POP == FALSE) {

      Dataset_reshape$Sample_ID <- NULL

    } else if (input$remove_ID ==  FALSE &&
               input$remove_POP == TRUE) {

      Dataset_reshape$Pop_ID <- NULL

    }

  } else if (("Sample_ID" %in% colnames(Data_PER_Str())) &&
             (!"Pop_ID" %in% colnames(Data_PER_Str())) &&
             ("Loc_ID" %in% colnames(Data_PER_Str()))) {

    Dataset$Sample_ID <- factor(Dataset$Sample_ID, levels = Dataset$Sample_ID)

    COLNAMES <- c(colnames(Dataset[-c(1, 2)]))

    Dataset_reshape <- reshape(Dataset,
                               direction = "long",
                               varying = COLNAMES,
                               timevar = NULL,
                               idvar = "Sample_ID"
    )

    Dataset_reshape$Sample_ID <- factor(Dataset_reshape$Sample_ID,
                                        levels = Dataset$Sample_ID)

    Dataset_reshape <- Dataset_reshape[order(Dataset_reshape$Sample_ID), ]

    if (input$remove_ID == TRUE &&
        input$remove_LOC == TRUE) {

      Dataset_reshape$Sample_ID <- NULL

      Dataset_reshape$Loc_ID <- NULL

    } else if (input$remove_ID ==  TRUE &&
               input$remove_LOC == FALSE) {

      Dataset_reshape$Sample_ID <- NULL

    } else if (input$remove_ID ==  FALSE &&
               input$remove_LOC == TRUE) {

      Dataset_reshape$Loc_ID <- NULL

    }

  } else if ((!"Sample_ID" %in% colnames(Data_PER_Str())) &&
             ("Pop_ID" %in% colnames(Data_PER_Str())) &&
             ("Loc_ID" %in% colnames(Data_PER_Str()))) {

    COLNAMES <- c(colnames(Dataset[-c(1, 2)]))

    ID <- factor(seq(from = 1, to = nrow(Dataset), by = 1))

    Dataset_reshape <- reshape(Dataset,
                               direction = "long",
                               varying = COLNAMES,
                               timevar = NULL
    )

    Dataset_reshape$id <- factor(Dataset_reshape$id, levels = ID)

    Dataset_reshape <- Dataset_reshape[order(Dataset_reshape$id), ]

    Dataset_reshape$id <- NULL

    if (input$remove_POP == TRUE &&
        input$remove_LOC == TRUE) {

      Dataset_reshape$Pop_ID <- NULL

      Dataset_reshape$Loc_ID <- NULL

    } else if (input$remove_POP ==  TRUE &&
               input$remove_LOC == FALSE) {

      Dataset_reshape$Pop_ID <- NULL

    } else if (input$remove_POP ==  FALSE &&
               input$remove_LOC == TRUE) {

      Dataset_reshape$Loc_ID <- NULL

    }

  } else if (all(!c("Sample_ID", "Pop_ID", "Loc_ID") %in%
                 colnames(Data_PER_Str()))) {

      COLNAMES <- c(colnames(Dataset))

      ID <- factor(seq(from = 1, to = nrow(Dataset), by = 1))

      Dataset_reshape <- reshape(Dataset, # must take all variables of dataset
                                 direction = "long",
                                 varying = COLNAMES,
                                 timevar = NULL
      )

      Dataset_reshape$id <- factor(Dataset_reshape$id, levels = ID)

      Dataset_reshape <- Dataset_reshape[order(Dataset_reshape$id), ]

      Dataset_reshape$id <- NULL

  }

  return(Dataset_reshape)

})



### Output table exported
output$table_export <- renderDataTable({

  Data_export()

}, options = list(pageLength = 10))



### Individuals ID
output$individuals_ID <- renderText({

  if ("Sample_ID" %in% colnames(Data_PER_Str())) {

    print("YES")

  } else {

    print("NO")

  }

})



### Populations ID
output$populations_ID <- renderText({

  if ("Pop_ID" %in% colnames(Data_PER_Str())) {

    print("YES")

  } else {

    print("NO")

  }

})



### Location ID
output$locations_ID <- renderText({

  if ("Loc_ID" %in% colnames(Data_PER_Str())) {

    print("YES")

  } else {

    print("NO")

  }

})



### Number of individuals
output$individuals_number <- renderText({

  nrow(Data_PER_Str())

})



### Number of loci
loci <- reactive({

  req(c(Data_export(), Data_PER_Str()))

  x <- c(colnames(Data_export())[seq(from = 1,
                                     to = ncol(Data_export()),
                                     by = 1)])

  y <- c("Sample_ID", "Pop_ID", "Loc_ID")

  loci <- length(x) - length(intersect(x, y))

  return(loci)

})



# loci output
output$loci_number <- renderText({

  loci()

})


### Ploidy
ploidy <- reactive({

  req(c(Data_export(), Data_PER_Str()))

  x <- c(colnames(Data_export())[seq(from = 1,
                                     to = ncol(Data_export()),
                                     by = 1)])

  y <- c("Sample_ID", "Pop_ID", "Loc_ID")

  z <- c(colnames(Data_PER_Str())[seq(from = 1,
                                      to = ncol(Data_PER_Str()),
                                      by = 1)])

  ploidy <- (length(z) - length(intersect(z, y)))/
    (length(x) - length(intersect(x, y)))

  return(ploidy)

})



# ploidy output
output$ploidy <- renderText({

  ploidy()

})



### Download table for STRUCTURE
output$download <- downloadHandler(

  filename = function() {

    paste0(substr(input$Data_PER_Str, start = 1,
                  stop = nchar(input$Data_PER_Str)-4),
           "_FOR_STRUCTURE")

  },

  content = function(file) {

    if (all(c("Sample_ID", "Pop_ID", "Loc_ID") %in% colnames(Data_export()))) {

    write.table(Data_export(),
                file,
                row.names = FALSE,
                quote = FALSE,
                sep = " ",
                col.names = c("", "", "", colnames(Data_export()[-c(1, 2, 3)]))
                )

    } else if (("Sample_ID" %in% colnames(Data_export())) &&
               (!"Pop_ID" %in% colnames(Data_export())) &&
               (!"Loc_ID" %in% colnames(Data_export()))
               ) {

      write.table(Data_export(),
                  file,
                  row.names = FALSE,
                  quote = FALSE,
                  sep = " ",
                  col.names = c("", colnames(Data_export()[-1]))
      )

    } else if ((!"Sample_ID" %in% colnames(Data_export())) &&
               ("Pop_ID" %in% colnames(Data_export())) &&
               (!"Loc_ID" %in% colnames(Data_export()))
               ) {

      write.table(Data_export(),
                  file,
                  row.names = FALSE,
                  quote = FALSE,
                  sep = " ",
                  col.names = c("", colnames(Data_export()[-1]))
      )

    } else if ((!"Sample_ID" %in% colnames(Data_export())) &&
               (!"Pop_ID" %in% colnames(Data_export())) &&
               ("Loc_ID" %in% colnames(Data_export()))
               ) {

      write.table(Data_export(),
                  file,
                  row.names = FALSE,
                  quote = FALSE,
                  sep = " ",
                  col.names = c("", colnames(Data_export()[-1]))
      )

    } else if (("Sample_ID" %in% colnames(Data_export())) &&
               ("Pop_ID" %in% colnames(Data_export())) &&
               (!"Loc_ID" %in% colnames(Data_export()))
               ) {

      write.table(Data_export(),
                  file,
                  row.names = FALSE,
                  quote = FALSE,
                  sep = " ",
                  col.names = c("", "", colnames(Data_export()[-c(1, 2)]))
      )

    } else if (("Sample_ID" %in% colnames(Data_export())) &&
               (!"Pop_ID" %in% colnames(Data_export())) &&
               ("Loc_ID" %in% colnames(Data_export()))
               ) {

      write.table(Data_export(),
                  file,
                  row.names = FALSE,
                  quote = FALSE,
                  sep = " ",
                  col.names = c("", "", colnames(Data_export()[-c(1, 2)]))
      )

    } else if ((!"Sample_ID" %in% colnames(Data_export())) &&
               ("Pop_ID" %in% colnames(Data_export())) &&
               ("Loc_ID" %in% colnames(Data_export()))
               ) {

      write.table(Data_export(),
                  file,
                  row.names = FALSE,
                  quote = FALSE,
                  sep = " ",
                  col.names = c("", "", colnames(Data_export()[-c(1, 2)]))
      )

    } else {

      write.table(Data_export(),
                  file,
                  row.names = FALSE,
                  quote = FALSE,
                  sep = " "
                  )

    }

  }

)



##### Hierarchical cluster analysis

### Reactive hclust object
dend <- reactive({

  # COLNAMES of loci splitted
  if ("Sample_ID" %in% colnames(Data_PER_Str()) |
      "Pop_ID" %in% colnames(Data_PER_Str()) |
      "Loc_ID" %in% colnames(Data_PER_Str())) {

    COLNAMES_loci <- c(colnames(Data_PER_Str()[-c(1:(length(colnames(Data_PER_Str())) -
                                                loci()*ploidy()))]))

  } else {

    COLNAMES_loci <- colnames(Data_PER_Str())

  }

  # Dataset just with the loci splitted
  Dataset_s <- Data_PER_Str()[, COLNAMES_loci]

  # Combining columns based on ploidy
  Dataset_grouped <- as.data.frame(sapply(seq(from = 1,
                                              to = length(COLNAMES_loci),
                                              by = ploidy()),
                                          FUN = function(i)
                                      unite(Dataset_s[, i:(i + (ploidy() - 1))],
                                                  sep = "/")))

  colnames(Dataset_grouped) <- colnames(Data_export())[-c(1:(length(colnames(Data_export())) -
                                                               loci()))]

  if ("Sample_ID" %in% colnames(Data_PER_Str())) {

    Dataset_adegenet <- cbind("Sample_ID" = Data_PER_Str()$Sample_ID,
                              Dataset_grouped)

    rownames(Dataset_adegenet) <- Dataset_adegenet$Sample_ID

    Dataset_adegenet$Sample_ID <- NULL

  }

  else {

    rownames(Dataset_grouped) <- seq(from = 1,
                                     to = nrow(Data_PER_Str()),
                                     by = 1)

    Dataset_adegenet <- Dataset_grouped

  }

  Dataset_adegenet[Dataset_adegenet == "NA/NA"] <- NA

  Dataset_AD <- df2genind(X = Dataset_adegenet,
                          ploidy = ploidy(),
                          sep = "/",
                          loc.names = c(colnames(Dataset_adegenet))
                          )

  # Added popolutation to genind object if present in the main dataset
  if ("Pop_ID" %in% colnames(Data_PER_Str())) {

    pop(Dataset_AD) <- Data_PER_Str()$Pop_ID

  }

  tab_AD <- tab(Dataset_AD, NA.method = input$na_value)

  tab_AD

  if (input$distance == "binary") {

    DIST <- dist.binary(df = tab_AD,
                        method = input$similarity_coefficient)

  } else if (input$distance == "geometric") {

    DIST <- dist(tab_AD, method = input$geometric_distance)

  }

  dend <- hclust(DIST, method = input$hierarchical_method) %>%
    as.dendrogram %>%
    set("labels_cex", 0.4) %>% set("labels_col", "black") %>%
    set("branches_k_color",
        value = c("black",
                  "lightcoral",
                  "deepskyblue2",
                  "limegreen",
                  "orange",
                  "lightblue",
                  "yellow1",
                  "burlywood4",
                  "royalblue4",
                  "darkorange",
                  "magenta3",
                  "palegreen4",
                  "palegreen1",
                  "red",
                  "green",
                  "blue",
                  "brown",
                  "pink",
                  "deepskyblue3",
                  "magenta2"),
        k = input$cluster_count) %>%
    set("leaves_pch", 19) %>%
    set("leaves_cex", 0.2)

  return(dend)

})



### Reactive dendrogram plot
Dendrogram_plot <- reactive({

  dend_gg <- as.ggdend(dend())

  dend_gg_segments <- data.frame(dend_gg$segments)

  dend_gg_segments$col[is.na(dend_gg_segments$col)] = "black"

  dend_gg_data <- data.frame(dend_gg$labels)

  colnames(dend_gg_data)[3] = "Sample_ID"

  if ("Sample_ID" %in% colnames(Data_PER_Str())) {

    dend_gg_labels <- merge(dend_gg_data,
                            Data_PER_Str(),
                            by = "Sample_ID",
                            sort = FALSE)

    Dendrogram_plot <- ggplot() +
      geom_segment(data = dend_gg_segments,
                   aes(x = x,
                       y = y,
                       xend = xend,
                       yend = yend),
                   colour = dend_gg_segments$col, size = 0.6) +
      geom_text(data = dend_gg_labels,
                aes(x = x,
                    y = y,
                    label = Sample_ID,
                    angle = 90,
                    hjust = 1,
                    colour = dend_gg_labels$Pop_ID
                ),
                size = 3) +
      labs(x = "Sample_ID",
           y = NULL,
           title = input$dendrogram_title) +
      lims(y = c(-0.08, NA)) +
      theme(
        axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend.title = element_text(size = 15),
        # legend.text = element_text(size = 15),
        legend.position = "none"
      )

  } else {

    dend_gg_labels <- dend_gg_data

    Dendrogram_plot <- ggplot() +
      geom_segment(data = dend_gg_segments,
                   aes(x = x,
                       y = y,
                       xend = xend,
                       yend = yend),
                   colour = dend_gg_segments$col, size = 0.6) +
      geom_text(data = dend_gg_labels,
                aes(x = x,
                    y = y,
                    label = Sample_ID,
                    angle = 90,
                    hjust = 1
                ),
                size = 3) +
      labs(x = "Sample_ID",
           y = NULL,
           input$dendrogram_title) +
      lims(y = c(-0.08, NA)) +
      theme(
        axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend.title = element_text(size = 15),
        # legend.text = element_text(size = 15),
        legend.position = "none"
      )

  }

  return(Dendrogram_plot)

})



### Dendrogram plot
output$tree <- renderPlot({ # To put interactive when the bug is fixed

  Dendrogram_plot()

})



### Download dendrogram plot
output$download_dendrogram <- downloadHandler(

  filename <- function() {

    input$dendrogram_title

  },

  content <- function(file) {

    if (input$dendrogram_format == ".bmp") {

      device <- function(..., width, height) grDevices::bmp(...,
                                                            width = input$dendrogram_width*2.5,
                                                            height = input$dendrogram_height*2.5,
                                                            res = input$dendrogram_resolution,
                                                            units = "px")

    } else if (input$dendrogram_format == ".jpeg") {

      device <- function(..., width, height) grDevices::jpeg(...,
                                                             width = input$dendrogram_width*2.5,
                                                             height = input$dendrogram_height*2.5,
                                                             res = input$dendrogram_resolution,
                                                             quality = 100,
                                                             units = "px")

    } else if (input$dendrogram_format == ".png") {

      device <- function(..., width, height) grDevices::png(...,
                                                            width = input$dendrogram_width*2.5,
                                                            height = input$dendrogram_height*2.5,
                                                            res = input$dendrogram_resolution,
                                                            units = "px")

    } else if (input$dendrogram_format == ".tiff") {

      device <- function(..., width, height) grDevices::tiff(...,
                                                             width = input$dendrogram_width*2.5,
                                                             height = input$dendrogram_height*2.5,
                                                             res = input$dendrogram_resolution,
                                                             units = "px")

    }

    ggsave(file, plot = Dendrogram_plot(), device = device)

  }

)



############################### PLOT FROM STRUCTURE SECTION ####

### Reactive dataset FROM STRUCTURE - input
Data_DA_Str <- reactive({

  req(input$Data_DA_Str)

  File <- input$Data_DA_Str

  df2 <- read.csv(File$datapath,
                  header = TRUE,
                  sep = input$separator2,
                  quote = input$quote2)

  updateSelectInput(session,
                    inputId = "bottom_left",
                    choices = names(df2)[-c(1, 2)],
                    selected = names(df2)[3])

  updateSelectInput(session,
                    inputId = "bottom_right",
                    choices = names(df2)[-c(1, 2)],
                    selected = names(df2)[4])



  return(df2)

})



### Output table imported 2
output$table_import2 <- renderDataTable({

  Data_DA_Str()

}, options = list(pageLength = 10))



### Number of cluster from STRUCTURE
num_cluster_STR <- reactive({

  x <- c(colnames(Data_DA_Str())[seq(from = 1,
                                     to = ncol(Data_DA_Str()),
                                     by = 1)])

  y <- c("Sample_ID", "Pop_ID")

  num_cluster_STR <- length(x) - length(intersect(x, y))

  return(num_cluster_STR)

})

output$cluster_number <- renderText({

  num_cluster_STR()

})



### Reactive dataset TO PLOT
Data_plot <- reactive({

Dataset <- Data_DA_Str()



### COLNAMES selection for variables to use in the merge
x <- c(colnames(Dataset)[seq(from = 1, to = ncol(Dataset), by = 1)])

y <- c("Sample_ID", "Pop_ID")

z <- length(intersect(x, y))

  if (all(!c("Sample_ID", "Pop_ID") %in% colnames(Dataset))) {

    COLNAMES <- c(colnames(Dataset))

  } else {

    COLNAMES <- c(colnames(Dataset)[-c(seq(from = 1, to = z, by = 1))])

  }

  if (input$sort == "original") {

    Dataset_m <- melt(data = Dataset,
                      id.vars = c("Sample_ID", "Pop_ID"),
                      measure.vars = COLNAMES,
                      value.name = "Q"
                      )

    # sort Sample_ID by original order
    Dataset_m$Sample_ID <- factor(Dataset_m$Sample_ID,
                                  levels = unique(Dataset_m$Sample_ID)
                                  )

  } else if (input$sort == "Q") {

    # removed Sample_ID e Pop_ID column (if they're present)
    Dataset_c <- Dataset[, COLNAMES]

    # picks the max cluster and match the max value to cluster
    max_val <- apply(X = Dataset_c, MARGIN = 1, FUN = max) # takes the max value
                                                           # present in every
                                                           # column for every
                                                           # row
    match_val <- vector(length = nrow(Dataset_c))

    for(i in seq(from = 1, to = nrow(Dataset_c), by = 1)) match_val[i] <-
      match(max_val[i], Dataset_c[i,])

    # add max_val and match_val to dataframe
    Dataset_q <- Dataset_c
    Dataset_q$maxval <- max_val
    Dataset_q$matchval <- match_val

    # unite again Sample_ID and Pop_ID columns
    Dataset_q <- cbind(Dataset[, c(1, 2)], Dataset_q)

    # order dataframe ascending match and decending max
    Dataset_q <- Dataset_q[with(Dataset_q, order(matchval,-maxval)), ]

    # removed columns maxval and matchval
    Dataset_q$maxval <- NULL
    Dataset_q$matchval <- NULL

    # reshape to long format
    Dataset_m <- melt(Dataset_q,
                      id.vars = c("Sample_ID", "Pop_ID"),
                      measure.vars = COLNAMES,
                      value.name = "Q")

    # sort Sample_ID by q
    Dataset_m$Sample_ID <- factor(Dataset_m$Sample_ID,
                                  levels = unique(Dataset_m$Sample_ID))

  } else if (input$sort == "alphabetic") {

    Dataset_m <- melt(data = Dataset,
                      id.vars = c("Sample_ID", "Pop_ID"),
                      measure.vars = COLNAMES,
                      value.name = "Q"
                      )

    Dataset_m <- Dataset_m[order(Dataset_m$Sample_ID), ]

    # sort Sample_ID by alphabetic order
    Dataset_m$Sample_ID <- factor(Dataset_m$Sample_ID,
                                  levels = unique(Dataset_m$Sample_ID))

  }

  colnames(Dataset_m)[which(names(Dataset_m) == "variable")] <- "Cluster"

  Dataset_m$Cluster <- factor(Dataset_m$Cluster,
                              levels = COLNAMES,
                              labels = c(seq(from = 1,
                                             to = length(COLNAMES),
                                             by = 1)
                              )
  )

  # Pop_ID becomes factor for the Labels (Population) colors
  Dataset_m$Pop_ID <- factor(Dataset_m$Pop_ID,
                             levels = unique(Dataset_m$Pop_ID))

  return(Dataset_m)

})



### STRUCTURE Barplot ggplot
Structure_Plot <- reactive({

  Dataset_m <- Data_plot()

  # Cluster color vector
  Colours <- c(input$colour_1,
               input$colour_2,
               input$colour_3,
               input$colour_4,
               input$colour_5,
               input$colour_6,
               input$colour_7,
               input$colour_8,
               input$colour_9,
               input$colour_10,
               input$colour_11,
               input$colour_12,
               input$colour_13,
               input$colour_14,
               input$colour_15,
               input$colour_16,
               input$colour_17,
               input$colour_18,
               input$colour_19,
               input$colour_20
               )

  # number of colours needed
  numColors <- length(levels(Dataset_m$Pop_ID))

  Pop_Col <- distinctColorPalette(k = numColors) # genero un numero di colori uguale al numero che mi serve (potrebbe non funzionare per k > 40)
  names(Pop_Col) <- levels(Dataset_m$Pop_ID)

  # every variable of the dataset has ben changed in "character"
  Palette_Match <- as.data.frame(Pop_Col, stringsAsFactors = FALSE)

  # Dataframe with the enconding "populations - colors"
  Palette_Match$Pop_ID <- rownames(Palette_Match)

  y <- split(Dataset_m, f = Dataset_m$Cluster)
  Pop_ID <- data.frame(Pop_ID = y$`1`$Pop_ID) # presa la colonna Pop_ID dal dataset ordinato (considerando solo quella per un cluster)

  vec <- c(Palette_Match$Pop_Col[match(Pop_ID$Pop_ID, Palette_Match$Pop_ID)])

  # ggplot
  p <- ggplot(data = Dataset_m,
              aes(x = Sample_ID, name = Pop_ID, y = Q, fill = Cluster)) +
          geom_bar(stat = "identity",
                   colour = "black",
                   size = 0.2,
                   position = input$barpos) +
  # sometimes the sum of Q is not 1 ma "1.001", so better to abound
            scale_y_continuous(limits = c(0, 1.05),
                               breaks = c(seq(from = 0,
                                              to = 1,
                                              by = 0.1))) +
            scale_fill_manual("Cluster",
                              values = Colours) +
            labs(y = "Admixture index [%]",
                 title = input$barplot_title) +
              theme(
                axis.title = element_text(size = input$axis_title_size*2),
                axis.text.y = element_text(size = input$y_label_size),
                axis.text.x = element_text(size = input$x_label_size,
                                           angle = input$x_label_angle,
                                           hjust = 1,
                                           colour = vec),
                plot.title = element_text(size = input$axis_title_size*2,
                                          hjust = 0.5),
                legend.title = element_text(size = input$axis_title_size*2)
              )

  return(p)

})

# STRUCTURE Barplot plotly
output$structure_plot <- renderPlotly({

  ggplotly(Structure_Plot(),
           width = input$barplot_width,
           height = input$barplot_height)

})

# Download barplot
output$download_barplot <- downloadHandler(

  filename <- function() {

    input$barplot_title

    },

  content <- function(file) {

  if (input$barplot_format == ".bmp") {

    device <- function(..., width, height) grDevices::bmp(...,
                                              width = input$barplot_width*2.5,
                                              height = input$barplot_height*2.5,
                                              res = input$barplot_resolution,
                                              # quality = 100,
                                              units = "px")

  } else if (input$barplot_format == ".jpeg") {

    device <- function(..., width, height) grDevices::jpeg(...,
                                              width = input$barplot_width*2.5,
                                              height = input$barplot_height*2.5,
                                              res = input$barplot_resolution,
                                              quality = 100,
                                              units = "px")

  } else if (input$barplot_format == ".png") {

    device <- function(..., width, height) grDevices::png(...,
                                              width = input$barplot_width*2.5,
                                              height = input$barplot_height*2.5,
                                              res = input$barplot_resolution,
                                              # quality = 100,
                                              units = "px")

  } else if (input$barplot_format == ".tiff") {

    device <- function(..., width, height) grDevices::tiff(...,
                                              width = input$barplot_width*2.5,
                                              height = input$barplot_height*2.5,
                                              res = input$barplot_resolution,
                                              # quality = 100,
                                              units = "px")

  }

    ggsave(file, plot = Structure_Plot(), device = device)

  }

)



### Membership STRUCTURE cluster
Members_STR <- reactive({

  Data_cluster_STR <- Data_DA_Str()[, -2]

  Data_cluster_STR$Sample_ID <- factor(Data_cluster_STR$Sample_ID,
                                       levels = Data_cluster_STR$Sample_ID)

  rownames(Data_cluster_STR) <- Data_cluster_STR$Sample_ID

  Data_cluster_STR$Sample_ID <- NULL

  Data_cluster_STR$Members_STR <- apply(X = Data_cluster_STR,
                                        MARGIN = 1,
                                        FUN = which.max)

  Data_cluster_STR <- as.data.frame(cbind("Sample_ID" = rownames(Data_cluster_STR),
                                          "Members_STR" = Data_cluster_STR$Members_STR))

  return(Data_cluster_STR)

})



##### Triangle Plot

### Cluster number triangle plot
output$cluster_number_2 <- renderText({

  x <- c(colnames(Data_DA_Str())[seq(from = 1,
                                     to = ncol(Data_DA_Str()),
                                     by = 1)])

  y <- c("Sample_ID", "Pop_ID")

  length(x) - length(intersect(x, y))

})



### STRUCTURE Triangle plot
output$triangle_plot <- renderPlotly({

  a <- rowSums(Data_DA_Str()[, -c(1, 2)]) -
    (Data_DA_Str()[, input$bottom_left] + Data_DA_Str()[, input$bottom_right])

  b <- Data_DA_Str()[, input$bottom_left]

  c <- Data_DA_Str()[, input$bottom_right]

  axis <- function(title) {
    list(
      title = title,
      titlefont = list(
        size = 16
      ),
      tickfont = list(
        size = 12
      ),
      tickcolor = 'rgba(0,0,0,0)',
      ticklen = 5
    )
  }

  margin <- list(
    l = 40,
    r = 40,
    b = 40,
    t = 70,
    pad = 4
  )

  plot_ly(data = Data_DA_Str(),
          width = input$triangleplot_width,
          height = input$triangleplot_height) %>%
    add_trace(
      type = "scatterternary",
      mode = "markers",
      a = ~a,
      b = ~b,
      c = ~c,
      text = ~Sample_ID,
      name = ~Pop_ID,
      colors = ~Pop_ID,
      marker = list(
        # symbol = ,
        size = input$triangleplot_symbol_size
      )
    ) %>%
    layout(
      title = input$triangleplot_title,
      margin = margin,
      showlegend = TRUE,
      annotations = list(yref = "paper",
                         xref = "paper",
                         y = 1.05,
                         x = 1.1,
                         text = "Populations",
                         showarrow = F),
      ternary = list(
        sum = 1,
        aaxis = axis("All others"),
        baxis = axis(input$bottom_left),
        caxis = axis(input$bottom_right)
      )
    )

})



### Download triangle plot
output$download_triangleplot <- downloadHandler(

  filename <- function() {

    input$triangleplot_title

  },

  content <- function(file) {

    a <- rowSums(Data_DA_Str()[, -c(1, 2)]) -
      (Data_DA_Str()[, input$bottom_left] + Data_DA_Str()[, input$bottom_right])

    b <- Data_DA_Str()[, input$bottom_left]

    c <- Data_DA_Str()[, input$bottom_right]

    axis <- function(title) {
      list(
        title = title,
        titlefont = list(
          size = 16
        ),
        tickfont = list(
          size = 12
        ),
        tickcolor = 'rgba(0,0,0,0)',
        ticklen = 5
      )
    }

    margin <- list(
      l = 40,
      r = 40,
      b = 40,
      t = 70,
      pad = 4
    )

    triangleplot <- plot_ly(data = Data_DA_Str(),
            width = input$triangleplot_width,
            height = input$triangleplot_height) %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        text = ~Sample_ID,
        name = ~Pop_ID,
        colors = ~Pop_ID,
        marker = list(
          # symbol = ,
          size = input$triangleplot_symbol_size
        )
      ) %>%
      layout(
        title = input$triangleplot_title,
        margin = margin,
        showlegend = TRUE,
        annotations = list(yref = "paper",
                           xref = "paper",
                           y = 1.05,
                           x = 1.1,
                           text = "Populations",
                           showarrow = F),
        ternary = list(
          sum = 1,
          aaxis = axis("All others"),
          baxis = axis(input$bottom_left),
          caxis = axis(input$bottom_right)
        )
      )

    if(input$triangleplot_title != "") {

      orca(p = triangleplot,
           file = paste0(input$triangleplot_title, input$triangleplot_format),
           width = input$triangleplot_width*1.5,
           height = input$triangleplot_height*1.5)

    }

    else {

      orca(p = triangleplot,
           file = paste0("download_triangleplot", input$triangleplot_format),
           width = input$triangleplot_width*1.5,
           height = input$triangleplot_height*1.5)


    }

  }

)



############################### COMPARING PARTITIONS ####

### Cluster number hclust
output$Cluster_hclust <- renderText({

  print(input$cluster_count)

})



### Cluster number STRUCTURE
output$Cluster_STR <- renderText({

  num_cluster_STR()

})



### Comparison table
Comparison_table <- reactive({

  Members_hclust <- data.frame("Members_hclust" = cutree(tree = dend(),
                                                      k = input$cluster_count))

  Data_partitions <- cbind(rownames(Members_hclust),
                           Data_DA_Str()[, 2],
                           Members_hclust,
                           Members_STR()) # [, -1] aggiungi dopo i controlli che funziona bene

  colnames(Data_partitions) <- c("Sample_ID",
                                 "Pop_ID",
                                 "Hierarchic",
                                 "Sample_ID_2",
                                 "STRUCTURE")

  Data_partitions <- Data_partitions[, -4]

  return(Data_partitions)

})



### Partitions indeces
output$agreement_value <- renderText({

  comparing.Partitions(as.vector(Comparison_table()[, 2]),
                       as.vector(Comparison_table()[, 3]),
                       type = input$agreement_index)

})



### Table to compare partitions
output$comparison_table <- renderDataTable({

  Comparison_table()

}, options = list(pageLength = 25))



### Table plot to compare partitions
Tableplot <- reactive({

  Comparison_table_m <- melt(data = Comparison_table(),
                             id.vars = "Sample_ID",
                             measure.vars = c("Hierarchic",
                                              "STRUCTURE"))

  Tableplot <- ggplot() +
    geom_bar(data = Comparison_table_m,
             aes(x = value, fill = variable),
             position = input$comparison_plot_barpos) +
    labs(x = "Cluster",
         y = "Number of observations",
         fill = "Analysis",
         title = input$comparison_plot_title) +
    theme(
      legend.title = element_blank()
    )

  return(Tableplot)

})

# ggplotly Tableplot
output$comparison_plot <- renderPlotly({

  ggplotly(Tableplot(),
           width = input$comparison_plot_width,
           height = input$comparison_plot_height)

})

# Download Tableplot
output$download_comparison_plot <- downloadHandler(

  filename <- function() {

    input$comparison_plot_title

  },

  content <- function(file) {

    if (input$comparison_plot_format == ".bmp") {

      device <- function(..., width, height) grDevices::bmp(...,
                                                            width = input$comparison_plot_width*2.5,
                                                            height = input$comparison_plot_height*2.5,
                                                            res = input$comparison_plot_resolution,
                                                            units = "px")

    } else if (input$comparison_plot_format == ".jpeg") {

      device <- function(..., width, height) grDevices::jpeg(...,
                                                             width = input$comparison_plot_width*2.5,
                                                             height = input$comparison_plot_height*2.5,
                                                             res = input$comparison_plot_resolution,
                                                             quality = 100,
                                                             units = "px")

    } else if (input$comparison_plot_format == ".png") {

      device <- function(..., width, height) grDevices::png(...,
                                                            width = input$comparison_plot_width*2.5,
                                                            height = input$comparison_plot_height*2.5,
                                                            res = input$comparison_plot_resolution,
                                                            units = "px")

    } else if (input$comparison_plot_format == ".tiff") {

      device <- function(..., width, height) grDevices::tiff(...,
                                                             width = input$comparison_plot_width*2.5,
                                                             height = input$comparison_plot_height*2.5,
                                                             res = input$comparison_plot_resolution,
                                                             units = "px")

    }

    ggsave(file, plot = Tableplot(), device = device)

  }

)



} # Closes SERVER
