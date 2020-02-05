
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
  shinyjs::hideElement(id = "comparison_1")
  shinyjs::hideElement(id = "comparison_2")
  shinyjs::hideElement(id = "comparison_3")
  shinyjs::hideElement(id = "comparison_true")
})

observeEvent(input$back_results, {
  shinyjs::hideElement(id = "instructions")
  shinyjs::showElement(id = "wallpaper")
  shinyjs::showElement(id = "table_panel")
  shinyjs::showElement(id = "plot_panel")
  shinyjs::showElement(id = "comparison_1")
  shinyjs::showElement(id = "comparison_2")
  shinyjs::showElement(id = "comparison_3")
  shinyjs::showElement(id = "comparison_true")
})



# Show and hide check_table and customize_plot buttons
observeEvent(input$check_table, {
  shinyjs::hideElement(id = "check_table")
})

observeEvent(input$customize_plot, {
  shinyjs::hideElement(id = "customize_plot")
})



### Visualize StrucuRly pipeline
output$structurly_pipeline <- renderUI({

  img(src = "https://user-images.githubusercontent.com/35098432/73140059-05bf5d00-4075-11ea-867f-e59cd4a9724f.png",
      width = as.integer(input$pipeline_slider))

})


### Open STRUCTURE
observeEvent(input$open_structure, {

  System_info <- data.frame("Info" = Sys.info())

  if (System_info$Info[1] == "Darwin") {

    system("open -a structure")

  } else if (System_info$Info[1] == "Linux") {

    system("cd /usr/local/Structure/
           java -cp class/Structure.jar RunStructure")

  }

  # else if (System_info$Info[1] == "Windows") {
  #
  #     shell.exec("file:///C:/Program Files (x86)/Structure2.3.4/structure.exe")
  #
  # }

})



### Open ADMIXTURE
observeEvent(input$open_admixture, {

  System_info <- data.frame("Info" = Sys.info())

  if (System_info$Info[1] == "Darwin") {

    system("open -a terminal")

  } else if (System_info$Info[1] == "Linux") {

    system("gnome-terminal")

  }

  # else if (System_info$Info[1] == "Windows") {
  #
  #     shell.exec("file:///C:/Program Files (x86)/Structure2.3.4/structure.exe")
  #
  # }

})



############################### TABLE SECTION #####

##### Reactive dataset FOR STRUCTURE input #####
Data_PER_Str <- reactive({

  req(input$Data_PER_Str)

  File <- input$Data_PER_Str

  df <- read.csv(File$datapath,
                 header = TRUE,
                 sep = input$separator,
                 quote = input$quote)

  COLNAMES_all <- c(colnames(df)[seq(from = 1,
                                     to = ncol(df),
                                     by = 1)])

  COLNAMES <- c("Sample_ID",
                "Pop_ID",
                "Loc_ID")

  COLNAMES_loci <- setdiff(COLNAMES_all, COLNAMES)

  if (grepl(pattern = ".1",
            x = COLNAMES_loci[1],
            fixed = TRUE) == FALSE) {

    ploidy <- length(COLNAMES_loci)/length(COLNAMES_loci)

  } else {

    COLNAMES_loci_unique <- unique(substr(x = COLNAMES_loci,
                                          start = 1,
                                          stop = nchar(COLNAMES_loci)-2))

    ploidy <- length(COLNAMES_loci)/length(COLNAMES_loci_unique)

  }

  updateSelectInput(session,
                    inputId = "subset_variable",
                    choices = names(df),
                    selected = NULL)

  if (ploidy == 1) {

    updateSelectInput(session,
                      inputId = "stats_type",
                      choices = c("Types of different alleles",
                                  "Missing values per locus",
                                  "N° of different alleles per locus",
                                  "Diversity indices",
                                  "P-gen"),
                      selected = "Diversity indices")

    updateSelectInput(session,
                      inputId = "locus_name",
                      choices = COLNAMES_loci,
                      selected = COLNAMES_loci[1])

  } else if (ploidy > 1) {

    updateSelectInput(session,
                      inputId = "stats_type",
                      choices = c("Types of different alleles",
                                  "Missing values per locus",
                                  "N° of different alleles per locus",
                                  "Diversity indices",
                                  "P-gen",
                                  "H-W equilibrium"),
                      selected = "Diversity indices")

    updateSelectInput(session,
                      inputId = "locus_name",
                      choices = COLNAMES_loci_unique,
                      selected = COLNAMES_loci_unique[1])

  }

  return(df)

})



### Colnames Data_PER_Str
COLNAMES_all <- reactive({

  COLNAMES_all <- c(colnames(Data_PER_Str())[seq(from = 1,
                                                 to = ncol(Data_PER_Str()),
                                                 by = 1)])

  return(COLNAMES_all)

})



### Colnames loci
COLNAMES_loci <- reactive({

  COLNAMES <- c("Sample_ID",
                "Pop_ID",
                "Loc_ID")

  COLNAMES_loci <- setdiff(COLNAMES_all(), COLNAMES)

})



### Output table imported
output$table_import <- DT::renderDataTable({

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



### Ploidy
ploidy <- reactive({

    if (grepl(pattern = ".1",
              x = COLNAMES_loci()[1],
              fixed = TRUE) == FALSE) {

      ploidy <- length(COLNAMES_loci())/length(COLNAMES_loci())

    } else {

      COLNAMES_loci_unique <- unique(substr(x = COLNAMES_loci(),
                                            start = 1,
                                            stop = nchar(COLNAMES_loci())-2))

      ploidy <- length(COLNAMES_loci())/length(COLNAMES_loci_unique)

    }

  return(ploidy)

})



### Number of loci
loci <- reactive({

  if (ploidy() > 1) {

    loci <- length(COLNAMES_loci())/ploidy()

  } else {

    loci <- length(COLNAMES_loci())

  }

  return(loci)

})



##### Basic statistics panel #####

### Reactive Adegenet dataset
Dataset_AD <- reactive({

  Dataset <- Data_PER_Str()

  # COLNAMES of loci splitted
  if ("Sample_ID" %in% colnames(Dataset) |
      "Pop_ID" %in% colnames(Dataset) |
      "Loc_ID" %in% colnames(Dataset)) {

    COLNAMES_loci <- c(colnames(Dataset[-c(1:(length(colnames(Dataset)) -
                                                loci()*ploidy()))]))

  } else {

    COLNAMES_loci <- colnames(Dataset)

  }

  # Dataset just with the loci splitted
  Dataset_s <- Dataset[, COLNAMES_loci]

  # Unique colnames and combining columns on the basis of ploidy
  if (ploidy() > 1) {

    COLNAMES_loci_unique <- unique(substr(x = names(Dataset_s),
                                          start = 1,
                                          stop = nchar(names(Dataset_s))-2))

    Dataset_grouped <- do.call(cbind,
                               lapply(COLNAMES_loci_unique, function(x){unite_(Dataset_s,
                                                                               x,
                                                                               grep(x,
                                                                                    names(Dataset_s),
                                                                                    value = TRUE),
                                                                               sep = '/', remove = TRUE) %>% select_(x)}))

  } else {

    Dataset_grouped <- Dataset_s

  }

  if ("Sample_ID" %in% colnames(Dataset)) {

    Dataset_adegenet <- cbind("Sample_ID" = Dataset$Sample_ID,
                              Dataset_grouped)

    rownames(Dataset_adegenet) <- Dataset_adegenet$Sample_ID

    Dataset_adegenet$Sample_ID <- NULL

  } else {

    rownames(Dataset_grouped) <- seq(from = 1,
                                     to = nrow(Dataset),
                                     by = 1)

    Dataset_adegenet <- Dataset_grouped

  }

  Dataset_adegenet[Dataset_adegenet == paste(rep(x = "NA",
                                                 times = ploidy()),
                                             collapse = "/")] <- NA

  if (ploidy() > 1) {

    Dataset_AD <- df2genind(X = Dataset_adegenet,
                            ploidy = ploidy(),
                            sep = "/",
                            loc.names = c(colnames(Dataset_adegenet))
    )

  } else {

    Dataset_AD <- df2genind(X = Dataset_adegenet,
                            ploidy = ploidy(),
                            loc.names = c(colnames(Dataset_adegenet))
    )

  }

  if ("Pop_ID" %in% colnames(Dataset)) {

    pop(Dataset_AD) <- Dataset$Pop_ID

  }

  return(Dataset_AD)

})



### Types of different alleles
output$alleles_types <- DT::renderDataTable({

  List_alleles <- alleles(Dataset_AD())

  List_alleles <- lapply(List_alleles,
                         `length<-`,
                         max(lengths(List_alleles)))

  Data_alleles <- data.frame(matrix(unlist(List_alleles),
                                    nrow = max(lengths(List_alleles)),
                                    byrow = F))

  colnames(Data_alleles) <- levels(Dataset_AD()$loc.fac)

  Data_alleles

}, options = list(pageLength = 10))



### Barplot alleles frequency
output$allele_frequency <- renderPlotly({

  # ricordati la diversa ploidia

  if (ploidy() > 1) {

    Dataset_adegenet <- genind2df(Dataset_AD(),
                                  sep = "/")

    Dataset_locus <- data.frame(cbind("Sample_ID" = rownames(Dataset_adegenet),
                                      Dataset_adegenet[, c(input$locus_name)]))

    names(Dataset_locus)[2] <- input$locus_name

    Dataset_locus_sep <- separate(data = Dataset_locus,
                                  col = input$locus_name,
                                  into = paste0(rep(x = "L",
                                                    times = ploidy()),
                                                seq(from = 1,
                                                    to = ploidy(),
                                                    by = 1)),
                                  sep = "/")

    List_locus <- c(Dataset_locus_sep[, -1])

    Vector_locus <- unlist(List_locus,
                           use.names = FALSE)

  } else {

    Dataset_adegenet <- genind2df(Dataset_AD())

    Dataset_locus <- data.frame(cbind("Sample_ID" = rownames(Dataset_adegenet),
                                      Dataset_adegenet[, c(input$locus_name)]))

    names(Dataset_locus)[2] <- input$locus_name

    List_locus <- list(Dataset_locus[, -1])

    Vector_locus <- unlist(List_locus,
                           use.names = FALSE)

  }

  Dataset_alleles_freq <- count(Vector_locus)

  names(Dataset_alleles_freq) <- c("Allele",
                                   "Frequency")

  Freq_plot <- ggplot(data = Dataset_alleles_freq,
                      aes(x = Allele,
                          y = Frequency)) +
    geom_col(width = 0.4) +
      scale_y_continuous(breaks = seq(from = 0,
                                      to = max(Dataset_alleles_freq$Frequency),
                                      by = 15)) +
      labs(title = input$locus_name) +
        coord_flip()

  ggplotly(Freq_plot,
           height = 380)

  # Dataset_locus

})



### Plot of the number of alleles per locus
output$number_alleles_per_locus <- renderPlotly({

  Dataset_genind <- Dataset_AD()

  Alleles_count <- data.frame("Alleles_number" = Dataset_genind$loc.n.all)

  Alleles_per_locus <- data.frame("Locus" = rownames(Alleles_count),
                                  Alleles_count)

  Alleles_per_locus$Locus <- factor(Alleles_per_locus$Locus,
                                    levels = Alleles_per_locus$Locus)

  barplot_alleles_per_locus <- ggplot(Alleles_per_locus,
                                      aes(x = Locus,
                                          y = Alleles_number)) +
    geom_bar(aes(fill = Locus),
             stat = "identity",
             width = 0.4) +
    labs(y = "Number of alleles") +
    scale_y_continuous(breaks = c(seq(from = 0,
                                      to = max(Alleles_per_locus$Alleles_number),
                                      by = 2))) +
    theme(legend.position = "none")

  ggplotly(barplot_alleles_per_locus)

})



### SliderInput UI for H-W Monte Carlo test
output$hw.test_sliderInput <- renderUI({

  if (input$stats_type == "H-W equilibrium" &&
      ploidy() == 2) {

    sliderInput(inputId = "hw_replicates",
                label = h5("Monte Carlo replicates"),
                min = 0,
                max = 1000000,
                value = 100,
                ticks = FALSE)

  } else {NULL}

})



### Loci statistics
output$loci_stats <- DT::renderDataTable({

  Dataset_genind <- Dataset_AD()

  if (input$stats_type == "Diversity indices") {


    Locus_summary_Simpson <- data.frame(locus_table(x = Dataset_genind,
                                                    index = "simpson"))

    Locus_summary_Shannon <- data.frame(locus_table(x = Dataset_genind,
                                                    index = "shannon"))

    Locus_summary_Stoddard <- data.frame(locus_table(x = Dataset_genind,
                                                     index = "invsimpson"))

    Locus_summary <- cbind("Locus" = rownames(Locus_summary_Simpson),
                           "Simpson" = Locus_summary_Simpson[, 2],
                           "Shannon" = Locus_summary_Shannon[, 2],
                           "Stoddard" = Locus_summary_Stoddard[, 2],
                           Locus_summary_Simpson[, c(3, 4)])

    Locus_summary <- cbind("Locus" = Locus_summary[, 1],
                           round(Locus_summary[, -1],
                                 digits = 2))

    Locus_summary

  } else if (input$stats_type == "P-gen") {

    Dataset_genpop <- genind2genpop(Dataset_genind)

    genpop_freq <- tab(Dataset_genpop,
                       freq = TRUE)

    Dataset_pgen <- data.frame("Sample_ID" = rownames(Dataset_genind$tab),
                               "Pgen" = apply(pgen(gid = Dataset_genind,
                                                   log = FALSE,
                                                   freq = genpop_freq),
                                              1,
                                              prod,
                                              na.rm = TRUE))

    cbind("Sample_ID" = rownames(Dataset_genind$tab),
          "Pgen" = round(Dataset_pgen$Pgen,
                         digits = 5))

  } else if (input$stats_type == "H-W equilibrium") {

    if (ploidy() != 2) {

      HW_test <- as.data.frame(hw.test(x = Dataset_genind))

      HW_data <- data.frame("Locus" = rownames(HW_test),
                            "chi^2" = round(HW_test$`chi^2`,
                                            digits = 2),
                            "chi^2.p-value" = round(HW_test$`Pr(chi^2 >)`,
                                                    digits = 5))

    } else {

      if (input$hw_replicates != 0) {

        HW_test <- as.data.frame(hw.test(x = Dataset_genind,
                                         B = input$hw_replicates))

        HW_data <- data.frame("Locus" = rownames(HW_test),
                              "chi^2" = round(HW_test$`chi^2`,
                                              digits = 2),
                              "chi^2.p-value" = round(HW_test$`Pr(chi^2 >)`,
                                                      digits = 5),
                              "exact.p-value" = round(HW_test$Pr.exact,
                                                      digits = 5))

      } else {

        HW_test <- as.data.frame(hw.test(x = Dataset_genind,
                                         B = 0))

        HW_data <- data.frame("Locus" = rownames(HW_test),
                              "chi^2" = round(HW_test$`chi^2`,
                                              digits = 2),
                              "chi^2.p-value" = round(HW_test$`Pr(chi^2 >)`,
                                                      digits = 5))

      }

    }

    HW_data

  } else if (input$stats_type == "Missing values per locus") {

    Dataset_adegenet <- genind2df(Dataset_AD(),
                                  sep = "/")

    NA_per_locus <- data.frame("Number" = colSums(is.na(Dataset_adegenet)))

    NA_per_locus <- cbind("Locus" = rownames(NA_per_locus),
                          "NA number" = NA_per_locus$Number,
                          "%" = round(NA_per_locus$Number*100/length(Data_PER_Str()[, 1]),
                                      digits = 2))

    NA_per_locus <- rbind(NA_per_locus,
                          c("Total",
                            sum(is.na(Dataset_adegenet)),
                            round(sum(is.na(Dataset_adegenet))*100/length(Data_PER_Str()[, 1]),
                                  digits = 2)))

    if ("Pop_ID" %in% colnames(Data_PER_Str())) {

      NA_per_locus[-1, ]

    } else {

      NA_per_locus

    }

  }

})



##### Reactive dataset FOR STRUCTURE to export #####
Data_export <- reactive({

  Dataset <- Data_PER_Str()

  Dataset[is.na(Dataset)] <- -9 # To allow STRUCTURE read "NA" values

  if (all(c("Sample_ID", "Pop_ID", "Loc_ID") %in% colnames(Data_PER_Str()))) {

    # Definito l'ordine della colonna Sample_ID uguale a quella che si trova nel dataset originale importato dall'utente
    Dataset$Sample_ID <- factor(Dataset$Sample_ID, levels = Dataset$Sample_ID)

    COLNAMES <- c(colnames(Dataset[-c(1, 2, 3)]))

    if (ploidy() > 1) {

      Dataset_reshape <- reshape(Dataset, # must takes all variables of dataset
                                 direction = "long",
                                 varying = COLNAMES,
                                 timevar = NULL,
                                 # drop = c("UDO6.1", "UDO6.2", "UDO6.3"), # con drop specificare quali variabili lasciare indietro
                                 # times = c("UDO36"),
                                 idvar = "Sample_ID"
                                 # new.row.names = seq(from = 1, to = nrow(Dataset)*length(COLNAMES)/input$ploidy, by = 1) # 2 è il numero di loci, il tutto è la ploidia
      )

    } else {

      Dataset_reshape <- Dataset

    }

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

    if (ploidy() > 1) {

      Dataset_reshape <- reshape(Dataset,
                                 direction = "long",
                                 varying = COLNAMES,
                                 timevar = NULL,
                                 idvar = "Sample_ID"
      )

    } else {

      Dataset_reshape <- Dataset

    }

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

    if (ploidy() > 1) {

      Dataset_reshape <- reshape(Dataset,
                                 direction = "long",
                                 varying = COLNAMES,
                                 timevar = NULL
      )

    } else {

      Dataset_reshape <- Dataset

    }

    Dataset_reshape$id <- ID

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

    if (ploidy() > 1) {

      Dataset_reshape <- reshape(Dataset,
                                 direction = "long",
                                 varying = COLNAMES,
                                 timevar = NULL
      )

    } else {

      Dataset_reshape <- Dataset

    }

    Dataset_reshape$id <- ID

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

    if (ploidy() > 1) {

      Dataset_reshape <- reshape(Dataset,
                                 direction = "long",
                                 varying = COLNAMES,
                                 timevar = NULL,
                                 idvar = "Sample_ID"
      )

    } else {

      Dataset_reshape <- Dataset

    }

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

    if (ploidy() > 1) {

      Dataset_reshape <- reshape(Dataset,
                                 direction = "long",
                                 varying = COLNAMES,
                                 timevar = NULL,
                                 idvar = "Sample_ID"
      )

    } else {

      Dataset_reshape <- Dataset

    }

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

    if (ploidy() > 1) {

      Dataset_reshape <- reshape(Dataset,
                                 direction = "long",
                                 varying = COLNAMES,
                                 timevar = NULL
      )

    } else {

      Dataset_reshape <- Dataset

    }

    Dataset_reshape$id <- ID

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

    if (ploidy() > 1) {

      Dataset_reshape <- reshape(Dataset, # must take all variables of dataset
                                 direction = "long",
                                 varying = COLNAMES,
                                 timevar = NULL
      )

    } else {

      Dataset_reshape <- Dataset

    }

    Dataset_reshape$id <- ID

    Dataset_reshape$id <- factor(Dataset_reshape$id, levels = ID)

    Dataset_reshape <- Dataset_reshape[order(Dataset_reshape$id), ]

    Dataset_reshape$id <- NULL

  }

  rownames(Dataset_reshape) <- seq(from = 1,
                                   to = nrow(Dataset_reshape),
                                   by = 1)

  return(Dataset_reshape)

})



### Output table exported
output$table_export <- DT::renderDataTable({

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



# loci output
output$loci_number <- renderText({

  loci()

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
           "_FOR_STRUCTURE.txt")

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



#####===== Hierarchical cluster analysis =====#####

###= Reactive hclust object =#####
dend <- reactive({

  tab_AD <- tab(Dataset_AD(),
                NA.method = input$na_value)

  if (input$distance == "binary") {

    DIST <- dist.binary(df = tab_AD,
                        method = input$similarity_coefficient)

  } else if (input$distance == "geometric") {

    DIST <- dist(tab_AD,
                 method = input$geometric_distance)

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
                  "magenta2",
                  "grey10"),
        k = input$cluster_count) %>%
    set("leaves_pch", 19) %>%
    set("leaves_cex", 0.2)

  return(dend)

})



###= Reactive dendrogram plot =#####
Dendrogram_plot <- metaReactive({

  Dataset <- Data_PER_Str()

  dend_gg <- as.ggdend(dend())

  dend_gg_segments <- data.frame(dend_gg$segments)

  dend_gg_segments$col[is.na(dend_gg_segments$col)] <- "black"

  dend_gg_data <- data.frame(dend_gg$labels)

  colnames(dend_gg_data)[3] <- "Sample_ID"

  # First part of the dendrogram
  Dendrogram_plot <- ggplot() +
    geom_segment(data = dend_gg_segments,
                 aes(x = x,
                     y = y,
                     xend = xend,
                     yend = yend),
                 colour = dend_gg_segments$col,
                 size = input$dendrogram_branches_width) +
    labs(x = "Sample_ID",
         y = NULL,
         title = input$dendrogram_title) +
    lims(y = c(-0.16, NA)) +
    theme(
      axis.title.x = element_text(size = 15),
      axis.text.y = element_text(size = input$dendrogram_y_label_size),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )

  if (all(c("Sample_ID", "Pop_ID") %in% colnames(Dataset))) {

    dend_gg_labels <- merge(dend_gg_data,
                            Dataset,
                            by = "Sample_ID",
                            sort = FALSE)

    Dendrogram_plot <- Dendrogram_plot +
      geom_text(data = dend_gg_labels,
                aes(x = x,
                    y = y,
                    label = Sample_ID,
                    angle = input$dendrogram_leaves_angle,
                    hjust = 1,
                    colour = as.factor(dend_gg_labels$Pop_ID)
                ),
                size = input$dendrogram_leaves_size - 1)

  } else if ("Sample_ID" %in% colnames(Dataset) &&
             !"Pop_ID" %in% colnames(Dataset)) {

    dend_gg_labels <- merge(dend_gg_data,
                            Dataset,
                            by = "Sample_ID",
                            sort = FALSE)

    Dendrogram_plot <- Dendrogram_plot


  } else if ("Pop_ID" %in% colnames(Dataset) &&
             !"Sample_ID" %in% colnames(Dataset)) {

    Dataset$Sample_ID <- seq(from = 1,
                             to = length(Dataset$Pop_ID),
                             by = 1)

    dend_gg_labels <- merge(dend_gg_data,
                            Dataset,
                            by = "Sample_ID",
                            sort = FALSE)

    Dendrogram_plot <- Dendrogram_plot +
      geom_text(data = dend_gg_labels,
                aes(x = x,
                    y = y,
                    label = Sample_ID,
                    angle = input$dendrogram_leaves_angle,
                    hjust = 1,
                    colour = as.factor(dend_gg_labels$Pop_ID)
                ),
                size = input$dendrogram_leaves_size - 1)

  }  else {

    dend_gg_labels <- dend_gg_data

    Dendrogram_plot <- Dendrogram_plot +
      geom_text(data = dend_gg_labels,
                aes(x = x,
                    y = y,
                    label = Sample_ID,
                    angle = input$dendrogram_leaves_angle,
                    hjust = 1
                ),
                size = input$dendrogram_leaves_size - 1)

  }

  # return(Dendrogram_plot)

})



###= Dendrogram plot =#####
output$tree <- renderPlot({ # To be made interactive when the bug is fixed

  Dendrogram_plot()

})

observe({

  output$tree <- renderPlot({

    Dendrogram_plot()}, width = input$dendrogram_width,
    height = input$dendrogram_height)

})



###= Download dendrogram plot =#####
output$download_dendrogram <- downloadHandler(

  filename <- function() {

    if (input$dendrogram_title == "") {

      paste0("dendrogram", input$dendrogram_format)

    } else {

      paste0(input$dendrogram_title, input$dendrogram_format)

    }

  },

  content <- function(file) {

    if (input$dendrogram_format == ".bmp" |
        input$dendrogram_format == ".jpeg" |
        input$dendrogram_format == ".png" |
        input$dendrogram_format == ".tiff" |
        input$dendrogram_format == ".svg") {

      if (input$dendrogram_format == ".bmp") {

        device <- function(..., width, height) grDevices::bmp(...,
                                                              width = input$dendrogram_width*3.5,
                                                              height = input$dendrogram_height*3.5,
                                                              res = input$dendrogram_resolution,
                                                              # quality = 100,
                                                              units = "px")

      } else if (input$dendrogram_format == ".jpeg") {

        device <- function(..., width, height) grDevices::jpeg(...,
                                                               width = input$dendrogram_width*3.5,
                                                               height = input$dendrogram_height*3.5,
                                                               res = input$dendrogram_resolution,
                                                               quality = 100,
                                                               units = "px")

      } else if (input$dendrogram_format == ".png") {

        device <- function(..., width, height) grDevices::png(...,
                                                              width = input$dendrogram_width*3.5,
                                                              height = input$dendrogram_height*3.5,
                                                              res = input$dendrogram_resolution,
                                                              # quality = 100,
                                                              units = "px")

      } else if (input$dendrogram_format == ".tiff") {

        device <- function(..., width, height) grDevices::tiff(...,
                                                               width = input$dendrogram_width*3.5,
                                                               height = input$dendrogram_height*3.5,
                                                               res = input$dendrogram_resolution,
                                                               # quality = 100,
                                                               units = "px")

      } else if (input$dendrogram_format == ".svg") {

        device <- function(..., width, height) grDevices::svg(...,
                                                              width = input$dendrogram_width/75,
                                                              height = input$dendrogram_height/75)

      }

      ggsave(filename = file,
             plot = Dendrogram_plot(),
             device = device)

    } else {

      if (input$dendrogram_format == ".eps") {

        device <- "eps"

      } else if (input$dendrogram_format == ".pdf") {

        device <- "pdf"

      }

      ggsave(filename = file,
             plot = Dendrogram_plot(),
             width = input$dendrogram_width/110,
             height = input$dendrogram_height/110,
             limitsize = FALSE,
             dpi = input$dendrogram_resolution)

    }

  }

)




#####===== PCoA =====#####

###= Reactive PCoA dataset =#####
dataset_pcoa <- reactive({

  if ("Sample_ID" %in% colnames(Data_PER_Str()) |
      "Pop_ID" %in% colnames(Data_PER_Str()) |
      "Loc_ID" %in% colnames(Data_PER_Str())) {

    dataset_pcoa <- Data_PER_Str()[ , -which(names(Data_PER_Str()) %in% c("Sample_ID", "Pop_ID", "Loc_ID"))]

    if ("Sample_ID" %in% colnames(Data_PER_Str())) {

      rownames(dataset_pcoa) <- Data_PER_Str()$Sample_ID

    }

  } else {

    dataset_pcoa <- Data_PER_Str()

  }

  # if (input$pcoa_na_value == "Zero") {
  #
  #   dataset_pcoa[is.na(dataset_pcoa)] <- 0
  #
  # } else if (input$pcoa_na_value == "Mean") {

    for (i in 1:ncol(dataset_pcoa)){

      dataset_pcoa[is.na(dataset_pcoa[, i]), i] <- mean(dataset_pcoa[, i],
                                                        na.rm = TRUE)

    }

  # } else if (input$pcoa_na_value == "Remove sample") {
  #
  #   dataset_pcoa <- dataset_pcoa[complete.cases(dataset_pcoa), ]
  #
  # }

  return(dataset_pcoa)

})



###= PCoA dots colour =#####
pcoa_dots_colours <- reactive({

  if (input$pcoa_colours == "Default") {

    input$resample_pcoa_default_scale

    isolate({

      pcoa_dots_colours <- sample(x = distinctColorPalette(k = 50),
                                  size = 50,
                                  replace = FALSE)

    })

  } else {

    input$resample_pcoa_gray_scale

    isolate({

      pcoa_dots_colours <- sample(x = gray.colors(n = 50),
                                  size = 50,
                                  replace = FALSE)

    })

  }

  return(pcoa_dots_colours)

})



###= PCoA reactive plot =#####
pcoa_plot <- metaReactive({

  dissimilarity_matrix <- vegan::vegdist(x = dataset_pcoa(),
                                         method = input$pcoa_dissimilarity_indices,
                                         na.rm = TRUE)

  mds <- cmdscale(d = dissimilarity_matrix,
                  k = 2,
                  eig = TRUE)

  pcoa_scores <- as.data.frame(mds$points)

  names(pcoa_scores) <- c("PCo1", "PCo2")

  # round(x = mds$eig*100/sum(mds$eig),
  #       digits = 1)

  pcoa_scores$Sample_ID <- rownames(pcoa_scores)

  if ("Pop_ID" %in% colnames(Data_PER_Str())) {

    pcoa_scores$Population <- as.factor(Data_PER_Str()$Pop_ID)

  }

  if ("Loc_ID" %in% colnames(Data_PER_Str())) {

    pcoa_scores$Location <- as.factor(Data_PER_Str()$Loc_ID)

  }

  pcoa_plot <- ggplot(data = pcoa_scores,
                      aes(x = PCo1,
                          y = PCo2))

  if ("Pop_ID" %in% colnames(Data_PER_Str()) &
      !"Loc_ID" %in% colnames(Data_PER_Str())) {

    pcoa_plot <- pcoa_plot +
      geom_point(aes(colour = Population,
                     name = Sample_ID),
                 size = input$pcoa_dots_size) +
      scale_colour_manual(values = pcoa_dots_colours()) +
      labs(colour = "Pop ID") +
      theme(
        axis.title = element_text(size = input$pcoa_axis_title_size),
        axis.text.y = element_text(size = input$pcoa_y_label_size),
        axis.text.x = element_text(size = input$pcoa_x_label_size),
        axis.ticks = element_line(size = 0.3),
        legend.title = element_text(size = input$pcoa_axis_title_size)
      )

  } else if ("Loc_ID" %in% colnames(Data_PER_Str()) &
             !"Pop_ID" %in% colnames(Data_PER_Str())) {

    pcoa_plot <- pcoa_plot +
      geom_point(aes(shape = Location,
                     name = Sample_ID),
                 size = input$pcoa_dots_size) +
      labs(shape = "Loc_ID") +
      theme(
        axis.title = element_text(size = input$pcoa_axis_title_size),
        axis.text.y = element_text(size = input$pcoa_y_label_size),
        axis.text.x = element_text(size = input$pcoa_x_label_size),
        axis.ticks = element_line(size = 0.3),
        legend.title = element_text(size = input$pcoa_axis_title_size)
      )

  } else if (all(c("Pop_ID", "Loc_ID") %in% colnames(Data_PER_Str()))) {

    pcoa_plot <- pcoa_plot +
      geom_point(aes(colour = Population,
                     shape = Location,
                     name = Sample_ID),
                 size = input$pcoa_dots_size) +
      scale_colour_manual(values = pcoa_dots_colours()) +
      labs(colour = "Pop ID",
           shape = "Loc ID") +
      theme(
        axis.title = element_text(size = input$pcoa_axis_title_size),
        axis.text.y = element_text(size = input$pcoa_y_label_size),
        axis.text.x = element_text(size = input$pcoa_x_label_size),
        axis.ticks = element_line(size = 0.3),
        legend.title = element_text(size = input$pcoa_axis_title_size)
      )

  } else {

    pcoa_plot <- pcoa_plot +
      geom_point(aes(name = Sample_ID),
                 colour = "black",
                 size = input$pcoa_dots_size) +
      theme(
        axis.title = element_text(size = input$pcoa_axis_title_size),
        axis.text.y = element_text(size = input$pcoa_y_label_size),
        axis.text.x = element_text(size = input$pcoa_x_label_size),
        axis.ticks = element_line(size = 0.3)
      )

  }

})



###= PCoA ggplotly plot =#####
output$pcoa_plot <- renderPlotly({

  ggplotly(pcoa_plot(),
           width = input$pcoa_width,
           height = input$pcoa_height)

})

###= Download PCoA plot =#####
output$download_pcoa_plot <- downloadHandler(

  filename <- function() {

    paste0("pcoa_plot", input$pcoa_plot_format)

  },

  content <- function(file) {

    if (input$pcoa_plot_format == ".bmp" |
        input$pcoa_plot_format == ".jpeg" |
        input$pcoa_plot_format == ".png" |
        input$pcoa_plot_format == ".tiff" |
        input$pcoa_plot_format == ".svg") {

      if (input$pcoa_plot_format == ".bmp") {

        device <- function(..., width, height) grDevices::bmp(...,
                                                              width = input$pcoa_width*2.5,
                                                              height = input$pcoa_height*2.5,
                                                              res = input$pcoa_resolution,
                                                              # quality = 100,
                                                              units = "px")

      } else if (input$pcoa_plot_format == ".jpeg") {

        device <- function(..., width, height) grDevices::jpeg(...,
                                                               width = input$pcoa_width*2.5,
                                                               height = input$pcoa_height*2.5,
                                                               res = input$pcoa_resolution,
                                                               quality = 100,
                                                               units = "px")

      } else if (input$pcoa_plot_format == ".png") {

        device <- function(..., width, height) grDevices::png(...,
                                                              width = input$pcoa_width*2.5,
                                                              height = input$pcoa_height*2.5,
                                                              res = input$pcoa_resolution,
                                                              # quality = 100,
                                                              units = "px")

      } else if (input$pcoa_plot_format == ".tiff") {

        device <- function(..., width, height) grDevices::tiff(...,
                                                               width = input$pcoa_width*2.5,
                                                               height = input$pcoa_height*2.5,
                                                               res = input$pcoa_resolution,
                                                               # quality = 100,
                                                               units = "px")

      } else if (input$pcoa_plot_format == ".svg") {

        device <- function(..., width, height) grDevices::svg(...,
                                                              width = input$pcoa_width/75,
                                                              height = input$pcoa_height/75)

      }

      ggsave(filename = file,
             plot = pcoa_plot(),
             device = device)

    } else {

      if (input$pcoa_plot_format == ".eps") {

        device <- "eps"

      } else if (input$pcoa_plot_format == ".pdf") {

        device <- "pdf"

      }

      ggsave(filename = file,
             plot = pcoa_plot(),
             width = input$pcoa_width/110,
             height = input$pcoa_height/110,
             limitsize = FALSE,
             dpi = input$pcoa_resolution)

    }

  }

)

############################### PLOT FROM STRUCTURE SECTION #####

### Reactive dataset FROM STRUCTURE - input
Data_DA_Str <- reactive({

  req(input$Data_DA_Str)

  File <- input$Data_DA_Str

  if (has_extension(path = File$datapath,
                    ext = "Q")) {

    df2 <- read.table(File$datapath)

    names(df2) <- c(paste0(rep(x = "K",
                               times = ncol(df2)),
                           seq(from = 1,
                               to = ncol(df2),
                               by = 1)))

    output$dataFormat <- renderText("Q")
    outputOptions(output, "dataFormat", suspendWhenHidden = FALSE)

    if (!is.null(input$FAM_file)) {

      req(input$FAM_file)

      File_FAM <- input$FAM_file

      df_FAM <- read.table(File_FAM$datapath)

      names(df_FAM)[c(1, 2)] <- c("Pop_ID", "Sample_ID")

      df2 <- cbind("Sample_ID" = df_FAM$Sample_ID,
                   "Pop_ID" = df_FAM$Pop_ID,
                   df2)

    }

  } else {

    df2 <- read.csv(File$datapath,
                    header = TRUE,
                    sep = input$separator2,
                    quote = input$quote2)

    output$dataFormat <- renderText("csvtxt")
    outputOptions(output, "dataFormat", suspendWhenHidden = FALSE)

  }

  updateSelectInput(session,
                    inputId = "bottom_left",
                    choices = names(df2[, grepl(pattern = "K",
                                                x = names(df2))]),
                    selected = names(df2[, grepl(pattern = "K",
                                                 x = names(df2))])[1])

  updateSelectInput(session,
                    inputId = "bottom_right",
                    choices = names(df2[, grepl(pattern = "K",
                                                x = names(df2))]),
                    selected = names(df2[, grepl(pattern = "K",
                                                 x = names(df2))])[2])

  COLNAMES_K <- names(df2[, grepl(pattern = "K",
                                  x = names(df2))])

  COLNAMES <- setdiff(names(df2),
                      COLNAMES_K)

  if ("Sample_ID" %in% colnames(df2)) {

    updateSelectInput(session,
                      inputId = "group_barplot_by",
                      choices = c("-----",
                                  COLNAMES[-1]))

    if (all(c("Pop_ID", "Loc_ID") %in% colnames(df2))) {

      updateSelectInput(session,
                        inputId = "group_barplot_by",
                        choices = c("-----",
                                    COLNAMES[-1],
                                    "Pop_ID & Loc_ID"))

    }

  } else {

    updateSelectInput(session,
                      inputId = "group_barplot_by",
                      choices = c("-----",
                                  COLNAMES))

    if (all(c("Pop_ID", "Loc_ID") %in% colnames(df2))) {

      updateSelectInput(session,
                        inputId = "group_barplot_by",
                        choices = c("-----",
                                    COLNAMES,
                                    "Pop_ID & Loc_ID"))

    }

  }

  return(df2)

})



### Output table imported 2
output$table_import2 <- DT::renderDataTable({

  Data_DA_Str()

}, options = list(pageLength = 10))



### Number of cluster from STRUCTURE
num_cluster_STR <- reactive({

  num_cluster_STR <- length(names(Data_DA_Str()[, grepl(pattern = "K",
                                                        x = names(Data_DA_Str()))]))

  return(num_cluster_STR)

})

output$cluster_number <- renderText({

  num_cluster_STR()

})



#### UI to display the location mark option
output$show_location_marks <- renderUI({

  if ("Loc_ID" %in% colnames(Data_DA_Str())) {

    checkboxInput(inputId = "location_marks",
                  label = h5(icon(name = "map-marker-alt"),
                             "Show location marks"),
                  width = "100%")

  }

})



##### Reactive dataset for the Barplot #####
Data_plot <- reactive({

  Dataset <- Data_DA_Str()

  if (!"Sample_ID" %in% colnames(Dataset)) {

    Dataset <- cbind("Sample_ID" = seq(from = 1,
                                       to = length(Dataset[, 2]),
                                       by = 1),
                     Dataset)

  }

  ### COLNAMES selection for variables to use in the merge
  COLNAMES_K <- names(Dataset[, grepl(pattern = "K",
                                      x = names(Dataset))])

  COLNAMES <- setdiff(names(Dataset),
                      COLNAMES_K)

  ### Defined order of the samples
  if (input$sort == "original") {

    Dataset_m <- melt(data = Dataset,
                      id.vars = COLNAMES,
                      measure.vars = COLNAMES_K,
                      value.name = "Q"
    )



    ### Sort Sample_ID by original order
    Dataset_m$Sample_ID <- factor(Dataset_m$Sample_ID,
                                  levels = unique(Dataset_m$Sample_ID)
    )

  } else if (input$sort == "Q") {

    # removed Sample_ID e Pop_ID column (if they're present)
    Dataset_c <- Dataset[, COLNAMES_K]

    # picks the max cluster and match the max value to cluster
    max_val <- apply(X = Dataset_c, MARGIN = 1, FUN = max) # takes the max value
    # present in every column for every row
    match_val <- vector(length = nrow(Dataset_c))

    for(i in seq(from = 1, to = nrow(Dataset_c), by = 1)) match_val[i] <-
      match(max_val[i], Dataset_c[i,])

    # add max_val and match_val to dataframe
    Dataset_q <- Dataset_c
    Dataset_q$maxval <- max_val
    Dataset_q$matchval <- match_val

    # unite again Sample_ID and Pop_ID columns
    Dataset_q <- cbind(Dataset[, 1:length(COLNAMES)],
                       Dataset_q)

    # Generic function to give the original names back
    names(Dataset_q)[1:length(COLNAMES)] <- COLNAMES

    # order dataframe ascending match and decending max
    Dataset_q <- Dataset_q[with(Dataset_q, order(matchval, -maxval)), ]

    # removed columns maxval and matchval
    Dataset_q$maxval <- NULL
    Dataset_q$matchval <- NULL

    # reshape to long format
    Dataset_m <- melt(Dataset_q,
                      id.vars = COLNAMES,
                      measure.vars = COLNAMES_K,
                      value.name = "Q")



    ### Sort Sample_ID by q
    Dataset_m$Sample_ID <- factor(Dataset_m$Sample_ID,
                                  levels = unique(Dataset_m$Sample_ID))

  } else if (input$sort == "alphabetic") {

    Dataset_m <- melt(data = Dataset,
                      id.vars = COLNAMES,
                      measure.vars = COLNAMES_K,
                      value.name = "Q"
    )

    Dataset_m <- Dataset_m[order(Dataset_m$Sample_ID), ]



    ### Sort Sample_ID by alphabetic order
    Dataset_m$Sample_ID <- factor(Dataset_m$Sample_ID,
                                  levels = unique(Dataset_m$Sample_ID))

  }

  colnames(Dataset_m)[which(names(Dataset_m) == "variable")] <- "Cluster"

  Dataset_m$Cluster <- factor(Dataset_m$Cluster,
                              levels = COLNAMES_K,
                              labels = c(seq(from = 1,
                                             to = length(COLNAMES_K),
                                             by = 1)
                              )
  )

  if (all(c("Pop_ID", "Loc_ID") %in% colnames(Dataset))) {

    # Pop_ID and #Loc_ID become factors for the Labels colors and shape;
    # the order is alphabetical for the facet_grid option
    Dataset_m$Pop_ID <- factor(Dataset_m$Pop_ID)

    Dataset_m$Loc_ID <- factor(Dataset_m$Loc_ID)

  } else if ("Pop_ID" %in% colnames(Dataset) &&
             !"Loc_ID" %in% colnames(Dataset)) {

    Dataset_m$Pop_ID <- factor(Dataset_m$Pop_ID)

  } else if ("Loc_ID" %in% colnames(Dataset) &&
             !"Pop_ID" %in% colnames(Dataset)) {

    Dataset_m$Loc_ID <- factor(Dataset_m$Loc_ID)

  }

  return(Dataset_m)

})



### STRUCTURE Barplot ggplot
Structure_Plot <- reactive({

  Dataset <- Data_DA_Str()

  if ("Loc_ID" %in% colnames(Dataset)) {

    Dataset$Loc_ID <- factor(Dataset$Loc_ID)

  }

  # Vector of shapes for the collection site
  Collection_site_shape <- as.factor(seq(from = 0,
                                         to = 25,
                                         by = 1))

  # Added Sample_ID if it's not present in the first dataset
  if (!"Sample_ID" %in% colnames(Dataset)) {

    Dataset <- cbind("Sample_ID" = seq(from = 1,
                                       to = length(Dataset[, 2]),
                                       by = 1),
                     Dataset)

  }

  Dataset_m <- Data_plot()

  # Cluster color vector
  Colours <- reactive({

    if (input$barplot_colours_vector == "Customized") {

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
                   input$colour_20)

    } else {

      input$resample_gray_scale

      isolate({

        Colours <- sample(x = gray.colors(n = 50),
                          size = 20,
                          replace = FALSE)

      })

    }

    return(Colours)

  })

  if (all(c("Pop_ID", "Loc_ID") %in% colnames(Dataset))) {

    # number of colours needed
    numColors <- length(levels(Dataset_m$Pop_ID))

    # genero un numero di colori uguale al numero che mi serve (potrebbe non funzionare per k > 40)
    Pop_Col <- distinctColorPalette(k = numColors)
    names(Pop_Col) <- levels(Dataset_m$Pop_ID)

    # every variable of the dataset has ben changed in "character"
    Palette_Match <- as.data.frame(Pop_Col, stringsAsFactors = FALSE)

    # Dataframe with the enconding "populations - colors"
    Palette_Match$Pop_ID <- rownames(Palette_Match)

    # presa la colonna Pop_ID dal dataset ordinato (considerando solo quella per un cluster)
    y <- split(Dataset_m, f = Dataset_m$Cluster)
    Pop_ID <- data.frame(Pop_ID = y$`1`$Pop_ID)

    vec <- c(Palette_Match$Pop_Col[match(Pop_ID$Pop_ID, Palette_Match$Pop_ID)])

    if (input$group_barplot_by == "-----") {

      # ggplot
      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        # sometimes the sum of Q is not 1 ma "1.001", so better to abound
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1,
                                     colour = vec),
          axis.ticks = element_line(size = 0.3),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Pop_ID") {

      facet_formula <- as.formula(paste("~", input$group_barplot_by))

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(facet_formula,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Loc_ID") {

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(~Loc_ID,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          strip.text = element_text(size = input$axis_title_size*2),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Pop_ID & Loc_ID") {

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(Pop_ID ~ Loc_ID,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          strip.text = element_text(size = input$axis_title_size*2),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    }

    if (input$location_marks == TRUE) {

      p <- p +
        geom_point(data = Dataset,
                   aes(x = Sample_ID,
                       y = 1.05,
                       shape = Loc_ID,
                       colour = Loc_ID),
                   size = 1) +
        scale_shape_manual(values = Collection_site_shape)
    }

  } else if ("Pop_ID" %in% colnames(Dataset) &&
             !"Loc_ID" %in% colnames(Dataset)) {

    # number of colours needed
    numColors <- length(levels(Dataset_m$Pop_ID))

    Pop_Col <- distinctColorPalette(k = numColors)
    names(Pop_Col) <- levels(Dataset_m$Pop_ID)

    # every variable of the dataset has ben changed in "character"
    Palette_Match <- as.data.frame(Pop_Col, stringsAsFactors = FALSE)

    # Dataframe with the enconding "populations - colors"
    Palette_Match$Pop_ID <- rownames(Palette_Match)

    y <- split(Dataset_m, f = Dataset_m$Cluster)
    Pop_ID <- data.frame(Pop_ID = y$`1`$Pop_ID)

    vec <- c(Palette_Match$Pop_Col[match(Pop_ID$Pop_ID, Palette_Match$Pop_ID)])

    if (input$group_barplot_by == "-----") {

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        scale_y_continuous(limits = c(0, 1.01),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title) +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1,
                                     colour = vec),
          axis.ticks = element_line(size = 0.3),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Pop_ID") {

      facet_formula <- as.formula(paste("~", input$group_barplot_by))

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(facet_formula,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.01),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title) +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          strip.text = element_text(size = input$axis_title_size*2),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    }

  } else if ("Loc_ID" %in% colnames(Dataset) &&
             !"Pop_ID" %in% colnames(Dataset)) {

    if (input$group_barplot_by == "-----") {

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Loc_ID") {

      facet_formula <- as.formula(paste("~", input$group_barplot_by))

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(facet_formula,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          strip.text = element_text(size = input$axis_title_size*2),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    }

    if (input$location_marks == TRUE) {

      p <- p +
        geom_point(data = Dataset,
                   aes(x = Sample_ID,
                       y = 1.05,
                       shape = Loc_ID,
                       colour = Loc_ID),
                   size = 1) +
        scale_shape_manual(values = Collection_site_shape)

    }

  } else if (all(!c("Pop_ID", "Loc_ID") %in% colnames(Dataset))) {

    # ggplot
    p <- ggplot() +
      geom_bar(data = Dataset_m,
               aes(x = Sample_ID,
                   y = Q,
                   fill = Cluster),
               stat = "identity",
               colour = "black",
               size = 0.2,
               position = input$barpos) +
      scale_y_continuous(limits = c(0, 1.06),
                         breaks = c(seq(from = 0,
                                        to = 1,
                                        by = 0.1))) +
      scale_fill_manual("Cluster",
                        values = Colours()) +
      labs(y = "Admixture index [%]",
           title = input$barplot_title,
           colour = "Collection site",
           shape = "Collection site") +
      theme(
        axis.title = element_text(size = input$axis_title_size*2),
        axis.text.y = element_text(size = input$y_label_size),
        axis.text.x = element_text(size = input$x_label_size,
                                   angle = input$x_label_angle,
                                   hjust = 1),
        axis.ticks = element_line(size = 0.3),
        plot.title = element_text(size = input$axis_title_size*2,
                                  hjust = 0.5),
        legend.title = element_text(size = input$axis_title_size*2)
      )

  }

  return(p)

})

#####===== STRUCTURE Barplot plotly =====#####
output$structure_plot <- renderPlotly({

  ggplotly(Structure_Plot(),
           width = input$barplot_width,
           height = input$barplot_height)

})

###= Download barplot =#####
output$download_barplot <- downloadHandler(

  filename <- function() {

    if (input$barplot_title == "") {

      paste0("barplot", input$barplot_format)

    } else {

      paste0(input$barplot_title, input$barplot_format)

    }

  },

  content <- function(file) {

    if (input$barplot_format == ".bmp" |
        input$barplot_format == ".jpeg" |
        input$barplot_format == ".png" |
        input$barplot_format == ".tiff" |
        input$barplot_format == ".svg") {

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

      } else if (input$barplot_format == ".svg") {

        device <- function(..., width, height) grDevices::svg(...,
                                                              width = input$barplot_width/75,
                                                              height = input$barplot_height/75)

      }

      ggsave(filename = file,
             plot = Structure_Plot(),
             device = device)

    } else {

      if (input$barplot_format == ".eps") {

        device <- "eps"

      } else if (input$barplot_format == ".pdf") {

        device <- "pdf"

      }

      ggsave(filename = file,
             plot = Structure_Plot(),
             width = input$barplot_width/110,
             height = input$barplot_height/110,
             limitsize = FALSE,
             dpi = input$barplot_resolution)

    }

  }

)

###= Membership STRUCTURE cluster =#####
Members_STR <- reactive({

  Dataset_DA <- Data_DA_Str()

  Data_cluster_STR <- Dataset_DA[, grepl(pattern = "K",
                                         x = names(Dataset_DA))]

  if ("Sample_ID" %in% colnames(Dataset_DA)) {

    Dataset_DA$Sample_ID <- factor(Dataset_DA$Sample_ID,
                                   levels = Dataset_DA$Sample_ID)

    rownames(Data_cluster_STR) <- Dataset_DA$Sample_ID

  }

  Data_cluster_STR$Members_STR <- apply(X = Data_cluster_STR,
                                        MARGIN = 1,
                                        FUN = which.max)

  Data_cluster_STR <- as.data.frame(cbind("Sample_ID" = rownames(Data_cluster_STR),
                                          "Members_STR" = Data_cluster_STR$Members_STR))

  return(Data_cluster_STR)

})



#####===== Triangleplot =====#####

### Cluster number triangle plot
output$cluster_number_2 <- renderText({

  length(names(Data_DA_Str()[, grepl(pattern = "K",
                                     x = names(Data_DA_Str()))])
  )

})



### Triangleplot with plotly
output$triangle_plot <- renderPlotly({

  # Defined how the points should be represented (because for the top vertex we
  # need the sum of the columns not chosen)
  a <- rowSums(Data_DA_Str()[, grepl(pattern = "K",
                                     x = names(Data_DA_Str()))]) -
    (Data_DA_Str()[, input$bottom_left] + Data_DA_Str()[, input$bottom_right])

  b <- Data_DA_Str()[, input$bottom_left]

  c <- Data_DA_Str()[, input$bottom_right]

  # Function to define the axis
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

  # Defined list for the margins
  margin <- list(
    l = 40,
    r = 40,
    b = 40,
    t = 70,
    pad = 4
  )

  # Triangleplot
  Triangleplot <- plot_ly(data = Data_DA_Str(),
                          width = input$triangleplot_width,
                          height = input$triangleplot_height)


  if (all(c("Sample_ID", "Pop_ID", "Loc_ID") %in% colnames(Data_DA_Str()))) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        text = ~Sample_ID,
        name = ~Pop_ID,
        colors = ~Pop_ID,
        symbol = ~Loc_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if (all(c("Sample_ID", "Pop_ID") %in% colnames(Data_DA_Str())) &&
             !"Loc_ID" %in% colnames(Data_DA_Str())) {

    Triangleplot <- Triangleplot %>%
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
          size = input$triangleplot_symbol_size
        )
      )

  } else if (all(c("Sample_ID", "Loc_ID") %in% colnames(Data_DA_Str())) &&
             !"Pop_ID" %in% colnames(Data_DA_Str())) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        text = ~Sample_ID,
        name = ~Loc_ID,
        colors = ~Loc_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if (all(c("Pop_ID", "Loc_ID") %in% colnames(Data_DA_Str())) &&
             !"Sample_ID" %in% colnames(Data_DA_Str())) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        name = ~Pop_ID,
        colors = ~Pop_ID,
        symbol = ~Loc_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if ("Sample_ID" %in% colnames(Data_DA_Str()) &&
             !all(c("Pop_ID", "Loc_ID") %in% colnames(Data_DA_Str()))) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        text = ~Sample_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if ("Pop_ID" %in% colnames(Data_DA_Str()) &&
             !all(c("Sample_ID", "Loc_ID") %in% colnames(Data_DA_Str()))) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        name = Pop_ID,
        colors = ~Pop_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if ("Loc_ID" %in% colnames(Data_DA_Str()) &&
             !all(c("Sample_ID", "Pop_ID") %in% colnames(Data_DA_Str()))) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        name = ~Loc_ID,
        colors = ~Loc_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  }

  Triangleplot %>% layout(
    title = input$triangleplot_title,
    margin = margin,
    showlegend = TRUE,
    ternary = list(
      sum = 1,
      aaxis = axis("All others"),
      baxis = axis(input$bottom_left),
      caxis = axis(input$bottom_right)
    )
  )

})



###= Download triangle plot =#####
output$download_triangleplot <- downloadHandler(

  filename <- function() {

    input$triangleplot_title

  },

  content <- function(file) {

    a <- rowSums(Data_DA_Str()[, grepl(pattern = "K",
                                       x = names(Data_DA_Str()))]) -
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

    Triangleplot <- plot_ly(data = Data_DA_Str(),
                            width = input$triangleplot_width,
                            height = input$triangleplot_height)


    if (all(c("Sample_ID", "Pop_ID", "Loc_ID") %in% colnames(Data_DA_Str()))) {

      Triangleplot <- Triangleplot %>%
        add_trace(
          type = "scatterternary",
          mode = "markers",
          a = ~a,
          b = ~b,
          c = ~c,
          text = ~Sample_ID,
          name = ~Pop_ID,
          colors = ~Pop_ID,
          symbol = ~Loc_ID,
          marker = list(
            size = input$triangleplot_symbol_size
          )
        )

    } else if (all(c("Sample_ID", "Pop_ID") %in% colnames(Data_DA_Str())) &&
               !"Loc_ID" %in% colnames(Data_DA_Str())) {

      Triangleplot <- Triangleplot %>%
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
            size = input$triangleplot_symbol_size
          )
        )

    } else if (all(c("Sample_ID", "Loc_ID") %in% colnames(Data_DA_Str())) &&
               !"Pop_ID" %in% colnames(Data_DA_Str())) {

      Triangleplot <- Triangleplot %>%
        add_trace(
          type = "scatterternary",
          mode = "markers",
          a = ~a,
          b = ~b,
          c = ~c,
          text = ~Sample_ID,
          name = ~Loc_ID,
          colors = ~Loc_ID,
          marker = list(
            size = input$triangleplot_symbol_size
          )
        )

    } else if (all(c("Pop_ID", "Loc_ID") %in% colnames(Data_DA_Str())) &&
               !"Sample_ID" %in% colnames(Data_DA_Str())) {

      Triangleplot <- Triangleplot %>%
        add_trace(
          type = "scatterternary",
          mode = "markers",
          a = ~a,
          b = ~b,
          c = ~c,
          name = ~Pop_ID,
          colors = ~Pop_ID,
          symbol = ~Loc_ID,
          marker = list(
            size = input$triangleplot_symbol_size
          )
        )

    } else if ("Sample_ID" %in% colnames(Data_DA_Str()) &&
               !all(c("Pop_ID", "Loc_ID") %in% colnames(Data_DA_Str()))) {

      Triangleplot <- Triangleplot %>%
        add_trace(
          type = "scatterternary",
          mode = "markers",
          a = ~a,
          b = ~b,
          c = ~c,
          text = ~Sample_ID,
          marker = list(
            size = input$triangleplot_symbol_size
          )
        )

    } else if ("Pop_ID" %in% colnames(Data_DA_Str()) &&
               !all(c("Sample_ID", "Loc_ID") %in% colnames(Data_DA_Str()))) {

      Triangleplot <- Triangleplot %>%
        add_trace(
          type = "scatterternary",
          mode = "markers",
          a = ~a,
          b = ~b,
          c = ~c,
          name = Pop_ID,
          colors = ~Pop_ID,
          marker = list(
            size = input$triangleplot_symbol_size
          )
        )

    } else if ("Loc_ID" %in% colnames(Data_DA_Str()) &&
               !all(c("Sample_ID", "Pop_ID") %in% colnames(Data_DA_Str()))) {

      Triangleplot <- Triangleplot %>%
        add_trace(
          type = "scatterternary",
          mode = "markers",
          a = ~a,
          b = ~b,
          c = ~c,
          name = ~Loc_ID,
          colors = ~Loc_ID,
          marker = list(
            size = input$triangleplot_symbol_size
          )
        )

    } else {

      Triangleplot <- Triangleplot %>%
        add_trace(
          type = "scatterternary",
          mode = "markers",
          a = ~a,
          b = ~b,
          c = ~c,
          marker = list(
            size = input$triangleplot_symbol_size
          )
        )

    }

    Triangleplot <- Triangleplot %>% layout(
      title = input$triangleplot_title,
      margin = margin,
      showlegend = TRUE,
      ternary = list(
        sum = 1,
        aaxis = axis("All others"),
        baxis = axis(input$bottom_left),
        caxis = axis(input$bottom_right)
      )
    )

    if (input$triangleplot_title != "") {

      orca(p = Triangleplot,
           file = paste0(input$triangleplot_title, input$triangleplot_format),
           width = input$triangleplot_width*1.5,
           height = input$triangleplot_height*1.5)

    }

    else {

      orca(p = Triangleplot,
           file = paste0("download_triangleplot", input$triangleplot_format),
           width = input$triangleplot_width*1.5,
           height = input$triangleplot_height*1.5)

    }

  }

)



############################### COMPARING PARTITIONS #####

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

  Dataset_PER <- Data_PER_Str()

  Dataset_DA <- Data_DA_Str()

  # Data_cluster_STR <- Data_DA_Str()[, -2]
  #
  # Data_cluster_STR$Sample_ID <- factor(Data_cluster_STR$Sample_ID,
  #                                      levels = Data_cluster_STR$Sample_ID)
  #
  # rownames(Data_cluster_STR) <- Data_cluster_STR$Sample_ID
  #
  # Data_cluster_STR$Sample_ID <- NULL
  #
  # Data_cluster_STR[apply(Data_cluster_STR, 1, function(x) any(x >= 0.75)), ]
  #
  # Names_Data_cluster_STR <- as.factor(c(rownames(Data_cluster_STR)))

  Members_hclust <- data.frame("Members_hclust" = cutree(tree = dend(),
                                                         k = input$cluster_count))

  if (!"Sample_ID" %in% colnames(Dataset_PER) &&
      !"Sample_ID" %in% colnames(Dataset_DA)) {

    if ("Pop_ID" %in% colnames(Dataset_DA)) {

      Data_partitions <- cbind(rownames(Members_hclust),
                               Dataset_DA$Pop_ID,
                               Members_hclust,
                               Members_STR()[, -1])

      colnames(Data_partitions) <- c("Sample_ID",
                                     "Pop_ID",
                                     "Hierarchic",
                                     "Population_analysis")

    } else {

      Data_partitions <- cbind(rownames(Members_hclust),
                               Members_hclust,
                               Members_STR()[, -1])

      colnames(Data_partitions) <- c("Sample_ID",
                                     "Hierarchic",
                                     "Population_analysis")

    }

  } else if ("Sample_ID" %in% colnames(Dataset_PER) &&
             !"Sample_ID" %in% colnames(Dataset_DA)) {

    if ("Pop_ID" %in% colnames(Dataset_DA)) {

      Data_partitions <- cbind(rownames(Members_hclust),
                               Dataset_DA$Pop_ID,
                               Members_hclust,
                               Members_STR()[, -1])

      colnames(Data_partitions) <- c("Sample_ID",
                                     "Pop_ID",
                                     "Hierarchic",
                                     "Population_analysis")

    } else {

      Data_partitions <- cbind(rownames(Members_hclust),
                               Members_hclust,
                               Members_STR()[, -1])

      colnames(Data_partitions) <- c("Sample_ID",
                                     "Hierarchic",
                                     "Population_analysis")

    }

  } else if (!"Sample_ID" %in% colnames(Dataset_PER) &&
             "Sample_ID" %in% colnames(Dataset_DA)) {

    if ("Pop_ID" %in% colnames(Dataset_DA)) {

      Data_partitions <- cbind(Dataset_DA$Sample_ID ,
                               Dataset_DA$Pop_ID,
                               Members_hclust,
                               Members_STR()[, -1])

      colnames(Data_partitions) <- c("Sample_ID",
                                     "Pop_ID",
                                     "Hierarchic",
                                     "Population_analysis")

    } else {

      Data_partitions <- cbind(Dataset_DA$Sample_ID,
                               Members_hclust,
                               Members_STR()[, -1])

      colnames(Data_partitions) <- c("Sample_ID",
                                     "Hierarchic",
                                     "Population_analysis")

    }

  } else if ("Sample_ID" %in% colnames(Dataset_PER) &&
             "Sample_ID" %in% colnames(Dataset_DA)) {

    if ("Pop_ID" %in% colnames(Dataset_DA)) {

      Data_partitions <- cbind(Dataset_DA$Sample_ID ,
                               Dataset_DA$Pop_ID,
                               Members_hclust,
                               Members_STR()[, -1])

      colnames(Data_partitions) <- c("Sample_ID",
                                     "Pop_ID",
                                     "Hierarchic",
                                     "Population_analysis")

    } else {

      Data_partitions <- cbind(Dataset_DA$Sample_ID,
                               Members_hclust,
                               Members_STR()[, -1])

      colnames(Data_partitions) <- c("Sample_ID",
                                     "Hierarchic",
                                     "Population_analysis")

    }

  }

  return(Data_partitions)

})



### Barplot cluster hierarchical
output$barplot_cluster_hierarchical <- renderPlotly({

  Comparison_table <- Comparison_table()

  Comparison_table$Hierarchic <- factor(Comparison_table$Hierarchic)

  Barplot_hierarchical <- ggplot(data = Comparison_table,
                                 aes(x = Hierarchic)) +
    geom_bar(width = 0.3) +
    labs(x = "Hierarchic cluster",
         y = "Units") +
    scale_y_continuous(position = "right") +
    coord_flip()

  ggplotly(Barplot_hierarchical,
           width = 825,
           height = 220)

})



### Barplot cluster hierarchical
output$barplot_cluster_pop_analysis <- renderPlotly({

  Comparison_table <- Comparison_table()

  Comparison_table$Population_analysis <- factor(Comparison_table$Population_analysis)

  Barplot_population_analysis <- ggplot(data = Comparison_table,
                                        aes(x = Population_analysis)) +
    geom_bar(width = 0.3,
             colour = "gray60",
             fill = "gray60") +
    labs(x = "Pop analysis cluster",
         y = "Units")

  ggplotly(Barplot_population_analysis,
           width = 180,
           height = 560)

})



### Partitions indeces
output$agreement_value <- renderText({

  if (input$agreement_index == "rand") {

    arandi(Comparison_table()$Hierarchic,
           Comparison_table()$Population_analysis,
           adjust = FALSE)

  } else if (input$agreement_index == "crand") {

    arandi(Comparison_table()$Hierarchic,
           Comparison_table()$Population_analysis,
           adjust = TRUE)

  }

})



### Table to compare partitions
output$comparison_table <- DT::renderDataTable({

  Comparison_table()

}, options = list(pageLength = 25))



###= Table plot to compare partitions =#####
Tableplot <- metaReactive({

  Comparison_table <- Comparison_table()

  Table <- as.data.frame(table(Comparison_table$Hierarchic,
                               Comparison_table$Population_analysis))

  colnames(Table) <- c("Hierarchic",
                       "Population_analysis",
                       "Frequency")

  Tableplot <- ggplot(Table,
                      aes(x = Hierarchic,
                          y = Population_analysis,
                          fill = Frequency)) +
    geom_tile(color = "black") +
    scale_fill_gradient(low = "ghostwhite",
                        high = "firebrick") +
    labs(x = "Hierarchical cluster",
         y = "Population analysis cluster",
         fill = "Common units",
         title = input$comparison_plot_title) +
    theme(
      axis.title = element_text(size = input$comp_axis_title_size*2),
      axis.text.y = element_text(size = input$comp_y_label_size,
                                 angle = input$comp_label_angle),
      axis.text.x = element_text(size = input$comp_x_label_size,
                                 angle = input$comp_label_angle),
      axis.ticks = element_line(size = 0.3),
      plot.title = element_text(size = input$comp_axis_title_size*2,
                                hjust = 0.5),
      legend.title = element_text(size = input$comp_axis_title_size*2)
    )

})

# ggplotly Tableplot
output$comparison_plot <- renderPlotly({

  ggplotly(Tableplot(),
           width = input$comparison_plot_width,
           height = input$comparison_plot_height)

})



###= Contingency table =#####
output$contingency_table <- renderPrint({

  Comparison_table() %>%
    tabyl(Population_analysis,
          Hierarchic) %>%
    adorn_totals(c("row", "col"))

  # addmargins(table(Comparison_table()$Population_analysis,
  #                  Comparison_table()$Hierarchic))

})



###= Download Tableplot =#####
output$download_comparison_plot <- downloadHandler(

  filename <- function() {

    if (input$comparison_plot_title == "") {

      paste0("comparison_plot", input$comparison_plot_format)

    } else {

      paste0(input$comparison_plot_title, input$comparison_plot_format)

    }

  },

  content <- function(file) {

    if (input$comparison_plot_format == ".bmp" |
        input$comparison_plot_format == ".jpeg" |
        input$comparison_plot_format == ".png" |
        input$comparison_plot_format == ".tiff" |
        input$comparison_plot_format == ".svg") {

      if (input$comparison_plot_format == ".bmp") {

        device <- function(..., width, height) grDevices::bmp(...,
                                                              width = input$comparison_plot_width*2.5,
                                                              height = input$comparison_plot_height*2.5,
                                                              res = input$comparison_plot_resolution,
                                                              # quality = 100,
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
                                                              # quality = 100,
                                                              units = "px")

      } else if (input$comparison_plot_format == ".tiff") {

        device <- function(..., width, height) grDevices::tiff(...,
                                                               width = input$comparison_plot_width*2.5,
                                                               height = input$comparison_plot_height*2.5,
                                                               res = input$comparison_plot_resolution,
                                                               # quality = 100,
                                                               units = "px")

      } else if (input$comparison_plot_format == ".svg") {

        device <- function(..., width, height) grDevices::svg(...,
                                                              width = input$comparison_plot_width/75,
                                                              height = input$comparison_plot_height/75)

      }

      ggsave(filename = file,
             plot = Tableplot(),
             device = device)

    } else {

      if (input$comparison_plot_format == ".eps") {

        device <- "eps"

      } else if (input$comparison_plot_format == ".pdf") {

        device <- "pdf"

      }

      ggsave(filename = file,
             plot = Tableplot(),
             width = input$comparison_plot_width/110,
             height = input$comparison_plot_height/110,
             limitsize = FALSE,
             dpi = input$comparison_plot_resolution)

    }

  }

)



#####===== Download R code =====#####

###= Hierarchical cluster analysis =#####
observeEvent(input$show_r_code_dendrogram, {

  withBusyIndicatorServer(buttonId = "show_r_code_dendrogram", {

    showModal(modalDialog(
      title = "R Code - Dendrogram",
      tags$pre(
        id = "r_code_dendrogram",
        expandChain(
          library_code,
          Dendrogram_plot()
        ) %>% formatCode() %>% paste(collapse = "\n")
      ),
      footer = tagList(

        fluidRow(
          column(width = 8),
          column(width = 2,
                 align = "right",
                 withBusyIndicatorUI(
                   actionButton(inputId = "copyRCode_dendrogram",
                                label = h6("Copy to clipboard"))
                 )
          ),
          column(width = 2,
                 modalButton(h6("Dismiss"))
          )
        )

      ),
      size = "l",
      easyClose = TRUE
    ))

  })

})

observeEvent(input$copyRCode_dendrogram, {

  withBusyIndicatorServer(buttonId = "copyRCode_dendrogram", {

    clipr::write_clip(content =
                        as.character(expandChain(
                          library_code,
                          Dendrogram_plot()
                        ) %>% formatCode() %>% paste(collapse = "\n")
                        ),
                      allow_non_interactive = TRUE)

  })

})

###= PCoA =#####
observeEvent(input$show_r_code_pcoa, {

  withBusyIndicatorServer(buttonId = "show_r_code_pcoa", {

  showModal(modalDialog(
    title = "R Code - PCoA plot",
    tags$pre(
      id = "r_code_pcoa",
      expandChain(
        library_code,
        pcoa_plot()
      ) %>% formatCode() %>% paste(collapse = "\n")
    ),
    footer = tagList(

      fluidRow(
        column(width = 8),
        column(width = 2,
               align = "right",
               withBusyIndicatorUI(
               actionButton(inputId = "copyRCode_pcoa",
                            label = h6("Copy to clipboard"))
               )
        ),
        column(width = 2,
               modalButton(h6("Dismiss"))
        )
      )

    ),
    size = "l",
    easyClose = TRUE
  ))

  })

})

observeEvent(input$copyRCode_pcoa, {

  withBusyIndicatorServer(buttonId = "copyRCode_pcoa", {

  clipr::write_clip(content =
    as.character(expandChain(
      library_code,
      pcoa_plot()
    ) %>% formatCode() %>% paste(collapse = "\n")
  ),
  allow_non_interactive = TRUE)

  })

})

###= Barplot =#####
str_plot <- metaReactive({

  Dataset <- Data_DA_Str()

  if ("Loc_ID" %in% colnames(Dataset)) {

    Dataset$Loc_ID <- factor(Dataset$Loc_ID)

  }

  # Vector of shapes for the collection site
  Collection_site_shape <- as.factor(seq(from = 0,
                                         to = 25,
                                         by = 1))

  # Added Sample_ID if it's not present in the first dataset
  if (!"Sample_ID" %in% colnames(Dataset)) {

    Dataset <- cbind("Sample_ID" = seq(from = 1,
                                       to = length(Dataset[, 2]),
                                       by = 1),
                     Dataset)

  }

  Dataset_m <- Data_plot()

  # Cluster color vector
  Colours <- reactive({

    if (input$barplot_colours_vector == "Customized") {

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
                   input$colour_20)

    } else {

      input$resample_gray_scale

      isolate({

        Colours <- sample(x = gray.colors(n = 50),
                          size = 20,
                          replace = FALSE)

      })

    }

    return(Colours)

  })

  if (all(c("Pop_ID", "Loc_ID") %in% colnames(Dataset))) {

    # number of colours needed
    numColors <- length(levels(Dataset_m$Pop_ID))

    # genero un numero di colori uguale al numero che mi serve (potrebbe non funzionare per k > 40)
    Pop_Col <- distinctColorPalette(k = numColors)
    names(Pop_Col) <- levels(Dataset_m$Pop_ID)

    # every variable of the dataset has ben changed in "character"
    Palette_Match <- as.data.frame(Pop_Col, stringsAsFactors = FALSE)

    # Dataframe with the enconding "populations - colors"
    Palette_Match$Pop_ID <- rownames(Palette_Match)

    # presa la colonna Pop_ID dal dataset ordinato (considerando solo quella per un cluster)
    y <- split(Dataset_m, f = Dataset_m$Cluster)
    Pop_ID <- data.frame(Pop_ID = y$`1`$Pop_ID)

    vec <- c(Palette_Match$Pop_Col[match(Pop_ID$Pop_ID, Palette_Match$Pop_ID)])

    if (input$group_barplot_by == "-----") {

      # ggplot
      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        # sometimes the sum of Q is not 1 ma "1.001", so better to abound
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1,
                                     colour = vec),
          axis.ticks = element_line(size = 0.3),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Pop_ID") {

      facet_formula <- as.formula(paste("~", input$group_barplot_by))

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(facet_formula,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Loc_ID") {

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(~Loc_ID,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          strip.text = element_text(size = input$axis_title_size*2),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Pop_ID & Loc_ID") {

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(Pop_ID ~ Loc_ID,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          strip.text = element_text(size = input$axis_title_size*2),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    }

    if (input$location_marks == TRUE) {

      p <- p +
        geom_point(data = Dataset,
                   aes(x = Sample_ID,
                       y = 1.05,
                       shape = Loc_ID,
                       colour = Loc_ID),
                   size = 1) +
        scale_shape_manual(values = Collection_site_shape)
    }

  } else if ("Pop_ID" %in% colnames(Dataset) &&
             !"Loc_ID" %in% colnames(Dataset)) {

    # number of colours needed
    numColors <- length(levels(Dataset_m$Pop_ID))

    Pop_Col <- distinctColorPalette(k = numColors)
    names(Pop_Col) <- levels(Dataset_m$Pop_ID)

    # every variable of the dataset has ben changed in "character"
    Palette_Match <- as.data.frame(Pop_Col, stringsAsFactors = FALSE)

    # Dataframe with the enconding "populations - colors"
    Palette_Match$Pop_ID <- rownames(Palette_Match)

    y <- split(Dataset_m, f = Dataset_m$Cluster)
    Pop_ID <- data.frame(Pop_ID = y$`1`$Pop_ID)

    vec <- c(Palette_Match$Pop_Col[match(Pop_ID$Pop_ID, Palette_Match$Pop_ID)])

    if (input$group_barplot_by == "-----") {

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        scale_y_continuous(limits = c(0, 1.01),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title) +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1,
                                     colour = vec),
          axis.ticks = element_line(size = 0.3),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Pop_ID") {

      facet_formula <- as.formula(paste("~", input$group_barplot_by))

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     name = Pop_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(facet_formula,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.01),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title) +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          strip.text = element_text(size = input$axis_title_size*2),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    }

  } else if ("Loc_ID" %in% colnames(Dataset) &&
             !"Pop_ID" %in% colnames(Dataset)) {

    if (input$group_barplot_by == "-----") {

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    } else if (input$group_barplot_by == "Loc_ID") {

      facet_formula <- as.formula(paste("~", input$group_barplot_by))

      p <- ggplot() +
        geom_bar(data = Dataset_m,
                 aes(x = Sample_ID,
                     Site = Loc_ID,
                     y = Q,
                     fill = Cluster),
                 stat = "identity",
                 colour = "black",
                 size = 0.2,
                 position = input$barpos) +
        facet_grid(facet_formula,
                   scales = "free_x") +
        scale_y_continuous(limits = c(0, 1.06),
                           breaks = c(seq(from = 0,
                                          to = 1,
                                          by = 0.1))) +
        scale_fill_manual("Cluster",
                          values = Colours()) +
        labs(y = "Admixture index [%]",
             title = input$barplot_title,
             colour = "Collection site",
             shape = "Collection site") +
        theme(
          axis.title = element_text(size = input$axis_title_size*2),
          axis.text.y = element_text(size = input$y_label_size),
          axis.text.x = element_text(size = input$x_label_size,
                                     angle = input$x_label_angle,
                                     hjust = 1),
          axis.ticks = element_line(size = 0.3),
          strip.text = element_text(size = input$axis_title_size*2),
          plot.title = element_text(size = input$axis_title_size*2,
                                    hjust = 0.5),
          legend.title = element_text(size = input$axis_title_size*2)
        )

    }

    if (input$location_marks == TRUE) {

      p <- p +
        geom_point(data = Dataset,
                   aes(x = Sample_ID,
                       y = 1.05,
                       shape = Loc_ID,
                       colour = Loc_ID),
                   size = 1) +
        scale_shape_manual(values = Collection_site_shape)

    }

  } else if (all(!c("Pop_ID", "Loc_ID") %in% colnames(Dataset))) {

    # ggplot
    p <- ggplot() +
      geom_bar(data = Dataset_m,
               aes(x = Sample_ID,
                   y = Q,
                   fill = Cluster),
               stat = "identity",
               colour = "black",
               size = 0.2,
               position = input$barpos) +
      scale_y_continuous(limits = c(0, 1.06),
                         breaks = c(seq(from = 0,
                                        to = 1,
                                        by = 0.1))) +
      scale_fill_manual("Cluster",
                        values = Colours()) +
      labs(y = "Admixture index [%]",
           title = input$barplot_title,
           colour = "Collection site",
           shape = "Collection site") +
      theme(
        axis.title = element_text(size = input$axis_title_size*2),
        axis.text.y = element_text(size = input$y_label_size),
        axis.text.x = element_text(size = input$x_label_size,
                                   angle = input$x_label_angle,
                                   hjust = 1),
        axis.ticks = element_line(size = 0.3),
        plot.title = element_text(size = input$axis_title_size*2,
                                  hjust = 0.5),
        legend.title = element_text(size = input$axis_title_size*2)
      )

  }

})

observeEvent(input$show_r_code_barplot, {

  withBusyIndicatorServer(buttonId = "show_r_code_barplot", {

    showModal(modalDialog(
      title = "R Code - Barplot",
      tags$pre(
        id = "r_code_barplot",
        expandChain(
          library_code,
          str_plot()
        ) %>% formatCode() %>% paste(collapse = "\n")
      ),
      footer = tagList(

        fluidRow(
          column(width = 8),
          column(width = 2,
                 align = "right",
                 withBusyIndicatorUI(
                   actionButton(inputId = "copyRCode_barplot",
                                label = h6("Copy to clipboard"))
                 )
          ),
          column(width = 2,
                 modalButton(h6("Dismiss"))
          )
        )

      ),
      size = "l",
      easyClose = TRUE
    ))

  })

})

observeEvent(input$copyRCode_barplot, {

  withBusyIndicatorServer(buttonId = "copyRCode_barplot", {

    clipr::write_clip(content =
                        as.character(expandChain(
                          library_code,
                          str_plot()
                        ) %>% formatCode() %>% paste(collapse = "\n")
                        ),
                      allow_non_interactive = TRUE)

  })

})

###= Triangleplot =#####
Triangleplot <- metaReactive({

  a <- rowSums(Data_DA_Str()[, grepl(pattern = "K",
                                     x = names(Data_DA_Str()))]) -
    (Data_DA_Str()[, input$bottom_left] + Data_DA_Str()[, input$bottom_right])

  b <- Data_DA_Str()[, input$bottom_left]

  c <- Data_DA_Str()[, input$bottom_right]

  # Function to define the axis
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

  # Defined list for the margins
  margin <- list(
    l = 40,
    r = 40,
    b = 40,
    t = 70,
    pad = 4
  )

  # Triangleplot
  Triangleplot <- plot_ly(data = Data_DA_Str(),
                          width = input$triangleplot_width,
                          height = input$triangleplot_height)


  if (all(c("Sample_ID", "Pop_ID", "Loc_ID") %in% colnames(Data_DA_Str()))) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        text = ~Sample_ID,
        name = ~Pop_ID,
        colors = ~Pop_ID,
        symbol = ~Loc_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if (all(c("Sample_ID", "Pop_ID") %in% colnames(Data_DA_Str())) &&
             !"Loc_ID" %in% colnames(Data_DA_Str())) {

    Triangleplot <- Triangleplot %>%
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
          size = input$triangleplot_symbol_size
        )
      )

  } else if (all(c("Sample_ID", "Loc_ID") %in% colnames(Data_DA_Str())) &&
             !"Pop_ID" %in% colnames(Data_DA_Str())) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        text = ~Sample_ID,
        name = ~Loc_ID,
        colors = ~Loc_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if (all(c("Pop_ID", "Loc_ID") %in% colnames(Data_DA_Str())) &&
             !"Sample_ID" %in% colnames(Data_DA_Str())) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        name = ~Pop_ID,
        colors = ~Pop_ID,
        symbol = ~Loc_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if ("Sample_ID" %in% colnames(Data_DA_Str()) &&
             !all(c("Pop_ID", "Loc_ID") %in% colnames(Data_DA_Str()))) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        text = ~Sample_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if ("Pop_ID" %in% colnames(Data_DA_Str()) &&
             !all(c("Sample_ID", "Loc_ID") %in% colnames(Data_DA_Str()))) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        name = Pop_ID,
        colors = ~Pop_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else if ("Loc_ID" %in% colnames(Data_DA_Str()) &&
             !all(c("Sample_ID", "Pop_ID") %in% colnames(Data_DA_Str()))) {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        name = ~Loc_ID,
        colors = ~Loc_ID,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  } else {

    Triangleplot <- Triangleplot %>%
      add_trace(
        type = "scatterternary",
        mode = "markers",
        a = ~a,
        b = ~b,
        c = ~c,
        marker = list(
          size = input$triangleplot_symbol_size
        )
      )

  }

  Triangleplot %>% layout(
    title = input$triangleplot_title,
    margin = margin,
    showlegend = TRUE,
    ternary = list(
      sum = 1,
      aaxis = axis("All others"),
      baxis = axis(input$bottom_left),
      caxis = axis(input$bottom_right)
    )
  )

})

observeEvent(input$show_r_code_triangleplot, {

  withBusyIndicatorServer(buttonId = "show_r_code_triangleplot", {

    showModal(modalDialog(
      title = "R Code - Triangle plot",
      tags$pre(
        id = "r_code_triangleplot",
        expandChain(
          library_code,
          Triangleplot()
        ) %>% formatCode() %>% paste(collapse = "\n")
      ),
      footer = tagList(

        fluidRow(
          column(width = 8),
          column(width = 2,
                 align = "right",
                 withBusyIndicatorUI(
                   actionButton(inputId = "copyRCode_triangleplot",
                                label = h6("Copy to clipboard"))
                 )
          ),
          column(width = 2,
                 modalButton(h6("Dismiss"))
          )
        )

      ),
      size = "l",
      easyClose = TRUE
    ))

  })

})

observeEvent(input$copyRCode_triangleplot, {

  withBusyIndicatorServer(buttonId = "copyRCode_triangleplot", {

    clipr::write_clip(content =
                        as.character(expandChain(
                          library_code,
                          Triangleplot()
                        ) %>% formatCode() %>% paste(collapse = "\n")
                        ),
                      allow_non_interactive = TRUE)

  })

})

###= Comparison plot =#####
observeEvent(input$show_r_code_comparison_plot, {

  withBusyIndicatorServer(buttonId = "show_r_code_comparison_plot", {

    showModal(modalDialog(
      title = "R Code - Comparison plot",
      tags$pre(
        id = "r_code_comparison_plot",
        expandChain(
          library_code,
          Tableplot()
        ) %>% formatCode() %>% paste(collapse = "\n")
      ),
      footer = tagList(

        fluidRow(
          column(width = 8),
          column(width = 2,
                 align = "right",
                 withBusyIndicatorUI(
                   actionButton(inputId = "copyRCode_comparison_plot",
                                label = h6("Copy to clipboard"))
                 )
          ),
          column(width = 2,
                 modalButton(h6("Dismiss"))
          )
        )

      ),
      size = "l",
      easyClose = TRUE
    ))

  })

})

observeEvent(input$copyRCode_comparison_plot, {

  withBusyIndicatorServer(buttonId = "copyRCode_comparison_plot", {

    clipr::write_clip(content =
                        as.character(expandChain(
                          library_code,
                          Tableplot()
                        ) %>% formatCode() %>% paste(collapse = "\n")
                        ),
                      allow_non_interactive = TRUE)

  })

})

} # Closes SERVER
