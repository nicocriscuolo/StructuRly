################################### UI ########################################
ui <- fluidPage(
  useShinyjs(), # Recall every function of Shinyjs in the UI
    fluidRow(
        column(width = 12,
               p(h3(strong(em("StructuRly")),
                    strong("0.1.0"), "-",
                    "Elegant, detailed and interactive plots for",
                    a("STRUCTURE",
          href = "https://web.stanford.edu/group/pritchardlab/structure.html"),
                     "and",
                     a("ADMIXTURE",
                       href = "http://software.genetics.ucla.edu/admixture/"),
                     "population analysis",
                    align = "center")
               )
        )
    ),
br(),
br(),
    fluidRow(
        column(width = 2,
               actionButton(inputId = "import_data",
                            label = h4(icon(name = "upload"),
                                       "Import the data"),
                            width = "100%"),
br(),
br(),
  conditionalPanel(conditio = "input.import_data > 0",
            radioButtons(inputId = "analysis_type",
                         label = h5(icon(name = "list-ul"), "Choose an action"),
                         choices = list("Import raw genetic data" = 1,
                                        "Import population analysis" = 2,
                                        "Compare partitions" = 3),
                         selected = 1),
  conditionalPanel(condition = "input.analysis_type == 1",
            fileInput(inputId = "Data_PER_Str",
                      label = h5(icon(name = "file"),
                                 "Input file (.csv, .txt)"),
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                      ),
    conditionalPanel(condition = "output.fileUploaded_PER",
            actionButton(inputId = "check_table",
                         label = h4(icon(name = "check"),
                                    "Start the analysis!"),
                         width = "100%")
    )
  ),
  conditionalPanel(condition = "input.analysis_type == 2",
            fileInput(inputId = "Data_DA_Str",
                      label = h5(icon(name = "file"),
                                 "Input file (.csv, .txt, .Q)"),
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".Q")
                      ),
  conditionalPanel(condition = "output.dataFormat == 'Q'",
            fileInput(inputId = "FAM_file",
                      label = h5(icon(name = "file-alt"),
                                 "Input information file (.fam)"),
                      accept = c(".fam")
            )
  ),
  conditionalPanel(condition = "output.fileUploaded_DA",
             actionButton(inputId = "customize_plot",
                           label = h4(icon(name = "check"),
                                      "Start the analysis!"),
                           width = "100%")
  )
  )
  ),
br(),
br(),
br(),
    fluidRow(
      column(width = 5,
             actionButton(inputId = "open_structure",
                          label = h6(icon(name = "bar-chart"),
                                     "STRUCTURE")
             )
      ),
      column(width = 5,
             actionButton(inputId = "open_admixture",
                          label = h6(icon(name = "bar-chart"),
                                     "ADMIXTURE")
             )
      )
    ),
br(),
br(),
br(),
br(),
    fluidRow(
      column(width = 12,
             actionButton(inputId = "back_instructions",
                          label = h6(icon(name = "book"),
                                     "Instructions"),
                          width = "100%"
             ),
             shinyjs::hidden(actionButton(inputId = "back_results",
                                          label = h6(icon(name = "book"),
                                                     "StructuRly"),
                                          width = "100%"
             )
             )
      )
    )
        ), # Closes input column

######################### UI DATA FOR STRUCUTRE ####
        column(width = 10,
  shinyjs::hidden(mainPanel(id = "instructions",
                            # img(src = "Pipeline_Structurly.png")
                            h4("Detailed instructions for the usage of", em("StructuRly"), "will be soon available.."),
br(),
                            "GitHub page:", a("StructuRly",
                                              href = "https://github.com/nicocriscuolo/StructuRly"), "  "
                 )
  ),
  conditionalPanel(condition = "input.check_table == 0 &
                   input.customize_plot == 0 &
                   input.analysis_type != 3" ,
            mainPanel(id = "wallpaper",
                      img(src = "Wallpaper_StructuRly.png",
                          height = "650px",
                          width = "1150px"),
br(),
            a("Nicola Criscuolo",
              href = "mailto:nico.criscuolo981@gmail.com"),
            " - Department of Chemistry and Biology “A. Zambelli”, University of Salerno",
br(),
br(),
            a("Claudia Angelini",
              href = "mailto:c.angelini@na.iac.cnr.it"),
            " - Institute for Applications of Calculus “M. Picone”, National Research Council",
br(),
br(),
             "GitHub page:", a("StructuRly",
              href = "https://github.com/nicocriscuolo/StructuRly"), "  ",

br(),
br()
            )
  ),
  conditionalPanel(condition = "input.analysis_type == 1 &
                   input.check_table > 0",
                   mainPanel(id = "table_panel",
  tabsetPanel(type = "pills",
    tabPanel(title = h4("Input table"),
br(),
        fluidRow(
          column(width = 2,
                 radioButtons(inputId = "separator", label = h5("Separator"),
                          choices = c(Comma = ",",
                                      Semicolon = ";",
                                      Tab = "\t"),
                          selected = ",")
          ),
          column(width = 2,
             radioButtons(inputId = "quote", label = h5("Quote"),
                          choices = c(None = "",
                                      Single = "'",
                                      Double = '"'),
                          selected = "")
      ),
      column(width = 4, selectInput(inputId = "subset_variable",
                                    label = h5("Subset of the variables"),
                                    choices = "",
                                    multiple = TRUE),
             downloadButton(outputId = "download_subset",
                            label = "Download subset")
      )
    ),
br(),
    fluidRow(
      column(width = 12,
             dataTableOutput(outputId = "table_import")
      )
    )
  ),
  tabPanel(title = h4("Basic stats"),
br(),
    fluidRow(
      column(width = 4,
             selectInput(inputId = "stats_type",
                         label = h5("Choose information"),
                         choices = "")
      ),
  conditionalPanel(condition = "input.stats_type == 'Types of different alleles'",
      column(width = 3,
             selectInput(inputId = "locus_name",
                         label = h5("Select locus"),
                         choices = "")
      )
  ),
      column(width = 7,
             uiOutput(outputId = "hw.test_sliderInput")
      )
    ),
br(),
    fluidRow(
  conditionalPanel(condition = "input.stats_type == 'Types of different alleles'",
      column(width = 12,
             dataTableOutput(outputId = "alleles_types")
      )
  ),
  conditionalPanel(condition = "input.stats_type == 'N° of different alleles per locus'",
      column(width = 12,
             plotlyOutput(outputId = "number_alleles_per_locus")
      )
  ),
  conditionalPanel(condition = " input.stats_type == 'Missing values per locus' || input.stats_type == 'Diversity indices'
                                 || input.stats_type == 'P-gen' || input.stats_type == 'H-W equilibrium'",
      column(width = 8,
             dataTableOutput(outputId = "loci_stats")
      )
  )
    ),
br(),
  conditionalPanel(condition = "input.stats_type == 'Types of different alleles'",
    fluidRow(
      column(width = 12,
             plotlyOutput(outputId = "allele_frequency")
      )
    )
  )
  ),
  tabPanel(title = h4("Cluster analysis"),
br(),
    fluidRow(
      column(width = 2,
             radioButtons(inputId = "na_value",
                          label = h5("NA value"),
                          choices = list("Zero" = "zero",
                                         "Mean" = "mean")
             )

      ),
      column(width = 2,
             radioButtons(inputId = "distance",
                          label = h5("Distance"),
                          choices = list("Binary" = "binary",
                                         "Geometric" = "geometric")
            )
     ),
  conditionalPanel(condition = "input.distance == 'binary'",
        column(width = 3,
               selectInput(inputId = "similarity_coefficient",
                           label = h5("Similarity coefficient"),
                           choices = list("Jaccard Index" = 1,
                                          "Sokal & Michener" = 2,
                                          "Sokal & Sneath" = 3,
                                          "Rogers & Tanimoto" = 4,
                                          "Dice & Sorensen" = 5,
                                          "Hamann coefficient" = 6,
                                          "Ochiai" = 7,
                                          "Sokal & Sneath 2" = 8,
                                          "Phi of Pearson" = 9,
                                          "S2 Coefficent" = 10),
                                         selected = 1)
                      )
     ),
  conditionalPanel(condition = "input.distance == 'geometric'",
     column(width = 3,
               selectInput(inputId = "geometric_distance",
                           label = h5("Geometric distance"),
                           choices = list("Euclidean" = "euclidean",
                                          "Maximum" = "maximum",
                                          "Manhattan" = "manhattan",
                                          "Canberra" = "canberra",
                                          "Minkowski" = "minkowski")
                             )
      )
     ),
     column(width = 3,
            selectInput(inputId = "hierarchical_method",
                        label = h5("Method"),
                        choices = list("Complete" = "complete",
                                       "Single" = "single",
                                       "Ward" = "ward.D",
                                       "Ward - 2" = "ward.D2",
                                       "UPGMA" = "average",
                                       "WPGMA" = "mcquitty",
                                       "Median" = "median",
                                       "UPGMC" = "centroid"))
      ),
        column(width = 2,
               numericInput(inputId = "cluster_count",
                            label = h5("Cluster count"),
                            value = 1,
                            min = 1, max = 20)
        )
      ),
      fluidRow(
        column(width = 5
        ),
        column(width = 3,
               h5("Plot title"),
               textInput(inputId = "dendrogram_title",
                         label = NULL,
                         value = "")
        ),
        column(width = 2,
               h5("Image format"),
               selectInput(inputId = "dendrogram_format",
                           label = NULL,
                           choices = list(".bmp" = ".bmp",
                                          ".jpeg" = ".jpeg",
                                          ".png" = ".png",
                                          ".tiff" = ".tiff"),
                           selected = ".jpeg")
        ),
        column(width = 2,
               h5("Plot"),
               downloadButton(outputId = "download_dendrogram",
                              label = "Download")
        )
      ),
      fluidRow(
        column(width = 4,
               h5("Plot width"),
               sliderInput(inputId = "dendrogram_width",
                           label = NULL,
                           value = 1000,
                           min = 500,
                           max = 5000
               )
        ),
        column(width = 4,
               h5("Plot height"),
               sliderInput(inputId = "dendrogram_height",
                           label = NULL,
                           value = 570,
                           min = 200,
                           max = 900
               )
        ),
        column(width = 4,
               h5("Plot resolution"),
               sliderInput(inputId = "dendrogram_resolution",
                           label = NULL,
                           value = 300,
                           min = 100,
                           max = 400)
        )
      ),
br(),
   fluidRow(
     column(width = 12,
            plotOutput(outputId = "tree",
                       width = "1100px",
                       height = "550px")
     )
   ),
br()
  ),
  tabPanel(title = h4("STRUCTURE input table"),
br(),
    fluidRow(
      column(width = 2,
             h5("Individuals ID"),
             verbatimTextOutput(outputId = "individuals_ID"
             )
      ),
      column(width = 2,
             h5("Populations ID"),
             verbatimTextOutput(outputId = "populations_ID"
             )
      ),
      column(width = 2,
             h5("Locations ID"),
             verbatimTextOutput(outputId = "locations_ID"
             )
      ),
      column(width = 2,
             h5("Individuals"),
             verbatimTextOutput(outputId = "individuals_number")
      ),
      column(width = 2,
             h5("Loci"),
             verbatimTextOutput(outputId = "loci_number")
      ),
      column(width = 2,
             h5("Ploidy"),
             verbatimTextOutput(outputId = "ploidy")
      )
    ),
    fluidRow(
  conditionalPanel(condition = "output.individuals_ID == 'YES'",
      column(width = 2,
             checkboxInput(inputId = "remove_ID",
                           label = "Remove",
                           value = FALSE)
      )
  ),
  conditionalPanel(condition = "output.individuals_ID == 'NO'",
                   column(width = 2
                   )
  ),
  conditionalPanel(condition = "output.populations_ID == 'YES'",
                   column(width = 2,
                          checkboxInput(inputId = "remove_POP",
                                        label = "Remove",
                                        value = FALSE)
                   )
  ),
  conditionalPanel(condition = "output.populations_ID == 'NO'",
                   column(width = 2
                   )
  ),
  conditionalPanel(condition = "output.locations_ID == 'YES'",
                   column(width = 2,
                          checkboxInput(inputId = "remove_LOC",
                                        label = "Remove",
                                        value = FALSE)
                   )
  ),
  conditionalPanel(condition = "output.locations_ID == 'NO'",
                   column(width = 2
                   )
  ),
      column(width = 3,
             offset = 3,
             downloadButton(outputId = "download",
                            label = "Download the table")
      )
    ),
br(),
    fluidRow(
      column(width = 12,
             dataTableOutput(outputId = "table_export")
       )
    )
  )
    )
  )),

####################### UI DATA FROM STRUCUTRE ####
  conditionalPanel(condition = "input.analysis_type == 2 &
                                input.customize_plot > 0",
                   mainPanel(id = "plot_panel",
  tabsetPanel(type = "pills",
    tabPanel(title = h4("Input table"),
 br(),
  conditionalPanel(condition = "output.dataFormat != 'Q'",
     fluidRow(
       column(width = 2,
              radioButtons(inputId = "separator2", label = h5("Separator"),
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ",")
       ),
       column(width = 2,
              radioButtons(inputId = "quote2", label = h5("Quote"),
                           choices = c(None = "",
                                       Single = "'",
                                       Double = '"'),
                           selected = "")
       )
     )
  ),
br(),
     fluidRow(
       column(width = 12,
              dataTableOutput(outputId = "table_import2")
       )
     )
  ),
  tabPanel(title = h4("Barplot"),
br(),
    fluidRow(
      column(width = 1,
             h5("K"),
             verbatimTextOutput(outputId = "cluster_number")
             ),
      column(width = 2,
             h5("Order"),
             radioButtons(inputId = "sort",
                          label = NULL,
                          choices = c("Original" = "original",
                                      "Sort by Q" = "Q",
                                      "Alphabetic" = "alphabetic"),
                          selected = "original")
             ),
      column(width = 2,
             h5("Bar position"),
             radioButtons(inputId = "barpos",
                          label = NULL,
                          choices = c("Stack" = "stack",
                                      "Dodge" = "dodge"),
                          selected = "stack"

             )
      ),
      column(width = 3,
             h5("Plot title"),
             textInput(inputId = "barplot_title",
                       label = NULL,
                       value = "")
      ),
      column(width = 2,
             h5("Image format"),
             selectInput(inputId = "barplot_format",
                         label = NULL,
                         choices = list(".bmp" = ".bmp",
                                        ".jpeg" = ".jpeg",
                                        ".png" = ".png",
                                        ".tiff" = ".tiff"),
                         selected = ".jpeg")
      ),
      column(width = 2,
             h5("Plot"),
             downloadButton(outputId = "download_barplot",
                            label = "Download")
      )
    ),
    fluidRow(
      column(width = 4,
             h5("Plot width"),
             sliderInput(inputId = "barplot_width",
                         label = NULL,
                         value = 1000,
                         min = 500,
                         max = 5000
             )
      ),
      column(width = 4,
             h5("Plot height"),
             sliderInput(inputId = "barplot_height",
                         label = NULL,
                         value = 570,
                         min = 200,
                         max = 2000
             )
      ),
      column(width = 4,
             h5("Plot resolution"),
             sliderInput(inputId = "barplot_resolution",
                         label = NULL,
                         value = 300,
                         min = 100,
                         max = 400)
      )
    ),
    fluidRow(
        column(width = 3,
               sliderInput(inputId = "x_label_size",
                           label = h5("X labels size"),
                           min = 1,
                           max = 10,
                           value = 5)
        ),
        column(width = 3,
               sliderInput(inputId = "y_label_size",
                           label = h5("Y labels size"),
                           min = 1,
                           max = 10,
                           value = 8)
        ),
        column(width = 3,
               sliderInput(inputId = "x_label_angle",
                           label = h5("X labels angle"),
                           min = 0,
                           max = 90,
                           value = 90)
        ),
        column(width = 3,
               sliderInput(inputId = "axis_title_size",
                           label = h5("Titles size"),
                           min = 1,
                           max = 10,
                           value = 5)
        )
    ),
    fluidRow(
      column(width = 1,
  conditionalPanel(condition = "output.cluster_number >= 1",
                          colourpicker::colourInput(inputId = "colour_1",
                                                    label = h5("K 1"),
                                                    value = "#FF5733",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 2",
                          colourpicker::colourInput(inputId = "colour_2",
                                                    label = h5("K 2"),
                                                    value = "#A4F771",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 3",
                          colourpicker::colourInput(inputId = "colour_3",
                                                    label = h5("K 3"),
                                                    value = "#71F7EF",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >=4",
                          colourpicker::colourInput(inputId = "colour_4",
                                                    label = h5("K 4"),
                                                    value = "#EEC764",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 5",
                          colourpicker::colourInput(inputId = "colour_5",
                                                    label = h5("K 5"),
                                                    value = "#EDA590",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 6",
                          colourpicker::colourInput(inputId = "colour_6",
                                                    label = h5("K 6"),
                                                    value = "#74AA8A",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
      column(width = 1,
  conditionalPanel(condition = "output.cluster_number >= 7",
                   colourpicker::colourInput(inputId = "colour_7",
                                             label = h5("K 7"),
                                             value = "#337489",
                                             showColour = "background",
                                             allowTransparent = TRUE)
  )
      ),
      column(width = 2,
             uiOutput(outputId = "show_location_marks")
      ),
      column(width = 3,
             selectInput(inputId = "group_barplot_by",
                         label = h5("Group by"),
                         choices = "")
      )
    ),
  fluidRow(
    column(width = 1,
           conditionalPanel(condition = "output.cluster_number >= 8",
                            colourpicker::colourInput(inputId = "colour_8",
                                                      label = h5("K 8"),
                                                      value = "#9972A4",
                                                      showColour = "background",
                                                      allowTransparent = TRUE)
           )
    ),
    column(width = 1,
           conditionalPanel(condition = "output.cluster_number >= 9",
                            colourpicker::colourInput(inputId = "colour_9",
                                                      label = h5("K 9"),
                                                      value = "#F1F359",
                                                      showColour = "background",
                                                      allowTransparent = TRUE)
           )
    ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 10",
                          colourpicker::colourInput(inputId = "colour_10",
                                                    label = h5("K 10"),
                                                    value = "#FAB86D",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 11",
                          colourpicker::colourInput(inputId = "colour_11",
                                                    label = h5("K 11"),
                                                    value = "#D6FFD6",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 12",
                          colourpicker::colourInput(inputId = "colour_12",
                                                    label = h5("K 12"),
                                                    value = "#F1CBF1",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 13",
                          colourpicker::colourInput(inputId = "colour_13",
                                                    label = h5("K 13"),
                                                    value = "#FFF000",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 14",
                          colourpicker::colourInput(inputId = "colour_14",
                                                    label = h5("K 14"),
                                                    value = "#8CE0F0",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 15",
                          colourpicker::colourInput(inputId = "colour_15",
                                                    label = h5("K 15"),
                                                    value = "#ACE4BD",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 16",
                          colourpicker::colourInput(inputId = "colour_16",
                                                    label = h5("K 16"),
                                                    value = "#C6BF47",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 17",
                          colourpicker::colourInput(inputId = "colour_17",
                                                    label = h5("K 17"),
                                                    value = "#24941C",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
  column(width = 1,
         conditionalPanel(condition = "output.cluster_number >= 18",
                          colourpicker::colourInput(inputId = "colour_18",
                                                    label = h5("K 18"),
                                                    value = "#57BFFE",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
         )
  ),
      column(width = 1,
  conditionalPanel(condition = "output.cluster_number >= 19",
                          colourpicker::colourInput(inputId = "colour_19",
                                                    label = h5("K 19"),
                                                    value = "#D96492",
                                                    showColour = "background",
                                                    allowTransparent = TRUE)
  )
      ),
    fluidRow(
      column(width = 1,
             conditionalPanel(condition = "output.cluster_number >= 20",
                              colourpicker::colourInput(inputId = "colour_20",
                                                        label = h5("K 20"),
                                                        value = "#9E7183",
                                                        showColour = "background",
                                                        allowTransparent = TRUE)
             )
      )
    )
    ),
    fluidRow(
      column(width = 12,
             plotlyOutput(outputId = "structure_plot",
                          height = "600px")
      )
    )
  ),
  tabPanel(title = h4("Triangle plot"),
br(),
    fluidRow(
      column(width = 1,
             h5("K"),
             verbatimTextOutput(outputId = "cluster_number_2")
      ),
      column(width = 2,
             h5("Bottom left"),
             selectInput(inputId = "bottom_left",
                         label = NULL,
                         choices = "")
      ),
      column(width = 2,
             h5("Bottom right"),
             selectInput(inputId = "bottom_right",
                         label = NULL,
                         choices = "")
      ),
      column(width = 3,
             h5("Plot title"),
             textInput(inputId = "triangleplot_title",
                       label = NULL,
                       value = "")
      ),
      column(width = 2,
             h5("Image format"),
             selectInput(inputId = "triangleplot_format",
                         label = NULL,
                         choices = list(".pdf" = ".pdf",
                                        ".jpeg" = ".jpeg",
                                        ".png" = ".png",
                                        ".svg" = ".svg"),
                         selected = ".jpeg")
      ),
      column(width = 2,
             h5("Plot"),
             downloadButton(outputId = "download_triangleplot",
                            label = "Download")
      )
    ),
    fluidRow(
      column(width = 4,
             h5("Plot width"),
             sliderInput(inputId = "triangleplot_width",
                         label = NULL,
                         value = 600,
                         min = 250,
                         max = 1000
             )
      ),
      column(width = 4,
             h5("Plot height"),
             sliderInput(inputId = "triangleplot_height",
                         label = NULL,
                         value = 600,
                         min = 250,
                         max = 1000
             )
      ),
      column(width = 4,
             h5("Symbol size"),
             sliderInput(inputId = "triangleplot_symbol_size",
                         label = NULL,
                         value = 8,
                         min = 1,
                         max = 20
             )
      )
    ),
    fluidRow(
      column(width = 12, align = "center",
             plotlyOutput(outputId = "triangle_plot",
                          height = "550px"

             )
      )
    )
  )
    ),
br(),
br(),
br()
    )
  ),
####################### COMPARING PARTITIONS ####
  conditionalPanel(condition = "input.analysis_type == 3 &
                                input.check_table == 0 &
                                input.customize_plot == 0",
                   mainPanel(id = "comparison_1",
                             img(src = "Instructions_comparison.png",
                                 width = "1100px",
                                 height = "619px")
                   )
  ),
# Controllare se funziona quando si importano nuovi dataset dopo i primi importati e
# non sono più disponibili i bottoni "Start the analysis"
  conditionalPanel(condition = "input.analysis_type == 3 &
                                input.check_table > 0 &
                                input.customize_plot == 0",
                   mainPanel(id = "comparison_2",
                             img(src = "Instructions_comparison.png",
                                 width = "1100px",
                                 height = "619px")
                   )
  ),
  conditionalPanel(condition = "input.analysis_type == 3 &
                                input.check_table == 0 &
                                input.customize_plot > 0",
                   mainPanel(id = "comparison_3",
                             img(src = "Instructions_comparison.png",
                                 width = "1100px",
                                 height = "619px")
                   )
  ),
  conditionalPanel(condition = "input.analysis_type == 3 &
                                input.check_table > 0 &
                                input.customize_plot > 0",
                   mainPanel(id = "comparison_true",
    fluidRow(
      column(width = 2,
             h5("Hierarchic"),
             verbatimTextOutput(outputId = "Cluster_hclust")
      ),
      column(width = 2,
             h5("Pop analysis"),
             verbatimTextOutput(outputId = "Cluster_STR")
      ),
      column(width = 3,
             h5("Agr. index"),
             selectInput(inputId = "agreement_index",
                         label = NULL,
                         choices = list("Rand Index" = "rand",
                                        "Adj. Rand Index" = "crand"))
      ),
      column(width = 2,
             h5("Agr. value"),
             verbatimTextOutput(outputId = "agreement_value")
      ),
      column(width = 3,
             h5("Comparison outputs"),
             selectInput(inputId = "show_comparison_outputs",
                         label = NULL,
                         choices = c("Table",
                                     "Contingency table",
                                     "Contingency plot"))
      )
    ),
br(),
  conditionalPanel(condition = "input.show_comparison_outputs == 'Table'",
    fluidRow(
      column(width = 12,
             dataTableOutput(outputId = "comparison_table")
      )
    )
  ),
  conditionalPanel(condition = "input.show_comparison_outputs == 'Contingency table'",
    fluidRow(
      column(width = 12,
             column(width = 12,
                    verbatimTextOutput(outputId = "contingency_table")
             )
      )
    )
  ),
  conditionalPanel(condition = "input.show_comparison_outputs == 'Contingency plot'",
    fluidRow(
      column(width = 5),
      column(width = 3,
             h5("Plot title"),
             textInput(inputId = "comparison_plot_title",
                       label = NULL,
                       value = "")
      ),
      column(width = 2,
             h5("Image format"),
             selectInput(inputId = "comparison_plot_format",
                         label = NULL,
                         choices = list(".bmp" = ".bmp",
                                        ".jpeg" = ".jpeg",
                                        ".png" = ".png",
                                        ".tiff" = ".tiff"),
                         selected = ".jpeg")
      ),
      column(width = 2,
             h5("Plot"),
             downloadButton(outputId = "download_comparison_plot",
                            label = "Download")
      )
    ),
    fluidRow(
      column(width = 4,
             h5("Plot width"),
             sliderInput(inputId = "comparison_plot_width",
                         label = NULL,
                         value = 950,
                         min = 500,
                         max = 5000
             )
      ),
      column(width = 4,
             h5("Plot height"),
             sliderInput(inputId = "comparison_plot_height",
                         label = NULL,
                         value = 570,
                         min = 200,
                         max = 900
             )
      ),
      column(width = 4,
             h5("Plot resolution"),
             sliderInput(inputId = "comparison_plot_resolution",
                         label = NULL,
                         value = 300,
                         min = 100,
                         max = 400)
      )
    ),
br(),
    fluidRow(
      column(width = 3,
             sliderInput(inputId = "comp_x_label_size",
                         label = h5("X labels size"),
                         min = 1,
                         max = 10,
                         value = 8)
      ),
      column(width = 3,
             sliderInput(inputId = "comp_y_label_size",
                         label = h5("Y labels size"),
                         min = 1,
                         max = 10,
                         value = 8)
      ),
      column(width = 3,
             sliderInput(inputId = "comp_label_angle",
                         label = h5("Labels angle"),
                         min = 0,
                         max = 90,
                         value = 0)
      ),
      column(width = 3,
             sliderInput(inputId = "comp_axis_title_size",
                         label = h5("Titles size"),
                         min = 1,
                         max = 10,
                         value = 5)
      )
    ),
br(),
    fluidRow(
      column(width = 3
      ),
      column(width = 9,
             fluidRow(style = "height:250px;",
             plotlyOutput(outputId = "barplot_cluster_hierarchical")
             )
      )
    ),
    fluidRow(
      column(width = 3,
             plotlyOutput(outputId = "barplot_cluster_pop_analysis")
      ),
      column(width = 9,
             plotlyOutput(outputId = "comparison_plot")
      )
    )
  )
                   )
    )
        ) # Closes output column
    )
) # Closes fluidPage
