################################### UI ########################################
ui <- fluidPage(
  useShinyjs(), # Recall every function of Shinyjs in the UI
    fluidRow(
        column(width = 12,
               p(h3(strong(em("StructureIt")),
                    strong("0.1.0"), "-",
                    "Tables and elegant detailed plots of the",
                    a("STRUCTURE",
          href = "https://web.stanford.edu/group/pritchardlab/structure.html"),
                      "software",
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
                         label = h5(icon(name = "book"), "Choose an action"),
                         choices = list("Download the formatted table ready
                                        for STRUCTURE" = 1,
                                        "Customize the plot of the results
                                        obtained from STRUCTURE" = 2),
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
                                      "Check the table"),
                           width = "100%")
    )
  ),
  conditionalPanel(condition = "input.analysis_type == 2",
            fileInput(inputId = "Data_DA_Str",
                      label = h5(icon(name = "file"),
                                 "Input file (.csv, .txt)"),
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                      ),
    conditionalPanel(condition = "output.fileUploaded_DA",
              actionButton(inputId = "customize_plot",
                           label = h4(icon(name = "check"),
                                      "Customize the plot"),
                           width = "100%")
    )
  )
  ),
br(),
br(),
br(),
  conditionalPanel(condition = "input.check_table > 0 &
                                input.analysis_type == 1",
            actionButton(inputId = "back_instructions_1",
                         label = h6(icon(name = "book"),
                                    "Back to instructions")
                         ),
            shinyjs::hidden(actionButton(inputId = "back_results_1",
                                         label = h6(icon(name = "book"),
                                                    "Back to results")
                                         )
                            )
  ),
  conditionalPanel(condition = "input.customize_plot > 0 &
                                input.analysis_type == 2",
                   actionButton(inputId = "back_instructions_2",
                                label = h6(icon(name = "book"),
                                           "Back to instructions")),
                   shinyjs::hidden(actionButton(inputId = "back_results_2",
                                                label = h6(icon(name = "book"),
                                                           "Back to results")
                                                )
                                   )
  )
        ), # Closes input column

######################### UI DATA FOR STRUCUTRE ####
        column(width = 10,
    mainPanel(id = "instructions",
              img(src = "Wallpaper_StructureIt.png",
                  height = "650px",
                  width = "1150px")),
  conditionalPanel(condition = "input.analysis_type == 1",
  shinyjs::hidden(mainPanel(id = "instructions_2",
                            "hhahahsdjsdhshdoshsdohdsoihidhso
                            hohdhosdssdhshdahahaahhhhahahahhah")
  )),
  conditionalPanel(condition = "input.analysis_type == 2",
  shinyjs::hidden(mainPanel(id = "instructions_3",
                            "hhahahsdjsdhshdoshsdohdsoihidhsohohdhos
                            dssdhshdahahaahhhhahahahhah")
  )),
  conditionalPanel(condition = "input.analysis_type == 1",
  shinyjs::hidden(mainPanel(id = "table_panel",
  tabsetPanel(type = "pills",
    tabPanel(title = h4("User's table"),
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
  tabPanel(title = h4("Table for STRUCTURE"),
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
  ))),

####################### UI DATA FROM STRUCUTRE ####
  conditionalPanel(condition = "input.analysis_type == 2",
  shinyjs::hidden(mainPanel(id = "plot_panel",
  tabsetPanel(type = "pills",
    tabPanel(title = h4("User's table"),
 br(),
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
             h5("Plot width"),
             sliderInput(inputId = "width",
                         label = NULL,
                         value = 1000,
                         min = 500,
                         max = 5000
             )
      ),
      column(width = 2,
             h5("Image format"),
             selectInput(inputId = "format",
                         label = NULL,
                         choices = list(".bmp" = ".bmp",
                                        ".jpeg" = ".jpeg",
                                        ".png" = ".png",
                                        ".tiff" = ".tiff"),
                         selected = ".jpeg")
             ),
      column(width = 2,
             h5("Plot"),
             downloadButton(outputId = "download_plot",
                            label = "Download")
      )
    ),
    fluidRow(
        column(width = 3,
               sliderInput(inputId = "x_label_size",
                           label = h5("Size X Labels"),
                           min = 1,
                           max = 10,
                           value = 5)
        ),
        column(width = 3,
               sliderInput(inputId = "y_label_size",
                           label = h5("Size Y Labels"),
                           min = 1,
                           max = 10,
                           value = 8)
        ),
        column(width = 3,
               sliderInput(inputId = "x_label_angle",
                           label = h5("Angle X Labels"),
                           min = 0,
                           max = 90,
                           value = 90)
        ),
        column(width = 3,
               sliderInput(inputId = "axis_title_size",
                           label = h5("Size of titles"),
                           min = 1,
                           max = 10,
                           value = 5)
        )
    ),
    fluidRow(
      column(width = 12,
             plotlyOutput(outputId = "structure_plot",
                          height = "560px"
             )
      )
    ),
br(),
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
      )
    ),
    fluidRow(
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
  )
    ),
br(),
br(),
br()
  )))

        ) # Closes output column
    )

) # Closes fluidPage
