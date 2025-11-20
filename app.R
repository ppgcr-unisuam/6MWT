# create dir
dir.name <- 'www'
if (!dir.exists(dir.name)) {
  dir.create(dir.name, recursive = TRUE, showWarnings = FALSE)
}
shiny::addResourcePath(prefix = "www", directoryPath = "www")

# copy favicon folder to the www dir
if (dir.exists("favicon_io")) {
  R.utils::copyDirectory("favicon_io", file.path(dir.name, "favicon_io"))
}

# use this code to debug
# rsconnect::showLogs()

# read XLSX
data <- readxl::read_xlsx("0208 Aggregation published models.xlsx")
equations <- colnames(data)[-1]
# sort by name
equations <- sort(equations)

ui <- shiny::fluidPage(
  # add favicon
  shiny::tags$head(
    shiny::tags$link(rel = "shortcut icon", href = "favicon_io/favicon.ico"),
    shiny::tags$link(rel = "icon", href = "favicon_io/favicon-32x32.png"),
    shiny::tags$link(rel = "icon", href = "favicon_io/favicon-16x16.png"),
    shiny::tags$link(rel = "apple-touch-icon", href = "favicon_io/apple-touch-icon.png"),
    shiny::tags$link(rel = "icon", href = "favicon_io/android-chrome-192x192.png"),
    shiny::tags$link(rel = "icon", href = "favicon_io/android-chrome-512x512.png")
  ),
  
  # ----- Hide the built-in DT PDF button -----
  tags$head(tags$style(htmltools::HTML("
    .dt-button.buttons-pdf { 
      display: none !important; 
    }
  "))),
  
  # ----- Input card style -----
  tags$head(
    tags$style(htmltools::HTML("
    .input-card {
      background-color: #f5f7fa;
      border-radius: 12px;
      padding: 20px;
      margin-top: 25px;
      margin-bottom: 25px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    }
  ")),
  ),
  
  # ----- Prediction card style -----
  tags$head(
    tags$style(htmltools::HTML("
      .prediction-card {
        background-color: #f5f7fa;
        border-radius: 12px;
        padding: 20px;
        margin-top: 25px;
        text-align: center;
        font-size: 32px;
        font-weight: bold;
        color: #1a2a33;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }
    ")),
  ),
  
  # ----- Highlight 3rd column -----
  tags$head(
    tags$style(htmltools::HTML("
    table.dataTable tbody td:nth-child(3) {
      background-color: #d3d3d3 !important;
      font-weight: bold;
    }
  ")),
  ),
  
  # center table values
  tags$head(tags$style(htmltools::HTML("
    table.dataTable td, table.dataTable th {
      text-align: center !important;
    }
  "))),
  
  # ----- JavaScript to trigger hidden DT PDF button -----
  tags$script(htmltools::HTML("
    Shiny.addCustomMessageHandler('trigger_pdf', function(message) {
      $('.buttons-pdf').click();
    });
  ")),
  
  # ----- Page title -----
  tags$head(
    tags$title("Six-Minute Walk Test")
  ),
  shiny::tags$div(
    style = "
    font-size: 28px;
    font-weight: bold;
    margin-top: 20px;
    margin-bottom: 20px;
    display: flex;
    align-items: center;
    gap: 10px;
  ",
    htmltools::img(src = "favicon_io/favicon-32x32.png", height = "32px"),
    "Six-Minute Walk Test"
  ),
  
  # ----- Input card -----
  shiny::div(
    class = "input-card",
    shiny::selectInput(
      "equation",
      "Select Equation",
      choices = equations,
      width = "100%"
    ),
    shiny::uiOutput("footnotes_container"),
    shiny::uiOutput("doi_container")
  ),
  
  # table title
  shiny::br(),
  DT::DTOutput("dataTable"),
  shiny::hr(),
  
  shiny::div(class = "prediction-card",
             "Predicted Value",
             shiny::br(),
             shiny::textOutput("prediction", inline = TRUE)
  ),
  
  # ----- Bottom download button -----
  shiny::div(style = "margin-top: 40px; text-align: center;",
             shiny::actionButton(
               "download_pdf_btn",
               "Download PDF",
               class = "btn btn-primary btn-lg"
             )
  )
)

server <- function(input, output, session) {
  
  edited_data <- shiny::reactiveVal(NULL)
  
  # load data when equation changes
  shiny::observeEvent(input$equation, {
    shiny::req(input$equation)
    
    df <- data[, c(1, which(colnames(data) == input$equation))]
    colnames(df) <- c("Variable", "Coefficient")
    
    # remove doi row + first 2 footnotes
    df <- df[!grepl("DOI", df$Variable, ignore.case = TRUE), ]
    df <- df[-c(1,2), ]
    
    df <- df[!is.na(df$Coefficient), ]
    df$`Patient's Value` <- NA
    
    edited_data(df)
  })
  
  # render table with hidden PDF button
  output$dataTable <- DT::renderDT({
    shiny::req(edited_data())
    
    DT::datatable(
      edited_data(),
      extensions = "Buttons",
      editable = list(target = "cell", columns = 3),
      rownames = FALSE,
      class = "compact row-border",
      options = list(
        dom = "Brt",
        scrollX = FALSE,
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        buttons = list(
          list(
            extend = "pdf",
            text = "hidden_pdf_button",
            title = "Six-Minute Walk Test Report",
            pageSize = "A4",
            exportOptions = list(columns = ":visible"),
            customize = DT::JS("
              function (doc) {
              
                // Make tables full-width
                doc.content.forEach(function(section) {
                  if (section.table && section.table.body) {
                    var colCount = section.table.body[0].length;
                    section.table.widths = Array(colCount).fill('*');
                  }
                });            
                var prediction = $('#prediction').text();
                var doi = $('#doi').text() || '';
                var footnotes = $('#footnotes').text() || '';
                var timestamp = new Date().toLocaleString();
                
                // --------------------------------------------------------
                // Predicted Value CARD (centered)
                // --------------------------------------------------------
                doc.content.push({
                  margin: [0, 20, 0, 10],
                  alignment: 'center',
                  table: {
                    widths: ['*'],
                    body: [
                      [
                        {
                          stack: [
                            { text: 'Predicted Value', fontSize: 14, bold: true, margin: [0, 0, 0, 8] },
                            { text: prediction, fontSize: 22, bold: true, color: '#003f6b' }
                          ],
                          fillColor: '#f2f5f7',
                          margin: [0, 12, 0, 12],
                          alignment: 'center'
                        }
                      ]
                    ]
                  },
                  layout: {
                    hLineWidth: function(){ return 0; },
                    vLineWidth: function(){ return 0; },
                    paddingLeft: function(){ return 15; },
                    paddingRight: function(){ return 15; },
                    paddingTop: function(){ return 12; },
                    paddingBottom: function(){ return 12; }
                  }
                });

                doc.content.push({ text: ' ' });
                
                doc.content.push({
                  text: 'DOI:',
                  style: 'subheader',
                  margin: [0,10,0,3]
                });
                
                doc.content.push({
                  text: doi,
                  link: 'https://doi.org/' + doi,
                  color: '#0645AD',          // standard hyperlink blue
                  decoration: 'underline',
                  margin: [0,0,0,10]
                });
                                
                doc.content.push({ text: 'Footnotes:', style: 'subheader', margin: [0,10,0,3] });
                doc.content.push({ text: footnotes, margin: [0,0,0,10] });
                
                doc.content.push({ text: 'Timestamp:', style: 'subheader', margin: [0,10,0,3] });
                doc.content.push({
                  text: 'Report generated at: ' + timestamp,
                  italics: true,
                  fontSize: 10,
                  margin: [0, 0, 0, 10]
                });
                
                doc.styles.header = {
                  fontSize: 18,
                  bold: true,
                  alignment: 'left'
                };
                
                doc.styles.subheader = {
                  fontSize: 14,
                  bold: true
                };
              }
            ")
          )
        )
      )
    ) |>
      DT::formatRound(columns = c("Coefficient"), digits = 3)
  })
  
  # update edited data on cell edit
  shiny::observeEvent(input$dataTable_cell_edit, {
    info <- input$dataTable_cell_edit
    df <- edited_data()
    col_idx <- info$col + 1  # JS index → R index
    df[info$row, col_idx] <- info$value
    edited_data(df)
  })
  
  # footnotes container
  output$footnotes_container <- shiny::renderUI({
    shiny::req(input$equation)
    
    df <- data[, c(1, which(colnames(data) == input$equation))]
    colnames(df) <- c("Variable", "Coefficient")
    
    foot <- df[1:2, ]
    
    shiny::div(
      id = "footnotes",
      htmltools::HTML(paste0(
        "Footnotes: ",
        foot[1,1], ": ", round(as.numeric(foot[1,2]), 0), ", ",
        foot[2,1], ": ", foot[2,2]
      ))
    )
  })
  
  # doi container
  output$doi_container <- shiny::renderUI({
    shiny::req(input$equation)
    
    df <- data[, c(1, which(colnames(data) == input$equation))]
    colnames(df) <- c("Variable", "Coefficient")
    
    doi_row <- df[grepl("DOI", df$Variable, ignore.case = TRUE), ]
    doi_value <- as.character(doi_row$Coefficient[1])
    
    shiny::div(
      id = "doi",
      htmltools::HTML(sprintf(
        'DOI: <a href="https://doi.org/%s" target="_blank">%s</a>',
        doi_value, doi_value
      ))
    )
  })
  
  # prediction
  output$prediction <- shiny::renderText({
    df <- edited_data()
    shiny::req(df)
    
    df$Coefficient <- suppressWarnings(as.numeric(df$Coefficient))
    df$`Patient's Value` <- suppressWarnings(as.numeric(df$`Patient's Value`))
    
    df_no_intercept <- df[!df$Variable %in% c("Intercept"), ]
    
    if (any(is.na(df_no_intercept$`Patient's Value`))) {
      return("")
    }
    
    df$`Patient's Value`[is.na(df$`Patient's Value`)] <- 0
    df$prod <- df$Coefficient * df$`Patient's Value`
    
    if ("Intercept" %in% df$Variable) {
      intercept_value <- df$Coefficient[df$Variable == "Intercept"]
      df$prod[df$Variable == "Intercept"] <- intercept_value
    }
    
    pred <- sum(df$prod, na.rm = TRUE)
    
    paste0(format(round(pred, 0), nsmall = 0), " meters")
  })
  
  # ----- Compute prediction range (min–max) -----
  prediction_range <- shiny::reactive({
    df <- edited_data()
    shiny::req(df)
    
    df$Coefficient <- suppressWarnings(as.numeric(df$Coefficient))
    df$`Patient's Value` <- suppressWarnings(as.numeric(df$`Patient's Value`))
    
    # Keep intercept separate
    intercept <- if ("Intercept" %in% df$Variable) {
      df$Coefficient[df$Variable == "Intercept"]
    } else 0
    
    df2 <- df[df$Variable != "Intercept", ]
    
    # For each coefficient: calculate min and max contribution
    df2$min_contrib <- ifelse(
      df2$Coefficient >= 0,
      df2$Coefficient * min(df2$`Patient's Value`, na.rm = TRUE),
      df2$Coefficient * max(df2$`Patient's Value`, na.rm = TRUE)
    )
    
    df2$max_contrib <- ifelse(
      df2$Coefficient >= 0,
      df2$Coefficient * max(df2$`Patient's Value`, na.rm = TRUE),
      df2$Coefficient * min(df2$`Patient's Value`, na.rm = TRUE)
    )
    
    min_pred <- sum(df2$min_contrib, na.rm = TRUE) + intercept
    max_pred <- sum(df2$max_contrib, na.rm = TRUE) + intercept
    
    list(
      min = min_pred,
      max = max_pred,
      df_details = df2
    )
  })
  
  # ----- bottom button triggers DT PDF -----
  shiny::observeEvent(input$download_pdf_btn, {
    session$sendCustomMessage("trigger_pdf", list())
  })
}

shiny::shinyApp(ui, server)
