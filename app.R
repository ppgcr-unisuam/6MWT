# create dir
dir.name <- 'www'
if (!dir.exists(dir.name)) {
  dir.create(dir.name, recursive = TRUE, showWarnings = FALSE)
}
shiny::addResourcePath(prefix = "www", directoryPath = "www")

# create results folder
if (!dir.exists(file.path(getwd(), "results"))){
  dir.create(file.path(getwd(), "results"), showWarnings = FALSE)
}

# copy favicon folder to the www dir
if (dir.exists("favicon_io")) {
  R.utils::copyDirectory("favicon_io", file.path(dir.name, "favicon_io"))
}

# use this code to debug
# rsconnect::showLogs()

# read XLSX
data <- readxl::read_xlsx("models.xlsx")

# rename data columns: read again without column names, then set names
rawdata <- readxl::read_xlsx("models.xlsx", col_names = FALSE)
labels <- paste0(rawdata[1, ], " (", rawdata[which(rawdata[, 1] == "Sex scope"), ], ")")
colnames(data) <- labels
# get equations names
equations <- colnames(data)[-c(1:4)]
# unique keys
eq_keys <- sprintf("%02d", seq_along(equations))
equations <- paste0(eq_keys, " - ", equations)
names(data) <- c(names(data)[1:4], equations)
# sort by name
equations <- sort(equations)

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  
  # ----- HEAD (favicon + CSS + JS) -----
  shiny::tags$head(
    shiny::tags$link(rel = "shortcut icon", href = "favicon_io/favicon.ico"),
    shiny::tags$link(rel = "icon", href = "favicon_io/favicon-32x32.png"),
    shiny::tags$link(rel = "icon", href = "favicon_io/favicon-16x16.png"),
    shiny::tags$link(rel = "apple-touch-icon", href = "favicon_io/apple-touch-icon.png"),
    shiny::tags$link(rel = "icon", href = "favicon_io/android-chrome-192x192.png"),
    shiny::tags$link(rel = "icon", href = "favicon_io/android-chrome-512x512.png")
  ),
  
  # Hide DT PDF button
  tags$head(tags$style(htmltools::HTML("
    .dt-button.buttons-pdf { display: none !important; }
  "))),
  
  # Card styles
  tags$head(tags$style(htmltools::HTML("
    .input-card {
      background-color: #eef4ff;
      border-radius: 12px;
      padding: 20px;
      margin-top: 25px;
      margin-bottom: 25px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    }
    .prediction-card {
      background-color: #eef4ff;
      border-radius: 12px;
      padding: 20px;
      margin-top: 25px;
      text-align: center;
      font-size: 32px;
      font-weight: bold;
      color: #1a2a33;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    }
  "))),
  
  # Highlight table column + center text
  tags$head(tags$style(htmltools::HTML("
    table.dataTable tbody td:nth-child(3) { background-color: #d3d3d3 !important; font-weight: bold; }
    table.dataTable td, table.dataTable th { text-align: center !important; }
  "))),
  
  # JS to trigger hidden PDF button
  tags$script(htmltools::HTML("
    Shiny.addCustomMessageHandler('trigger_pdf', function(message) {
      $('.buttons-pdf').click();
    });
  ")),
  
  # Page title
  tags$head(tags$title("Six-Minute Walk Test")),
  
  shiny::tags$div(
    style = "font-size: 28px; font-weight: bold; margin-top: 20px; margin-bottom: 20px; 
             display: flex; align-items: center; gap: 10px;",
    htmltools::img(src = "favicon_io/favicon-32x32.png", height = "32px"),
    "Six-Minute Walk Test"
  ),
  
  shiny::HTML(
    "<a href=\"https://doi.org/10.5281/zenodo.17663547\" style=\"vertical-align:middle;\"><img src=\"https://zenodo.org/badge/DOI/10.5281/zenodo.17663547.svg\" alt=\"DOI\"  style=\"vertical-align:top;\"></a>"
  ),
  shiny::br(),
  shiny::br(),
  
  tags$script(htmltools::HTML("
      $(document).on('shiny:connected', function() {
      
          var INITIAL_SECONDS = 6 * 60;
          var totalSeconds = INITIAL_SECONDS;
      
          var timerDisplay = document.getElementById('timer_display');
          var startBtn     = document.getElementById('timer_start');
          var resetBtn     = document.getElementById('timer_reset');
      
          if (!timerDisplay || !startBtn || !resetBtn) {
              console.error('Timer elements NOT found when JS executed');
              return;
          }
      
          var timerInterval = null;
          var alarmPlaying = false;
      
          function formatTime(sec) {
              var m = Math.floor(sec/60);
              var s = sec % 60;
              return String(m).padStart(2,'0') + ':' + String(s).padStart(2,'0');
          }
      
          function stopAlarm() {
              try {
                  if (alarmPlaying && window.alarm) {
                      window.alarm.pause();
                      window.alarm.currentTime = 0;
                  }
              } catch(e){}
              alarmPlaying = false;
          }
      
          function finishTimer() {
              clearInterval(timerInterval);
              timerInterval = null;
      
              timerDisplay.textContent = 'Time is up!';
              timerDisplay.style.color = 'red';
      
              try { 
                  window.alarm.currentTime = 0; 
                  window.alarm.play(); 
                  alarmPlaying = true;
              } catch(e){}
      
              startBtn.textContent = 'Stop';
              startBtn.disabled = false;
              startBtn.setAttribute('data-state','ended');
          }
      
          function resetTimer() {
              clearInterval(timerInterval);
              stopAlarm();
      
              totalSeconds = INITIAL_SECONDS;
              timerDisplay.textContent = formatTime(totalSeconds);
              timerDisplay.style.color = '#003f6b';
      
              startBtn.textContent = 'Start';
              startBtn.disabled = false;
              startBtn.setAttribute('data-state','idle');
          }
      
          // START
          startBtn.addEventListener('click', function(){
              var state = startBtn.getAttribute('data-state');
      
              if (state === 'idle') {
                  startBtn.textContent = 'Running...';
                  startBtn.setAttribute('data-state','running');
      
                  timerInterval = setInterval(function(){
                      totalSeconds--;
                      timerDisplay.textContent = formatTime(totalSeconds);
      
                      if (totalSeconds <= 0) finishTimer();
                  }, 1000);
      
              } else if (state === 'ended') {
                  stopAlarm();
                  startBtn.textContent = 'Stopped';
                  startBtn.disabled = true;
                  startBtn.setAttribute('data-state','stopped');
              }
          });
      
          // RESET
          resetBtn.addEventListener('click', function(){
              resetTimer();
          });
      
          // init state
          resetTimer();
      
      });
  ")),
  
  # ----- TABS -----
  shiny::tabsetPanel(
    
    # ======== TAB 1 — TEST ========
    shiny::tabPanel("1. Test",
                    shiny::div(
                      style = "display: flex; flex-direction: column; height: 80vh; justify-content: space-between; align-items: center;",
                      # ----- TOP HALF -----
                      shiny::div(
                        style = "flex: 1; width: 100%;display: flex; flex-direction: column; align-items: center; justify-content: center;",
                        shiny::div(
                          class = "prediction-card",
                          style = "background-color: #eef4ff; width: 100%;height: 100%; display: flex; flex-direction: column; justify-content: center;",
                          shiny::div(
                            id = "timer_display",
                            style = "font-size: 48px; font-weight: bold; color: #003f6b; margin-bottom: 20px;",
                            "06:00"
                          ),
                          shiny::div(
                            style = "display: flex; gap: 20px; justify-content: center;",
                            shiny::actionButton("timer_start", "Start", class = "btn btn-success btn-lg"),
                            shiny::actionButton("timer_reset", "Reset", class = "btn btn-secondary btn-lg")
                          )
                        )
                      ),
                      
                      # ----- BOTTOM HALF -----
                      shiny::div(
                        style = "flex: 1; width: 100%; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                        shiny::div(
                          class = "input-card",
                          style = "background-color: #eef4ff; width: 100%; text-align: center; height: 100%; display: flex; flex-direction: column; justify-content: center;",
                          shiny::tags$style(htmltools::HTML("
                                  #distance_walked { font-size: 22px; text-align: center; }
                                  #distance_walked-label { font-size: 22px; text-align: center; }
                                ")),
                          
                          shiny::numericInput(
                            inputId = "distance_walked",
                            label = "Walked distance (meters)",
                            value = NULL,
                            min = 0,
                            max = 2000,
                            step = 1,
                            width = "100%"
                          )
                        )
                      )
                    )
    ),
    
    # ======== TAB 2 — PREDICT ========
    shiny::tabPanel("2. Predict",
                    shiny::div(
                      class = "input-card",
                      shiny::selectInput("equation", "Select Equation", choices = equations, width = "100%"),
                      shiny::uiOutput("footnotes_container"),
                      shiny::uiOutput("doi_container")
                    ),
                    DT::DTOutput("dataTable"),
    ),
    
    # ======== TAB 3 — REPORT ========
    shiny::tabPanel("3. Report",
                    shiny::hr(),
                    shiny::div(class = "prediction-card",
                               "Predicted Value",
                               shiny::br(),
                               shiny::textOutput("prediction", inline = TRUE),
                               shiny::br(),
                               shiny::textOutput("percent_predicted", inline = TRUE)
                    ),
                    shiny::div(style = "margin-top: 40px; text-align: center;",
                               shiny::actionButton(
                                 "download_pdf_btn",
                                 "Download PDF",
                                 class = "btn btn-primary btn-lg"
                               ),
                    ),
    ),
    
    # ======== TAB 4 — Group ========
    shiny::tabPanel(
      title = list(fontawesome::fa("people-group"), "Credits"),
      shiny::br(),
      shiny::HTML(
        "<b>Arthur de Sá Ferreira, DSc</b> (Developer)"),
      shiny::br(),
      shiny::HTML(
        '<a id="cy-effective-orcid-url" class="underline" 
         href="https://orcid.org/0000-0001-7014-2002"
         target="orcid.widget" rel="me noopener noreferrer" style="vertical-align: top">
         <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width: 1em; margin-inline-start: 0.5em" alt="ORCID iD icon"/>
         https://orcid.org/0000-0001-7014-2002
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<b>Mariana de Almeida Pereira, MSc</b> (Contributor)"),
      shiny::br(),
      shiny::HTML(
        '<a id="cy-effective-orcid-url" class="underline" 
         href="https://orcid.org/0009-0004-4179-3489"
         target="orcid.widget" rel="me noopener noreferrer" style="vertical-align: top">
         <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width: 1em; margin-inline-start: 0.5em" alt="ORCID iD icon"/>
         https://orcid.org/0009-0004-4179-3489
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<b>Gabriel Batista de Lira, BSc</b> (Contributor)"),
      shiny::br(),
      shiny::HTML(
        '<a id="cy-effective-orcid-url" class="underline" 
         href="https://orcid.org/0009-0001-6056-4903"
         target="orcid.widget" rel="me noopener noreferrer" style="vertical-align: top">
         <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width: 1em; margin-inline-start: 0.5em" alt="ORCID iD icon"/>
         https://orcid.org/0009-0001-6056-4903
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<b>Centro Universitário Augusto Motta</b> (Affiliation) <br>
        Programa de Pós-graduação em Ciências da Reabilitação <br>
        Rio de Janeiro, RJ, Brazil"),
      shiny::br(),
      shiny::HTML(
        '<a
         href="https://ror.org/02ab1bc46"
         style="vertical-align: top">
         <img src="https://ror.org/assets/ror-logo-small-671ea83ad5060ad5c0c938809aab4731.png" style="width: 1em; margin-inline-start: 0.5em"/>
         https://ror.org/02ab1bc46
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML("<b>License</b>"),
      shiny::HTML(
        "This work is licensed under an <a rel=\"license\" data-spdx=\"Apache-2.0\" href=\"https://www.apache.org/licenses/LICENSE-2.0\">Apache License 2.0</a>."
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML("<b>Citation</b>"),
      shiny::HTML(
        "Arthur de Sá Ferreira, & PPGCR. (2025). FerreiraAS/UsIA: v1.0.0 (v1.0.0). Zenodo. h<a tref=\"htps://doi.org/10.5281/zenodo.17663547\">ttps://doi.org/10.5281/zenodo.17663547</a>."
      ),
      shiny::br(),
      shiny::br(),
    ),
  ),
)

server <- function(input, output, session) {
  
  edited_data <- shiny::reactiveVal(NULL)
  
  # load data when equation changes
  shiny::observeEvent(input$equation, {
    shiny::req(input$equation)
    
    df <- data[, c(1, 3, which(colnames(data) == input$equation))]
    colnames(df) <- c("Variable", "Unit", "Coefficient")
    
    # remove Year row
    df <- df[!grepl("Year", df$Variable, ignore.case = TRUE), ]
    # remove DOI row
    df <- df[!grepl("DOI", df$Variable, ignore.case = TRUE), ]
    # remove link row
    df <- df[!grepl("Link", df$Variable, ignore.case = TRUE), ]
    # remove Sample Size
    df <- df[!grepl("Sample Size", df$Variable, ignore.case = TRUE), ]
    # remove Sex scope
    df <- df[!grepl("Sex scope", df$Variable, ignore.case = TRUE), ]
    # remove Sex code female
    df <- df[!grepl("Sex code female", df$Variable, ignore.case = TRUE), ]
    # remove Country
    df <- df[!grepl("Country", df$Variable, ignore.case = TRUE), ]
    
    df <- df[!is.na(df$Coefficient), ]
    df$`Value` <- NA
    
    edited_data(df)
  })
  
  # render table with hidden PDF button
  output$dataTable <- DT::renderDT({
    shiny::req(edited_data())
    
    DT::datatable(
      edited_data(),
      extensions = "Buttons",
      editable = list(target = "cell", columns = 4),
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
                // Predicted Value + Walked Distance + Percent Predicted
                // --------------------------------------------------------
                
                var walked = parseFloat($('#distance_walked').val()) || 0;
                
                var predicted_clean = parseFloat(
                    prediction.replace(new RegExp('[^0-9.]','g'), '')
                ) || 0;
                
                var percent = (walked > 0 && predicted_clean > 0)
                    ? ((walked / predicted_clean) * 100).toFixed(1) + '%'
                    : '';
                                
                doc.content.push({
                    margin: [0, 20, 0, 10],
                    columns: [
                
                        // 1) Predicted Value card
                        {
                            width: '33%',
                            table: {
                                widths: ['*'],
                                body: [[
                                    {
                                        stack: [
                                            { text: 'Predicted Value', fontSize: 14, bold: true, margin: [0, 0, 0, 8] },
                                            { text: prediction, fontSize: 22, bold: true, color: '#003f6b' }
                                        ],
                                        fillColor: '#f2f5f7',
                                        margin: [0, 12, 0, 12],
                                        alignment: 'center'
                                    }
                                ]]
                            },
                            layout: 'noBorders'
                        },
                
                        // 2) Walked Distance card
                        {
                            width: '33%',
                            table: {
                                widths: ['*'],
                                body: [[
                                    {
                                        stack: [
                                            { text: 'Walked Distance', fontSize: 14, bold: true, margin: [0, 0, 0, 8] },
                                            { text: walked + ' meters', fontSize: 22, bold: true, color: '#003f6b' }
                                        ],
                                        fillColor: '#f2f5f7',
                                        margin: [0, 12, 0, 12],
                                        alignment: 'center'
                                    }
                                ]]
                            },
                            layout: 'noBorders'
                        },
                
                        // 3) Percent Predicted card
                        {
                            width: '33%',
                            table: {
                                widths: ['*'],
                                body: [[
                                    {
                                        stack: [
                                            { text: '% Predicted', fontSize: 14, bold: true, margin: [0, 0, 0, 8] },
                                            { text: percent, fontSize: 22, bold: true, color: '#003f6b' }
                                        ],
                                        fillColor: '#f2f5f7',
                                        margin: [0, 12, 0, 12],
                                        alignment: 'center'
                                    }
                                ]]
                            },
                            layout: 'noBorders'
                        }
                
                    ]
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
    # get Country and Sample Size from df
    foot <- c(
      gsub("Country", "", df$Coefficient[grepl("Country", df$Variable, ignore.case = TRUE)]),
      gsub("Sample Size", "", df$Coefficient[grepl("Sample Size", df$Variable, ignore.case = TRUE)])
    )
    
    shiny::div(
      id = "footnotes",
      htmltools::HTML(paste0(
        "Country: ", foot[1],
        "; ",
        "Sample Size: ", foot[2]
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
    link_row <- df[grepl("Link", df$Variable, ignore.case = TRUE), ]
    link_value <- as.character(link_row$Coefficient[1])
    
    # If DOI is NA, use the Link column
    if (is.na(doi_value)) {
      shiny::div(
        id = "doi",
        htmltools::HTML(sprintf(
          'Link: <a href="%s" target="_blank">%s</a>',
          link_value, link_value
        ))
      )
    } else {
      shiny::div(
        id = "doi",
        htmltools::HTML(sprintf(
          'DOI: <a href="https://doi.org/%s" target="_blank">%s</a>',
          doi_value, doi_value
        ))
      )
    }
  })
  
  # prediction
  output$prediction <- shiny::renderText({
    df <- edited_data()
    shiny::req(df)
    
    df$Coefficient <- suppressWarnings(as.numeric(df$Coefficient))
    df$`Value` <- suppressWarnings(as.numeric(df$`Value`))
    
    df_no_intercept <- df[!df$Variable %in% c("Intercept"), ]
    
    if (any(is.na(df_no_intercept$`Value`))) {
      return("")
    }
    
    df$`Value`[is.na(df$`Value`)] <- 0
    df$prod <- df$Coefficient * df$`Value`
    
    if ("Intercept" %in% df$Variable) {
      intercept_value <- df$Coefficient[df$Variable == "Intercept"]
      df$prod[df$Variable == "Intercept"] <- intercept_value
    }
    
    pred <- sum(df$prod, na.rm = TRUE)
    
    paste0(format(round(pred, 0), nsmall = 0), " meters")
  })
  
  output$percent_predicted <- shiny::renderText({
    df <- edited_data()
    shiny::req(df)
    
    walked <- suppressWarnings(as.numeric(input$distance_walked))
    if (is.na(walked) || walked <= 0) return("")
    
    df$Coefficient <- suppressWarnings(as.numeric(df$Coefficient))
    df$`Value` <- suppressWarnings(as.numeric(df$`Value`))
    
    pred <- NULL
    df_no_intercept <- df[!df$Variable %in% c("Intercept"), ]
    if (any(is.na(df_no_intercept$`Value`))) {
      return("")
    }
    df$`Value`[is.na(df$`Value`)] <- 0
    df$prod <- df$Coefficient * df$`Value`
    if ("Intercept" %in% df$Variable) {
      intercept_value <- df$Coefficient[df$Variable == "Intercept"]
      df$prod[df$Variable == "Intercept"] <- intercept_value
    }
    pred <- sum(df$prod, na.rm = TRUE)
    pct <- (walked / pred) * 100
    paste0(round(pct, 1), "%")
  })
  
  # ----- bottom button triggers DT PDF -----
  shiny::observeEvent(input$download_pdf_btn, {
    session$sendCustomMessage("trigger_pdf", list())
  })
}

shiny::shinyApp(ui, server)
