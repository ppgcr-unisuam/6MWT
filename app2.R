# app.R - atualizado com timer, cores e proteção do Intercept
library(shiny)
library(readxl)
library(DT)
library(htmltools)

# ----------------------------
# Try to read Excel (relative path)
# ----------------------------
excel_path <- "./0208 Aggregation published models.xlsx"
data <- tryCatch({
  df <- read_xlsx(excel_path, col_names = TRUE)
  df[df == ""] <- NA
  df
}, error = function(e) {
  NULL
})

# create www dir for resources if needed
dir.name <- 'www'
if (!dir.exists(dir.name)) {
  dir.create(dir.name, recursive = TRUE, showWarnings = FALSE)
}
shiny::addResourcePath(prefix = "www", directoryPath = "www")

# Prepare equations if data available
equations <- if (!is.null(data)) colnames(data)[-1] else character(0)

# UI
ui <- fluidPage(
  tags$head(
    # basic font + palette + small UI tweaks
    tags$style(HTML("
      body { font-family: -apple-system, BlinkMacSystemFont, 'Inter', sans-serif; background: #FFFFFF; color: #222222; }
      .title-main { color: #0A49FF; font-weight: 700; text-align: center; margin-top: 8px; }
      .subtitle { color: #6B7280; text-align:center; margin-bottom: 12px; }
      .timer-row { display:flex; justify-content:center; align-items:center; gap:8px; margin-bottom: 12px; }
      #timer { font-size: 48px; font-weight: 700; color: #0A49FF; letter-spacing: 2px; font-family: 'SF Mono', 'Roboto Mono', monospace; }
      #resetIcon { cursor:pointer; font-size:20px; padding:6px; border-radius:8px; }
      #resetIcon:hover { background: rgba(10,73,255,0.06); }
      .start-btn { background-color: #0A49FF; color: white; border: none; border-radius: 12px; padding: 10px 18px; font-size: 16px; font-weight:600; }
      .start-btn[disabled] { opacity: 0.55; }
      .prediction-card { background-color: #f2f6fb; border-radius: 12px; padding: 18px; margin-top: 20px; text-align: center; font-size: 20px; font-weight: 600; color: #0A49FF; box-shadow: 0 2px 6px rgba(0,0,0,0.06); }
      .pred-value { font-size: 36px; font-weight: 800; color: #003f6b; margin-top:8px; }
      .top-right-toggle { position: fixed; top: 8px; right: 12px; z-index: 999; cursor: pointer; font-size:16px; }
      .dark-mode { background: #0b0b0d; color: #dcdcdc !important; }
      .dark-mode .prediction-card { background-color: #0f1215; color: #9fc7ff; }
      /* center DT cells */
      table.dataTable td, table.dataTable th { text-align: center !important; vertical-align: middle !important; }
    "))
  ),
  
  # theme toggle icon
  span(class = "top-right-toggle", id = "themeToggle", "\u263C"),
  
  # App Title
  div(class = "title-main", h2("Six-Minute Walk Test")),
  div(class = "subtitle", "Clinical tool"),
  
  # Timer row (centered)
  div(class = "timer-row",
      div(id = "timer", "06:00"),
      span(id = "resetIcon", "\u21BA", title = "Reset timer (click)"),
      actionButton("startBtn", "Start", class = "start-btn", `data-state` = "idle")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      # If data missing, show warning and file.choose suggestion
      uiOutput("eq_selector_ui"),
      uiOutput("footnotes_container"),
      uiOutput("doi_container")
    ),
    
    mainPanel(
      br(),
      DT::DTOutput("dataTable"),
      hr(),
      div(class = "prediction-card",
          "Predicted Value:",
          div(class = "pred-value", textOutput("prediction", inline = TRUE))
      ),
      div(style = "margin-top: 24px; text-align:center;",
          actionButton("download_pdf_btn", "Download PDF", class = "btn btn-primary btn-lg")
      )
    )
  ),
  
  # hidden audio element for alarm
  tags$audio(id = "alarmSound", src = "https://actions.google.com/sounds/v1/emergency/beeper_emergency_call.ogg", type = "audio/ogg"),
  
  # JS handlers:
  tags$script(HTML("
    // Theme toggle
    document.getElementById('themeToggle').addEventListener('click', function() {
      document.body.classList.toggle('dark-mode');
      this.textContent = document.body.classList.contains('dark-mode') ? '\\u263E' : '\\u263C';
    });

    // Timer functionality
    (function(){
      var INITIAL_SECONDS = 6*60;
      var totalSeconds = INITIAL_SECONDS;
      var timerDisplay = document.getElementById('timer');
      var startBtn = document.getElementById('startBtn');
      var resetIcon = document.getElementById('resetIcon');
      var alarm = document.getElementById('alarmSound');
      alarm.volume = 1.0;
      var timerInterval = null;
      var alarmPlaying = false;

      function formatTime(sec) {
        var m = Math.floor(sec/60);
        var s = sec % 60;
        return String(m).padStart(2,'0') + ':' + String(s).padStart(2,'0');
      }

      function shortBeep() {
        try {
          var ctx = new (window.AudioContext || window.webkitAudioContext)();
          var o = ctx.createOscillator();
          var g = ctx.createGain();
          o.type = 'sine';
          o.frequency.value = 880;
          g.gain.value = 0.04;
          o.connect(g);
          g.connect(ctx.destination);
          o.start();
          setTimeout(function(){ o.stop(); ctx.close(); }, 110);
        } catch(e){}
      }

      function resetTimer(playBeep) {
        clearInterval(timerInterval);
        timerInterval = null;
        totalSeconds = INITIAL_SECONDS;
        if (timerDisplay) { timerDisplay.textContent = formatTime(totalSeconds); timerDisplay.style.color = '#0A49FF'; }
        if (startBtn) { startBtn.textContent = 'Start'; startBtn.disabled = false; startBtn.setAttribute('data-state','idle'); }
        try { alarm.pause(); alarm.currentTime = 0; alarmPlaying = false; } catch(e){}
        if (playBeep) shortBeep();
      }

      function finishTimer() {
        clearInterval(timerInterval);
        timerInterval = null;
        if (timerDisplay) { timerDisplay.textContent = 'Time is up!'; timerDisplay.style.color = 'red'; }
        try { alarm.currentTime = 0; alarm.play(); alarmPlaying = true; } catch(e){}
        if (startBtn) { startBtn.textContent = 'Stop'; startBtn.disabled = false; startBtn.setAttribute('data-state','ended'); }
      }

      if (startBtn) {
        startBtn.addEventListener('click', function(){
          var state = startBtn.getAttribute('data-state');
          if (state === 'idle') {
            // start
            startBtn.textContent = 'Waiting…';
            startBtn.disabled = true;
            startBtn.setAttribute('data-state','running');
            if (timerDisplay) timerDisplay.textContent = formatTime(totalSeconds);
            timerInterval = setInterval(function(){
              totalSeconds--;
              if (totalSeconds >= 0) {
                if (timerDisplay) timerDisplay.textContent = formatTime(totalSeconds);
              }
              if (totalSeconds < 0) {
                finishTimer();
              }
            }, 1000);
          } else if (state === 'ended') {
            // Stop button: stop alarm and set to Stopped (disabled)
            try { if (alarmPlaying) { alarm.pause(); alarm.currentTime = 0; alarmPlaying = false; } } catch(e){}
            startBtn.textContent = 'Stopped';
            startBtn.disabled = true;
            startBtn.setAttribute('data-state','stopped');
          }
        });
      }

      if (resetIcon) {
        resetIcon.addEventListener('click', function(){
          resetTimer(true);
        });
      }

      // init
      if (timerDisplay) timerDisplay.textContent = formatTime(totalSeconds);
      if (startBtn) { startBtn.textContent = 'Start'; startBtn.disabled = false; startBtn.setAttribute('data-state','idle'); }

      // allow server side to reset beep via custom message
      Shiny.addCustomMessageHandler('resetTimer', function(msg) { resetTimer(false); });
      Shiny.addCustomMessageHandler('beep', function(msg) { shortBeep(); });

    })();
  ")),
  
  # JS helper to trigger PDF click (kept from original)
  tags$script(HTML("
    Shiny.addCustomMessageHandler('trigger_pdf', function(message) {
      $('.buttons-pdf').click();
    });
  "))
)

# Server
server <- function(input, output, session) {
  
  # reactive to hold edited data
  edited_data <- reactiveVal(NULL)
  
  # Provide UI for equation selector (if data missing, show message)
  output$eq_selector_ui <- renderUI({
    if (is.null(data)) {
      tagList(
        div(style = "color:#900; font-weight:700;", "Spreadsheet not found."),
        div("Place './0208 Aggregation published models.xlsx' in the app folder and restart the app.")
      )
    } else {
      selectInput("equation", "Select Equation:", choices = colnames(data)[-1], width = "100%")
    }
  })
  
  # Load data when equation changes
  observeEvent(input$equation, {
    req(input$equation)
    # build df like original code
    df <- data[, c(1, which(colnames(data) == input$equation))]
    colnames(df) <- c("Subject", "Coefficient")
    
    # remove DOI row(s) and first two footnotes like original
    df <- df[!grepl("DOI", df$Subject, ignore.case = TRUE), ]
    # careful with indexing: if less than 2 rows, guard
    if (nrow(df) >= 2) {
      df <- df[-c(1,2), , drop = FALSE]
    }
    df <- df[!is.na(df$Coefficient), , drop = FALSE]
    # add patient's value column
    df$`Patient's Value` <- NA
    edited_data(df)
  }, ignoreNULL = TRUE)
  
  # Render DT with hidden PDF button (keeps original PDF customization)
  output$dataTable <- DT::renderDT({
    req(edited_data())
    df <- edited_data()
    
    # prepare datatable; enable cell editing only in column 3 (Patient's Value)
    DT::datatable(
      df,
      extensions = "Buttons",
      editable = list(target = "cell", columns = 3),
      rownames = FALSE,
      options = list(
        dom = "Brt",
        scrollX = FALSE,
        paging = FALSE,
        searching = FALSE,
        buttons = list(
          list(
            extend = "pdf",
            text = "hidden_pdf_button",
            title = "Six-Minute Walk Test Report",
            pageSize = "A4",
            exportOptions = list(columns = ":visible"),
            customize = DT::JS("
              function (doc) {
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
                doc.content.push({ text: 'DOI:', style: 'subheader', margin: [0,10,0,3] });
                doc.content.push({
                  text: doi,
                  link: 'https://doi.org/' + doi,
                  color: '#0645AD',
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
                doc.styles.header = { fontSize: 18, bold: true, alignment: 'left' };
                doc.styles.subheader = { fontSize: 14, bold: true };
              }
            ")
          )
        )
      )
    )
  }, server = FALSE)
  
  # intercept edit protection: if user edits Intercept row Patient's Value, revert
  observeEvent(input$dataTable_cell_edit, {
    info <- input$dataTable_cell_edit
    df <- edited_data()
    # JS col index is 0-based; info$col gives 0-based
    r <- info$row
    c <- info$col + 1
    # if editing the Patient's Value column (3) and that row is intercept, ignore change
    if (!is.null(df) && r >= 1 && r <= nrow(df)) {
      subj <- as.character(df$Subject[r])
      if (c == 3 && subj %in% c("Intercept", "INTERCEPT", "intercept")) {
        showNotification("Intercept is read-only.", type = "warning")
        # trigger a redraw by resetting edited_data to itself (reverting)
        edited_data(df)
        return()
      }
    }
    # otherwise apply edit
    if (!is.null(df)) {
      df[r, c] <- info$value
      edited_data(df)
    }
  })
  
  # footnotes container
  output$footnotes_container <- renderUI({
    req(input$equation)
    df <- data[, c(1, which(colnames(data) == input$equation))]
    colnames(df) <- c("Subject", "Coefficient")
    # guard in case fewer rows
    if (nrow(df) >= 2) {
      foot <- df[1:2, , drop = FALSE]
      htmltools::HTML(paste0(
        '<div id="footnotes">Footnotes: ',
        foot[1,1], ': ', ifelse(is.na(foot[1,2]), '', round(as.numeric(foot[1,2]), 0)),
        ', ', foot[2,1], ': ', ifelse(is.na(foot[2,2]), '', foot[2,2]),
        '</div>'
      ))
    } else {
      NULL
    }
  })
  
  # doi container
  output$doi_container <- renderUI({
    req(input$equation)
    df <- data[, c(1, which(colnames(data) == input$equation))]
    colnames(df) <- c("Subject", "Coefficient")
    doi_row <- df[grepl("DOI", df$Subject, ignore.case = TRUE), , drop = FALSE]
    if (nrow(doi_row) >= 1) {
      doi_value <- as.character(doi_row$Coefficient[1])
      htmltools::HTML(sprintf('<div id="doi">DOI: <a href="https://doi.org/%s" target="_blank">%s</a></div>', doi_value, doi_value))
    } else {
      NULL
    }
  })
  
  # prediction text
  output$prediction <- renderText({
    df <- edited_data()
    req(df)
    # ensure numeric
    df$Coefficient <- suppressWarnings(as.numeric(df$Coefficient))
    df$`Patient's Value` <- suppressWarnings(as.numeric(df$`Patient's Value`))
    
    # check that non-intercept Patient's values are provided
    df_no_intercept <- df[!df$Subject %in% c("Intercept"), , drop = FALSE]
    if (any(is.na(df_no_intercept$`Patient's Value`))) {
      return("")  # not enough data to compute
    }
    
    # compute product
    df$`Patient's Value`[is.na(df$`Patient's Value`)] <- 0
    df$prod <- df$Coefficient * df$`Patient's Value`
    
    if ("Intercept" %in% df$Subject) {
      intercept_value <- df$Coefficient[df$Subject == "Intercept"]
      df$prod[df$Subject == "Intercept"] <- intercept_value
    }
    
    pred <- sum(df$prod, na.rm = TRUE)
    paste0(format(round(pred, 0), nsmall = 0), " meters")
  })
  
  # compute prediction range (kept original)
  prediction_range <- reactive({
    df <- edited_data()
    req(df)
    df$Coefficient <- suppressWarnings(as.numeric(df$Coefficient))
    df$`Patient's Value` <- suppressWarnings(as.numeric(df$`Patient's Value`))
    
    intercept <- if ("Intercept" %in% df$Subject) df$Coefficient[df$Subject == "Intercept"] else 0
    df2 <- df[df$Subject != "Intercept", , drop = FALSE]
    
    # handle NA gracefully for min/max
    if (all(is.na(df2$`Patient's Value`))) {
      return(list(min = intercept, max = intercept, df_details = df2))
    }
    
    df2$min_contrib <- ifelse(df2$Coefficient >= 0,
                              df2$Coefficient * min(df2$`Patient's Value`, na.rm = TRUE),
                              df2$Coefficient * max(df2$`Patient's Value`, na.rm = TRUE))
    df2$max_contrib <- ifelse(df2$Coefficient >= 0,
                              df2$Coefficient * max(df2$`Patient's Value`, na.rm = TRUE),
                              df2$Coefficient * min(df2$`Patient's Value`, na.rm = TRUE))
    
    min_pred <- sum(df2$min_contrib, na.rm = TRUE) + intercept
    max_pred <- sum(df2$max_contrib, na.rm = TRUE) + intercept
    
    list(min = min_pred, max = max_pred, df_details = df2)
  })
  
  # trigger hidden DT PDF button when bottom button clicked
  observeEvent(input$download_pdf_btn, {
    session$sendCustomMessage("trigger_pdf", list())
  })
  
  # If no data loaded, show a modal once to inform
  observe({
    if (is.null(data)) {
      showModal(modalDialog(
        title = "Spreadsheet not found",
        paste0("Could not find './0208 Aggregation published models.xlsx' in the app folder. Please place the file in the app folder and restart the app."),
        easyClose = TRUE
      ))
    }
  })
  
}

shinyApp(ui, server)
