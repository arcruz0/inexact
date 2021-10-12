#' Launches the inexact RStudio Addin
#'
#' This function launches a GUI to supervise a fuzzy join.
#'
#' @importFrom stringr "%>%"
#'
#' @export

inexact_addin <- function() {
  # Function to allow selectize plugins
  # source: https://gist.github.com/saurfang/6684a66601a7e5b6467f
  addUIDep <- function(x) {
    jqueryUIDep <- htmltools::htmlDependency("jqueryui", "1.10.4",
                                             c(href = "shared/jqueryui/1.10.4"),
                                             script = "jquery-ui.min.js",
                                             stylesheet = "jquery-ui.min.css"
    )
    
    htmltools::attachDependencies(
      x, c(htmltools::htmlDependencies(x), list(jqueryUIDep)))
  }
  
  # Build UI
  ui <- miniUI::miniPage(
    shiny::includeCSS(system.file("css",
                                  "highlight.css", package = "inexact")),
    
    shiny::uiOutput("title_bar"),
    
    miniUI::miniTabstripPanel(
      id = "active_tab",
      selected = "1. Settings",
      miniUI::miniTabPanel(
        title = "1. Settings",
        icon = shiny::icon("cogs"),
        miniUI::miniContentPanel(
          shiny::fillCol(
            shiny::fillRow(
              shiny::selectizeInput("df_x", "x (1st data frame)",
                                    choices = Filter(
                                      function(x) {is.data.frame(get(x))},
                                      ls(.GlobalEnv)
                                    ),
                                    options = list(items = "[]"),
                                    multiple = F
              ),
              shiny::uiOutput("ui_df_y")
            ),
            shiny::fillRow(
              shiny::uiOutput("ui_by")
            ),
            shiny::fillRow(
              shiny::selectizeInput("join_type", "Join Mode",
                                    choices = c(
                                      "left", "full"
                                    ),
                                    multiple = F
              ),
              shiny::selectizeInput("dist_alg", "Distance Algorithm",
                                    choices = c(
                                      "osa", "lv",
                                      "dl", "hamming", "lcs", "qgram",
                                      "cosine", "jaccard", "jw"
                                    ),
                                    multiple = F
              )
            ),
            shiny::fillRow(
              shiny::numericInput("num_choices", "Number of choices in UI",
                                  value = 5, min = 1)
            )
          )
        )
      ),
      
      miniUI::miniTabPanel(
        title = "2. Review",
        icon = shiny::icon("hand-pointer"),
        miniUI::miniContentPanel(
          DT::dataTableOutput("ui_df_matches", height = "100%")
        )
      ),
      
      miniUI::miniTabPanel(
        title = "3. Final Code",
        icon = shiny::icon("code"),
        
        miniUI::miniContentPanel(
          shiny::fillCol(
            htmltools::div(
              style = "padding-left: 1em; padding-right: 1em;",
              shiny::fluidRow(
                shiny::checkboxInput("checkbox_clipboard",
                                     "Copy to clipboard after clicking 'Done'", 
                                     value = T)
              )
            ),
            htmltools::div(
              style = "padding-left: 1em; padding-right: 1em;",
              shiny::fluidRow(
                shiny::checkboxInput("checkbox_match_cols",
                                     "Keep matching columns in final data frame")
              )
            ),
            htmltools::div(
              style = "padding-left: 1em; padding-right: 1em;",
              shiny::fluidRow(
                shiny::htmlOutput("ui_code")
              )
            ),
            flex = c(1.5, 1.5, 7)
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    r_checkbox_clipboard <- shiny::reactive({input$checkbox_clipboard})
    
    # The Done button closes the app
    shiny::observeEvent(
      eventExpr = input$btn_done,
      handlerExpr = {
        suppressWarnings(try(
          message(
            glue::glue(
              "

              ------------------------------------- inexact output
              {stringr::str_replace_all(.code_text, '&nbsp;', ' ')}
              -------------------------------------
              "
            )
          )
        ))
        
        if (r_checkbox_clipboard()){
          clipr::write_clip(stringr::str_replace_all(.code_text, '&nbsp;', ' '))
        }
        
        shiny::stopApp()
      }
    )
    
    # Create the title bar programatically
    output$title_bar <- shiny::renderUI(miniUI::gadgetTitleBar(
      "inexact: fuzzy join supervisor",
      left = if (shiny::req(input$active_tab) == "1. Settings") {
        NULL
      } else {
        miniUI::miniTitleBarButton("btn_prev", "Previous", primary = F)
      },
      right = if (shiny::req(input$active_tab) == "3. Final Code") {
        miniUI::miniTitleBarButton("btn_done", "Done", primary = T)
      } else {
        miniUI::miniTitleBarButton("btn_next", "Next", primary = T)
      }
    ))
    
    # Program the "previous" button
    shiny::observeEvent(
      input$btn_prev,
      if (shiny::isTruthy(input$active_tab)) {
        if (input$active_tab == "2. Review") {
          shiny::showTab("active_tab", "1. Settings", select = T)
        } else {
          shiny::showTab("active_tab", "2. Review", select = T)
        }
      }
    )
    
    # Program the "next" button
    shiny::observeEvent(
      input$btn_next,
      if (shiny::isTruthy(input$active_tab)) {
        if (input$active_tab == "1. Settings") {
          shiny::showTab("active_tab", "2. Review", select = T)
        } else {
          shiny::showTab("active_tab", "3. Final Code", select = T)
        }
      }
    )
    
    # Create the selectize box for the second df
    output$ui_df_y <- shiny::renderUI(
      if (shiny::isTruthy(input$df_x)) {
        # Don't allow to select the same x df
        possible_df_y <- setdiff(
          Filter(
            function(x) {is.data.frame(get(x))},
            ls(.GlobalEnv)
          ),
          input$df_x
        )
        shiny::selectizeInput("df_y", "y (2nd data frame)",
                              choices = possible_df_y,
                              options = list(items = "[]"),
                              multiple = F
        )
      } else {
        shiny::selectizeInput("df_y", "y (2nd data frame)",
                              choices = Filter(
                                function(x) {
                                  is.data.frame(get(x))
                                },
                                ls(.GlobalEnv)
                              ),
                              options = list(items = "[]"),
                              multiple = F
        )
      }
    )
    
    # Create the selectize box for "by"
    output$ui_by <- shiny::renderUI(
      if (shiny::isTruthy(input$df_x) & shiny::isTruthy(input$df_y)) {
        possible_by_vars <- intersect(
          names(get(input$df_x)),
          names(get(input$df_y))
        )
        addUIDep(shiny::selectizeInput(
          "by_vars", "by (id variable)",
          choices = possible_by_vars,
          options = list(
            items = "[]",
            plugins = list("drag_drop", "remove_button")
          ),
          multiple = F
        ))
      } else {
        # Empty selectize box if dfs are not selected
        shiny::selectizeInput("by_vars", "by (id variable)",
                              choices = "",
                              multiple = F
        )
      }
    )
    
    f_w_list_matches <- function(x, y, by, method, num_choices) {
      
      x <- eval(parse(text = x)); y <- eval(parse(text = y))
      
      x <- unique(x[[by]]); y <- unique(y[[by]])
      
      x <- setdiff(x, y)
      
      if (identical(x, character(0))){
        return(NULL)
      }
      
      if (num_choices > length(y)){num_choices <- length(y)}

      matrix_match <- stringdist::stringdistmatrix(x, y, method = method)
      
      rownames(matrix_match) <- x
      colnames(matrix_match) <- y
      
      res <- vector("list", length = length(x))
      
      names(res) <- x
      
      for (i in 1:length(res)){
        matrix_match_row <- sort(matrix_match[i,])[1:num_choices]
        
        res[[i]] <- list(
          matrix_original = matrix_match_row,
          matrix_show = c(paste0("(", matrix_match_row,
                               ") ", names(matrix_match_row)),
                          "<NA>"),
          min = min(matrix_match_row)
        )
      }
      
      res <- res[order(purrr::map_dbl(res, ~ .x$min), decreasing = T)]
      
      return(res)
      
    }
    
    # Make the function reactive to input
    r_w_list_matches <- shiny::reactive({
      f_w_list_matches(input$df_x,
                       input$df_y,
                       input$by_vars,
                       input$dist_alg,
                       input$num_choices)
    })
    
    f_df_datatable_inicial <- function(with_id){
      if (shiny::isTruthy(input$df_x) & shiny::isTruthy(input$df_x)) {
        
        w_list_matches <- r_w_list_matches()
        
        if (is.null(w_list_matches) == T){
          NULL
        } else {
          values_to_show <- purrr::map(w_list_matches, ~ .x[["matrix_show"]])
          
          df_datatable <- data.frame(
            id_1 = names(w_list_matches),
            id_2 = purrr::map2_chr(
              .x = stringr::str_c("review_", 1:length(w_list_matches)),
              .y = values_to_show,
              .f = ~ as.character(
                htmltools::div(
                  shiny::selectizeInput(inputId = .x,
                                        choices = .y,
                                        label   = NULL,
                                        width   = "90%"),
                  style = "height: 30px;"
                )
              )
            ),
            stringsAsFactors = F
          )
          
          if (with_id == T) {
            df_datatable$input_id <- purrr::map_chr(values_to_show, ~ .x[[1]])
          }
          
          return(df_datatable)
        }
      }
    }
    
    
    
    # Create the DT with matches, for the review process
    output$ui_df_matches <- DT::renderDataTable(
      if (shiny::isTruthy(input$df_x) & shiny::isTruthy(input$df_x)) {
        
        # Load the df with suggested matches
        
        df_datatable <- f_df_datatable_inicial(with_id = F)
        
        DT::datatable(
          df_datatable,
          escape = FALSE, selection = "none",
          colnames = c("ID from x", "ID from y?"),
          options = list(
            pageLength      = 5,
            searching       = FALSE,
            language.emptyTable = "Empty Table",
            # https://github.com/rstudio/shiny/issues/1246
            preDrawCallback = htmlwidgets::JS('function() { Shiny.unbindAll(this.api().table().node());}'),
            drawCallback    = htmlwidgets::JS('function() { Shiny.bindAll(this.api().table().node()); } '))) %>%
          DT::formatStyle(0, lineHeight='20%')
        # }
      },
      server = FALSE)
    
    r_checkbox_match_cols <- shiny::reactive({input$checkbox_match_cols})
    
    output$ui_code <- shiny::renderUI(
      if (shiny::isTruthy(input$df_x) & shiny::isTruthy(input$df_y)) {
        
        w_list_matches <- r_w_list_matches()
        
        df_datatable <- f_df_datatable_inicial(with_id = T)
        
        if (length(w_list_matches) > 0){

          v_reviewed_id <- df_datatable$input_id
          
          for (i in 1:length(v_reviewed_id)) {
            new_value <- eval(parse(text = paste("input$review_", i, sep = "")))
            
            if (length(new_value) > 0){
              v_reviewed_id[[i]] <- new_value
            }
            
          }
          
          df_review <- data.table::data.table(
            orig_id = names(w_list_matches),
            suggested_id = purrr::map_chr(w_list_matches, ~ .x[["matrix_show"]][1]),
            reviewed_id = v_reviewed_id,
            reviewed_id_clean = stringr::str_remove(v_reviewed_id, "^\\(\\d+\\)\\s"),
            stringsAsFactors = F
          ) %>%
            .[, agree := data.table::fifelse(suggested_id == reviewed_id, 1, 0)] %>% 
            .[agree == 0] %>% 
            .[, code_expr := stringr::str_c("\"", orig_id, "\" = \"", reviewed_id_clean, "\",")] %>%
            .[, code_expr := stringr::str_replace(code_expr, "\"<NA>\"", "NA")]
        
        } else {
          df_review <- NULL
        }
        
        chr_join_function <- stringr::str_c("stringdist_", input$join_type)
        
        chr_dplyr_join <- stringr::str_c(input$join_type, "_join")
        
        r_checkbox_match_cols2 <- r_checkbox_match_cols()
        
        code_show_cols <- ""
        
        if (r_checkbox_match_cols2 == F) {
          code_show_cols <- ""
        } else if (r_checkbox_match_cols2 == T) {
          code_show_cols <- stringr::str_c(",\n", strrep(" ", 2), "match_cols = T")
        }
        
        if (is.null(df_review)){
          .code_text <- glue::glue(
            "
          # There are only perfect matches! A regular join is sufficient:
          dplyr::{chr_dplyr_join}(
            x  = {input$df_x},
            y  = {input$df_y},
            by = \"{input$by_vars}\"
          )
          ")
        } else if (nrow(df_review) > 0) {
          coll_string <- paste("\n", strrep('&nbsp;', 2))
          
          chr_changes <- stringr::str_c(df_review$code_expr, collapse = coll_string) %>%
            stringr::str_remove(",$")
          
          .code_text <- glue::glue(
            "
          # You added custom matches:
          inexact::inexact_join(
            x  = {input$df_x},
            y  = {input$df_y},
            by = \"{input$by_vars}\",
            method = \"{input$dist_alg}\",
            mode = \"{input$join_type}\",
            custom_match = c(
             {chr_changes}
            ){code_show_cols}
          )
          "
          )
        } else {
          .code_text <- glue::glue(
            "
          # You didn't add any custom matches! Let's trust the algorithm:
          inexact::inexact_join(
            x    = {input$df_x},
            y    = {input$df_y},
            by   = \"{input$by_vars}\",
            method = \"{input$dist_alg}\",
            mode = \"{input$join_type}\"{code_show_cols}
          )
            "
          )
        }
        
        .code_text <<- .code_text
        
        htmltools::HTML(glue::glue(
          "
          <pre>{.code_text}</pre>
          "
        ))
        
      }
    )
  }
  shiny::runGadget(ui, server,
                   viewer = shiny::dialogViewer(
                     dialogName = "inexact: fuzzy join supervisor",
                     width = 600, height = 1000))
  
  suppressWarnings(rm(.code_text, envir = .GlobalEnv))
}
