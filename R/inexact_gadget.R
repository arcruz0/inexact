#' Launches the inexact RStudio Addin
#'
#' This function launches a GUI to supervise a fuzzy join.
#'
#' @import shiny
#' @import rstudioapi
#' @import miniUI
#' @import stringr
#' @import purrr
#' @import fuzzyjoin
#' @import glue
#' @import htmltools
#' @import tidyr
#' @import rlang
#' @export
inexact_gadget <- function() {
  # Function to allow selectize plugins
  # source: https://gist.github.com/saurfang/6684a66601a7e5b6467f
  addUIDep <- function(x) {
    jqueryUIDep <- htmlDependency("jqueryui", "1.10.4",
                                  c(href = "shared/jqueryui/1.10.4"),
                                  script = "jquery-ui.min.js",
                                  stylesheet = "jquery-ui.min.css"
    )

    attachDependencies(x, c(htmlDependencies(x), list(jqueryUIDep)))
  }

  # Build UI
  ui <- miniPage(
    includeCSS(system.file("css",
                           "highlight.css", package = "inexact"))
    ,

    uiOutput("title_bar"),

    miniTabstripPanel(
      id = "active_tab",
      selected = "1. Settings",
      miniTabPanel(
        title = "1. Settings",
        icon = icon("cogs"),
        miniContentPanel(
          fillCol(
            fillRow(
              selectizeInput("df_x", "x (1st data frame)",
                             choices = Filter(
                               function(x) {is.data.frame(get(x))},
                               ls(.GlobalEnv)
                             ),
                             options = list(items = "[]"),
                             multiple = F
              ),
              uiOutput("ui_df_y")
            ),
            fillRow(
              uiOutput("ui_by")
            ),
            fillRow(
              selectizeInput("join_type", "Join Mode",
                             choices = c(
                               "left", "full"
                             ),
                             multiple = F
              ),
              selectizeInput("dist_alg", "Distance Algorithm",
                             choices = c(
                               "osa", "lv",
                               "dl", "hamming", "lcs", "qgram",
                               "cosine", "jaccard", "jw"
                             ),
                             multiple = F
              )
            )
          )
        )
      ),

      miniTabPanel(
        title = "2. Review",
        icon = icon("hand-pointer"),
        miniContentPanel(
          DT::dataTableOutput("ui_df_matches", height = "100%")
        )
      ),

      miniTabPanel(
        title = "3. Final Code",
        icon = icon("code"),

        miniContentPanel(
          fillCol(
            div(
              style = "padding-left: 1em; padding-right: 1em;",
              fluidRow(
                checkboxInput("checkbox_match_cols",
                              "Keep matching columns in final data frame")
              )
            ),
            div(
              style = "padding-left: 1em; padding-right: 1em; margin-top:-11em",
              fluidRow(
                htmlOutput("ui_code")
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {


    # The Done button closes the app
    observeEvent(
      eventExpr = input$btn_done,
      handlerExpr = {
        stopApp()
      }
    )

    # Create the title bar programatically
    output$title_bar <- renderUI(gadgetTitleBar(
      "inexact: fuzzy join supervisor",
      left = if (req(input$active_tab) == "1. Settings") {
        NULL
      } else {
        miniTitleBarButton("btn_prev", "Previous", primary = F)
      },
      right = if (req(input$active_tab) == "3. Final Code") {
        miniTitleBarButton("btn_done", "Done", primary = T)
      } else {
        miniTitleBarButton("btn_next", "Next", primary = T)
      }
    ))

    # Program the "previous" button
    observeEvent(
      input$btn_prev,
      if (isTruthy(input$active_tab)) {
        if (input$active_tab == "2. Review") {
          showTab("active_tab", "1. Settings", select = T)
        } else {
          showTab("active_tab", "2. Review", select = T)
        }
      }
    )

    # Program the "next" button
    observeEvent(
      input$btn_next,
      if (isTruthy(input$active_tab)) {
        if (input$active_tab == "1. Settings") {
          showTab("active_tab", "2. Review", select = T)
        } else {
          showTab("active_tab", "3. Final Code", select = T)
        }
      }
    )

    # Create the selectize box for the second df
    output$ui_df_y <- renderUI(
      if (isTruthy(input$df_x)) {
        # Don't allow to select the same x df
        possible_df_y <- setdiff(
          Filter(
            function(x) {is.data.frame(get(x))},
            ls(.GlobalEnv)
          ),
          input$df_x
        )
        selectizeInput("df_y", "y (2nd data frame)",
                       choices = possible_df_y,
                       options = list(items = "[]"),
                       multiple = F
        )
      } else {
        selectizeInput("df_y", "y (2nd data frame)",
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
    output$ui_by <- renderUI(
      if (isTruthy(input$df_x) & isTruthy(input$df_y)) {
        possible_by_vars <- intersect(
          names(get(input$df_x)),
          names(get(input$df_y))
        )
        addUIDep(selectizeInput("by_vars", "by (id variable)",
                                choices = possible_by_vars,
                                options = list(
                                  items = "[]",
                                  plugins = list("drag_drop", "remove_button")
                                ),
                                multiple = F
        ))
      } else {
        # Empty selectize box if dfs are not selected
        selectizeInput("by_vars", "by (id variable)",
                       choices = "",
                       multiple = F
        )
      }
    )


    # The function that creates a df with 5 suggested matches
    f_w_df_matches <- function(x, y, by, method) {
      id_x <- sym(str_c(by, ".x"))
      id_y <- sym(str_c(by, ".y"))

      # A: adapt this for the other join functions!
      stringdist_left_join(
        # This makes sure that the function doesn't use the full datasets
        eval(parse(text = str_c("distinct(select(", x, ", ", by, "))"))),
        eval(parse(text = str_c("distinct(select(", y, ", ", by, "))"))),
        by = by,
        max_dist = Inf, distance_col = "d",
        method = method
      ) %>%
        select(!!id_x, !!id_y, d) %>%
        arrange(!!id_x, d) %>%
        group_by(!!id_x) %>%
        filter(row_number() <= 5) %>%
        mutate(min_d = min(d)) %>%
        # remove the perfect matches from the review process
        filter(min_d != 0) %>%
        ungroup() %>%
        arrange(-min_d, !!id_x, d) %>%
        select(-min_d) %>%
        mutate(d           = round(d, 3),
               y_with_dist = str_c("(", d, ") ", !!id_y))
    }

    # Make the function reactive to input
    r_w_df_matches <- reactive({
      f_w_df_matches(input$df_x,
                     input$df_y,
                     input$by_vars,
                     input$dist_alg)
    })

    # Create the DT with matches, for the review process
    output$ui_df_matches <- DT::renderDataTable(
      if (isTruthy(input$df_x) & isTruthy(input$df_x)) {

        # Load the df with suggested matches
        w_df_matches <- r_w_df_matches()

        df_dt <- data.frame(
          id_1 = unique(w_df_matches[[str_c(input$by_vars, ".x")]]),
          id_2 = map2_chr(
            .x = str_c(
              "review_",
              1:length(unique(w_df_matches[[str_c(input$by_vars, ".x")]]))
            ),
            .y = split(
              w_df_matches[["y_with_dist"]],
              ceiling(seq_along(w_df_matches[["y_with_dist"]]) / 5)
            ),
            .f = ~ as.character(selectizeInput(inputId = .x,
                                               choices = .y,
                                               label   = NULL,
                                               width   = "90%") %>%
                                  div(style = "height: 35px;",
                                      .))
          ),
          stringsAsFactors = FALSE
        )

        DT::datatable(df_dt,
                      escape = FALSE, selection = "none",
                      colnames = c("ID from x", "ID from y?"),
                      options=list(
                        pageLength      = 5,
                        searching       = FALSE,
                        language.emptyTable = "Empty Table",
                        # https://github.com/rstudio/shiny/issues/1246
                        preDrawCallback = htmlwidgets::JS('function() { Shiny.unbindAll(this.api().table().node());}'),
                        drawCallback    = htmlwidgets::JS('function() { Shiny.bindAll(this.api().table().node()); } '))) %>%
          DT::formatStyle(0, lineHeight='20%')

      },
      server = FALSE)


    r_checkbox <- reactive({input$checkbox_match_cols})

    output$ui_code <- renderUI(
      if (isTruthy(input$df_x) & isTruthy(input$df_y)) {

        w_df_matches <- r_w_df_matches()



        if (nrow(w_df_matches) > 0){
          v_reviewed_id <- vector("character",
                                  length(unique(w_df_matches[[str_c(input$by_vars, ".x")]])))
          for (i in 1:length(unique(w_df_matches[[str_c(input$by_vars, ".x")]]))) {
            v_reviewed_id[[i]] <- eval(parse(text = (str_c("input$review_", i))))
          }

          v_suggested <- map_chr(
            .x = split(
              w_df_matches[["y_with_dist"]],
              ceiling(seq_along(w_df_matches[["y_with_dist"]]) / 5)
            ),
            ~.x[[1]]
          )

        }

        if (nrow(w_df_matches) > 0){
          df_review <- data.frame(
            orig_id = unique(w_df_matches[[str_c(input$by_vars, ".x")]]),
            suggested_id = v_suggested,
            reviewed_id = v_reviewed_id,
            reviewed_id_clean = v_reviewed_id %>% str_remove("^\\(\\d+\\)\\s"),
            # dist = v_reviewed_id %>%
            #   str_extract("^\\(\\d+\\)") %>%
            #   str_extract("\\d+") %>%
            #   as.numeric(),
            stringsAsFactors = F
          ) %>%
            mutate(agree = if_else(suggested_id == reviewed_id, 1, 0)) %>%
            filter(agree == 0) %>%
            mutate(code_expr = glue("'{orig_id}' = '{reviewed_id_clean}',") %>%
                     as.character())


          # mutate(code_expr = if_else(row_number() == 1,
          #                            str_remove(code_expr, strrep('&nbsp;', 6)),
          #                            code_expr))
        }


        # max_dist_suggested <- v_suggested %>%
        #   str_extract("^\\(\\d+\\)") %>%
        #   str_extract("\\d+") %>%
        #   as.numeric() %>%
        #   max()

        chr_join_function <- str_c("stringdist_", input$join_type)


        chr_dplyr_join <- str_c(input$join_type, "_join")

        r_checkbox2 <- r_checkbox()

        code_show_cols <- ""


        if (r_checkbox2 == F) {
          code_show_cols <- ""
        } else if (r_checkbox2 == T) {
          code_show_cols <- str_c(",\n", strrep("&nbsp;", 2), "match_cols = T")
        }

        if (nrow(w_df_matches) == 0) {
          code <- glue("
          <pre>
          # There are only perfect matches.
          # A regular join is sufficient!

          {chr_dplyr_join}(
            x  = {input$df_x},
            y  = {input$df_y},
            by = '{input$by_vars}'
          )
          </pre>
          ")
        } else if (as.numeric(count(df_review)) > 0) {
          coll_string <- paste("\n", strrep('&nbsp;', 2))

          chr_changes <- str_c(df_review$code_expr, collapse = coll_string) %>%
            str_remove(",$")


          code <- glue(
            "
          <pre>
          library(inexact)
          #
          trust_join(
            x  = {input$df_x},
            y  = {input$df_y},
            by = '{input$by_vars}',
            mode = '{input$join_type}',
            custom_match = c(
              {chr_changes}
            ){code_show_cols}
          )</pre>
          "
          )
        } else {
          code <- glue(
            "
            <pre>
          library(inexact)
          # You didn't add any custom matches!
          # Let's trust the algorithm:
          trust_join(
            x    = {input$df_x},
            y    = {input$df_y},
            by   = '{input$by_vars}',
            mode = '{input$join_type}'{code_show_cols}
          ) </pre>
            "
          )
        }

        HTML(code)

      }
    )
  }
  runGadget(ui, server,
            viewer = dialogViewer(dialogName = "RStudio Addins: inexact"))
}
