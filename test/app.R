library("shiny")

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
    
    status <- match.arg(status)
    # dropdown button content
    html_ul <- list(
        class = "dropdown-menu",
        style = if (!is.null(width)) 
            paste0("width: ", validateCssUnit(width), ";"),
        lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
    )
    # dropdown button apparence
    html_button <- list(
        class = paste0("btn btn-", status," dropdown-toggle"),
        type = "button", 
        `data-toggle` = "dropdown"
    )
    html_button <- c(html_button, list(label))
    html_button <- c(html_button, list(tags$span(class = "caret")))
    # final result
    tags$div(
        class = "dropdown",
        do.call(tags$button, html_button),
        do.call(tags$ul, html_ul),
        tags$script(
            "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
    )
}

ui <- fluidPage(
    tags$h1("Example dropdown button"),
    br(),
    fluidRow(
        column(
            width = 6,
            dropdownButton(
                label = "Check some boxes", status = "default", width = 450,
                tags$label("Choose :"),
                fluidRow(
                    column(
                        width = 4,
                        checkboxGroupInput(inputId = "check1a", label = NULL, choices = paste0(1:10, ") ", LETTERS[1:10]))
                    ),
                    column(
                        width = 4,
                        checkboxGroupInput(inputId = "check1b", label = NULL, choices = paste0(11:20, ") ", LETTERS[11:20]))
                    ),
                    column(
                        width = 4,
                        checkboxGroupInput(inputId = "check1c", label = NULL, choices = paste0(21:26, ") ", LETTERS[21:26]))
                    )
                )
            ),
            verbatimTextOutput(outputId = "res1")
        ),
        column(
            width = 6,
            tags$style(".container { border:2px solid steelblue; width: 100%; height: 200px; overflow-y: scroll; }"),
            dropdownButton(
                label = "Check some boxes", status = "default", width = 120,
                tags$div(
                    class = "container",
                    checkboxGroupInput(inputId = "check2", label = "Choose", choices = paste0(1:26, ") ", LETTERS))
                )
            ),
            verbatimTextOutput(outputId = "res2")
        )
    )
)
server <- function(input, output, session) {
    
    valuesCheck1 <- reactiveValues(x = NULL)
    observeEvent(input$check1a, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check1a)))
    observeEvent(input$check1b, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check1b)))
    observeEvent(input$check1c, valuesCheck1$x <- unique(c(valuesCheck1$x, input$check1c)))
    
    output$res1 <- renderPrint({
        valuesCheck1$x
    })
    
    output$res2 <- renderPrint({
        input$check2
    })
    
}
shinyApp(ui = ui, server = server)





showModal(modalDialog(
  title = "Update Dataset?",
  "Do you want to update the dataset with the most recent data?",
  footer = tagList(
    #modalButton("No, show last loaded data (fast!)"),
    actionButton("No", "No, show last loaded data (fast!)"),
    
    actionButton("ok", "Yes! Update my data (may take a minute)")
  )
))

observeEvent(input$ok, {
  update_dataset(silence = TRUE)
})