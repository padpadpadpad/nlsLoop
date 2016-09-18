#' an interactive addin to help evaluate the fits of nlsLoop
#'
#' an interactive addin to help evaluate the fits of nlsLoop
#'
#' @param data the data frame that contains your raw data
#' @param predictions the data frame that contains your predictions if desired
#' @param id_col the column that splits your data frame by
#' @param x the x variable
#' @param y the y variable
#' @return a dataframe of the rows that are to be deleted
#' @description opens a pane from which you can select each set of data and select points to be dropped. The undo button gets rid of the last selection. Press "DONE" to get a dataframe of the selected outliers.
#' @examples
#' # load in data
#'
#' data("Chlorella_TRC")
#' Chlorella_TRC_test <- Chlorella_TRC[Chlorella_TRC$curve_id %in% c(1:10),]
#'
#' # run nlsLoop()
#'
#' fits <- nlsLoop(ln.rate ~ schoolfield.high(ln.c, Ea, Eh, Th, temp = K, Tc = 20),
#'                 data = Chlorella_TRC_test,
#'                 tries = 500,
#'                 id_col = 'curve_id',
#'                 param_bds = c(-10, 10, 0.1, 2, 0.5, 5, 285, 330),
#'                 lower = c(ln.c=-10, Ea=0, Eh=0, Th=0))
#'
#' outliers <- nlsViewer(data = Chlorella_TRC_test, predictions = fits$predictions,
#' id_col = 'curve_id', x = 'K', y = 'ln.rate')
#'
#' @export

nlsViewer <- function(data, predictions = NULL, id_col, x, y){

    # create all id_col
    id <- as.character(unique(data[,id_col]))

    # define the UI for the gadget
    ui <- miniUI::miniPage(
      # set title
      miniUI::gadgetTitleBar("nlsViewer"),
      # set up content panel
      miniUI::miniContentPanel(
                  shiny::selectInput("data", "Choose curve:", choices = id),
                  shiny::plotOutput("plot1",
                           click = "plot1_click")),
      miniUI::miniButtonBlock(
        shiny::actionButton("undo_last_point", "Undo"),
        shiny::actionButton("go_to_next", "Next")
      )
    )

    server <- function(input, output, session){
      # For storing which rows have been excluded
      vals <- shiny::reactiveValues(
        deleted_rows = data[FALSE,]
      )

      # Define plot1
      output$plot1 <- shiny::renderPlot({
        # create y variable and x variable
        # subset for keep rows
        dat <- data[data[,id_col] == input$data,]
        keep    <- dat[! rownames(dat) %in% row.names(vals$deleted_rows),]
        exclude <- dat[rownames(dat) %in% row.names(vals$deleted_rows),]

        # no predictions
        if(is.null(predictions)){
          # plot 1
          ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), col = 'black', size = 3, keep) +
            ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), col = 'black', size = 3, exclude, alpha = 0.25) +
            ggplot2::theme_bw(base_size = 18, base_family = 'Helvetica') +
            ggplot2::ggtitle(input$data)
        }   else {
          # with predictions
          preds <- predictions[predictions[,id_col] == input$data,]
          # plot 1
          ggplot2::ggplot() +
            ggplot2::geom_line(ggplot2::aes_string(x = x, y = y), col = 'red', linetype = 2, size = 1.5, preds) +
            ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), col = 'black', size = 3, keep) +
            ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), col = 'black', size = 3, exclude, alpha = 0.25) +
            ggplot2::theme_bw(base_size = 18, base_family = 'Helvetica') +
            ggplot2::ggtitle(input$data)
            }

    })

      # Make points that are clicked turn grey
      shiny::observeEvent(input$plot1_click,{
        dat <- data[data[,id_col] == input$data,]
        vals$deleted_rows <- rbind(vals$deleted_rows, nearPoints(dat, input$plot1_click, allRows = FALSE))
      })

      # Undo last click
      shiny::observeEvent(input$undo_last_point,{
        vals$deleted_rows <- vals$deleted_rows[-nrow(vals$deleted_rows),]
      })

      # Move to next id
      shiny::observeEvent(input$go_to_next, {

      })

      # When the Done button is clicked, return a value
      shiny::observeEvent(input$done, {
        deleted_rows <- vals$deleted_rows
        # Return the kept points ###
        shiny::stopApp(
          list(
            data = deleted_rows
          )
        )
      })

      }
    # Run the app in the dialog viewer
    shiny::runGadget(ui, server, viewer = shiny::dialogViewer('nlsViewer', width = 700, height = 1200))
  }

