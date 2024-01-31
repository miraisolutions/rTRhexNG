#' @import shiny
#' @importFrom colourpicker colourInput



# palette <- base
palette <- mesch_V2

# detect Inkscape
# inkscape <- system2("inkscape", "-V") == 0

# Define UI for app
ui <- function() {
  .columnVerticalLayout <- function(width, ...) {
    column(width, ...)
  }
  fluidPage(

    # App title ----
    titlePanel("R-generated hexagon sticker for rTRNG"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(
        style = "overflow-y:scroll; max-height: 90vh; position:relative;",
        width = 3,

        shinyWidgets::dropdownButton(
          label = "Extras",
          tooltip = TRUE,
          icon = icon("cog"), width = "100%", size = "sm",
          sliderInput(
            inputId = "n_poly",
            label = "# polygon sides",
            pre = "#sides=",
            min = 3,
            max = 24,
            value = 6
          ),
          shinyWidgets::materialSwitch(
            inputId = "circle",
            label = "round",
            inline = TRUE
          ),
          numericInput(
            inputId = "seed",
            label = "random seed (!=0)",
            value = 0
          )
        ),

        sliderInput(inputId = "n",
                    label = "# of elements per side",
                    pre = "N=",
                    min = 5,
                    max = 50,
                    value = 9),

        sliderInput(inputId = "n_full_frac",
                    label = NULL, # "elements of full sequence",
                    pre = "n_full=",
                    post = "xNx#sides",
                    min = 0,
                    max = 1,
                    step = 0.02,
                    value = 0.88,
                    animate = animationOptions(
                      interval = 200,
                      loop = FALSE
                    )),

        sliderInput(inputId = "jump_size",
                    label = "jump ahead by number of steps",
                    pre = "jump(steps=",
                    post = ")",
                    min = 0,
                    max = 50,
                    value = 3),

        sliderInput(inputId = "n_jump",
                    label = NULL, # "# elements jump sequence",
                    pre = "n_jump=",
                    min = 0,
                    max = 50,
                    value = 9),

        sliderInput(inputId = "split_p",
                    label = "split(p, s): s-th of p sub-sequences",
                    pre = "p=",
                    min = 0,
                    max = 50,
                    value = 9),

        sliderInput(inputId = "split_s",
                    label = NULL, # "split sub-sequence [1, p]",
                    pre = "s=",
                    min = 0,
                    max = 50,
                    value = 5),

        sliderInput(inputId = "n_split",
                    label = NULL, # "# elements split sequence",
                    pre = "n_split=",
                    min = 0,
                    max = 50,
                    value = 9),

        sliderInput(inputId = "text_size",
                    label = "text size",
                    step = 0.01,
                    min = 0,
                    max = 1,
                    value = 0.23),

        sliderInput(inputId = "poly_pad",
                    label = "pad",
                    step = 0.05,
                    min = 0,
                    max = 1,
                    value = 0.1),

        NULL
      ),


      # Main panel for displaying outputs ----
      mainPanel(
        width = 8,

        plotOutput(outputId = "myImage", height = 550),

        fluidRow(
          .columnVerticalLayout(
            2,
            colourInput(inputId = "bg_col",
                        label = "background/text",
                        value = palette$bg),
            colourInput(inputId = "text_col",
                        label = NULL,
                        value = palette$txt)
          ),
          .columnVerticalLayout(
            2,
            colourInput(inputId = "full_fill",
                        label = "full",
                        value = palette$full_fill),
            colourInput(inputId = "full_stroke",
                        label = NULL,
                        value = palette$full_stroke)
          ),
          .columnVerticalLayout(
            2,
            colourInput(inputId = "jump_fill",
                        label = "jump",
                        value = palette$jump_fill),
            colourInput(inputId = "jump_stroke",
                        label = NULL,
                        value = palette$jump_stroke)
          ),
          .columnVerticalLayout(
            2,
            colourInput(inputId = "split_fill",
                        label = "split",
                        value = palette$split_fill),

            colourInput(inputId = "split_stroke",
                        label = NULL,
                        value = palette$split_stroke)
          )
        ),

        fluidRow(
          column(
            12,
            radioButtons("svg_postprocess", label = NULL, inline = TRUE,
                         choices = c("as-is", "rsvg", "rsvg2", "inkscape", "inkscape-text2path"),
                         selected = "rsvg")
          )
        ),

        fluidRow(
          column(
            12,
            downloadButton("save_png", "Save as PNG"),
            downloadButton("save_svg", "Save as SVG"),
            downloadButton("save_pdf", "Save as PDF"),
            downloadButton("save_R", "Download R code")
          )
        ),

        NULL

      )
    )
  )
}

# Define server logic----
#' @importFrom grDevices hsv
server <- function(input, output) {

  split_ok <- reactive({
    input$split_p > 0 && input$split_s > 0 && input$split_s <= input$split_p
  })

  n_full <- reactive({
    pmax(1L, ceiling(input$n * input$n_poly * input$n_full_frac))
  })

  length_full <- reactive({
    pmax(
      input$n_poly * input$n,
      n_full(),
      input$jump_size + input$n_jump,
      input$jump_size + input$split_s + input$n_split * input$split_p
    )
  })

  sq_cols_jump_split <- reactive({
    cols <- rep(input$full_fill, len = length_full())
    if (input$jump_size > 0) {
      cols[1 + input$jump_size] <- input$jump_fill
    }
    if (split_ok()) {
      cols[seq(input$jump_size + input$split_s, by = input$split_p, length_full())] <- input$split_fill
    }
    cols
  })

  sq_cols <- reactive(
    if (input$seed != 0) {
      function(x) hsv(x, 0.75, 1)
    } else {
      sq_cols_jump_split()
    }
  )

  stickR_args <- reactive({
    within(list(), {
      file <- tempfile("rTRNG-", fileext = ".svg")
      n_poly <- input$n_poly
      n <- input$n
      jump_size <- input$jump_size
      split_p <- input$split_p # based on the jump
      split_s <- input$split_s # based on the jump
      sq_cols <- sq_cols()
      full_col <- input$full_stroke
      jump_col <- input$jump_stroke
      split_col <- input$split_stroke
      n_split <- if (split_ok()) input$n_split else 0
      n_jump <- input$n_jump
      text_size <- input$text_size
      text_col <- input$text_col
      bg_col <- input$bg_col
      n_full <- n_full()
      poly_pad <- input$poly_pad
      postprocess <- input$svg_postprocess
      text_font <- "GothamBook"
      circle <- input$circle
      seed <- input$seed
    })
  })

  create_sticker <- reactive({
    do.call(
      rTRNGstickR,
      stickR_args()
    )
  })

  output$save_png <- downloadHandler(
    filename = "rTRNG.png",
    content = function(file) {
      message(file)
      rsvg::rsvg_png(create_sticker(), file, 1200*sqrt(3)/2, 1200)
    })

  output$save_svg <- downloadHandler(
    filename = "rTRNG.svg",
    content = function(file) {
      message(file)
      file.copy(create_sticker(), file)
    })

  output$save_pdf <- downloadHandler(
    filename = "rTRNG.pdf",
    content = function(file) {
      message(file)
      rsvg::rsvg_pdf(create_sticker(), file)
    })

  output$save_R <- downloadHandler(
    filename = "rTRNG-stickR.R",
    content = function(file) {
      cl <- as.call(c(quote(rTRNGstickR), stickR_args()))
      cl$file <- "rTRNG.svg"
      out <- file(file, "w")
      on.exit(close(out))
      dput(bquote(rTRNGstickR <- .(rTRNGstickR)), out)
      dput(cl, out)
    })

  output$myImage <- renderImage({

    outfile <- create_sticker()
    # outfile <- tempfile("rTRNG-", fileext = ".png")
    # rsvg_png(create_sticker(), outfile, 1200*sqrt(3)/2, 1200)

    # Return a list containing the filename
    list(src = outfile,
         # contentType = 'image/svg+xml',
         height = 550,
         alt = "rTRNG")
  }, deleteFile = FALSE)
}

#' rTRhexNG Shiny App
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()]
#'
#' @export
rTRhexNG_app <- function(...) {
  shinyApp(ui = ui, server = server, ...)
}
