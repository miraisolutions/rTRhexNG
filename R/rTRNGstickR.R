rTRNGstickR <- function(
  file = "rTRNG.svg",
  n_poly = 6,
  n, # number of elements per side of the polygon
  jump_size, # jump size
  split_p = n, # number of sub-sequences, after jump
  split_s, # index of sub-sequence [1, split_p], after jump
  n_full = ceiling((n_poly - 0.7) * n), # nr of full sequence elements
  n_jump = 9, # nr of full sequence elements
  n_split = 9, # nr of full sequence elements
  n_path_ext = 2, # number of extra elements for the arrowed paths
  bg_col, # background color
  sq_cols = function(x) hsv(x, 0.75, 1), # color vector or function for the elements in the full sequence
  full_col, # color of the full sequqnce path
  jump_col, # color of the jump sequqnce path
  split_col, # color of the split sequqnce path
  box_jump_col = jump_col, # color of the jump box
  box_split_col = split_col, # color of the split box
  text_size, # rTRNG text size (fraction of polygon inner circle diameter)
  text_col, # rTRNG text color
  text_font = "GothamBook",
  text_width = 0.025, # fraction of text_size to make the font fatter
  height = 50.8, # mm
  poly_pad = 0, # fraction of the square size
  postprocess = "rsvg2", # fast and reliable
  seed = 12358, # random seed used if colors are generated randomly via function sq_cols()
  circle = FALSE,
  guides = FALSE
) {

  full_highlight_split_jump <- is.function(sq_cols)

  if (circle) {
    n_full <- n_full
    n_poly <- n * n_poly
    n <- 1
  }

  # geometry and sizes ----

  .sind <- function(deg) sinpi(deg / 180)
  .cosd <- function(deg) cospi(deg / 180)
  .tand <- function(deg) tanpi(deg / 180)

  # sector angles of the plygon sides, starting from the top-middle
  poly_sector <- 360/n_poly
  poly_angles <- seq(-90, by = poly_sector, len = n_poly)
  # start from the closest vetex to 210 deg.
  start_angle <- which.min(abs(210 - poly_angles))
  poly_angles <- c(
    tail(poly_angles, -(start_angle - 1)),
    head(poly_angles, start_angle - 1)
  )

  if (circle) {
    sticker_size <- list(x = height, y = height)
    poly_r <- height/2
  } else {
    sticker_size <- list(x = height*diff(range(.cosd(poly_angles))) /
                           diff(range(.sind(poly_angles))),
                         y = height)
    poly_r <- sticker_size$y / diff(range(.sind(poly_angles)))
  }
  poly_center <- list(x = sticker_size$x/2,
                      y = poly_r)

  # border size => spacing between squares = 2*border size
  sq_border_perc <- 0.12

  # size of squares incl. spacing between them
  sq_size <-
    2 * poly_r * .sind(poly_sector/2) / (
      n + 2 * (1 + (1 - sq_border_perc)*poly_pad) * .tand(poly_sector/2)
    )

  # border size
  sq_border <- sq_size * sq_border_perc

  path_size <- sq_size * 0.2
  connector_size <- path_size*0.8

  # inside-polygon outer and inner circle radius
  in_poly_outer_r <- sq_size * n / .sind(poly_sector/2) / 2
  in_poly_inner_r <- in_poly_outer_r * .cosd(poly_sector/2)

  # actual text size
  text_size <- text_size * 2 * in_poly_inner_r

  id <- 1

  # utils  ----
  .sub <- function(., ...) {
    subs <- data.frame(.s = ., ..., stringsAsFactors = FALSE)
    for (i in seq_len(nrow(subs))) {
      for (which in names(subs)) {
        subs$.s[i] <-
          gsub(sprintf("@%s@", which), subs[[which]][i], subs$.s[i])
      }
    }
    subs$.s
  }
  .df <- function(...) data.frame(..., stringsAsFactors = FALSE)

  # SVG base ----
  svg <- .sub(
    '
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" preserveAspectRatio="xMidYMid"
width="@w@mm"
height="@h@mm"
viewBox="0 0 @w@ @h@">

  <rect id="canvas" width="100%" height="100%" fill="@canvas@"/>

  @content@

</svg>',
    h = sticker_size$y,
    w = sticker_size$x
  )


  # SVG utils ----

  rotate_square <- function(angle, x, y) {
    sprintf("rotate(%s, %s, %s)", angle, x, y)
  }

  square_center <- function(squares) {
    with(squares, data.frame(
      x = x,
      y = y
    ))
  }

  box <- function(squares) {
    with(squares, expand.grid(x = range(x) + c(-1, 1)*sq_size/2, y = range(y) + c(-1, 1)*sq_size/2))
  }

  svg_squares <- function(x, y, fill, stroke = bg_col, angle = 0) {
    id <- id + seq(0, len = pmax(length(x), length(y)))
    svg <-
      .sub('
  <rect
    id="rect-@id@"
    width="@s@" height="@s@" x="@x@" y="@y@"
    style="fill:@fill@;stroke:@stroke@;fill-opacity:1;stroke-opacity:1;stroke-width:@border@"
    transform="@rotate@"
  />',
           id = id,
           s = sq_size, x = x - sq_size/2, y = y - sq_size/2,
           stroke = stroke, fill = fill, border = sq_border,
           rotate = rotate_square(angle, x, y)
      )
    id <<- max(id) + 1
    svg
  }

  svg_path <- function(points, stroke = "black", fill = "none",
                       width, close = FALSE) {
    svg <- .sub(
      '
  <path
    id="path-@id@"
    style="stroke: @stroke@; fill:@fill@; stroke-width:@width@;"
    d="@d@"
  />',
      id = id,
      stroke = stroke,
      fill = fill,
      width = width,
      d =  paste(c(
        paste0("M ", points$x[1], ",", points$y[1]),
        paste0("L ", points$x[-1], ",", points$y[-1]),
        "Z"[close]
      ), collapse = " " )

    )
    id <<- id + 1
    svg
  }

  svg_poly_bg <- function(color) {

    poly_v <- .df(
      x = poly_center$x + poly_r * .cosd(poly_angles),
      y = poly_center$y + poly_r * .sind(poly_angles)
    )
    svg_path(poly_v, stroke = "none", fill = color, width = 0, close = TRUE)
  }

  svg_squares_path <- function(squares, stroke) {
    centers <-
      cbind(
        square_center(squares),
        corner = c(FALSE, diff(squares$angle) != 0),
        r = sq_size * (1 + 1/.tand(poly_sector/2))/2
      )
    path <- with(
      centers,
      .sub('
  <defs>
    <marker id="arrow-@id@" markerWidth="5" markerHeight="3" refX="2.5" refY="1.5"
    orient="auto" markerUnits="strokeWidth">
      <path d="M0,0 l5,1.5 l-5,1.5 l1,-1.5 Z" style="fill: @stroke@;"/>
    </marker>
    <marker id="circle-@id@" markerWidth="2.5" markerHeight="2.5" refX="1.25" refY="1.25"
    orient="auto" markerUnits="strokeWidth">
      <circle cx="1.25" cy="1.25" r="1.25" style="fill: @stroke@"/>
    </marker>
  </defs>
  <path
    id="path-@id@"
    style="stroke: @stroke@; fill:none; stroke-width:@size@;
    marker-start: url(#circle-@id@);
    marker-end: url(#arrow-@id@)"
    d="@d@"
  />',
           id = id,
           stroke = stroke,
           size = path_size,
           d =
             paste(c(
               paste0("M ", x[1], ",", y[1]),
               # paste0(ifelse(corner[-1], "M ", "L "),
               paste0(ifelse(corner[-1], paste0("A ", r[-1], ",", r[-1], " 0 0,1 "), "L "),
                      paste(x[-1], y[-1], sep = ","))
             ), collapse = " " ))
    )
    id <<- id + 1
    path
  }

  svg_connect_A <- function(from, to, r, stroke) {
    svg <-
      .sub('
  <path
    id="path-@id@"
    d="M@x1@,@y1@ A@rx@,@ry@ 0 0,@conc@ @x2@,@y2@"
    style="stroke:@stroke@; fill:none; stroke-width:@size@;
    stroke-dasharray:@da@,@da2@; stroke-dashoffset:@do@; stroke-linecap:round;"
  />',
           id = id,
           x1 = from$x, y1 = from$y, rx = abs(r[1]), ry = abs(tail(r, 1)), x2 = to$x, y2 = to$y,
           stroke = stroke, size = connector_size, conc = 1*(r[1] > 0),
           da = connector_size*2, da2 = connector_size*3, do = path_size*1.25
      )
    id <<- id + 1
    svg
  }

  svg_txt <- function(txt, x, y, size, anchor = "middle") {
    w <- size*0.025 * 1
    if (size > 0) {
      svg <-
        .sub('
  <text
    id="text-@id@"
    style="text-anchor:@anchor@;font-style:normal;font-variant:normal;
    font-weight:normal;font-stretch:semi-narrower;font-size:@size@;font-family:@font@;letter-spacing:0px;word-spacing:0px;
    fill:@color@;fill-opacity:1;stroke:@color@;stroke-width:@w@"
    x="@x@"
    y="@y@"
  >@txt@</text>',
             id = id, txt = txt, x = x, y = y - w/2, size = size, color = text_col, font = text_font,
             anchor = anchor,
             w = w
        )
      id <<- id + 1
      svg
    }
  }


  # full sequence ----
  sq_alpha <- rep(poly_angles, each = n)
  angles <- sq_alpha + poly_sector/2
  sq_size/2 * (1 + .tand(poly_sector/2))
  seq_s <- seq(0, by = sq_size, length.out = n) + sq_size/2 * (1 + .tand(poly_sector/2))
  poly_sq_r <- in_poly_outer_r + sq_size / 2 / .cosd(poly_sector/2)
  full_seq_sq <-
    .df(
      x = poly_center$x + poly_sq_r * .cosd(sq_alpha) - .sind(angles) * (seq_s),
      y = poly_center$y + poly_sq_r * .sind(sq_alpha) + .cosd(angles) * (seq_s),
      angle = angles
    )

  if (is.function(sq_cols)) {
    rng <- rTRNG::yarn2$new(seed)
    full_seq_sq$fill <-
      sq_cols(rTRNG::runif_trng(nrow(full_seq_sq), engine = rng))
  } else {
    full_seq_sq$fill <- head(sq_cols, nrow(full_seq_sq))
  }

  poly_sq_svg <- do.call(svg_squares, head(full_seq_sq, n_full))
  poly_path_svg <- svg_squares_path(head(full_seq_sq, n_full + n_path_ext), full_col)


  # "T" location ----

  # angle of the square center wrt the polygon center
  sq_angles <- atan2(full_seq_sq$y - poly_center$y, full_seq_sq$x - poly_center$x)/pi*180
  # The location of the "T" is on the line connecting the polygon center and the
  # center of the square wit closest angle to -150 (starting square for the
  # hexagon case)
  T_sq_idx <- which.min(round(abs(-150 - sq_angles), 6))
  T_angle <- sq_angles[T_sq_idx]
  T_r <- (
    # distance between square center and polygon center
    sqrt(sum((full_seq_sq[T_sq_idx, c("x", "y")] - poly_center)^2)) -
      # safety sqeezing to avoid overlap
      sqrt(2)*sq_size
  ) * 0.81 # move towards the center (tuned to be close to the original ad-hoc hexagon case)
  T_x <-  poly_center$x + T_r * .cosd(T_angle)
  T_y <-  poly_center$y + T_r * .sind(T_angle)

  T_x_hex_orig <- 8.8857652319742
  T_y_hex_orig <- 16.7321688093897
  c(T_x_hex_orig - poly_center$x,
    T_y_hex_orig - poly_center$y) /
    c(T_x - poly_center$x,
      T_y - poly_center$y) - 1


  # jump sequence ----
  do_jump <- (jump_size > 0 && n_jump > 0)
  if (do_jump) {
    jump_sq_cols <-
      if (is.function(sq_cols)) {
        rng$seed(seed)
        rng$jump(jump_size)
        sq_cols(rTRNG::runif_trng(nrow(full_seq_sq) - jump_size, engine = rng))
      } else {
        tail(full_seq_sq$fill, -jump_size)
      }
    jump_seq_sq <- .df(
      x = T_x + seq(0, by = sq_size, len = nrow(full_seq_sq) - jump_size),
      y = T_y,
      fill = jump_sq_cols,
      stroke = bg_col,
      angle = 0
    )
  }

  # split sequence ----
  do_split <- split_s > 0 && n_split > 0
  if (do_split) {
    split_sq_cols <-
      if (is.function(sq_cols)) {
        rng$seed(seed)
        rng$jump(jump_size)
        rng$split(split_p, split_s)
        sq_cols(rTRNG::runif_trng(n_split + n_path_ext, engine = rng))
      } else {
        sq_cols[seq(jump_size + split_s, by = split_p, len = n_split + n_path_ext)]
      }
    split_seq_sq <- .df(
      x = T_x + (split_s - 1) * sq_size,
      y = T_y + seq(0, by = sq_size, len = n_split + n_path_ext),
      fill = split_sq_cols,
      stroke = bg_col,
      angle = 0
    )
  }


  full_jump_highlight_svg <-
    if (full_highlight_split_jump && do_jump) {
      svg_squares_path(head(tail(full_seq_sq, -jump_size), n_jump + n_path_ext), jump_col)
    }

  full_split_highlight_svg <-
    if (full_highlight_split_jump && do_split) {
      full_split_sq <-
        full_seq_sq[intersect(seq(jump_size + split_s, by = split_p, len = n_split), seq_len(n_full)), ]
      if (nrow(full_split_sq) > 0L) {
        do.call(svg_squares, c(full_split_sq, list(stroke = box_split_col)))
      }
    }

  # jump&split "T" ----
  T_sq_svg <- c(
    if (do_jump) {
      c(
        do.call(svg_squares, head(jump_seq_sq, n_jump)),
        svg_path(box(head(jump_seq_sq, n_jump))[c(2, 1, 3, 4), ],
                 box_jump_col, "none", sq_border),
        NULL
      )
    },
    if (do_split) {
      c(
        do.call(svg_squares, head(split_seq_sq, n_split)),
        NULL
      )
    }
  )
  T_path_svg <- c(
    if (do_jump) {
      c(
        svg_squares_path(head(jump_seq_sq, n_jump + n_path_ext), jump_col),
        NULL
      )

    },
    if (do_split) {
      c(
        svg_squares_path(head(split_seq_sq, n_split + n_path_ext), split_col),
        svg_path(box(head(split_seq_sq, n_split))[c(4,2,1,3), ],
                 box_split_col, "none", sq_border)
      )
    }
  )

  # rTRNG text ----

  G_sq_idx <- head(which(full_seq_sq$x > poly_center$x & full_seq_sq$y > poly_center$y & sq_angles > 30), 1) - 1
  txt_y <- T_y + sq_size/2 + text_size
  txt_x <- T_x + sq_size*(split_s - 1)
  r_x <- txt_x - text_size/2 - sq_size/4
  R_x <- txt_x + text_size/1.8 + sq_size/4
  if (circle) {
    Dx <- (poly_center$x + .cosd(30) * poly_sq_r - (R_x - txt_x) - R_x) / 2 #text_size * 0.8 * sqrt(3)/2
    Dy <- (poly_center$y + .sind(30) * poly_sq_r - txt_y) / 2 # Dx / sqrt(3) #text_size * 0.8 * 1/2
  } else {
    Dx <- (full_seq_sq$x[G_sq_idx] - (R_x - txt_x) - R_x) / 2 #text_size * 0.8 * sqrt(3)/2
    Dy <- (full_seq_sq$y[G_sq_idx] + sq_size/2 - txt_y) / 2 # Dx / sqrt(3) #text_size * 0.8 * 1/2
  }


  rTRNG_svg <- c(
    svg_txt("r", r_x, txt_y, text_size, "middle"),
    svg_txt("R", R_x, txt_y, text_size, "middle"),
    svg_txt("N", R_x + Dx, txt_y + Dy, text_size, "middle"),
    svg_txt("G", R_x + 2*Dx, txt_y + 2*Dy, text_size, "middle"),
    if (guides) {
      c(
        svg_path(points = .df(x = c(full_seq_sq$x[T_sq_idx], poly_center$x),
                              y = c(full_seq_sq$y[T_sq_idx], poly_center$y)), stroke = "red", width = 0.2),
        svg_path(points = .df(x = txt_x, y = c(txt_y, txt_y - text_size)), stroke = "red", width = 0.2),
        svg_path(points = .df(x = c(R_x, R_x + 2*Dx), y = c(txt_y, txt_y + 2*Dy)), stroke = "red", width = 0.2),
        svg_path(points = .df(x = c(r_x, R_x), y = c(txt_y, txt_y)), stroke = "red", width = 0.2),
        NULL
      )
    }
  )

  # write SVG ----
  writeLines(.sub(
    svg,
    canvas =
      if (bg_col %in% c("white", "#FFFFFF")) "lightgray" else "none",
    content =
      paste(c(
        if (circle) {
          .sub('  <circle cx="@cx@" cy="@cy@" r="@r@" style="fill: @bg@"/>',
               cx = poly_center$x, cy = poly_center$y, r = poly_center$x, bg = bg_col)
        } else {
          svg_poly_bg(bg_col)
        },
        poly_sq_svg,
        full_split_highlight_svg,
        T_sq_svg,
        if (do_jump) {
          svg_connect_A(square_center(full_seq_sq[1, ]),
                        square_center(jump_seq_sq[1, ]),
                        r = -pmax((T_x - full_seq_sq$x[1]), (T_y - full_seq_sq$y[1]))*1.5,
                        jump_col)
        },
        if (do_split & !do_jump) {
          svg_connect_A(square_center(full_seq_sq[1, ]),
                        square_center(split_seq_sq[1, ]),
                        r = (T_x - full_seq_sq$x[1] + (split_s - 1)*sq_size),
                        split_col)
        },
        if (do_split & do_jump) {
          svg_connect_A(square_center(jump_seq_sq[1, ]),
                        square_center(split_seq_sq[1, ]),
                        r = sq_size*(split_s - 1)/2,
                        split_col)
        },
        poly_path_svg,
        full_jump_highlight_svg,
        T_path_svg,
        rTRNG_svg,
        if (guides) {
          c(
            .sub('  <circle cx="@cx@" cy="@cy@" r="@r@" style="stroke:black; fill:@bg@; stroke-width:@size@"/>',
                 cx = tl_x, cy = tl_y, r = sq_size*0.1, bg = "black", size = path_size/5),
            .sub('  <circle cx="@cx@" cy="@cy@" r="@r@" style="stroke:cyan; fill:@bg@; stroke-width:@size@"/>',
                 cx = poly_center$x, cy = poly_center$y, r = in_poly_inner_r, bg = "none", size = path_size/5),
            .sub('  <circle cx="@cx@" cy="@cy@" r="@r@" style="stroke:magenta; fill:@bg@; stroke-width:@size@"/>',
                 cx = poly_center$x, cy = poly_center$y, r = in_poly_inner_r*0.9, bg = "none", size = path_size/5),
            .sub('  <circle cx="@cx@" cy="@cy@" r="@r@" style="stroke:red; fill:@bg@; stroke-width:@size@"/>',
                 cx = poly_center$x, cy = poly_center$y, r = in_poly_outer_r, bg = "none", size = path_size/5),
            .sub('  <circle cx="@cx@" cy="@cy@" r="@r@" style="stroke:red; fill:@bg@; stroke-width:@size@"/>',
                 cx = poly_center$x, cy = poly_center$y, r = T_r, bg = "none", size = path_size/5),
            .sub('  <circle cx="@cx@" cy="@cy@" r="@r@" style="stroke:green; fill:@bg@; stroke-width:@size@"/>',
                 cx = poly_center$x, cy = poly_center$y, r = poly_r, bg = "none", size = path_size/5),
            .sub('  <circle cx="@cx@" cy="@cy@" r="@r@" style="stroke:blue; fill:@bg@; stroke-width:@size@"/>',
                 cx = poly_center$x, cy = poly_center$y, r = poly_sq_r, bg = "none", size = path_size/5),
            NULL
          )
        }
      ), collapse = "\n")
  ), file)

  # post-rpocess SVG ----
  switch(
    postprocess,
    "rsvg" = {
      rsvg::rsvg_svg(file, file, sticker_size$x*2.834645669, sticker_size$y*2.834645669)
    },
    "rsvg2" = {
      rsvg::rsvg_svg(file, file, 100*sticker_size$x*2.834645669, 100*sticker_size$y*2.834645669)
      rsvg::rsvg_svg(file, file, sticker_size$x*2.834645669, sticker_size$y*2.834645669)
    },
    "inkscape-text2path" =
      if (system(paste("inkscape -z -l", file, "-T", file)) != 0) {
        stop("inkscape is not available!")
      },
    "inkscape" =
      if (system(paste("inkscape -z -l", file, file)) != 0) {
        stop("inkscape is not available!")
      }
  )

  invisible(file)
}
