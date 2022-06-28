#' Make a waffle chart.
#'
#' Waffle chart is a pie chart that visually represents abundances with the number of squares.
#'
#' The waffle chart consists of squares plotted on a rectangular lattice according to a design
#' matrix that is constructed automatically from a vector of abundances or can be directly provided.
#'
#' The \code{waffle} function accepts a vector of abundances and constructs the design matrix,
#' which is then parsed to the \code{waffle.mat} function. By default, the design matrix is
#' constructed by row with sequential numbers according to the vector of abundances \code{x}.
#' This means that for the \code{x=c(3,5)}, the matrix will be filled with three \code{1} and five
#' \code{2}, all other cells of the matrix are set as unknown values \code{NA}.
#'
#' By default, \code{waffle} tries to make squared waffle plots, with the number of rows and columns
#' derived from the total sum of abundances in \code{x}. If `nrow` or `ncol` are specified,
#' the remaining dimension is calculated to fit all abundances.
#' If abundances are too large, it might be a good idea to rescale them and round them,
#' otherwise waffles will be too busy and the interpretability might suffer.
#'
#' The function \code{waffle.mat} accepts a custom-made design matrix and thus allows a better
#' control of colored regions. It is called internally by the \code{waffle} function, which serves
#' as an easy to use interface for the \code{waffle.mat}. For this reason, the \code{waffle.mat}
#' does not checks for the validity of input arguments and does not set a default colors.
#'
#' The assumed and allocated coordinate system is from \code{1} to \code{ncol+1} for x and
#' from \code{1} to \code{ncol+1} for y. Squares are filled from top right corner of this coordinate
#' system.
#'
#' If \code{add=FALSE}, a new window with a fixed aspect ratio x/y=1 is allocated so that
#' plotted rectangles are squares. This might cause the plot margins, and thus the main title,
#' to be quite far away. In this case, plotting the title using \code{text} instead of \code{title}
#' might be a better idea. In this case, the coordinates might be:
#' \code{text(x=(ncol+2)/2, y=nrow+1,...)}
#'
#'
#'
#' @param x a vector of abundances
#' @param mat a design matrix (see details)
#' @param nrow **optional** an amount of rows
#' @param ncol **optional** an amount of columns
#' @param col **optional**  a vector of colors, must be the same length as x.
#'  If not specified, the "Set1" palette from the package RColorBrewer is used.
#' @param border the border color of squares, by default no border is plotted
#' @param adj an amount by which squares are shrinked so they do not touch each other
#' @param byrow whether to fill matrix by rows (default) or columns
#' @param add whether to add to a current plot
#'
#' @seealso \code{\link[waffle]{waffle}} in the \code{waffle} package for a ggplot2 version.
#'
#' @examples
#' waffle(c(50,25,25))
#' waffle(c(25,75), c("darkorchid", "lightgray"))
#' waffle(c(14,8,4), nrow=3)
#'
#' # custom design matrix with a more complex structure
#' cols = RColorBrewer::brewer.pal(3, "Set1")
#' design_mat = matrix(NA, 7,10)
#' design_mat[1,] = 1
#' design_mat[2,1:8] = 1
#' design_mat[4,] = 2
#' design_mat[5,1:4] = 2
#' design_mat[7,1:5] = 3
#' waffle.mat(design_mat, cols)
#'
#' @export
waffle = function(x, nrow=NULL, ncol=NULL, col=NULL, border=NA, adj=0.1, byrow=TRUE, add=FALSE){
    total = sum(x)

    if(is.null(col))
        col = RColorBrewer::brewer.pal(9, "Set1")[seq_along(x)]

    if(is.null(nrow) && is.null(ncol))
        nrow = ncol = ceiling(sqrt(total))
    if(is.null(nrow))
        nrow = ceiling(total / ncol)
    if(is.null(ncol))
        ncol = ceiling(total / nrow)

    cells = nrow*ncol

    if(total > cells)
        stop("The sum of elements in x must be smaller or equal to the total number of cells")
    if(length(col) < length(x))
        stop("x and col must have the same length")

    # build the design matrix:
    mat = rep(seq_along(x), x)
    mat = c(mat, rep(NA, cells - total))
    mat = matrix(mat, nrow, ncol, byrow=byrow)

    # plot the waffle using the design matrix
    waffle.mat(mat, col=col, border=border, adj=adj, add=add)
    }


#' @rdname waffle
#' @export
waffle.mat = function(x, col, border=NA, adj=0.1, add=FALSE){
    nrow = nrow(x)
    ncol = ncol(x)
    x = as.vector(x)
    pick = !is.na(x)
    x = x[pick]

    xx = matrix(1:ncol, nrow, ncol, byrow=TRUE)
    xx = as.vector(xx)[pick]
    x1 = xx + adj
    x2 = xx + 1 - adj

    yy = matrix(nrow:1, nrow, ncol)
    yy = as.vector(yy)[pick]
    y1 = yy + adj
    y2 = yy + 1 - adj

    if(!add){
        graphics::plot.new()
        graphics::plot.window(xlim=c(1, ncol+1), ylim=c(1, nrow+1), asp=1)
        }
    graphics::rect(x1, y1, x2, y2, col=col[x], border=border)
    }
