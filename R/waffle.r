#' Make a waffle chart.
#'
#' Waffle chart is a pie chart that visually represents abundances with the number of squares.
#' The waffle chart consists of squares plotted on a rectangular lattice according to a design
#' matrix that is constructed automatically from a vector of abundances or can be provided directly.
#'
#' @details
#' The `waffle()` function accepts a vector of abundances and constructs the design matrix
#' using the `design()` function, which is then parsed to the `waffle.mat()` function.
#'
#' The function `design()` construct the design matrix according to specification.
#' The design matrix is filled by an integer vector derived from the abundance vector `x`.
#' Each integer correspond to the order of abundances in `x`, with the quantity equal to its value
#' in `x`. This means that for the `x=c(3,5)`, the design matrix will be filled with three `1`
#' and five `2`, all other cells of the design matrix are set as unknown values `NA`.
#'
#' By default, the design matrix is filled by row, starting from the bottom left corner,
#' this can be changed by setting the variables `byrow`, `bottom` and `left`.
#' By setting `byrow=FALSE`, the matrix is filled by columns first, if `bottom=FALSE`, the matrix
#' is filled starting from upper left corner and so on.
#'
#' If `ncol` or `nrow` is not specified, a squared matrix that will fit the sum of abundances
#' will be constructed.
#'
#' The function `waffle.mat()` accepts a custom-made design matrix and thus allows a better
#' control of colored regions. It is called internally by the `waffle()` function, which serves
#' as an easy to use interface for the `waffle.mat()`. For this reason, the `waffle.mat()`
#' does not checks for the validity of input arguments and does not set a default colors.
#'
#' The assumed and allocated coordinate system is from `0` to `ncol` for x and
#' from `0` to `ncol` for y. Squares are filled from top right corner of this coordinate
#' system.
#'
#' If `add=FALSE`, a new window with a fixed aspect ratio x/y=1 is allocated so that
#' plotted polygons are squares (by default). This might cause the plot margins,
#' and thus the main title, to be quite far away. In this case, plotting the title using `text()`
#' instead of `title()` might be a better idea. In this case, the coordinates might be:
#' \code{text(x=(ncol+2)/2, y=nrow+1,...)}
#'
#'
#'
#' @param x a vector of abundances or a design matrix (see details)
#' @param nrow **optional** the number of rows
#' @param ncol **optional** the number of columns
#' @param byrow **optional** fill matrix by rows
#' @param bottom **optional** fill matrix starting from the bottom
#' @param left **optional** fill matrix starting from the left side
#' @param col **optional**  a vector of colors, must be the same length as x.
#'  If not specified, the "Set1" palette from the package RColorBrewer is used.
#' @param add **optional** whether to add to a current plot
#' @param f **optional** a shape function (see details)
#' @param ... **optional** other parameters passed to the `f` function 
#'
#' @seealso the `waffle` package for a `ggplot2` version.
#'
#' @examples
#' waffle(c(50,25,25))
#' waffle(c(25,75), col=c("darkorchid", "lightgray"))
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
#' waffle.mat(design_mat, col=cols)
#' @export
waffle = function(x, f=NULL, ..., nrow=NULL, ncol=NULL, col=NULL, byrow=TRUE, bottom=TRUE, left=TRUE, add=FALSE){

    # construct design matrix
    mat = design(x, nrow=nrow, ncol=ncol, byrow=byrow, bottom=bottom, left=left)

    # plot the waffle using the design matrix
    waffle.mat(mat, f=NULL, ..., col=col, add=add)
    }

# TODO S3 classes?
#' @rdname waffle
#' @export
waffle.mat = function(x, f=square, col=NULL, ..., add=FALSE){
    # TODO
    # turn x into factor, we need factor
    # we need factor to preserve elements of x from waffle that have 0 abundances
    nrow = nrow(x)
    ncol = ncol(x)
    x = as.vector(x)
    pick = !is.na(x)
    x = x[pick]

    if(is.null(f))
        f = square

    if(is.null(col))
        col = RColorBrewer::brewer.pal(9, "Set1")[unique(x)]

    if(length(col) < length(unique(x)))
        stop("x and col must have the same length")


    xx = matrix(1:ncol-1, nrow, ncol, byrow=TRUE)
    xx = as.vector(xx)[pick]

    yy = matrix(nrow:1-1, nrow, ncol)
    yy = as.vector(yy)[pick]

    if(!add){
        graphics::plot.new()
        graphics::plot.window(xlim=c(0, ncol), ylim=c(0, nrow), asp=1)
        }

    f(xx + 0.5, yy + 0.5, col=col[x], ...)
    }


# Design a matrix for waffle chart.
#
# Create a design matrix for a waffle chart out of vector of abundances.
#
#@template design_details
#
#
# If `x` is a named vector, the names corresponding to the integers are preserved in the `levels`
# attribute.
#
# @template design_params
# @return an integer matrix, where integers correspond to the order of input abundances, and their
#   count to the individual abundances. The remaining cells are filled with `NA`.
#   If the vector of abundances is a named vector, the names are preserved in the `levels`
#   attribute.

#' @rdname waffle
#' @export
design = function(x, nrow=NULL, ncol=NULL, byrow=TRUE, bottom=TRUE, left=TRUE){
    total = sum(x)

    if(any(x < 0))
        stop("Elements of `x` need to be positive.")
    if(any(as.integer(x) != x))
        stop("`x` must be integer.")
    if(total == 0 && (is.null(nrow) || is.null(ncol) ) )
        stop("Unable to determine dimension of the matrix. The sum of `x` is zero.")

    if(is.null(nrow) && is.null(ncol))
        nrow = ncol = ceiling(sqrt(total))
    if(is.null(nrow))
        nrow = ceiling(total / ncol)
    if(is.null(ncol))
        ncol = ceiling(total / nrow)

    cells = nrow*ncol

    if(total > cells)
        stop("The sum of elements in `x` must be smaller or equal to the total number of cells")

    mat = rep(seq_along(x), x)
    mat = c(mat, rep(NA, cells - total))
    mat = matrix(mat, nrow, ncol, byrow=byrow)

    # by default, matrix() is filled from top left corner, so we use some switches to flip it
    if(bottom)
        mat = mat[nrow:1, , drop=FALSE]
    if(!left)
        mat = mat[, ncol:1, drop=FALSE]

    if(! names(x) |> is.null() )
        levels(mat) = names(x)

    mat
    }
