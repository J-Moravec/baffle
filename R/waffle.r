#' Make a waffle chart.
#'
#' Waffle chart is a pie chart that visually represents abundances with the number of squares.
#' The waffle chart consists of squares plotted on a rectangular lattice according to a design
#' matrix that is constructed from a vector of abundances or can be provided directly.
#'
#' @details
#' The `waffle()` function accepts a vector of abundances and plots a waffle chart.
#' This is done by first constructing a design matrix using the `design()` function,
#' which is then parsed to the `waffle.mat()` function.
#'
#' The `design()` functions construct a design matrix according.
#' The design matrix is filled by an integer vector derived from the abundance vector `x`.
#' Each integer correspond to the order of abundances in `x`, with the quantity equal to the value
#' in `x`. This means that for the `x=c(3,5)`, the design matrix will be filled with three `1`
#' and five `2`, all other cells of the design matrix are set as unknown values `NA`.
#'
#' By default, the design matrix is filled by row, starting from the bottom left corner,
#' this can be changed by setting the variables `byrow` and `from`.
#' By setting `byrow=FALSE`, the matrix is filled by columns first.
#'
#' If `ncol` or `nrow` is not specified, a squared matrix that will fit the sum of abundances
#' will be constructed.
#'
#' By default, the design matrix is stacked, i.e., all the elements are placed in a single
#' matrix object, similarly how barplot produces stacked lines. By specifying `stack=FALSE`,
#' the `design()` unstacks the elements, each are then placed in their own matricies, which
#' are then connected by `NA` elements of size `gap`. More complex 
#'
#' If the input vector `x` is a named vector, the names are preserved in the `levels`
#' attribute. These levels are not currently used, but might be used in the future
#' for automatic legend creation and subsetting.
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
#' @param byrow **optional** fill matrix by rows or by columns
#' @param from **optional** starting position from which the matrix is filled, one of
#'   "bottomleft", "bottomright", "topleft" or "topright".
#' @param stacked **optional** if FALSE, produce an unstacked waffle chart, see details
#' @param gap **optional** gap between unstacked subwaffles, works only for unstacked waffle charts
#' @param horiz **optional** if FALSE, vertical instead of horizontal waffle chart is produced,
#'   works only for unstacked waffle charts
#' @param add **optional** whether to add to a current plot
#' @param f **optional** a shape function (see details)
#' @param ... **optional** other parameters passed to the `f` function
#' @return `design()` returns a design matrix (see details), in addition, if `x` is a named vector,
#'   the names are preserved in the `levels` attribute.
#'   `waffle()` and `waffle.mat()` do not have a return value
#'
#' @seealso the `waffle` package for a `ggplot2` version.
#'
#' @examples
#' waffle(c(50,25,25))
#' waffle(c(25,75), col=c("darkorchid", "lightgray"))
#' waffle(c(14,8,4), nrow=3)
#'
#' # custom design matrix with a more complex structure
#' cols = palette.colors(3, "Set 1")
#' design_mat = matrix(NA, 7,10)
#' design_mat[1,] = 1
#' design_mat[2,1:8] = 1
#' design_mat[4,] = 2
#' design_mat[5,1:4] = 2
#' design_mat[7,1:5] = 3
#' waffle.mat(design_mat, col=cols)
#' @export
waffle = function(
    x, f=NULL, ...,
    nrow=NULL, ncol=NULL, byrow=TRUE, from="bottomleft",
    stacked=TRUE, gap=0, horiz=TRUE, add=FALSE
    ){

    # construct design matrix
    mat = design(
        x, nrow=nrow, ncol=ncol, byrow=byrow, from=from,
        stacked=stacked, gap=gap, horiz=horiz
        )

    # plot the waffle using the design matrix
    waffle.mat(mat, f=f, ..., add=add)
    }


#' @rdname waffle
#' @export
waffle.mat = function(x, f=square, ..., add=FALSE){
    # TODO can we utilize the levels() from [design]? Perhaps to make automatic legend?

    nrow = nrow(x)
    ncol = ncol(x)
    x = as.vector(x)
    pick = !is.na(x)
    x = x[pick]

    if(is.null(f))
        f = square

    dots = list(...)

    n = max(x)
    if(is.null(dots[["col"]]) && n < 10)
        dots[["col"]] = grDevices::palette.colors(n, "Set 1")

    if(is.null(dots[["col"]]) && n >= 10)
        dots[["col"]] = grDevices::hcl.colors(n, "Zissou 1")

    dots = do.call(recycle_dots, c("x"=list(x), dots))

    xx = matrix(1:ncol-1, nrow, ncol, byrow=TRUE)
    xx = as.vector(xx)[pick] + 0.5

    yy = matrix(nrow:1-1, nrow, ncol)
    yy = as.vector(yy)[pick] + 0.5

    if(!add){
        graphics::plot.new()
        graphics::plot.window(xlim=c(0, ncol), ylim=c(0, nrow), asp=1)
        }

    do.call(f, args=c("x"=list(xx), "y"=list(yy), dots))
    }



#' Recycle dots args
#'
#' @param x an integer vector, typically with values ranging from 1 to n, with each integer
#'   with each integer specifying an index of element for each item in `...`
#' @param ... additional arguments, if length of item of `...` is larger than one, its elements are
#'   is recycled according to indiced specified by the vector `x`
#' @return recycled arguments of `...`
#' @keywords internal
recycle_dots = function(x, ...){
    recycle = function(x, vector, length){
        if(length(x) > 1){
            x = rep(x, length.out=length)
            x = x[vector]
            }

        x
        }

    n = max(x) # TODO integrate with levels later

    dots = list(...)

    dots[] = lapply(dots, recycle, vector=x, length=n)

    dots
    }

#' @rdname waffle
#' @export
design = function(
    x, nrow=NULL, ncol=NULL, byrow=TRUE, from="bottomleft",
    stacked=TRUE, horiz=FALSE, gap=0
    ){
    if(stacked){
        design_stacked(x, nrow=nrow, ncol=ncol, byrow=byrow, from=from)
        } else {
        design_unstacked(x, nrow=nrow, ncol=ncol, byrow=byrow, from=from, horiz=horiz, gap=gap)
        }
    }


# internal
design_stacked = function(x, nrow=NULL, ncol=NULL, byrow=TRUE, from="bottomleft"){
    from = match.arg(from, c("bottomleft", "bottomright", "topleft", "topright"))

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
    if(from %in% c("bottomleft", "bottomright"))
        mat = mat[nrow:1, , drop=FALSE]

    if(from %in% c("bottomright", "topright"))
        mat = mat[, ncol:1, drop=FALSE]

    if(! names(x) |> is.null() )
        levels(mat) = names(x)

    mat
    }


# internal
design_unstacked = function(
    x, nrow=NULL, ncol=NULL, byrow=TRUE, from="bottomleft", gap=0, horiz=FALSE
    ){
    if(is.null(nrow) && !horiz)
        nrow = max(ceiling(sqrt(x)))

    if(is.null(ncol) && horiz)
        ncol = max(ceiling(sqrt(x)))

    n = length(x)
    template = vector("numeric", length=n)
    mat = vector("list", length=n)
    for(i in seq_len(n)){
        y = template
        y[i] = x[i]
        
        mat[[i]] = design(y, nrow=nrow, ncol=ncol, byrow=byrow, from=from)
        }
    mat
    
    if(horiz){
        empty = matrix(NA, ncol=ncol, nrow=gap)
        mat[[n+1]] = empty
        mat = mat[interleave(n+1, seq_len(n))[-1]]
        mat = do.call(rbind, mat)
        } else {
        empty = matrix(NA, nrow=nrow, ncol=gap)
        mat[[n+1]] = empty
        mat = mat[interleave(n+1, seq_len(n))[-1]]
        mat = do.call(cbind, mat)
        }
        
    mat
    }

# internal
interleave = function(...){
    as.vector(rbind(...))
    }
