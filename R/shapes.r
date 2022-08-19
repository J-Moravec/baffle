#' Plot convex polygons
#'
#' A collection of functions for plotting polygonal shapes.
#'
#' Polygons are drawn centered on the `x` and `y` coordinates, with a diameter `d`
#' (or `dx` and `dy`). Typically, different shapes are obtained through a parametrization
#' of the `shapes()` function, which draws a convex polygon using the [graphics::polygon()],
#' with the exception of `square()` and `rectangle()` function, which use the [graphics::rect()]
#' function instead, behave slightly differently, and should be slightly faster.
#'
#' The diameter parameter `d` is interpreted differently depending for `square()` and `rectangle()`
#' and for other polygonal functions build on the `shapes()` function (`circle()`, `ellipse()`
#' and `rcpoly()`). For the `square()` and `rectangle()`, the diameter is the size of the square,
#' `d=1` thus fills the whole 1x1 tile. For `shapes()` function, `d` is the diameter
#' of the inscribed circle to the square of size `d`. This is more convenient solution to prevent
#' accidental overplotting when individual shapes are plotted next to each other in regular
#' intervals, the distance between such points would be equal to the diameter in both cases.
#' See examples.
#'
#' All shapes function accept the graphical parameters `col`, `border`, `lty` and `lwd`, which
#' are passed to the [graphics::polygon()] and [graphics::rect()]. Apart of a different default
#' values, they behave in the same way.
#'
#' All parameters are vectorized and will recycle as required, with the typical warning if
#' parameters are not multiply of each other. This can be used to create pleasant geometric images.
#' See examples. 
#'
#' @param x,y coordinates
#' @param n the number of vertices of polygon, with the minimum of three (triangle). Large `n`, such
#'   as `n=1000` approximate circle. The vertices start at the 12 o'clock position and are placed
#'   clockwise in a regular intervals.
#' @param d **optional** diameter, see details
#' @param dx,dy **optional** diameter in either coordinate direction
#' @param rotate **optional** clockwise rotation in degrees (0-360°),
#'   not available for `square` and `rectangle`
#' @param ... **optional** graphical parameters `col`, `border`, `lty` and `lwd` passed to
#'   [graphics::polygon()] or [graphics::rect()]
#' @importFrom graphics par
#'
#' @examples
#' plot(0, 0) # create plotting window
#'
#' # Following calls are equivalent
#' square(0, 0, 1)
#' rectangle(0, 0, 1)
#' rectangle(0, 0, dx=1, dy=1)
#'
#' # Not equivalent to `square`
#' rcpoly(0, 0, 4, d=1)
#'
#' # Same output as `square`, but not equivalent
#' rcpoly(0, 0, 4, d=sqrt(2), rotate=45)
#'
#' # Vectorizing parameters
#' plot(0, 0)
#' rotate = seq(0, 18, by=30)
#' d = seq(1, by=-0.1, length.out = length(rotate))
#' rcpoly(0,0,3, border="red", lwd=3, rotate=rotate, d=d)
#'
#' @seealso [graphics::polygon()] and [graphics::rect()] for the underlying plotting functions
#' @name Shapes
NULL

#' @describeIn Shapes draw squares
#' @export
square = function(x, y, d=0.9, ...){
    .rectangle(x=x, y=y, d=d, ...)
    }

#' @describeIn Shapes draw rectangles
#' @export
rectangle = function(x, y, d=0.9, dx=d, dy=d, ...){
    .rectangle(x=x, y=y, d=d, dx=dx, dy=dy, ...)
    }

# internal
.rectangle = function(
    x, y, d=0.9, dx=d, dy=d,
    col=par("fg"), border=NA, lty=par("lty"), lwd=par("lwd")
    ){
    graphics::rect(x - dx/2, y - dy/2, x + dx/2, y+ dy/2, col=col, border=border, lty=lty, lwd=lwd)
    }


#' @describeIn Shapes draw circles
#' @export
circle = function(x, y, d=0.9, n=1000, ...){
    shapes(x=x, y=y, n=n, d=d, ...)
    }

#' @describeIn Shapes draw ellipses
#' @export
ellipse = function(x, y, d=0.9, dx=d, dy=d, n=1000, ...){
    shapes(x=x, y=y, n=n, d=d, dx=dx, dy=dy, ...)
    }

#' @describeIn Shapes draw regular convex polygons
#' @export
rcpoly = function(x, y, n, d=0.9, rotate=0, ...){
    shapes(x=x, y=y, n=n, d=d, rotate=rotate, ...)
    }


# internal
shape = function(
    x, y, n, d=0.9, dx=d, dy=d, rotate=0,
    col=par("fg"), border=NA, lty=par("lty"), lwd=par("lwd")
    ){

    points = seq(0, 2*pi, length.out=n+1)
    rotate = (pi / 180) * rotate

    graphics::polygon(
        sin(points + rotate)/2*dx + x,
        cos(points + rotate)/2*dy + y,
        col=col, border=border, lty=lty, lwd=lwd
        )
    }


#' @describeIn Shapes draw convex polygons
#' @export
shapes = function(x, y, n, d=0.9, dx=d, dy=d, rotate=0, ...){

    if(any(n < 3))
        stop("The minimum number of vertices must be 3")

    Map(f=shape, x=x, y=y, n=n, d=d, dx=dx, dy=dy, rotate=rotate, ...)

    invisible(NULL)
    }
