square.matrix = function(x, n){
    matrix(as.integer(x), n, n, byrow=TRUE)
    }
        
test_that("Design matrix is filled in the correct direction", {
    x = c(2,0,1)
    
    expect_identical(design(x), square.matrix(c(3, NA, 1, 1), 2))
    expect_identical(design(x, byrow=FALSE), square.matrix(c(1, NA, 1, 3), 2))
    expect_identical(design(x, bottom=FALSE), square.matrix(c(1, 1, 3, NA), 2))
    expect_identical(design(x, left=FALSE), square.matrix(c(NA, 3, 1, 1), 2))
    expect_identical(design(x, byrow=FALSE, bottom=FALSE, left=FALSE),
                     square.matrix(c(3, 1, NA, 1), 2))
    })



test_that("Design matrix is constructed with the correct dimensions", {
    expect_identical(design(c(3,3,3)) |> dim(), c(3L,3L))
    expect_identical(design(c(3,3,3,1)) |> dim(), c(4L,4L))
    expect_identical(design(c(0,1,0)) |> dim(), c(1L,1L))
    
    expect_identical(design(0, nrow=3, ncol=3) |> dim(), c(3L,3L))
    expect_error(design(0), "Unable to determine dimension of the matrix.")
    })