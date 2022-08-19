square.matrix = function(x, n){
    matrix(as.integer(x), n, n, byrow=TRUE)
    }
        
test_that("Design matrix is filled in the correct direction", {
    x = c(2,0,1)
    
    expect_identical(design(x), square.matrix(c(3, NA, 1, 1), 2))
    expect_identical(design(x, byrow=FALSE), square.matrix(c(1, NA, 1, 3), 2))
    expect_identical(design(x, from="topleft"), square.matrix(c(1, 1, 3, NA), 2))
    expect_identical(design(x, from="bottomright"), square.matrix(c(NA, 3, 1, 1), 2))
    expect_identical(design(x, byrow=FALSE, from="topright"),
                     square.matrix(c(3, 1, NA, 1), 2))
    })



test_that("Design matrix is constructed with the correct dimensions", {
    expect_identical(design(c(3,3,3)) |> dim(), c(3L,3L))
    expect_identical(design(c(3,3,3,1)) |> dim(), c(4L,4L))
    expect_identical(design(c(0,1,0)) |> dim(), c(1L,1L))
    
    expect_identical(design(0, nrow=3, ncol=3) |> dim(), c(3L,3L))
    expect_error(design(0), "Unable to determine dimension of the matrix.")
    })


test_that("Unstacked design matrix is constructed correctly", {
    expect_identical(design(c(1, 2, 3), stacked=FALSE), 
        matrix(as.integer(c(NA, 1, 2, 2, 3, 3, NA, 3)), 2, 4))
    expect_identical(design(c(1, 2, 3), stacked=FALSE, horiz=FALSE),
        matrix(as.integer(c(1, 2, 3, 3, NA, 2, NA, 3)), 4, 2))
    })
