test_that("Recycling dots is done correctly", {
    expect_identical(
        recycle_dots(
            x = c(1, 2, 3, 1 , 2),
            col = c("red", "green", "blue"),
            pch = 19,
            lwd = c(1, 2)
            ),
        list(
            col = c("red", "green", "blue", "red", "green"),
            pch = 19,
            lwd = c(1, 2, 1, 1, 2)
            )
        )
    })
