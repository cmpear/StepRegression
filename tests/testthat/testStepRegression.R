context("Checks that StepRegression gets the correct residuals")


test_that("Do residuals match lm() residuals?",{
  data(mtcars)
  x<- as.matrix(mtcars[,-1])
  y<- as.matrix(mtcars[,1])
  res <- step_regression(x,y, addIntercept = TRUE)
  fit <- lm(y ~ x)
  mse1 <-sum(res[,ncol(res)]^2)
  mse2 <-sum(residuals(fit)^2)
  expect_equal(mse1,mse2)
})
