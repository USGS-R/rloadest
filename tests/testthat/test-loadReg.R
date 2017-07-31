context('loadReg')

test_that("loadReg model can be created", {
  app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, flow = "FLOW",
                     dates = "DATES", conc.units="mg/L",
                     station="Illinois River at Marseilles, Ill.")
  expect_is(app1.lr, "loadReg")
})
