options(shiny.testmode = TRUE)
library(gpssampling)
samp <- sampler()
samp$launch(open = FALSE, method = "RS_SMP")
