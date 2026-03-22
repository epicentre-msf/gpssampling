devtools::load_all(here::here())
samp <- sampler()
samp$launch(open = FALSE, method = "RS_SMP")
