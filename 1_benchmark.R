library("benchmarkme")

runs = 100

write(paste0("Time: ",Sys.time()), "./data/info.txt", append=T)
info <- benchmarkme::get_linear_algebra()
write(paste0("BLAS: ",info$blas), "./data/info.txt", append=T)
write(paste0("LAPACK: ",info$lapack), "./data/info.txt", append=T)

## Increase runs if you have a higher spec machine
res = benchmark_std(runs = runs)
write.csv(res, "./data/results.csv")



