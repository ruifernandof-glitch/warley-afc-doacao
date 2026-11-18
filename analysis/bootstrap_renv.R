# Restaura as versões exatas registradas em renv.lock.
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore(lockfile = "renv.lock", prompt = FALSE)
# Opcional: MVN para normalidade multivariada (sem ele, usa-se psych::mardia).
if (!requireNamespace("MVN", quietly = TRUE)) try(install.packages("MVN"), silent = TRUE)
