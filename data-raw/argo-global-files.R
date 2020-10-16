
library(argodata)

with_argo_mirror(NULL, {
  with_argo_cache_dir("inst/cache-test", {
    argo_download("ar_index_global_meta.txt.gz")
    argo_download("ar_index_global_prof.txt.gz")
    argo_download("ar_index_global_tech.txt.gz")
    argo_download("ar_index_global_traj.txt.gz")
  })
})
