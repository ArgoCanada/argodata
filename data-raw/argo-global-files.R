
library(argodata)

options(argodata.max_global_cache_age = Inf)
with_argo_mirror(NULL, {
  with_argo_cache_dir("inst/cache-test", {
    argo_download("ar_index_global_meta.txt.gz")
    argo_download("ar_index_global_prof.txt.gz")
    argo_download("ar_index_global_tech.txt.gz")
    argo_download("ar_index_global_traj.txt.gz")
    argo_download("argo_bio-profile_index.txt.gz")
    argo_download("argo_bio-traj_index.txt.gz")
    argo_download("argo_synthetic-profile_index.txt.gz")
  })
})
