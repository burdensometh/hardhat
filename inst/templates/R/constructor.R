# Only the coefficients and the blueprint are stored (not the `glm` object),
# so the model can be saved and reloaded (e.g. with `saveRDS()`) without any
# special handling.
new_{{model}} <- function(coefs, blueprint) {
  hardhat::new_model(coefs = coefs, blueprint = blueprint, class = "{{model}}")
}
