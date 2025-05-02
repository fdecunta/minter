# Compute variance covariance matrix for all the effect sizes

You pass the names of the variances as a list. The function compute the vcvs and
returns a list with the matrixes. Thus, you can use them in a model like this:

```r
res <- rma.mv(lnRR_A, vcv$A, ...)
```

Maybe let you run ALL the models with metafor and return a list with the models.

# Visualization for interaction

You have one meta-analytic model for each effect size: 3 or 5 depending if you want the simple
effects or the overall ones.

You can pass the models as a list:

```r
models <- list("Epichloë" = res_epic,
               "AMF"      = res_amf,
               "Interaction" = res_inter)

inter_orchard_plot(models, xlab = "lnRR", mod = "~ plant_type", ...)

inter_dist_plot(models, xlab = "lnRR", mod = "~ plant_type", ...)
```


