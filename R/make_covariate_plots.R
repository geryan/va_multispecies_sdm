make_covariate_plots <- function(
    spatial_values,
    target_species,
    model_notna_idx_pa,
    model_notna_idx_po,
    w = 3200,
    h = 2000
){

  cvnames <- names(spatial_values)

  ncvs <- length(cvnames)

  nspp <- length(target_species)

  idx_sp_pa <- model_notna_idx_pa
  idx_sp_pa[,2] <- target_species[idx_sp_pa[,2]]

  idx_sp_po <- model_notna_idx_po
  idx_sp_po[,2] <- target_species[idx_sp_po[,2]]

  for(i in 1:ncvs){

    # pa plot
    plotdatpa <- tibble(
      species = idx_sp_pa[,2],
      value = spatial_values[idx_sp_pa[,1], i]
    )

    paplot <- ggplot(plotdatpa) +
      geom_violin(
        aes(
          x = species,
          y = value,
          fill = species
        )
      ) +
      labs(
        title = cvnames[i]
      )

    ggsave(
      filename = sprintf(
        "outputs/figures/cov_violins/pa_%s.png",
        cvnames[i]
      ),
      plot = paplot,
      width = w,
      height = h,
      units = "px"
    )

    # po plot
    plotdatpo <- tibble(
      species = idx_sp_po[,2],
      value = spatial_values[idx_sp_po[,1], i]
    )

    poplot <- ggplot(plotdatpo) +
      geom_violin(
        aes(
          x = species,
          y = value,
          fill = species
        )
      ) +
      labs(
        title = cvnames[i]
      )

    ggsave(
      filename = sprintf(
        "outputs/figures/cov_violins/po_%s.png",
        cvnames[i]
      ),
      plot = poplot,
      width = w,
      height = h,
      units = "px"
    )

  }





}
