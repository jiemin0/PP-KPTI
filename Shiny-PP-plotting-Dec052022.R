# FURTHER
# add title

# "XPORT-MF-035-0010-001"
# library(ggplot2)
# library(patchwork)
# library(ggpubr)
# path <- getwd()



# make exposure table for selinexor arm ----------------------------------------
make_exposure_p <- function(ex, ex_rgm, pid){
  dose_color <- c(unique(ex$EXDOSE),unique(ex_rgm$EXRGM))
  dose_color <- dose_color[!duplicated(dose_color)]
  if(!pid %in% unique(ex$USUBJID)){
    on.exit(print('The patient dose not take any Selinesor Treatment'))
    stop('The patient dose not take any Selinesor Treatment')
  }
  {
  ggplot(data = ex, aes(x = EXSTDY)) + 
    #  make dose points - if exdose not equal to 0, color by dose
    # error if usubjid is not in ex, output, the patient dose not have ex treatment,
    # and don't show exposure table. 
    geom_point(
      data = subset(ex, EXDOSE != '0' & USUBJID == pid),
      aes(x = EXSTDY, y = "Actucal Dose(seli)", color = EXDOSE), 
      size = 3, shape = 16
    )  + 
    # if exdose = 0, mark dose as black cross
    geom_point(
      data = subset(ex, EXDOSE == '0' & USUBJID == pid),
      aes(x = EXSTDY, y = "Actucal Dose(seli)"),
      color = "black", size = 3, shape = 4
    ) + 
    # Duration of regmin 
    geom_segment(
      data = subset(ex_rgm, USUBJID == pid ),
      aes(x = RGMSTDY, xend = RGMENDY, y = "Regimen(Seli)", yend = "Regimen(Seli)", color = EXRGM), 
      size = 3
    ) +
  labs(y = "", x = "") +
    # theme(
    #   plot.title = element_text(size = 10, hjust = 0, vjust = 3))
    guides(
      color = guide_legend(title = 'Dose')
      
      ) +
    # make color consistent across different pts
    lims(colour = levels (dose_color))+
    theme(legend.position = "top") +
      geom_vline(xintercept = 1, color = "purple", linetype = "dotted") 
  }
}
# title = df_ptinfo[df_ptinfo$USUBJID == "XPORT-MF-035-0017-001"]
# ex_dfs <- process_ex(df_ex)
# ex <- ex_dfs[[1]]
# ex_rgm <- ex_dfs[[2]]
# 
# make_exposure_p(ex, ex_rgm, "XPORT-MF-035-1101-002") +
#   geom_vline(xintercept = seq(0, 300, 12*7), color = "purple", linetype = "dotted") +
#   scale_x_continuous(
#     breaks = seq(0, 300, 7*2),
#     labels = c(seq(0, 300/7, 2)),
#     limits = (c(0, 300))
#   )


# AE ----------------------------------------------------------------------
make_AE_p <- function(df,df_sameday,pid){
  ggplot() +
    geom_segment(
      data = subset(df, USUBJID == pid), 
      mapping = aes(
        x = AESTDY, xend = AEENDY2, 
        y = AEDECOD, yend = AEDECOD, 
        color = AETOXGR,
        size = bar_size
      ), 
      alpha = .7
    )  +
    geom_text(
      data =  subset(df, USUBJID ==  pid), 
      aes(label = AEACN2, x = text_x, y = AEDECOD), 
      size = 3, 
      vjust = -1
    ) +
    # what dose this mean, why should shape like that?
    geom_point(
      data = subset(df_sameday, USUBJID == pid ), 
      aes(x = AESTDY, y = AEDECOD, color = AETOXGR), 
      size = 5, 
      shape = "|"
    ) +
    guides(
      size = 'none',
      color = guide_legend("Toxicity Grade")
    ) +
    #formatting
    theme(legend.position = "top") + 
    xlab("") + ylab("")  +
    geom_vline(xintercept = 1, color = "purple", linetype = "dotted") 
}
# ae <- ae_dfs[[1]]
# ae_oneday <- ae_dfs[[2]]
# make_AE_p(ae, ae_oneday,"XPORT-MF-035-1200-001")




# Make table for pts information - 



#LB--------------------------------------------------------------

make_lb_p <- function(lb, pid) {
  ggplot( data = subset(lb, USUBJID == pid) ) +
    # make plin plot - color by different categories 
    geom_line(
      aes(x = LBDY, y = PCH2, color = LBTESTCD)
    ) + 
    # add label to each truning point
    geom_text(
      aes(label = LBSTRESN, x = LBDY, y = PCH2, color = LBTESTCD), 
      size = 3, 
      nudge_y = 0.02
    ) + 
    geom_text(
      aes(label =paste0(round(PCH*100, 0), "%"), x = LBDY, y = PCH2, color = LBTESTCD),
      size = 3,
      nudge_y = -0.02
    ) +
    # label out the masurement in plot
    geom_text(
      data = subset(lb, LBDY == 1),
      aes(label = LBTEST, x = -3, y = PCH2, color = LBTESTCD),
      size = 3
    ) + 
    #Data formatting
    # remove legend
    guides(color = FALSE) +
    theme(
      axis.text.x = element_text(), 
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(angle = 0, vjust = 0.5)
    ) + 
    labs(x = "", y = "PCH - %") + 
    geom_vline(xintercept = 1, color = "purple", linetype = "dotted") +
    #make color consistent across group
    lims(color = unique(lb$LBTESTCD))
}

# make_lb_p(lb,"XPORT-MF-035-1101-001")
#lb %>%
#  select(USUBJID, PCH, LBTESTCD, LBSTRESN,LBDY) %>% write_clip()
  



# data issue------------------------
#pt 1200-002, schduled as PC, but took first dose as selinexor
# TSS was mapped using the QE label, since no cycle infomation in EX 
# further - make color in AE consistent


  