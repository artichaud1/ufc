
featurize_fight_advantages <- function(model_df){
  
  # Compute advantages for fight stats, wins and win method stats
  # Note: original clumns are dropped
  
  fight_stats <- 
    c('Kd', 'SigStrikes', 'Total_SigStrikes', 'TotStrikes', 'Total_TotStrikes',
      'Td', 'Total_Td', 'SubAtt', 'Pass', 'Rev', 'Head', 'Total_Head',
      'Body', 'Total_Body', 'Leg', 'Total_Leg', 'Distance', 'Total_Distance',
      'Clinch', 'Total_Clinch', 'Ground', 'Total_Ground')
  
  for(var in fight_stats){
    adv_attack <- 
      model_df[[paste0('Prev_Cume_', var, '_PM_1')]] - 
      model_df[[paste0('Prev_Cume_', var, '_PM_2')]]
    
    adv_defense <- 
      model_df[[paste0('Prev_Cume_', var, '_Against_PM_2')]] - 
      model_df[[paste0('Prev_Cume_', var, '_Against_PM_1')]]
    
    adv_overall <- adv_attack + adv_defense
    
    model_df[[paste0('Adv_', var, '_PM')]] <- adv_overall
    
    model_df[[paste0('Prev_Cume_', var, '_PM_1')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_PM_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_PM_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_PM_1')]] <- NULL
  }
  
  win_stats <- 
    c('Wins', 'Method_KO', 'Method_other', 'Method_S.DEC', 'Method_SUB', 'Method_U.DEC')
  
  for(var in win_stats){
    # feature PM
    adv_attack <- 
      model_df[[paste0('Prev_Cume_', var, '_PM_1')]] - 
      model_df[[paste0('Prev_Cume_', var, '_PM_2')]]
    
    adv_defense <- 
      model_df[[paste0('Prev_Cume_', var, '_Against_PM_2')]] - 
      model_df[[paste0('Prev_Cume_', var, '_Against_PM_1')]]
    
    adv_overall <- adv_attack + adv_defense
    
    model_df[[paste0('Adv_', var, '_PM')]] <- adv_overall
    
    #feature Ratio
    adv_attack <- 
      model_df[[paste0('Prev_Cume_', var, '_Ratio_1')]] - 
      model_df[[paste0('Prev_Cume_', var, '_Ratio_2')]]
    
    adv_defense <- 
      model_df[[paste0('Prev_Cume_', var, '_Against_Ratio_2')]] - 
      model_df[[paste0('Prev_Cume_', var, '_Against_Ratio_1')]]
    
    adv_overall <- adv_attack + adv_defense
    
    model_df[[paste0('Adv_', var, '_Ratio')]] <- adv_overall
    
    model_df[[paste0('Prev_Cume_', var, '_PM_1')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_PM_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_PM_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_PM_1')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_Ratio_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_Ratio_1')]] <- NULL
  }
  
  model_df
}



featurize_fight_advantages2 <- function(model_df){
  
  # Compute advantages for fight stats, wins and win method stats
  # Note: original clumns are dropped
  
  fight_stats <- 
    c('Kd', 'SigStrikes', 'Total_SigStrikes', 'TotStrikes', 'Total_TotStrikes',
      'Td', 'Total_Td', 'SubAtt', 'Pass', 'Rev', 'Head', 'Total_Head',
      'Body', 'Total_Body', 'Leg', 'Total_Leg', 'Distance', 'Total_Distance',
      'Clinch', 'Total_Clinch', 'Ground', 'Total_Ground')
  
  for(var in fight_stats){
    adv_attack <- 
      model_df[[paste0('Prev_Cume_', var, '_PM_1')]] - 
      model_df[[paste0('Prev_Cume_', var, '_PM_2')]]
    
    adv_defense <- 
      model_df[[paste0('Prev_Cume_', var, '_Against_PM_2')]] - 
      model_df[[paste0('Prev_Cume_', var, '_Against_PM_1')]]
    
    adv_overall <- adv_attack + adv_defense
    
    model_df[[paste0('AdvAtt_', var, '_PM')]] <- adv_attack
    model_df[[paste0('AdvDef_', var, '_PM')]] <- adv_defense
    model_df[[paste0('Adv_', var, '_PM')]] <- adv_overall
    
    model_df[[paste0('Prev_Cume_', var, '_PM_1')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_PM_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_PM_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_PM_1')]] <- NULL
  }
  
  win_stats <- 
    c('Wins', 'Method_KO', 'Method_other', 'Method_S.DEC', 'Method_SUB', 'Method_U.DEC')
  
  for(var in win_stats){
    # feature PM
    adv_attack <- 
      model_df[[paste0('Prev_Cume_', var, '_PM_1')]] - 
      model_df[[paste0('Prev_Cume_', var, '_PM_2')]]
    
    adv_defense <- 
      model_df[[paste0('Prev_Cume_', var, '_Against_PM_2')]] - 
      model_df[[paste0('Prev_Cume_', var, '_Against_PM_1')]]
    
    adv_overall <- adv_attack + adv_defense
    
    model_df[[paste0('AdvAtt_', var, '_PM')]] <- adv_attack
    model_df[[paste0('AdvDef_', var, '_PM')]] <- adv_defense
    model_df[[paste0('Adv_', var, '_PM')]] <- adv_overall
    
    #feature Ratio
    adv_attack <- 
      model_df[[paste0('Prev_Cume_', var, '_Ratio_1')]] - 
      model_df[[paste0('Prev_Cume_', var, '_Ratio_2')]]
    
    adv_defense <- 
      model_df[[paste0('Prev_Cume_', var, '_Against_Ratio_2')]] - 
      model_df[[paste0('Prev_Cume_', var, '_Against_Ratio_1')]]
    
    adv_overall <- adv_attack + adv_defense
    
    model_df[[paste0('AdvAtt_', var, '_Ratio')]] <- adv_attack
    model_df[[paste0('AdvDef_', var, '_Ratio')]] <- adv_defense
    model_df[[paste0('Adv_', var, '_Ratio')]] <- adv_overall
    
    model_df[[paste0('Prev_Cume_', var, '_PM_1')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_PM_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_PM_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_PM_1')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_Ratio_2')]] <- NULL
    model_df[[paste0('Prev_Cume_', var, '_Against_Ratio_1')]] <- NULL
  }
  
  model_df
}