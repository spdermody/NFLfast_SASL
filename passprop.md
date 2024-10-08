## R Markdown

    library(nflfastR)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(readr)
    play_by_play_2023 <- read_csv("play_by_play_2023.csv")

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 49665 Columns: 372

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (114): game_id, home_team, away_team, season_type, posteam, posteam_typ...
    ## dbl  (206): play_id, old_game_id, week, yardline_100, quarter_seconds_remain...
    ## lgl   (45): lateral_receiver_player_id, lateral_receiver_player_name, latera...
    ## dttm   (3): time_of_day, end_clock_time, drive_real_start_time
    ## time   (4): time, drive_time_of_possession, drive_game_clock_start, drive_ga...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    data1 <- play_by_play_2023
    data2 <- data1 %>% mutate(opp_FG_TD_prob=opp_td_prob+opp_fg_prob)

    punts <- data2 %>% filter(play_type == "punt")
    punts

    ## # A tibble: 2,352 × 373
    ##    play_id game_id     old_game_id home_team away_team season_type  week posteam
    ##      <dbl> <chr>             <dbl> <chr>     <chr>     <chr>       <dbl> <chr>  
    ##  1     245 2023_01_AR…  2023091007 WAS       ARI       REG             1 WAS    
    ##  2     427 2023_01_AR…  2023091007 WAS       ARI       REG             1 ARI    
    ##  3    1601 2023_01_AR…  2023091007 WAS       ARI       REG             1 ARI    
    ##  4    1722 2023_01_AR…  2023091007 WAS       ARI       REG             1 WAS    
    ##  5    1947 2023_01_AR…  2023091007 WAS       ARI       REG             1 ARI    
    ##  6    2870 2023_01_AR…  2023091007 WAS       ARI       REG             1 WAS    
    ##  7    3026 2023_01_AR…  2023091007 WAS       ARI       REG             1 ARI    
    ##  8    3170 2023_01_AR…  2023091007 WAS       ARI       REG             1 WAS    
    ##  9    3592 2023_01_AR…  2023091007 WAS       ARI       REG             1 ARI    
    ## 10    3748 2023_01_AR…  2023091007 WAS       ARI       REG             1 WAS    
    ## # ℹ 2,342 more rows
    ## # ℹ 365 more variables: posteam_type <chr>, defteam <chr>, side_of_field <chr>,
    ## #   yardline_100 <dbl>, game_date <chr>, quarter_seconds_remaining <dbl>,
    ## #   half_seconds_remaining <dbl>, game_seconds_remaining <dbl>,
    ## #   game_half <chr>, quarter_end <dbl>, drive <dbl>, sp <dbl>, qtr <dbl>,
    ## #   down <dbl>, goal_to_go <dbl>, time <time>, yrdln <chr>, ydstogo <dbl>,
    ## #   ydsnet <dbl>, desc <chr>, play_type <chr>, yards_gained <dbl>, …

    names(punts)

    ##   [1] "play_id"                             
    ##   [2] "game_id"                             
    ##   [3] "old_game_id"                         
    ##   [4] "home_team"                           
    ##   [5] "away_team"                           
    ##   [6] "season_type"                         
    ##   [7] "week"                                
    ##   [8] "posteam"                             
    ##   [9] "posteam_type"                        
    ##  [10] "defteam"                             
    ##  [11] "side_of_field"                       
    ##  [12] "yardline_100"                        
    ##  [13] "game_date"                           
    ##  [14] "quarter_seconds_remaining"           
    ##  [15] "half_seconds_remaining"              
    ##  [16] "game_seconds_remaining"              
    ##  [17] "game_half"                           
    ##  [18] "quarter_end"                         
    ##  [19] "drive"                               
    ##  [20] "sp"                                  
    ##  [21] "qtr"                                 
    ##  [22] "down"                                
    ##  [23] "goal_to_go"                          
    ##  [24] "time"                                
    ##  [25] "yrdln"                               
    ##  [26] "ydstogo"                             
    ##  [27] "ydsnet"                              
    ##  [28] "desc"                                
    ##  [29] "play_type"                           
    ##  [30] "yards_gained"                        
    ##  [31] "shotgun"                             
    ##  [32] "no_huddle"                           
    ##  [33] "qb_dropback"                         
    ##  [34] "qb_kneel"                            
    ##  [35] "qb_spike"                            
    ##  [36] "qb_scramble"                         
    ##  [37] "pass_length"                         
    ##  [38] "pass_location"                       
    ##  [39] "air_yards"                           
    ##  [40] "yards_after_catch"                   
    ##  [41] "run_location"                        
    ##  [42] "run_gap"                             
    ##  [43] "field_goal_result"                   
    ##  [44] "kick_distance"                       
    ##  [45] "extra_point_result"                  
    ##  [46] "two_point_conv_result"               
    ##  [47] "home_timeouts_remaining"             
    ##  [48] "away_timeouts_remaining"             
    ##  [49] "timeout"                             
    ##  [50] "timeout_team"                        
    ##  [51] "td_team"                             
    ##  [52] "td_player_name"                      
    ##  [53] "td_player_id"                        
    ##  [54] "posteam_timeouts_remaining"          
    ##  [55] "defteam_timeouts_remaining"          
    ##  [56] "total_home_score"                    
    ##  [57] "total_away_score"                    
    ##  [58] "posteam_score"                       
    ##  [59] "defteam_score"                       
    ##  [60] "score_differential"                  
    ##  [61] "posteam_score_post"                  
    ##  [62] "defteam_score_post"                  
    ##  [63] "score_differential_post"             
    ##  [64] "no_score_prob"                       
    ##  [65] "opp_fg_prob"                         
    ##  [66] "opp_safety_prob"                     
    ##  [67] "opp_td_prob"                         
    ##  [68] "fg_prob"                             
    ##  [69] "safety_prob"                         
    ##  [70] "td_prob"                             
    ##  [71] "extra_point_prob"                    
    ##  [72] "two_point_conversion_prob"           
    ##  [73] "ep"                                  
    ##  [74] "epa"                                 
    ##  [75] "total_home_epa"                      
    ##  [76] "total_away_epa"                      
    ##  [77] "total_home_rush_epa"                 
    ##  [78] "total_away_rush_epa"                 
    ##  [79] "total_home_pass_epa"                 
    ##  [80] "total_away_pass_epa"                 
    ##  [81] "air_epa"                             
    ##  [82] "yac_epa"                             
    ##  [83] "comp_air_epa"                        
    ##  [84] "comp_yac_epa"                        
    ##  [85] "total_home_comp_air_epa"             
    ##  [86] "total_away_comp_air_epa"             
    ##  [87] "total_home_comp_yac_epa"             
    ##  [88] "total_away_comp_yac_epa"             
    ##  [89] "total_home_raw_air_epa"              
    ##  [90] "total_away_raw_air_epa"              
    ##  [91] "total_home_raw_yac_epa"              
    ##  [92] "total_away_raw_yac_epa"              
    ##  [93] "wp"                                  
    ##  [94] "def_wp"                              
    ##  [95] "home_wp"                             
    ##  [96] "away_wp"                             
    ##  [97] "wpa"                                 
    ##  [98] "vegas_wpa"                           
    ##  [99] "vegas_home_wpa"                      
    ## [100] "home_wp_post"                        
    ## [101] "away_wp_post"                        
    ## [102] "vegas_wp"                            
    ## [103] "vegas_home_wp"                       
    ## [104] "total_home_rush_wpa"                 
    ## [105] "total_away_rush_wpa"                 
    ## [106] "total_home_pass_wpa"                 
    ## [107] "total_away_pass_wpa"                 
    ## [108] "air_wpa"                             
    ## [109] "yac_wpa"                             
    ## [110] "comp_air_wpa"                        
    ## [111] "comp_yac_wpa"                        
    ## [112] "total_home_comp_air_wpa"             
    ## [113] "total_away_comp_air_wpa"             
    ## [114] "total_home_comp_yac_wpa"             
    ## [115] "total_away_comp_yac_wpa"             
    ## [116] "total_home_raw_air_wpa"              
    ## [117] "total_away_raw_air_wpa"              
    ## [118] "total_home_raw_yac_wpa"              
    ## [119] "total_away_raw_yac_wpa"              
    ## [120] "punt_blocked"                        
    ## [121] "first_down_rush"                     
    ## [122] "first_down_pass"                     
    ## [123] "first_down_penalty"                  
    ## [124] "third_down_converted"                
    ## [125] "third_down_failed"                   
    ## [126] "fourth_down_converted"               
    ## [127] "fourth_down_failed"                  
    ## [128] "incomplete_pass"                     
    ## [129] "touchback"                           
    ## [130] "interception"                        
    ## [131] "punt_inside_twenty"                  
    ## [132] "punt_in_endzone"                     
    ## [133] "punt_out_of_bounds"                  
    ## [134] "punt_downed"                         
    ## [135] "punt_fair_catch"                     
    ## [136] "kickoff_inside_twenty"               
    ## [137] "kickoff_in_endzone"                  
    ## [138] "kickoff_out_of_bounds"               
    ## [139] "kickoff_downed"                      
    ## [140] "kickoff_fair_catch"                  
    ## [141] "fumble_forced"                       
    ## [142] "fumble_not_forced"                   
    ## [143] "fumble_out_of_bounds"                
    ## [144] "solo_tackle"                         
    ## [145] "safety"                              
    ## [146] "penalty"                             
    ## [147] "tackled_for_loss"                    
    ## [148] "fumble_lost"                         
    ## [149] "own_kickoff_recovery"                
    ## [150] "own_kickoff_recovery_td"             
    ## [151] "qb_hit"                              
    ## [152] "rush_attempt"                        
    ## [153] "pass_attempt"                        
    ## [154] "sack"                                
    ## [155] "touchdown"                           
    ## [156] "pass_touchdown"                      
    ## [157] "rush_touchdown"                      
    ## [158] "return_touchdown"                    
    ## [159] "extra_point_attempt"                 
    ## [160] "two_point_attempt"                   
    ## [161] "field_goal_attempt"                  
    ## [162] "kickoff_attempt"                     
    ## [163] "punt_attempt"                        
    ## [164] "fumble"                              
    ## [165] "complete_pass"                       
    ## [166] "assist_tackle"                       
    ## [167] "lateral_reception"                   
    ## [168] "lateral_rush"                        
    ## [169] "lateral_return"                      
    ## [170] "lateral_recovery"                    
    ## [171] "passer_player_id"                    
    ## [172] "passer_player_name"                  
    ## [173] "passing_yards"                       
    ## [174] "receiver_player_id"                  
    ## [175] "receiver_player_name"                
    ## [176] "receiving_yards"                     
    ## [177] "rusher_player_id"                    
    ## [178] "rusher_player_name"                  
    ## [179] "rushing_yards"                       
    ## [180] "lateral_receiver_player_id"          
    ## [181] "lateral_receiver_player_name"        
    ## [182] "lateral_receiving_yards"             
    ## [183] "lateral_rusher_player_id"            
    ## [184] "lateral_rusher_player_name"          
    ## [185] "lateral_rushing_yards"               
    ## [186] "lateral_sack_player_id"              
    ## [187] "lateral_sack_player_name"            
    ## [188] "interception_player_id"              
    ## [189] "interception_player_name"            
    ## [190] "lateral_interception_player_id"      
    ## [191] "lateral_interception_player_name"    
    ## [192] "punt_returner_player_id"             
    ## [193] "punt_returner_player_name"           
    ## [194] "lateral_punt_returner_player_id"     
    ## [195] "lateral_punt_returner_player_name"   
    ## [196] "kickoff_returner_player_name"        
    ## [197] "kickoff_returner_player_id"          
    ## [198] "lateral_kickoff_returner_player_id"  
    ## [199] "lateral_kickoff_returner_player_name"
    ## [200] "punter_player_id"                    
    ## [201] "punter_player_name"                  
    ## [202] "kicker_player_name"                  
    ## [203] "kicker_player_id"                    
    ## [204] "own_kickoff_recovery_player_id"      
    ## [205] "own_kickoff_recovery_player_name"    
    ## [206] "blocked_player_id"                   
    ## [207] "blocked_player_name"                 
    ## [208] "tackle_for_loss_1_player_id"         
    ## [209] "tackle_for_loss_1_player_name"       
    ## [210] "tackle_for_loss_2_player_id"         
    ## [211] "tackle_for_loss_2_player_name"       
    ## [212] "qb_hit_1_player_id"                  
    ## [213] "qb_hit_1_player_name"                
    ## [214] "qb_hit_2_player_id"                  
    ## [215] "qb_hit_2_player_name"                
    ## [216] "forced_fumble_player_1_team"         
    ## [217] "forced_fumble_player_1_player_id"    
    ## [218] "forced_fumble_player_1_player_name"  
    ## [219] "forced_fumble_player_2_team"         
    ## [220] "forced_fumble_player_2_player_id"    
    ## [221] "forced_fumble_player_2_player_name"  
    ## [222] "solo_tackle_1_team"                  
    ## [223] "solo_tackle_2_team"                  
    ## [224] "solo_tackle_1_player_id"             
    ## [225] "solo_tackle_2_player_id"             
    ## [226] "solo_tackle_1_player_name"           
    ## [227] "solo_tackle_2_player_name"           
    ## [228] "assist_tackle_1_player_id"           
    ## [229] "assist_tackle_1_player_name"         
    ## [230] "assist_tackle_1_team"                
    ## [231] "assist_tackle_2_player_id"           
    ## [232] "assist_tackle_2_player_name"         
    ## [233] "assist_tackle_2_team"                
    ## [234] "assist_tackle_3_player_id"           
    ## [235] "assist_tackle_3_player_name"         
    ## [236] "assist_tackle_3_team"                
    ## [237] "assist_tackle_4_player_id"           
    ## [238] "assist_tackle_4_player_name"         
    ## [239] "assist_tackle_4_team"                
    ## [240] "tackle_with_assist"                  
    ## [241] "tackle_with_assist_1_player_id"      
    ## [242] "tackle_with_assist_1_player_name"    
    ## [243] "tackle_with_assist_1_team"           
    ## [244] "tackle_with_assist_2_player_id"      
    ## [245] "tackle_with_assist_2_player_name"    
    ## [246] "tackle_with_assist_2_team"           
    ## [247] "pass_defense_1_player_id"            
    ## [248] "pass_defense_1_player_name"          
    ## [249] "pass_defense_2_player_id"            
    ## [250] "pass_defense_2_player_name"          
    ## [251] "fumbled_1_team"                      
    ## [252] "fumbled_1_player_id"                 
    ## [253] "fumbled_1_player_name"               
    ## [254] "fumbled_2_player_id"                 
    ## [255] "fumbled_2_player_name"               
    ## [256] "fumbled_2_team"                      
    ## [257] "fumble_recovery_1_team"              
    ## [258] "fumble_recovery_1_yards"             
    ## [259] "fumble_recovery_1_player_id"         
    ## [260] "fumble_recovery_1_player_name"       
    ## [261] "fumble_recovery_2_team"              
    ## [262] "fumble_recovery_2_yards"             
    ## [263] "fumble_recovery_2_player_id"         
    ## [264] "fumble_recovery_2_player_name"       
    ## [265] "sack_player_id"                      
    ## [266] "sack_player_name"                    
    ## [267] "half_sack_1_player_id"               
    ## [268] "half_sack_1_player_name"             
    ## [269] "half_sack_2_player_id"               
    ## [270] "half_sack_2_player_name"             
    ## [271] "return_team"                         
    ## [272] "return_yards"                        
    ## [273] "penalty_team"                        
    ## [274] "penalty_player_id"                   
    ## [275] "penalty_player_name"                 
    ## [276] "penalty_yards"                       
    ## [277] "replay_or_challenge"                 
    ## [278] "replay_or_challenge_result"          
    ## [279] "penalty_type"                        
    ## [280] "defensive_two_point_attempt"         
    ## [281] "defensive_two_point_conv"            
    ## [282] "defensive_extra_point_attempt"       
    ## [283] "defensive_extra_point_conv"          
    ## [284] "safety_player_name"                  
    ## [285] "safety_player_id"                    
    ## [286] "season"                              
    ## [287] "cp"                                  
    ## [288] "cpoe"                                
    ## [289] "series"                              
    ## [290] "series_success"                      
    ## [291] "series_result"                       
    ## [292] "order_sequence"                      
    ## [293] "start_time"                          
    ## [294] "time_of_day"                         
    ## [295] "stadium"                             
    ## [296] "weather"                             
    ## [297] "nfl_api_id"                          
    ## [298] "play_clock"                          
    ## [299] "play_deleted"                        
    ## [300] "play_type_nfl"                       
    ## [301] "special_teams_play"                  
    ## [302] "st_play_type"                        
    ## [303] "end_clock_time"                      
    ## [304] "end_yard_line"                       
    ## [305] "fixed_drive"                         
    ## [306] "fixed_drive_result"                  
    ## [307] "drive_real_start_time"               
    ## [308] "drive_play_count"                    
    ## [309] "drive_time_of_possession"            
    ## [310] "drive_first_downs"                   
    ## [311] "drive_inside20"                      
    ## [312] "drive_ended_with_score"              
    ## [313] "drive_quarter_start"                 
    ## [314] "drive_quarter_end"                   
    ## [315] "drive_yards_penalized"               
    ## [316] "drive_start_transition"              
    ## [317] "drive_end_transition"                
    ## [318] "drive_game_clock_start"              
    ## [319] "drive_game_clock_end"                
    ## [320] "drive_start_yard_line"               
    ## [321] "drive_end_yard_line"                 
    ## [322] "drive_play_id_started"               
    ## [323] "drive_play_id_ended"                 
    ## [324] "away_score"                          
    ## [325] "home_score"                          
    ## [326] "location"                            
    ## [327] "result"                              
    ## [328] "total"                               
    ## [329] "spread_line"                         
    ## [330] "total_line"                          
    ## [331] "div_game"                            
    ## [332] "roof"                                
    ## [333] "surface"                             
    ## [334] "temp"                                
    ## [335] "wind"                                
    ## [336] "home_coach"                          
    ## [337] "away_coach"                          
    ## [338] "stadium_id"                          
    ## [339] "game_stadium"                        
    ## [340] "aborted_play"                        
    ## [341] "success"                             
    ## [342] "passer"                              
    ## [343] "passer_jersey_number"                
    ## [344] "rusher"                              
    ## [345] "rusher_jersey_number"                
    ## [346] "receiver"                            
    ## [347] "receiver_jersey_number"              
    ## [348] "pass"                                
    ## [349] "rush"                                
    ## [350] "first_down"                          
    ## [351] "special"                             
    ## [352] "play"                                
    ## [353] "passer_id"                           
    ## [354] "rusher_id"                           
    ## [355] "receiver_id"                         
    ## [356] "name"                                
    ## [357] "jersey_number"                       
    ## [358] "id"                                  
    ## [359] "fantasy_player_name"                 
    ## [360] "fantasy_player_id"                   
    ## [361] "fantasy"                             
    ## [362] "fantasy_id"                          
    ## [363] "out_of_bounds"                       
    ## [364] "home_opening_kickoff"                
    ## [365] "qb_epa"                              
    ## [366] "xyac_epa"                            
    ## [367] "xyac_mean_yardage"                   
    ## [368] "xyac_median_yardage"                 
    ## [369] "xyac_success"                        
    ## [370] "xyac_fd"                             
    ## [371] "xpass"                               
    ## [372] "pass_oe"                             
    ## [373] "opp_FG_TD_prob"

\#Variables to Use \## Punt Distance

    dens <- density(punts$kick_distance)
    plot(dens$x,dens$y,main="Density of Punt Distance",xlab="Punt Distance (YDS)")
    abline(v=mean(punts$kick_distance),lwd=4,col="red")
    abline(v=quantile(punts$kick_distance)[2],lwd=4,col="yellow")
    abline(v=quantile(punts$kick_distance)[4],lwd=4,col="yellow")

![](passprop_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    plot(punts$kick_distance)

![](passprop_files/figure-markdown_strict/unnamed-chunk-5-2.png)

    summary(punts$kick_distance)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   41.00   47.00   47.21   54.00   83.00

## Score Difference

    dens <- density(punts$score_differential)
    plot(dens$x,dens$y,main="Density of Score Differential",xlab="Punting Team Score - Receiving Team Score")
    abline(v=mean(punts$score_differential),lwd=4,col="red")
    abline(v=quantile(punts$score_differential)[2],lwd=4,col="yellow")
    abline(v=quantile(punts$score_differential)[4],lwd=4,col="yellow")

![](passprop_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    plot(punts$score_differential)

![](passprop_files/figure-markdown_strict/unnamed-chunk-6-2.png)

    summary(punts$score_differential)

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -43.0000  -6.0000   0.0000  -0.2292   5.0000  49.0000

# Receiving Team TD Chance

    dens <- density(punts$opp_td_prob)
    plot(dens$x,dens$y,main="Density of Receiving Team TD Probability",xlab="Chance of Receiving Team TD Next")
    abline(v=mean(punts$opp_td_prob),lwd=4,col="red")
    abline(v=quantile(punts$opp_td_prob)[2],lwd=4,col="yellow")
    abline(v=quantile(punts$opp_td_prob)[4],lwd=4,col="yellow")

![](passprop_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    plot(punts$opp_td_prob)

![](passprop_files/figure-markdown_strict/unnamed-chunk-7-2.png)

    summary(punts$opp_td_prob)

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## 0.005572 0.264346 0.327678 0.303807 0.375163 0.490164

# Receiving Team FG Probability

    dens <- density(punts$opp_fg_prob)
    plot(dens$x,dens$y,main="Density of Receiving Team FG Probabilty",xlab="Chance of Receiving Team FG Next")
    abline(v=mean(punts$opp_fg_prob),lwd=4,col="red")
    abline(v=quantile(punts$opp_fg_prob)[2],lwd=4,col="yellow")
    abline(v=quantile(punts$opp_fg_prob)[4],lwd=4,col="yellow")

![](passprop_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    plot(punts$opp_fg_prob)

![](passprop_files/figure-markdown_strict/unnamed-chunk-8-2.png)

    summary(punts$opp_fg_prob)

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## 0.001598 0.169964 0.194625 0.192336 0.223127 0.382078

# Combined FG and TD receiving team chance

    dens <- density(punts$opp_FG_TD_prob)
    plot(dens$x,dens$y,main="Density of Receiving Team Score",xlab="Chance of Receiving Team Score Next")
    abline(v=mean(punts$opp_FG_TD_prob),lwd=4,col="red")
    abline(v=quantile(punts$opp_FG_TD_prob)[2],lwd=4,col="yellow")
    abline(v=quantile(punts$opp_FG_TD_prob)[4],lwd=4,col="yellow")

![](passprop_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    plot(punts$opp_FG_TD_prob)

![](passprop_files/figure-markdown_strict/unnamed-chunk-9-2.png)

    summary(punts$opp_FG_TD_prob)

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## 0.009337 0.436504 0.524021 0.496143 0.599271 0.799067

# Punting Team TD Probability

    dens <- density(punts$td_prob)
    plot(dens$x,dens$y,main="Density of Punting Team TD Probability",xlab="Chance of Punting Team TD Next")
    abline(v=mean(punts$td_prob),lwd=4,col="red")
    abline(v=quantile(punts$td_prob)[2],lwd=4,col="yellow")
    abline(v=quantile(punts$td_prob)[4],lwd=4,col="yellow")

![](passprop_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    plot(punts$td_prob)

![](passprop_files/figure-markdown_strict/unnamed-chunk-10-2.png)

    summary(punts$td_prob)

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## 0.003863 0.156368 0.212498 0.200551 0.256899 0.395792
