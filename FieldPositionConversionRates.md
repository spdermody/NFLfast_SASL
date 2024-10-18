    winners1 <- goforit %>% filter(home_team==posteam&result>0)
    winners2 <- goforit %>% filter(away_team==posteam&result<0)
    winners <- rbind(winners1,winners2)
    losers1 <- goforit %>% filter(home_team==posteam&result<0)
    losers2 <- goforit %>% filter(away_team==posteam&result>0)
    losers <- rbind(losers1,losers2)

    conversion_rates <- c()
    for (yds in 1:10) {
          tbl <- winners %>% filter(ydstogo == yds)
          conversion_rates[yds] <- mean(tbl$fourth_down_converted, na.rm = TRUE)
     }
    conversion_rates1 <- c()
    for (yds in 1:10) {
          tbl <- losers %>% filter(ydstogo == yds)
          conversion_rates1[yds] <- mean(tbl$fourth_down_converted, na.rm = TRUE)
     }
    x <- 1:10
    plot(x,conversion_rates,type="b",pch=16,col="green",ylim=c(0.2,0.8),main="Conversion Rates for Plays between 41-50 yard line",xlab="Yards to go",ylab="Conversion Rate")
    points(x,conversion_rates1,type="b",pch=16,col="red")
    legend(6,0.8,legend=c("Teams that won","Teams that lost"),col=c("green","red"),lty=1:2)

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    field_position_groups <- list(
      "1-10" = goforit %>% filter(yardline_100 >= 1 & yardline_100 <= 10),
      "11-20" = goforit %>% filter(yardline_100 >= 11 & yardline_100 <= 20),
      "21-30" = goforit %>% filter(yardline_100 >= 21 & yardline_100 <= 30),
      "31-40" = goforit %>% filter(yardline_100 >= 31 & yardline_100 <= 40),
      "41-50" = goforit %>% filter(yardline_100 >= 41 & yardline_100 <= 50),
      "51-60" = goforit %>% filter(yardline_100 >= 51 & yardline_100 <= 60),
      "61-70" = goforit %>% filter(yardline_100 >= 61 & yardline_100 <= 70),
      "71-80" = goforit %>% filter(yardline_100 >= 71 & yardline_100 <= 80),
      "81-90" = goforit %>% filter(yardline_100 >= 81 & yardline_100 <= 90),
      "91-100" = goforit %>% filter(yardline_100 >= 91 & yardline_100 <= 100)
    )

    conversion_rates <- sapply(field_position_groups, function(group) {
      mean(group$fourth_down_converted, na.rm = TRUE)
    })

    positions <- names(conversion_rates)
    rates <- conversion_rates

    barplot(rates, names.arg = positions, main = "Fourth Down Conversion Rates by Field Position",
            xlab = "Field Position (yards from end zone)", ylab = "Conversion Rate", col = "green")

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    for (position_group_name in names(field_position_groups)) {
      
      group_data <- field_position_groups[[position_group_name]]
      
      if (nrow(group_data) > 0) {
        
        conversion_rates <- numeric(10)
        
        for (yds in 1:10) {
          tbl <- group_data %>% filter(ydstogo == yds)
          conversion_rates[yds] <- mean(tbl$fourth_down_converted, na.rm = TRUE)
        }
        
        ydstogo <- 1:10
        correlation_value <- cor(ydstogo, conversion_rates, use = "complete.obs")
        
        print(paste("Correlation for field position group", position_group_name, ":", correlation_value))
        
        plot(1:10, conversion_rates, type = "b", pch = 16, col = "blue",
             main = paste("4th Down Conversion Rate for Field Position", position_group_name, "Yards from Endzone"),
             xlab = "Yards to Go", ylab = "Conversion Rate",
             ylim = c(0, 1), xlim = c(1, 10))
        
        Sys.sleep(2)
      } else {
        print(paste("No data available for field position group:", position_group_name))
      }
    }

    ## [1] "Correlation for field position group 1-10 : -0.869772451422295"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    ## [1] "Correlation for field position group 11-20 : -0.662610813861021"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-2.png)

    ## [1] "Correlation for field position group 21-30 : -0.969618668547444"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-3.png)

    ## [1] "Correlation for field position group 31-40 : -0.936602677045865"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-4.png)

    ## [1] "Correlation for field position group 41-50 : -0.655778647864192"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-5.png)

    ## [1] "Correlation for field position group 51-60 : -0.911838274203445"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-6.png)

    ## [1] "Correlation for field position group 61-70 : -0.576899256520267"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-7.png)

    ## [1] "Correlation for field position group 71-80 : -0.877720609634372"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-8.png)

    ## [1] "Correlation for field position group 81-90 : -0.65960571649582"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-9.png)

    ## [1] "Correlation for field position group 91-100 : 0.353553390593274"

![](FieldPositionConversionRates_files/figure-markdown_strict/unnamed-chunk-5-10.png)

    group_data_11_20 <- field_position_groups[["11-20"]]

    data_4th_and_2 <- group_data_11_20 %>% filter(ydstogo == 2)

    num_data_points <- nrow(data_4th_and_2)

    print(paste("Number of data points for 4th down and 2 yards to go in the '11-20' field position group:", num_data_points))

    ## [1] "Number of data points for 4th down and 2 yards to go in the '11-20' field position group: 74"
