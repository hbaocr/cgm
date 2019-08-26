How specific foods affect my glucose levels
================
Richard Sprague
2019-08-26

Here are my glucose levels each time I eat a particular food.

First, oatmeal:

``` r
foodlist <- c("Oatmeal (plain)","Oatmeal w/milk","Oatmeal w/butter", "Oatmeal w/cinnamon", "Oatmeal (Reducose)")
food_effect(foodlist) %>% group_by(experiment) %>% mutate(delta = (time - min(time))/60) %>% ggplot(aes(x=delta,y=value, color = experiment)) + geom_line(size = 2) + scale_x_continuous() + labs(title = "Glucose after eating oatmeal", x = "Minutes", y = "mg/dL")
```

![](food_effects_files/figure-gfm/foodEffectsOatmeal-1.png)<!-- -->

and a bunch of other foods:

![](food_effects_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-7.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-8.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-9.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-10.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-11.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-12.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-13.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-14.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-15.png)<!-- -->![](food_effects_files/figure-gfm/unnamed-chunk-2-16.png)<!-- -->
