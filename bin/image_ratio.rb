#!/usr/bin/ruby
printf("%.2f\n", `identify -format "%[fx:w/h]" #{ARGV.first}`.to_f)



