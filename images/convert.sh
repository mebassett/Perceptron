for file in *.html; do sed '/<\/TABLE>/d;s/<TABLE \(.*\)>//g;/<TR>/d;/<\/TR>/d;/&nbsp/d;/<\/TD>/d;s/<TD  BGCOLOR=#\(.*\)>/\1/g;/^$/d;s/ *//g' 9.0.html > 9.0.html.new; done
