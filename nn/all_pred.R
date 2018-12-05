#df.all <- read.csv('sparse.csv')
#setwd('E:/Seafile/Pitt/Term3/DM/Project/TF/aaafa')

library(keras)
model = load_model_hdf5("./train01/m2.hdf5")

source('../Random.r')
valid.heroes.id <- c(1:23, 25:114, 119:121)

gen_lineup = function(valid.heroes.id) {
  lineup <- integer(128)
  lineup <- random.pick(lineup, valid.heroes.id, side = 1)
  lineup <- random.pick(lineup, valid.heroes.id, side = -1)
  lineup <- random.pick(lineup, valid.heroes.id, side = -1)
  lineup <- random.pick(lineup, valid.heroes.id, side = 1)
  lineup <- random.pick(lineup, valid.heroes.id, side = 1)
  lineup <- random.pick(lineup, valid.heroes.id, side = -1)
  lineup <- random.pick(lineup, valid.heroes.id, side = -1)
  lineup <- random.pick(lineup, valid.heroes.id, side = 1)
  lineup <- random.pick(lineup, valid.heroes.id, side = 1)
  lineup <- random.pick(lineup, valid.heroes.id, side = -1)
  return(lineup)
}
#print(lineup)


con <- file("../all_pred.html", "w")
cat('<!DOCTYPE html>\n<html lang=en>\n<head>\n<meta charset=utf-8>\n<meta name=viewport content="width=device-width, initial-scale=1, shrink-to-fit=no">\n<link rel=stylesheet href=vendors/bower_components/material-design-iconic-font/dist/css/material-design-iconic-font.min.css>\n<link rel=stylesheet href=vendors/bower_components/animate.css/animate.min.css>\n<link rel=stylesheet href=vendors/bower_components/jquery.scrollbar/jquery.scrollbar.css>\n<link rel=stylesheet href=vendors/bower_components/fullcalendar/dist/fullcalendar.min.css>\n<link rel=stylesheet href=css/app.min.css>\n<style>img{width:75px}</style>\n</head>\n<body data-ma-theme=red>\n<main class=main>\n<div class=page-loader>\n<div class=page-loader__spinner>\n<svg viewBox="25 25 50 50">\n<circle cx=50 cy=50 r=20 fill=none stroke-width=2 stroke-miterlimit=10 />\n</svg>\n</div>\n</div>\n<header class=header>\n<div class="header__logo hidden-sm-down">\n<h1><a href>DOTA++</a></h1>\n</div>\n</header>\n<section class="content content--full">\n<div class=content__inner>\n<header class=content__title>\n<h1>Simulation Outcome</h1>\n</header>\n<div class=card>\n<div class=card-body>\n<table class="table mb-0">\n<thead>\n<tr>\n<th>Radiant</th>\n<th>Dire</th>\n<th>Radiant Win Rate</th>\n</tr>\n</thead>\n<tbody>\n', file=con)

winList=list()

for (i in 1:10000) {
  lu = gen_lineup(valid.heroes.id)
  prob = predict(model, t(data.matrix(lu)))[1,1]
  r = which(lu==1)
  d = which(lu==-1)
  winList[i]=prob
  lineData = sprintf('<tr>\n<td>\n<img src=img/images/%d.png>\n<img src=img/images/%d.png>\n<img src=img/images/%d.png>\n<img src=img/images/%d.png>\n<img src=img/images/%d.png>\n</td>\n<td>\n<img src=img/images/%d.png>\n<img src=img/images/%d.png>\n<img src=img/images/%d.png>\n<img src=img/images/%d.png>\n<img src=img/images/%d.png>\n</td>\n<td>%.2f%%</td>\n</tr>\n',r[1],r[2],r[3],r[4],r[5],d[1],d[2],d[3],d[4],d[5],prob*100)
  cat(lineData, file = con)
}

cat('</tbody>\n</table>\n</div>\n</div>\n<footer class="footer hidden-xs-down">\n<p>Â© INFSCI 2160 Fall 2018 - Group 16. All rights reserved.</p>\n</footer>\n</section>\n</main>\n<!--[if IE]>\n<div class=ie-warning>\n<h1>Warning!!</h1>\n<p>You are using an outdated version of Internet Explorer, please upgrade to any of the following web browsers to access this website.</p>\n<div class=ie-warning__downloads>\n<a href=http://www.google.com/chrome>\n<img src=img/browsers/chrome.png alt>\n</a>\n<a href=https://www.mozilla.org/en-US/firefox/new>\n<img src=img/browsers/firefox.png alt>\n</a>\n<a href=http://www.opera.com>\n<img src=img/browsers/opera.png alt>\n</a>\n<a href=https://support.apple.com/downloads/safari>\n<img src=img/browsers/safari.png alt>\n</a>\n<a href=https://www.microsoft.com/en-us/windows/microsoft-edge>\n<img src=img/browsers/edge.png alt>\n</a>\n<a href=http://windows.microsoft.com/en-us/internet-explorer/download-ie>\n<img src=img/browsers/ie.png alt>\n</a>\n</div>\n<p>Sorry for the inconvenience!</p>\n</div>\n<![endif]-->\n<script src=vendors/bower_components/jquery/dist/jquery.min.js></script>\n<script src=vendors/bower_components/popper.js/dist/umd/popper.min.js></script>\n<script src=vendors/bower_components/bootstrap/dist/js/bootstrap.min.js></script>\n<script src=vendors/bower_components/jquery.scrollbar/jquery.scrollbar.min.js></script>\n<script src=vendors/bower_components/jquery-scrollLock/jquery-scrollLock.min.js></script>\n<script src=vendors/bower_components/flot/jquery.flot.js></script>\n<script src=vendors/bower_components/flot/jquery.flot.resize.js></script>\n<script src=vendors/bower_components/flot.curvedlines/curvedLines.js></script>\n<script src=vendors/bower_components/jqvmap/dist/jquery.vmap.min.js></script>\n<script src=vendors/bower_components/jqvmap/dist/maps/jquery.vmap.world.js></script>\n<script src=vendors/bower_components/jquery.easy-pie-chart/dist/jquery.easypiechart.min.js></script>\n<script src=vendors/bower_components/salvattore/dist/salvattore.min.js></script>\n<script src=vendors/jquery.sparkline/jquery.sparkline.min.js></script>\n<script src=vendors/bower_components/moment/min/moment.min.js></script>\n<script src=vendors/bower_components/fullcalendar/dist/fullcalendar.min.js></script>\n<script src=demo/js/flot-charts/curved-line.js></script>\n<script src=demo/js/flot-charts/dynamic.js></script>\n<script src=demo/js/flot-charts/line.js></script>\n<script src=demo/js/flot-charts/chart-tooltips.js></script>\n<script src=demo/js/other-charts.js></script>\n<script src=demo/js/jqvmap.js></script>\n<script src=js/app.min.js></script>\n</body>\n</html>', file=con)
close(con)

# cutOffVal = 0.5, Win Count
print(sprintf("Win matches count: %d, for cut off value 0.5", length(winList[winList>0.5])))
