# advent-of-code
For sharing my adventofcode.com solutions

The runner has been completely revamped for 2020 using Scala.js! From sbt, run
`~fastOptJS` and open the `index.html` file in a web browser. I am hoping to
add visualizations and debugging information in an easier to digest form than printlns.

Run `npm install source-map-support` to get stack traces resolved.

Add the following as the url for a bookmark to view solving times for each star on a private leaderboard:

```javascript
javascript:(function() {
  var hoursAndMinutes = function(year, day, finish) {
    var start = Date.UTC(year, 11, day, 5);
    var minutes = Math.round((finish - start) / 1000 / 60);
    var hours = Math.floor(minutes / 60).toString();
    var remainder = (minutes % 60).toString();
    var hourPlural = (hours == 1) ? "" : "s";
    var minutePlural = (remainder == 1) ? "" : "s";
    return hours + " hour" + hourPlural + ", " + remainder + " minute" + minutePlural;
  };
  var done = function() {
    resp = JSON.parse(this.responseText);
    year = resp.event.toString();
    mems = Object.values(resp.members).sort((a,b) => b.local_score - a.local_score);
    rows = document.querySelectorAll(".privboard-row");
    for(i=1; i<rows.length; i++) {
      stars = rows[i].querySelectorAll(".privboard-star-both, .privboard-star-firstonly, .privboard-star-unlocked");
      for(let day in mems[i-1].completion_day_level) {
        var compl1 = mems[i-1].completion_day_level[day][1];
        var compl2 = mems[i-1].completion_day_level[day][2];
        stars[day-1].title = ""
          + (compl1 != null ? (hoursAndMinutes(year, day, compl1.get_star_ts*1000)) : "")
            + (compl2 != null ? ("\n"+hoursAndMinutes(year, day, compl2.get_star_ts*1000)) : "");
      }
    }
  };
  var apiEndpoint = window.location.href+".json";
  var req = new XMLHttpRequest(); req.addEventListener("load", done);
  req.open("GET", apiEndpoint);
  req.send();
})();
```
