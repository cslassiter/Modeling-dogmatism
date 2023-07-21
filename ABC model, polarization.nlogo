globals [ believer-list tote-patch neighbor-list majority]
patches-own [
  friends
  belief
  discriminating-value
  pop-my-friends
  memory
  z
  changeable?
  dog?
  dog-timer
  p-decay-type
  history
  who-converted
  repeat-conversion
  b-timer
  f-dog
  neighbor-beliefs
]

;;change everythng from turtles to patches. save memory

to setup
  ca
  reset-ticks
  ask patches [set-up-routine]
   if num-dog >= 0 [assign-dogmatists] ;; assign dogmatists, if applicable
  set believer-list (range 1 6 1)
  set neighbor-list (sentence n-values 47 [1.5] n-values 2 [2.5] n-values 1 [0])
  update-color
  set majority 0
 if max-discrim-value > 1 [user-message "can't have a max-discriminating-value equal to or greater than 1.0"] ;; juuuuust making sure these values stay below 1.0
end

to set-up-routine
  set belief one-of (range 1 6 1)
  set discriminating-value max-discrim-value
  set memory (list belief)
  repeat memory-size - 1 [
    set memory lput one-of (range 1 6 1) memory]
  set history []
  set history lput belief history
  set z 0
  set changeable? true
  set dog? false
  set tote-patch count patches
  set dog-timer 0
  set who-converted []
  set repeat-conversion []
  ifelse decay-type != "randomized" [set p-decay-type decay-type] [set p-decay-type one-of ["exponential" "linear" "log" "none"]]
  set b-timer 0

end

to assign-dogmatists ;; think about this -- assign dogmatists first and then fill out the rest or assign dogmatists from among population?
  let nd round ((num-dog * .2) * count patches)
    ask up-to-n-of nd patches with [belief = 1] [set discriminating-value 0 set dog? true set dog-timer 1]
    ask up-to-n-of (nd * multiple-for-excess-dogs) patches with [belief = 5] [set discriminating-value 0 set dog? true set dog-timer 1 set f-dog true]
  if (num-dog * multiple-for-excess-dogs) >= 1.1 [user-message "total number of dogmatists can't exceed more than .2 of the population. change either num-dog or excess-dogs so the product is less than or equal to 1"]
  if (num-dog = 1) [user-message "Note that 1-group and 5-group have maxed out their total number of dogmatists. Any value greater than 0 for multiple-for-excess-dogs has no effect on the model"]

end

to go
  ;if ticks <= 0 [reset-timer]
  set majority modes [belief] of patches
  adjust-d-v
  ask patches [influence]
  ask patches [set changeable? true]
  update-color
  update-dog-status
  plot-belief
  if new-generations? [ ;; this is if you want to have turtles die and get replaced
    die-and-replace]
  ;ask patches [set history lput belief history]

  tick
  if ticks >= 100 [ stop] ;; think about the issue once every 3 months (on average...i.e. 4x per year) for 25 years ;;
  ;if ticks >= 100 [print timer stop]
end


;; a smattering of useful reporters

to-report count-believers ;; reports list of the proportion of the population endorsing each belief
  let n []
  foreach (range 1 6 1) [ i -> set n lput ((count patches with [belief = i]) / tote-patch) n]
  report n
end

to-report list-of-beliefs ;; reports a list of beliefs that are still endorsed by agents
  report sort remove-duplicates [belief] of patches
end

to-report count-dogs
  report (count patches with [dog?] / count patches)
end

to-report count-non-dogs
  report 1 - count-dogs
end


to-report time-to-dog-data
  let a []
  let b []
  let c []
  let d []
  ifelse not empty? filter [i -> i > 1] [dog-timer] of patches[
    set a min filter [i -> i > 1] [dog-timer] of patches
    set b max filter [i -> i > 1] [dog-timer] of patches
    set c mean filter [i -> i > 1] [dog-timer] of patches
    set d median filter [i -> i > 1] [dog-timer] of patches][
    set a 0
    set b 0
    set c 0
    set d 0]
  report (list a b c d)
end

to-report pop-with-shared-belief ;; reports how many turtles endorse the most widely-endorsed belief (output is an integer)
  let b-list []
  set b-list [belief] of patches
  let m []
  set m modes b-list
  if length m > 1 [set m (list min m)] ;; if there is more than one mode, pick the smallest value
  let t 0
  foreach b-list [ ?1 -> if member? ?1 m [set t t + 1] ]
  report t
end

to-report beliefs-in-circulation ;; reports number of beliefs still in circulation (output is an integer)
  let m remove-duplicates [belief] of patches
  report length m
end

;;;; now getting to the mechanisms of the model

to adjust-d-v ;; all start at .05 and bottom out at 0 after 20 ticks (= 5 years in the model)
  ask patches [
    if dog? = false and length remove-duplicates memory = 1 [
    set z z + 1
    if p-decay-type = "exponential" [
        set discriminating-value discriminating-value - (discriminating-value * .27)
        if discriminating-value < .0001 [set discriminating-value 0]] ;; exponential, round down to zero if low enough
    if p-decay-type = "logistic-small" [
        let temp-dv1 (3.1 * discriminating-value * (1 - discriminating-value)) ;; boringly periodic, logistic equation
        set discriminating-value temp-dv1 * .25]
    if p-decay-type = "logistic-large" [
        let temp-dv1 (3.99999 * discriminating-value * (1 - discriminating-value)) ;; wackily periodic, logistic equation
        set discriminating-value temp-dv1 * .25]
    if p-decay-type = "linear" [
        set discriminating-value discriminating-value - .00125] ;; linear
    if p-decay-type = "log" [
        ifelse z < 20 [set discriminating-value (ln (20 - z)) * .0169815] [set discriminating-value 0]];; log
    if p-decay-type = "random" [
        set discriminating-value random-float 1.0]
    if p-decay-type = "none" []
  ;
  ]
if discriminating-value < 0 [set discriminating-value 0]
  ]
end
;;; main mechanisms for the model

to influence ;; mechanism of belief change. patch asks neighbors to adopt belief. friends roll dice to see if that happens
  let n one-of neighbor-list
  ifelse n != 0 [
    ask patches in-radius n with [changeable? = true] [
      if (distance myself > n - 1) [
       ;#1 -- maybe adopt belief if speaker's memory is diverse
       ;if length remove-duplicates [memory] of myself > 1 [ ;; this is new
       ;#2 -- maybe adopt belief if my belief isn't the majority
        ; if (list [belief] of myself) != majority [ ;;this is also new. fixed it: [belief] of myself needs to be a list
       ;#3 -- maybe adopt belief if speaker isn't a dogmatist
        ;if [dog?] of myself = false [  ;;also new -- number of 5-dogs is decreasing. no one is listening to them so they end up surrounded by different voices. they introspect and change their opinion
       ;#4 -- maybe adopt belief if speaker's belief is different from the most frequent belief in my memory
        ;let mb [belief] of myself
        ;ifelse not member? mb modes memory [
       ;#5 -- maybe adopt belief if there's no majority among neighbors
        let nb [belief] of neighbors
        let m modes nb
        if frequency m nb <= 4 [ ;; this is a good plan but it needs to be more efficient. 5/18/23: it's suddently working better??
          let b [belief] of myself
          let d [dog?] of myself
          let rf random-float 1.0
          let tempdv 0
          ifelse [belief] of self != b [;;
            set tempdv (discriminating-value * (1 / abs (b - [belief] of self)))] [
            set tempdv discriminating-value]
          if rf < tempdv [
            if [belief] of self != b [set who-converted fput d who-converted]
            if [belief] of self = b [set repeat-conversion fput d repeat-conversion]
            set belief b
            set changeable? false ]
      set memory but-last memory
      set memory fput b memory
        ] ;[set memory but-last memory set memory fput [belief] of myself memory] ; this goes with #4
    ]
  ]
]
  [ ;; 2% of the time turtles come to believe something in their memory
    set belief one-of memory
    set changeable? false
    if f-dog = true and dog? = true and belief != 5 [set f-dog false]
  ]

end

to update-color ;; this makes convergence more visible with the turtles
  ask patches [
    set pcolor (ifelse-value
      belief = 1 [yellow]
      belief = 2 [lime]
      belief = 3 [cyan]
      belief = 4 [violet]
      [magenta]
    )
  ]
end

to update-dog-status
  ask patches with [dog? = false] [
    if discriminating-value = 0 [
      set dog? true
      set dog-timer ticks
    ]
  ]
end

to die-and-replace
  ask patches [
    if (random-float 1.0 < replacement-rate)
      [ set-up-routine]
   ]

end

to-report frequency [an-item a-list]
    report length (filter [ i -> i = an-item] a-list)
end

to-report history-values
  let l []
  foreach (range 1 6 1) [ i ->
    set l lput frequency i history l]
  report l
end



to-report belief-neighbors ;; 3/17/22: want to count # of neighbors that 5-believers have who are 5-believers and scale that to the number of dogs or non-dogs
  ;;i've tried: (i) neighbors / dogs & neighbors / non-dogs. (ii) neighbors * dogs & neighbors * non-dogs. (iii) converting number of neighbors to z-scores.
  ;; you now have "who-converted" which tracks whether a patch had its belief changed by a dog or non-dog
  let l []
  let m []
  let a patches with [position (max history-values) history-values = 4 and discriminating-value != 0 ] ;[belief = 5 and discriminating-value != 0 ]
  let b patches with [position (max history-values) history-values = 4 and discriminating-value = 0 ];[belief = 5 and discriminating-value = 0 ]
  foreach sort-by > a [ p ->
    let n [neighbors] of p
    let c count n with [belief = [belief] of p]
    set l fput (c / 8) l]
  foreach sort-by > b [ p ->
    let n [neighbors] of p
    let c count n with [belief = [belief] of p]
    set m fput (c / 8) m]


  ;;calculate z scores
  let avgl mean l
  let avgm mean m
  let stl standard-deviation l
  let stm standard-deviation m
  let lz []
  let mz []

  foreach sort-by > a [ p ->
    let n [neighbors] of p
    let c count n with [belief = [belief] of p]
    let y ((c - avgl) / stl)
    set lz fput y lz]

  foreach sort-by > b [ p ->
    let n [neighbors] of p
    let c count n with [belief = [belief] of p]
    let y ((c - avgm) / stm)
    set mz fput y mz]

  ;report (list (mean l / 8) length l (mean m / 8) length m)
  ;report (list ((mean l / 8) * length l) ((mean m / 8) * length m))
  ;report (list (1 - (mean l / length l)) (1 - (mean m / length m)))
  ;report (list (avgl / 8) (avgm / 8))
  ;report (list length l length m)

  let lf []
  let ltr []
  let rpt []
  let rpf []
  let sub (patches with [position (max history-values) history-values = 4])
  foreach sort-by > sub [p ->
    let f frequency false [who-converted] of p
    let t frequency true [who-converted] of p
    let ff frequency false [repeat-conversion] of p
    let tt frequency true [repeat-conversion] of p
    set lf fput (f / (count a )) lf ;; conversions by non-dog relative to 5-non-dogs
    set ltr fput (t / (count b)) ltr ;; conversions by dogs relative to all 5-dogs
    set rpt fput (ff / (count a)) rpt ;; repeat conversaions (i.e. 5's adopting the belief of another 5) by non-dogs relative to 5-non-dogs
    set rpf fput (tt / ( count b)) rpf] ;; repeat conversaions (i.e. 5's adopting the belief of another 5) by dogs relative to 5-dogs



  ;report (list ((mean lf) / count a) ((mean ltr) / count b) ((mean rpf) / count a) ((mean rpt) / count b)) ;; scaling to pop size
  report (list (mean lf) (mean ltr) (mean rpf) (mean rpt) (count a) (count b))
 ;report (list (mean l) (mean m))
end


;;; now to do some plotting
to plot-belief
set-current-plot "hist-plot"
histogram [belief] of patches
end
@#$#@#$#@
GRAPHICS-WINDOW
258
12
476
231
-1
-1
5.122
1
10
1
1
1
0
1
1
1
-20
20
-20
20
1
1
1
ticks
30.0

BUTTON
737
12
800
45
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
809
12
872
45
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
943
10
1328
240
%-pop with shared belief
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"if count patches = 0 [stop]\nif pop-with-shared-belief > (count patches - 1) [stop]" "if count patches = 0 [stop]\nif pop-with-shared-belief > (count patches - 1) [stop]"
PENS
"default" 1.0 0 -16777216 true "" "plot (pop-with-shared-belief / count patches) * 100"

BUTTON
881
13
944
46
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
4
13
102
73
max-discrim-value
0.05
1
0
Number

PLOT
479
11
735
131
hist-plot
NIL
NIL
1.0
6.0
0.0
1000.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

SWITCH
0
172
156
205
New-generations?
New-generations?
1
1
-1000

SLIDER
0
208
162
241
replacement-rate
replacement-rate
0
.001
0.001
.0001
1
NIL
HORIZONTAL

INPUTBOX
169
185
251
245
multiple-for-excess-dogs
3.0
1
0
Number

INPUTBOX
169
120
252
180
memory-size
10.0
1
0
Number

SLIDER
4
87
138
120
num-dog
num-dog
0
1
0.2
.05
1
NIL
HORIZONTAL

CHOOSER
15
256
153
301
decay-type
decay-type
"exponential" "logistic-small" "logistic-large" "linear" "log" "random" "none" "randomized"
3

PLOT
943
261
1332
411
number of non-dogmatists
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"if ticks = 0 []" "if ticks = 0 []"
PENS
"default" 1.0 0 -16777216 true "" "if ticks != 0 [plot count patches with [discriminating-value != 0]]"

PLOT
168
251
368
401
number of 5 dogmatists
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count patches with [belief = 5 and discriminating-value = 0 ]"
"pen-1" 1.0 0 -7500403 true "" "plot count patches with [belief = 5 ]"
"pen-2" 1.0 0 -2674135 true "" "plot count patches with [belief = 5 and dog? = true ]"

PLOT
523
172
723
322
plot 1
NIL
NIL
0.0
1.0
0.0
0.005
true
false
"" ""
PENS
"default" 1.0 0 -5825686 true "" "if ticks > 0 [ plot item 0 belief-neighbors]"
"pen-1" 1.0 0 -7500403 true "" "if ticks > 0 [plot item 1 belief-neighbors]"

MONITOR
767
152
827
197
NIL
majority
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="PolND1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.01"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.19"/>
  </experiment>
  <experiment name="PolND2" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.02"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.18"/>
  </experiment>
  <experiment name="PolND3" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.03"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.17"/>
  </experiment>
  <experiment name="PolND4" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.04"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.16"/>
  </experiment>
  <experiment name="PolND5" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.15"/>
  </experiment>
  <experiment name="PolND6" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.06"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.14"/>
  </experiment>
  <experiment name="PolND7" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.07"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.13"/>
  </experiment>
  <experiment name="PolND8" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.08"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.12"/>
  </experiment>
  <experiment name="PolND9" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.09"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.11"/>
  </experiment>
  <experiment name="PolND10" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.1"/>
  </experiment>
  <experiment name="PolND11" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.11"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.09"/>
  </experiment>
  <experiment name="PolND12" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.12"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.08"/>
  </experiment>
  <experiment name="PolND13" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.13"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.07"/>
  </experiment>
  <experiment name="PolND14" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.14"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.06"/>
  </experiment>
  <experiment name="PolND15" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.05"/>
  </experiment>
  <experiment name="PolND16" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.16"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.04"/>
  </experiment>
  <experiment name="PolND17" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.17"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.03"/>
  </experiment>
  <experiment name="PolND18" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.18"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.02"/>
  </experiment>
  <experiment name="PolND19" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.19"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="excess-dogs" first="0" step="0.01" last="0.01"/>
  </experiment>
  <experiment name="PolND20" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-dogs</metric>
    <metric>count-believers</metric>
    <metric>count-non-dogs</metric>
    <metric>time-to-dog-data</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="memory-size" first="2" step="1" last="15"/>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;exponential&quot;"/>
      <value value="&quot;linear&quot;"/>
      <value value="&quot;log&quot;"/>
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="excess-dogs">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="dog_vs_nondog" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>belief-neighbors</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-dog" first="0.01" step="0.01" last="0.1"/>
    <enumeratedValueSet variable="memory-size">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="New-generations?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decay-type">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="replacement-rate">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="excess-dogs">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
