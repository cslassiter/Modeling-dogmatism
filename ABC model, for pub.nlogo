globals [ believer-list]
turtles-own [friends belief discriminating-value pop-my-friends ]

to setup
  ;; here are the globals that users input in the GUI:
  ;;1. max-discrim-value (input)
  ;;2. max-pop-my-friends (input)
  ;;3. density (slider)
  ;;4. num-dog (input)
  ;;5. new-generations? (switch)
  ;;6. replacement-rate (slider)
  ;;7. fixed-population? (switch)

  ca
  reset-ticks
  ifelse fixed-population? ;; if fixed-population? = TRUE, 100 turtles hatched. Otherwise it's a uniform probability based on the density value.
    [crt 100 [set-up-routine]]
    [ask patches [set pcolor blue + random-float 3 if random 100 < density  [sprout 1] ] ask turtles [set-up-routine]]

  if num-dog != 0 [assign-dogmatists] ;; assign dogmatists, if applicable
  set believer-list [] ;; believer-list is needed for looking at how beliefs are more or less popular over time. Esp useful for fixed pop with dogmatists.
  let n 0
  repeat count turtles
    [set believer-list lput n believer-list
      set n n + 1]

if max-discrim-value > 1 [user-message "can't have a max-discriminating-value equal to or greater than 1.0"] ;; juuuuust making sure these values stay below 1.0
if max-pop-my-friends > 1 [user-message "can't have a max-pop-my-friends equal to or greater than 1.0"]
  if num-dog != 0 and fixed-population? = FALSE [user-message "you sure you want to have (num-dog > 0) and (fixed-population? = FALSE) ? Model runs fine but you'll have to interpret reporter outputs accordingly."]

end

to set-up-routine
  move-to one-of patches
  if any? other turtles-here [set-up-routine]
    set size .75
    set color wrap-color who
    set belief who
    ;;; if you want turtles to be the same, move "random-float" after the semicolon
    set discriminating-value random-float max-discrim-value ;
    set pop-my-friends random-float max-pop-my-friends   ;
end

to assign-dogmatists
  ask n-of num-dog turtles [set discriminating-value 0]
end

to go
  find-friends
  influence
  update-color
  plot-belief
  plot-pop-by-belief
  if pop-with-shared-belief = count turtles [ end-routine stop] ;; this is for converging on a single belief or just letting the model run with num-dog > 1
  if (num-dog != 0) and (beliefs-in-circulation = num-dog) [ end-routine stop] ;; this is for converging on as many beliefs as there are dogmatists
  if new-generations? [ ;; this is if you want to have turtles die and get replaced
    die-and-replace]
  tick
end


;; a smattering of useful reporters
;; reporters are designed to work without replacing turtles, i.e. with "new-generations?" switched off.
;; in the case that a consensus is reached with "new-generations?" switched on and the winning turtle has died, many of the following reporters will throw errors.
;; 'winning turtle' refers to the turtle whose belief is adopted by everyone
;; these all served a purpose at one time or another when exploring the model.
;; i'm keeping them in because (a) i don't remember which plots and reporters depend on what and (b) it doesn't do any harm and leave them in and
;; (c) they still output handy information

to-report count-believers ;; reports list of the proportion of the population endorsing each belief
  let n []
  foreach believer-list [ i -> set n lput (count turtles with [belief = i]) n]
  report (map [j -> j / length n] n)
end

to-report winner-in-list-audience ;; makes a list ordered by pop-my-friends, and reports the position of the winning turtle in that list
  report ((position [pop-my-friends] of turtle final-belief (sort [pop-my-friends] of turtles)) + 1)
end

to-report winner-in-list-PBC ;; like the previous reporter but for PBC (output is a list)
  report ((position [discriminating-value] of turtle final-belief (sort [discriminating-value] of turtles)) + 1)
end

to-report winner-PBC ;; reports the PBC of the winning turtle (output is a float)
  report [discriminating-value] of turtle final-belief
end

to-report winner-audience ;; reports pop-my-friends for winning turtle (output is a float)
  report [pop-my-friends] of turtle final-belief
end

to-report max-discrim-diff ;; reports the difference between highest and lowest PBC (output is a float)
  let l [discriminating-value] of turtles
  report (max l) - (min l)
end

to-report min-discrim-value ;; reports the lowest PBC (output is a float)
  report min [discriminating-value] of turtles
end

to-report discrim-diff ;; reports the difference between the winning PBC and lowest PBC (output is a float)
  report [discriminating-value] of turtle final-belief - min-discrim-value
end

to-report low-discrim-agent ;; reports the identity of agent with lowest PBC (output is an integer)
  report [who] of turtles with [discriminating-value = min-discrim-value]
end

to-report list-of-beliefs ;; reports a list of beliefs that are still endorsed by agents
  report sort remove-duplicates [belief] of turtles
end

to-report final-belief ;; reports the belief that's converged upon (output is an integer)
  report item 0 list-of-beliefs
end

to-report pop-with-shared-belief ;; reports how many turtles endorse the most widely-endorsed belief (output is an integer)
  let b-list []
  set b-list [belief] of turtles
  let m []
  set m modes b-list
  if length m > 1 [set m (list min m)] ;; if there is more than one mode, pick the smallest value
  let t 0
  foreach b-list [ ?1 -> if member? ?1 m [set t t + 1] ]
  report t
end

to-report beliefs-in-circulation ;; reports number of beliefs still in circulation (output is an integer)
  let m remove-duplicates [belief] of turtles
  report length m
end

to-report agent-belief-pair ;; reports turtle identity and the belief it endorses
  report map [i -> (sentence i [belief] of turtles with [who = i])] believer-list
end

;;; main mechanisms for the model
to find-friends
  ask turtles [set friends n-of round (pop-my-friends * count turtles) other turtles]
end

to influence ;; mechanism of belief change. turtle asks friends to adopt belief. friends roll dice to see if that happens
  ask turtles [
    if friends != 0[
    ask friends  [
      let rf random-float 1.0
      if rf < discriminating-value [ set belief [belief] of myself]
    ]
  ]
]
end

to update-color ;; this makes convergence more visible with the turtles
  ask turtles[
    set color belief]
end

to die-and-replace
  ask turtles [
    if (random-float 1.0 < replacement-rate)[
      hatch 1 [
       set-up-routine]
      die]
  ]
end

;;; now to do some plotting
to plot-belief
set-current-plot "hist-plot"
histogram [belief] of turtles
end

to plot-pop-by-belief
set-current-plot "pop-by-belief"
  set-plot-pen-mode 2
  plotxy  (1 - (beliefs-in-circulation / count turtles)) (pop-with-shared-belief / count turtles) ;beliefs-in-circulation
end

to end-routine
  type "difference between winning turtle discriminating value and lowest discriminating value "
    print discrim-diff
    type "difference between highest and lowest discriminating value "
    print max-discrim-diff
    type "ratio of winner to max "
    print (discrim-diff / max-discrim-diff)
    type "winner is # " write winner-in-list-PBC print " in list of discriminating values"
    type "also winner is # " write winner-in-list-audience type " in list of audience size"
end
@#$#@#$#@
GRAPHICS-WINDOW
300
36
594
331
-1
-1
13.62
1
10
1
1
1
0
1
1
1
-10
10
-10
10
0
0
1
ticks
30.0

INPUTBOX
108
13
216
73
max-pop-my-friends
0.25
1
0
Number

BUTTON
659
12
722
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
661
58
724
91
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

SLIDER
5
81
177
114
density
density
0
100
75.0
1
1
NIL
HORIZONTAL

PLOT
744
10
1129
321
%-pop with shared belief
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"if count turtles = 0 [stop]\nif pop-with-shared-belief > (count turtles - 1) [stop]" "if count turtles = 0 [stop]\nif pop-with-shared-belief > (count turtles - 1) [stop]"
PENS
"default" 1.0 0 -16777216 true "" "plot (pop-with-shared-belief / count turtles) * 100"

BUTTON
661
105
724
138
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
0.25
1
0
Number

PLOT
1009
519
1588
817
beliefs-in-circulation
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"if ticks <= 0 [stop]" "if ticks <= 0 [stop]"
PENS
"default" 1.0 0 -16777216 true "" "plot length remove-duplicates [belief] of turtles"

PLOT
135
611
923
811
hist-plot
NIL
NIL
0.0
350.0
0.0
350.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

PLOT
158
349
854
586
pop-by-belief
1 - distinct beliefs / agents
Ratio of shared belief
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

INPUTBOX
5
120
106
180
num-dog
3.0
1
0
Number

SWITCH
8
232
164
265
New-generations?
New-generations?
1
1
-1000

SLIDER
6
267
267
300
replacement-rate
replacement-rate
0
.01
0.01
.001
1
NIL
HORIZONTAL

SWITCH
7
191
158
224
fixed-population?
fixed-population?
1
1
-1000

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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="pop = .05, max-d-val .01-.05" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <enumeratedValueSet variable="pop-my-friends">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newbies">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop = .04, max-d-val .01-.05" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <enumeratedValueSet variable="pop-my-friends">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newbies">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop = .03, max-d-val .01-.05" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <enumeratedValueSet variable="pop-my-friends">
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newbies">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop = .02, max-d-val .01-.05" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <enumeratedValueSet variable="pop-my-friends">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newbies">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop = .01, max-d-val .01-.05, converge on 1" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <enumeratedValueSet variable="pop-my-friends">
      <value value="0.01"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop = 05, max-d-val = 01-05, converge on 1" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-my-friends">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop = 04, max-d-val = 01-05, converge on 1" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-my-friends">
      <value value="0.04"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ABC, pop = 01" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>min-discrim-val</metric>
    <metric>low-discrim-agent</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discrim-value" first="0.01" step="0.01" last="0.1"/>
    <enumeratedValueSet variable="max-pop-my-friends">
      <value value="0.01"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop = 03, max-d-val = 01-05, converge on 1" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-my-friends">
      <value value="0.03"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop = 02, max-d-val = 01-05, converge on 1" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop-my-friends">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop = .06 to .1, max-d-val .06-.1, converge on 1" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discriminating-value" first="0.06" step="0.01" last="0.1"/>
    <steppedValueSet variable="pop-my-friends" first="0.06" step="0.01" last="0.1"/>
  </experiment>
  <experiment name="pop = .06 to .1, max-d-val .01-.05, converge on 1" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.05"/>
    <steppedValueSet variable="pop-my-friends" first="0.06" step="0.01" last="0.1"/>
  </experiment>
  <experiment name="pop = .01 to .1, max-d-val .01-.1, converge on 1" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discriminating-value" first="0.01" step="0.01" last="0.1"/>
    <steppedValueSet variable="pop-my-friends" first="0.01" step="0.01" last="0.1"/>
  </experiment>
  <experiment name="ABC, pop 08" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>min-discrim-val</metric>
    <metric>low-discrim-agent</metric>
    <metric>final-belief</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="max-discrim-value" first="0.01" step="0.01" last="0.1"/>
    <enumeratedValueSet variable="max-pop-my-friends">
      <value value="0.8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ABC, diverse, winner list" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>winner-in-list-PBC</metric>
    <metric>winner-in-list-audience</metric>
    <metric>winner-PBC</metric>
    <metric>winner-audience</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.1"/>
      <value value="0.33"/>
      <value value="0.66"/>
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pop-my-friends">
      <value value="0.1"/>
      <value value="0.33"/>
      <value value="0.66"/>
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="avg time to num of dogm beliefs" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <enumeratedValueSet variable="density">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pop-my-friends">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="evolution w N dogm" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2500"/>
    <metric>count-believers</metric>
    <metric>ticks</metric>
    <metric>agent-belief-pair</metric>
    <enumeratedValueSet variable="max-discrim-value">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-dog">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pop-my-friends">
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
