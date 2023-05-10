;; The Employment Game as an Agent-Based Model
;;
;;  Nigel Gilbert and Martha Bickey
;;  16 April 2022
;;
;; a proof of concept for the AI FORA project employment game
;;
;; version 1.0 - initial
;; version 1.2 - add voluntary solidaity fund, differing rates for benefits, diability benefits, unemployment benefit
;; Contact: n.gilbert@surrey.ac.uk
;;  Open source under the MIT licence
;;

globals [
  good-vacancies ;; current number of good jobs unfilled
  bad-vacancies  ;; current number of bad jobs unfilled
  voluntary-fund  ;; amount of money in the voluntary solidarity fund
]

breed [agnts agnt] ;; the players  (NB 'agent' is a NetLogo reserved word :-( )
breed [stations station] ;; the six stations (implemented as NetLogo turtles, so that they can be displayed as shapes)

agnts-own [

  ;; dynamic
  wealth ;; stock of money units
  happiness ;; accumulated happiness
  training ;; training level in months of training achieved
  employment-status ;; whether employed and if so, in what kind of job
  age
  network ;; extent of social network
  months-worked ;; number of months agent has been in work

  ;; fixed
  gender
  household
  ethnicity
  social-skills
  vulnerability
  home-area

  ;;
  location  ;; the station where this agent is located
  previous-location ;; location in previous round
]

;;--------------------------------------
;;  Initialise
;;--------------------------------------
to setup
  clear-all

  ;; create stations: home, job agency, workplace, training, resort, town hall and give each a shape
  let station-data [["home" "house"] [ "job centre" "target"] ["work" "factory"] ["training centre" "house ranch"]["resort" "campsite"] ["town hall""building institution" ]]

  ;; divide the grid into segments and place one station in each
  ;; this will accommodate changing the size of the world
  let segment-width world-width / (length station-data + 1)
  let x segment-width
  foreach station-data [ s ->
    create-stations 1 [
      setxy x 2
      set x x + segment-width
      set shape last s
      set size 3
      set label first s
    ]
  ]

  ;; create agents and give them their initial attributes.  All start at home
  set-default-shape agnts "person"
  ask one-of stations with [label = "home"] [
    hatch-agnts number-of-agnts [
      set wealth norm 10
      set training norm 5
      set happiness norm 10
      set employment-status "unemployed"
      set social-skills choose "low" "high"
      set age 20 + norm 40
      set household choose "single" "family"
      set gender choose "male" "female"
      set ethnicity choose "white" "minority"
      set network 0
      set vulnerability choose "vulnerable" "not vulnerable"
      set home-area choose "poor" "rich"
      set label ""
      set location myself
      set previous-location nobody
      set size 1
      set color pink  ;; unemployed
      set heading 0
      ;; pile them up above the station
      fd 2 + count agnts with [location = one-of stations with [label = "home"]]
    ]
  ]

  ;; set number of initial number of vacancies available to the job agency
  set good-vacancies good-jobs
  set bad-vacancies bad-jobs
  ;; clear the voluntary fund
  set voluntary-fund 0

  reset-ticks
end

;; report a value drawn from a normal distribution with mean and with s.d. half the mean, but always greater than or equal to zero
to-report norm [ the-mean ]
  let val random-normal the-mean the-mean / 2
  report  ifelse-value (val < 0) [0] [val]
end

;; report either val1 or val2 with equal probability
to-report choose [ val1 val2 ]
  report ifelse-value (random 2 = 0) [val1][val2]
end

;;--------------------------------------
;; Run
;;--------------------------------------
to go
  ;; stop when preset number of rounds has been reached
  if ticks > number-of-rounds [ stop ]

  ;; add 1 month to every agent's age (age may make a difference to job agency's assessment)
  ask agnts [ set age age + 1 ]

  ;; pay the monthly rent (proxy for all living expenses)

  ;; if unemployed, deduct one happiness unit
  ask agnts with [ employment-status = "unemployed" ] [set happiness happiness - 1 ]

  ;; carry out agent actions in their current stations
  ask agnts [
    (ifelse at "home" [ be-at-home ]
      at "job centre" [ be-at-agency ]
      at "work" [ be-at-work ]
      at "training centre" [ be-at-training ]
      at "resort" [ be-at-resort ]
      at "town hall" [ be-at-town-hall ])
  ]

  ;; ask each agent to contribute to the voluntary fund
  ;;  and choose a new station to go to
  ask agnts [
    ;; contribute a percentage of current wealth to the voluntary fund
    set voluntary-fund voluntary-fund + voluntary-fund-rate * wealth / 100
    choose-next-station
  ]

  ;; if the voluntary fund has enough money in it, add a good job (or a bad job if a good one cannot be afforded)
  (ifelse voluntary-fund > vol-fund-good-job-cost [
    set voluntary-fund voluntary-fund - vol-fund-good-job-cost
    set good-vacancies good-vacancies + 1
    ]
    voluntary-fund > vol-fund-bad-job-cost [
      set voluntary-fund voluntary-fund - vol-fund-bad-job-cost
      set bad-vacancies bad-vacancies + 1
  ])


  ;; show agents in a column above their station and in a colour according to their employment status
  ask agnts [
    setxy ([ xcor ] of location) (4 + count agnts-on location)
    set color (ifelse-value employment-status = "unemployed" [ pink ] employment-status = "badJob" [ blue ] [ white ])
  ]

  tick
end

;; report whether agent is at this location (true/false)
to-report at [ location-label ]
  report [label] of location = location-label
end

;;--------------------------------------
;; station actions
;;--------------------------------------

to be-at-home
  if wealth <= 0 [
    ifelse vulnerability = "vulnerable" [ receive-disability-benefit ] [receive-welfare-benefit ]
  ]
end

to be-at-agency
  ;; provide a good job or a bad job depending on assessment criteria, and whether there
  ;; are any jobs of that type available.  Otherwise leave as unemployed.
  (ifelse good-vacancies > 0 and agency-assessment = "goodJob" [
    set good-vacancies good-vacancies - 1
    set employment-status "goodJob"
    ]
    bad-vacancies > 0 and agency-assessment != "unemployed" [
      set bad-vacancies bad-vacancies - 1
      set employment-status "badJob"
    ]
    ;; no jobs available - leave as unemployed, but pay benefits
    [set employment-status "unemployed"
      ifelse vulnerability = "vulnerable" [ receive-disability-benefit ]
      [
        ifelse months-worked > 6 [ receive-unemployment-benefit ] [receive-welfare-benefit ]
      ]
  ])
end

to be-at-work
  ;; assumes that no one who is unemployed is at work
  ifelse employment-status = "goodJob"
  ;; those with good jobs get a good wage
  [set wealth wealth + good-job-wage]
  ;; those with bad jobs get a lower wage and lose a little happiness
  [set wealth wealth + bad-job-wage
    set happiness happiness - bad-job-work-stress ]
  set network network + 1
  set months-worked months-worked + 1

  ;; fire a proportion
  if random 100 < redundancy-rate [ fire ]
end

to fire
  ifelse employment-status = "goodJob" [set good-vacancies good-vacancies + 1] [ set bad-vacancies bad-vacancies + 1 ]
  set employment-status "unemployed"
end

to be-at-training
  set training training + 1
end

to be-at-resort
  set happiness happiness + holiday-delight
  set wealth wealth - holiday-cost
end

to be-at-town-hall
  set network network + 1
  ;; set criteria for assessment
end

to pay-rent
  set wealth wealth - rent
end

to receive-unemployment-benefit
  set wealth wealth + unemployment-benefit
end

to receive-welfare-benefit
  set wealth wealth + welfare-benefit
end

to receive-disability-benefit
  set wealth wealth + disability-benefit
end


;;--------------------------------------
;; choose where to go next
;;--------------------------------------

;; select which station to go to next
to choose-next-station

  ;; remember where agent is now
  set previous-location location

  let chosen false
  while [not chosen] [

    ;; select a station
    set location selected-station

    ;; check that the selection is legal (conforms to constraints) - if not, try again
    if at "home"  [ set chosen true ]
    if at "job centre" and employment-status = "unemployed" [ set chosen true]
    if at "work" and employment-status != "unemployed" [ set chosen true ]
    if at "training centre"  and training < 10 [ set chosen true ]
    if at "resort" and wealth >= holiday-cost [ set chosen true ]
    if at "town hall" and previous-location != location [ set chosen true ]
  ]

  ;; move to station
  setxy [xcor] of location [ycor] of location

end

;; report a desired next station
;; modify to set a station selection strategy
to-report selected-station
  (ifelse station-selection = "random" [
    report one-of stations
    ]
    station-selection = "job priority" [
      ifelse employment-status = "unemployed" [report the-station "job centre" ] [report the-station "work" ]
    ]
    station-selection = "happiness priority" [
      ifelse wealth >= holiday-cost [ report the-station "resort" ] [ report one-of stations ]
  ])
end

;; find the station with the label and report it
to-report the-station [ location-label ]
  report one-of stations with [label = location-label ]
end

;;--------------------------------------
;; Job agency assessment procedure
;;--------------------------------------
;; report whether this applicant is to be provided with a good job, a bad job, or no job
;; depending on their charactaristics and the assessment criteria in force
to-report agency-assessment
  (ifelse assessment = "random" [
    report one-of [ "unemployed" "goodJob" "badJob" ]
    ]
    assessment = "require social skills for good jobs" [
      ifelse social-skills = "high" [ report "goodJob" ] [ report "badJob"]
    ]
    assessment = "require social skills or training for good jobs" [
      ifelse social-skills = "high" or training >= 10 [ report "goodJob" ] [ report "badJob"]
  ])
end
@#$#@#$#@
GRAPHICS-WINDOW
295
100
826
376
-1
-1
12.76
1
10
1
1
1
0
0
0
1
0
40
0
20
0
0
1
months
30.0

SLIDER
45
130
217
163
number-of-agnts
number-of-agnts
0
20
15.0
1
1
NIL
HORIZONTAL

BUTTON
45
35
111
68
NIL
Setup
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
200
35
263
68
NIL
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
45
185
217
218
rent
rent
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
45
225
215
258
unemployment-benefit
unemployment-benefit
0
10
10.0
1
1
NIL
HORIZONTAL

SLIDER
45
465
217
498
good-jobs
good-jobs
0
20
6.0
1
1
NIL
HORIZONTAL

SLIDER
45
540
217
573
bad-jobs
bad-jobs
0
20
9.0
1
1
NIL
HORIZONTAL

SLIDER
70
500
242
533
good-job-wage
good-job-wage
0
20
6.0
1
1
NIL
HORIZONTAL

SLIDER
70
575
242
608
bad-job-wage
bad-job-wage
0
20
4.0
1
1
NIL
HORIZONTAL

PLOT
860
15
1060
165
Total wealth
NIL
Money
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [wealth] of agnts"

SLIDER
645
420
817
453
holiday-cost
holiday-cost
0
10
5.0
1
1
NIL
HORIZONTAL

BUTTON
115
35
197
68
Go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
860
180
1060
330
Total happiness
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
"default" 1.0 0 -16777216 true "" "plot sum [happiness] of agnts"

PLOT
860
340
1060
490
Total graduates
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
"default" 1.0 0 -16777216 true "" "plot count agnts with [training >= 10]"

SLIDER
40
655
212
688
redundancy-rate
redundancy-rate
0
100
13.0
1
1
%
HORIZONTAL

TEXTBOX
305
20
515
45
The employment game
14
63.0
1

TEXTBOX
485
20
635
38
Version 1.2
14
0.0
1

MONITOR
865
500
985
545
Vacant good jobs
good-vacancies
0
1
11

MONITOR
865
550
985
595
Vacant bad jobs
bad-vacancies
0
1
11

SLIDER
645
455
817
488
holiday-delight
holiday-delight
0
20
4.0
1
1
NIL
HORIZONTAL

SLIDER
70
610
247
643
bad-job-work-stress
bad-job-work-stress
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
45
90
217
123
number-of-rounds
number-of-rounds
0
500
100.0
1
1
NIL
HORIZONTAL

TEXTBOX
705
35
855
91
Current or last job:\n Pink: unemployed\n Blue: bad job\n White: good job
11
0.0
1

CHOOSER
300
395
462
440
station-selection
station-selection
"random" "job priority" "happiness priority"
0

CHOOSER
300
450
622
495
assessment
assessment
"random" "require social skills for good jobs" "require social skills or training for good jobs"
0

SLIDER
45
340
220
373
voluntary-fund-rate
voluntary-fund-rate
0
10
2.0
1
1
%
HORIZONTAL

MONITOR
865
605
1020
650
Amount in voluntary fund
voluntary-fund
0
1
11

SLIDER
45
380
245
413
vol-fund-good-job-cost
vol-fund-good-job-cost
0
100
48.0
1
1
NIL
HORIZONTAL

SLIDER
45
420
245
453
vol-fund-bad-job-cost
vol-fund-bad-job-cost
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
45
260
215
293
welfare-benefit
welfare-benefit
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
45
295
215
328
disability-benefit
disability-benefit
0
10
10.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

### In a nutshell: 
AI FORA empirically analyses value/context dependency in AI-based social assessment of social service provision comparing eleven countries as case studies, identifies welfare and technology gaps and develops chances for improving policies for contextualized AI systems that are responsive to value dynamics in societies
 
### Main hypotheses
1. What is perceived as good distribution practice in a national welfare system largely depends on cultural values:  
1. What is considered as “socially fair“  in one country/context, might be considered as highly discriminatory in another. Also, attitudes towards AI use for public service provision may vary between countries/contexts 
 
### Purpose of the game
The game is to help to test this hypothesis. It generates data, complementing surveys and qualitative research data collected in the case studies.  The game mimics a typical situation of AI-based social assessment in social service provision (job management and unemployment benefits) that occurs in all case study countries.
While the case studies and empirical data generated are highly heterogeneous, the game can be played in all case study countries as an opportunity to create comparable datasets.

## HOW IT WORKS

People play the game in case study workshops. A typical game will last for half a day and involve up to 20 players.

The game should be calibrated to correspond to the local cultural situation.  For example, the names of the stations and titles of the available jobs should be in the local language. 

### Agents
The characters in the game are individuals who seek good careers, measured by their wealth (their money units) and their quality of life (happiness units).  The agents’ aim is to maximise the sum of their wealth and happiness.

#### Agent attributes

Attributes that change as the game progresses:

* Employment status: unemployed, good job, bad job
* Age: increases by 1 month in each round. 
* Wealth: in money units, obtained from salary, differing according to type of job
* Happiness: obtained from being at the holiday resort
* Training: increases when agent attends school
* Extent of social network: expands for each round spent in work or in town hall

Attributes that are fixed from the start of the game: 

* Gender (male/female)
* Household composition(i.e. Whether single or a parent)
* Ethnicity (depends on locality; for Europe - white/ethnic minority)
* Interpersonal (‘social’) skills (e.g. leadership, empathy, communication): high/low
* Vulnerability (e.g. disability) (vulnerable/not vulnerable)
* Place of residence: (poor area/rich area)

#### Initial values of attributes


All drawn randomly from a normal distribution with mean as below, and standard deviation of 0.5 * mean, but with no negative values, or are boolean, with equal probability of true or false, except where noted.

* Wealth: 10 units
* Training: 5 units
* Happiness: 10 units
* Employment status: all are initially unemployed
* Household composition:  either single or family, with equal probability
* Gender: either male or female, with equal probability
* Ethnicity:  either white or ethnic minority, with equal probability
* Extent of social network: zero
* Interpersonal (‘social’) skills (e.g. leadership, empathy, communication): high/low
* Vulnerability (e.g. disability) (vulnerable/not vulnerable)
* Place of residence: (poor area/rich area)

### Resources
Quantities and features that are available in the environment for agents to obtain or use:

* Money (measured in money units) - unlimited amount available
* Education (measured as months of training)  - unlimited amount available
* Happiness (measured in happiness units)  - unlimited amount available
* Jobs (divided into bad jobs and good jobs) - totals of occupied jobs and vacancies for each type (good and bad) is fixed at the start, but may be increased using the Voluntary Solidarity Fund.

#### Stations
There are ‘stations’ (i.e. places) that agents visit:

Home: Agents who are unemployed, and not on holiday, at school, seeking a job at the job agency, or socialising at the town hall, stay at home. Summary information about the environment (e.g. total wealth, happiness education) is available here for agents to see if they wish. It costs one money unit to stay at home.  If they have no money, they receive money in welfare benefits, but are charged for living expenses. 

Job agency:  The agency has a stock of good and a stock of bad vacancies.  As players get jobs, these reduce, but get replenished as workers lose their jobs.  The agency can run out of jobs if its stock becomes exhausted, in which case it cannot provide that kind of job.
The job agency has an assessment procedure, which evaluates agents that arrive at the agency, and provides the agents with a job (good or bad) or provides no job, depending on the assessment  criteria in use.  The criteria used can be changed by regulations successfully voted on at the Town Hall (e.g. to use an AI based assessment or not; to weight some individual attributes over others).
If the agent doesn’t get a job at the agency, they do get unemployment benefits (if they have worked for some months previously) or welfare benefits.

Workplace: The workplace is only accessible to agents who are employed.  The agents earn a salary every round.  The amount depends on whether they are employed in a good or bad job. 
At the end of the round a proportion of those in the workplace are fired and become unemployed. 

Training Centre:
Agents receive one month’s training.  More highly trained agents are more likely to get good jobs. The maximum amount of training an agent can receive is 10 months. Education is free.

Holiday resort:
Agents on holiday increase their happiness each round.  However, it costs money per round to be on holiday. If an agent doesn’t have sufficient money units to afford to pay for their holiday, they cannot go on holiday.
 
Town hall: provides assessment criteria and selection algorithm for the job agency. Staying at the town hall is free of charge.

### Environment
#### Spatial environment: 
Fixed spatial geography in which the stations are placed (this remains the same in all localizations).

#### Temporal environment:
The game proceeds by ‘rounds’.  Each round corresponds to one month of the agents’ lives.  A round is completed when every agent has had a turn. Agents take turns in arbitrary, but fixed, order. The game proceeds for a specified number of rounds (there is no other termination condition).

#### Voluntary solidarity fund 
Represents the work of charities/NGOs.
Agents can contribute as much money as they like (provided that they have that much money) after every round.
The money is used to increase the number of jobs in the game.  A good job ‘costs’ some amount of money and a bad job a lesser amount. If there is not enough money to fund another job, the excess remains in the fund until more is contributed.

#### Environmental (global) attributes

* Round number, starting with zero
* Number of good and bad job vacancies at start of each round
* Amount in the voluntary support fund


### Job agency assessment:
Currently implemented:
Random: Agent is given a good job, a bad job, or left unemployed with equal probability
Require social skills for good jobs: As random, but agents with high social skills are always given good jobs (provided that there are good job vacancies)
Require social skills or degree for good jobs: As above, but good jobs also go to those with degrees

If good jobs run out, award bad jobs to those who are eligible for good jobs, but if the bad jobs run out, don’t award any job.

All other individual attributes can be used in regulations with different criteria.
However, an AI based assessment cannot use Interpersonal skills

## The simulation

At the beginning of each round, each agent in turn decides which station it wishes to visit next.  It then goes there, it carries out activities at the station and its wealth, happiness and education are adjusted.

Each agent grows older by one month.
Agents have to pay rent (and living expenses) every month.
If the agent is unemployed, they lose happiness units.
Agents may receive welfare benefits if they have no money and are at home or are in the job agency and have no recent work experience; unemployment benefit in the job agency if they are unemployed and have recently had a job; or disability benefit if they are at home or unemployed in the job agency and are vulnerable.  Disability benefit is worth more than unemployment benefit which is worth more than welfare benefit.

Agents do not interact directly except at the town hall. There they can deliberate about what they consider to be the best criteria to use to allocate jobs, and may propose regulations.  In order to have effect, a regulation must be approved by a majority of all agents.  If a new regulation is proposed, a vote about whether it should be passed (i.e. implemented) is taken at the end of the round.  If passed, the new regulation is implemented for the next round.

Agents can only go to the:

* Job agency if unemployed
* Training Centre if they have had less than 10 units of training
* Workplace if employed
* Holiday resort if wealth > holiday cost
* Town Hall if not been there the previous round
* There are no conditions for staying at home.

Agents can use a ‘strategy’ to choose which station to go to next.  For example, they might decide to focus on earning as much money as possible (by always selecting the workplace as their next station, or the job agency if they become unemployed), or they might focus on increasing their happiness (by always going on holiday if they can afford it).  

 
All agents start at the beginning of the game and continue in the game until the game ends (the stopping point is determined by the facilitator - the ‘winner’ is then the agent/player with the highest total of wealth and happiness).



## HOW TO USE IT

Press the Setup button

Adjust the sliders as desired

Press the Go button

Observe the outcome


## CREDITS AND REFERENCES

This model was developed by Nigel Gilbert (n.gilbert@surrey.ac.uk), with the assistance of Petra Ahrweiler, Martha Bicket, and Albert Sabater as a means of checking the coherence of the rules for the AI FORA 'Employment game', which is played by real humans in a workshop setting. See https://www.ai-fora.de/
 
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

building institution
false
0
Rectangle -7500403 true true 0 60 300 270
Rectangle -16777216 true false 130 196 168 256
Rectangle -16777216 false false 0 255 300 270
Polygon -7500403 true true 0 60 150 15 300 60
Polygon -16777216 false false 0 60 150 15 300 60
Circle -1 true false 135 26 30
Circle -16777216 false false 135 25 30
Rectangle -16777216 false false 0 60 300 75
Rectangle -16777216 false false 218 75 255 90
Rectangle -16777216 false false 218 240 255 255
Rectangle -16777216 false false 224 90 249 240
Rectangle -16777216 false false 45 75 82 90
Rectangle -16777216 false false 45 240 82 255
Rectangle -16777216 false false 51 90 76 240
Rectangle -16777216 false false 90 240 127 255
Rectangle -16777216 false false 90 75 127 90
Rectangle -16777216 false false 96 90 121 240
Rectangle -16777216 false false 179 90 204 240
Rectangle -16777216 false false 173 75 210 90
Rectangle -16777216 false false 173 240 210 255
Rectangle -16777216 false false 269 90 294 240
Rectangle -16777216 false false 263 75 300 90
Rectangle -16777216 false false 263 240 300 255
Rectangle -16777216 false false 0 240 37 255
Rectangle -16777216 false false 6 90 31 240
Rectangle -16777216 false false 0 75 37 90
Line -16777216 false 112 260 184 260
Line -16777216 false 105 265 196 265

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

campsite
false
0
Polygon -7500403 true true 150 11 30 221 270 221
Polygon -16777216 true false 151 90 92 221 212 221
Line -7500403 true 150 30 150 225

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

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

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

house ranch
false
0
Rectangle -7500403 true true 270 120 285 255
Rectangle -7500403 true true 15 180 270 255
Polygon -7500403 true true 0 180 300 180 240 135 60 135 0 180
Rectangle -16777216 true false 120 195 180 255
Line -7500403 true 150 195 150 255
Rectangle -16777216 true false 45 195 105 240
Rectangle -16777216 true false 195 195 255 240
Line -7500403 true 75 195 75 240
Line -7500403 true 225 195 225 240
Line -16777216 false 270 180 270 255
Line -16777216 false 0 180 300 180

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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
1
@#$#@#$#@
