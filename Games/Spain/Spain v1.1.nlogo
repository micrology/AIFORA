;;  An Agent-Based Model for the Spanish Case Study
;;
;;  Martha Bicket and Nigel Gilbert
;;  13 April 2023
;;
;; version 1.1 - Draft for demo with Albert, Petra, Elisabeth, David and Nigel on 13 April 2023 (code and interface have been updated from the unemployment game; information under the info tab still needs to be updated)
;; Contact: m.bicket@surrey.ac.uk / n.gilbert@surrey.ac.uk
;; Open source under the MIT licence
;;

globals [
  ;; available-appointments ;; number of applicants that agents can see each round
  ;; social-service-budget ;; At the end of the round the social service budget is distributed to successful applicants in order of severity (highest scoring applicant gets full payment etc. then second highest applicant etc. until budget for that round is used up)
  round-budget
  ;; number-of-applicants
  ;; threshold ;; critical threshold for neediness (currently 6*3 = minimally self-sufficient across all categories) If applicants' need scores are above this threshold then the next round's budget suffers a penalty reduction
  ;; penalty
]

breed [applicants applicant] ;; the pool of applicants that will meet with and be assessed by agents each round
breed [stations station] ;; the stations (implemented as NetLogo turtles, so that they can be displayed as shapes)

applicants-own [
  ;; dynamic
  seen ;; seen by an agent that round
  unallocated ;; seen by an agent but need score wasn't high enough to receive budget that round
  need-score ;;	Overall need score (highest scoring individual is deemed by the algorithm to be the most in need)
  household-income
  household-income-score ;; for calculating the need score in each round
  dependants-score ;; for calculating the need score in each round
  accommodation
  work-and-training
  mental-health
  physical-health
  location  ;; the station where this applicant is located

  ;; fixed
  dependants; number of dependants - this model assumes that this doesn't change over time
]

stations-own [
  applicant-counter
]


;;--------------------------------------
;;  Initialise
;;--------------------------------------
to setup
  clear-all

  ;; Q: do we want the ABM interface to visualise the agents at each of the stations? or the applicants at each of the agents' desks?
  ;; I think it is more interesting to show the applicants at each agent's desk, and show who doesn't get seen each round.

  ;; So, divide the grid into segments and place one agent at each place, with one additional place for home
  ;; this will accommodate changing the size of the world

  let agntno (range 1 ( number-of-agnts + 1 ) )
  let icons n-values number-of-agnts ["computer workstation"]
  let bases ( map [ [ a b ] -> ( list a b ) ] agntno icons )
  let frst ["home" "house"]
  let bases2 fput frst bases

  let segment-width world-width / (length bases2 + 1)
  let x segment-width
  foreach bases2 [ s ->
    create-stations 1 [
      setxy x 2
      set x x + segment-width
      set shape last s
      set size 3
      set label first s
    ]
  ]

  set round-budget social-service-budget

  ;; create applicants and give them their initial attributes.  All start at home
  set-default-shape applicants "person"
  ask one-of stations with [label = "home"] [
    hatch-applicants number-of-applicants [
      set household-income norm 50
      set dependants random 5
      set accommodation 1 + random 5
      set work-and-training 1 + random 5
      set mental-health 1 + random 5
      set physical-health 1 + random 5

      set label ""
      set location myself
      set size 1
      set color pink  ;; unseen
      set heading 0
      ;; pile them up above the station
      fd 2 + count applicants with [location = one-of stations with [label = "home"]]
    ]
  ]


  reset-ticks
end

;; report a value drawn from a normal distribution with mean and with s.d. half the mean, but always greater than or equal to zero
to-report norm [ the-mean ]
  let val random-normal the-mean the-mean / 2
  report  ifelse-value (val < 0) [0] [val]
end


;;--------------------------------------
;; Run
;;--------------------------------------
to go
  ;; stop when preset number of rounds has been reached or if there is no budget for this round
  if ticks > number-of-rounds [ stop ]
  if round-budget <= 0 [ stop ]

  ;; reset applicant-counter for each station back to 0 for the new round
  ask stations [
    set applicant-counter 0
  ]

  ;; reset round-based applicant attributes and allocate applicants to agents
  ;; *** STILL TO DO: DISTRIBUTE APPLICANTS TO AGENTS EVENLY
  ask applicants [
    set seen 0
    set unallocated 0
    set location one-of stations with [label != "home"]
    setxy [xcor] of location [ycor] of location
  ]

  ;; let applicants get seen in random order by the agents until all appointments have been filled
  ask applicants with [location != "home"] [
    if [applicant-counter] of location < available-appointments [
    set seen 1
      ask stations with [[location] of myself = self][
      set applicant-counter applicant-counter + 1
      ]
    ]
  ]

  ;; calculate the need-score for each applicant according to the current scoring algorithm:
  ;; - for all attributes except household income and number of dependants: 1 ‘need point’ according to the position on the applicant situation scale (5 = 'serious problems' to 1 = 'completely self-sufficient')
  ;; - for household income and number of dependants, applicants are ranked against each other and given between 1-5 need points based on their position relative to other applicants that round.

  ; reverse sort (so a lower income leads to a higher need score)
  let sorted-by-income sort-on [(- household-income)] applicants

  ; here a higher number of dependants leads to higher need score
  let sorted-by-dependants sort-on [dependants] applicants

  ask applicants [
    set household-income-score (( position self sorted-by-income / number-of-applicants ) * 4) + 1
    set dependants-score (( position self sorted-by-dependants / number-of-applicants ) * 4) + 1
    set need-score accommodation + work-and-training + mental-health + physical-health + household-income-score + dependants-score
  ]

  let sorted-by-score sort-on [(- need-score)] applicants
  foreach sort-on [(- need-score)] applicants [
    the-applicant -> ask the-applicant [
    ;; if they've been seen by an agent in this round (and if there's enough budget remaining), then in descending order from highest need-score: 'allocate' budget by giving them 1 unit of money for every need point until that round's budget has been used up
      ifelse seen = 1 and (round-budget - need-score) >= 0 [
        ;; *** COME BACK TO IMPROVE THIS *** currently we assume that being allocated social service budget improves one of their needs-categories at random. An improvement could be to change this so we assume that receiving budget alleviates their worst attribute, e.g. by 1.
        let k random 5
        (ifelse k = 0 [set accommodation max (list 0 (accommodation - 1))]
          k = 1 [set work-and-training max (list 0 (work-and-training - 1))]
          k = 2 [set mental-health max (list 0 (mental-health - 1))]
          k = 3 [set physical-health max (list 0 (physical-health - 1))]
          k = 4 [set household-income household-income + need-score])
        set round-budget round-budget - need-score
      ]
      [set unallocated 1]
    ]
  ]

  ;; *** COME BACK TO IMPROVE THIS *** NEED TO SET min (list 5 (accommodation + 1)) etc
  ;; *** CONSIDER CHANGING IT TO: If an applicant hasn't received any budget in this round, and if they had any categories where they had a score greater than or equal to 4 (not self-sufficient) then those and one other random category gets worse.
  ;; if applicants haven't been seen or didn't receive any budget, then worsen each non-sufficient attribute (or if already at the maximum 5, randomly increase one of the other attributes)
ask applicants[
    let random-attribute random 3
    let score-list (list accommodation work-and-training mental-health physical-health)
    if seen = 0 or unallocated = 1 [
      (ifelse
        accommodation >= 4 [set accommodation accommodation + 1]
        work-and-training >= 4 [set work-and-training work-and-training + 1]
        mental-health >= 4 [set mental-health mental-health + 1]
        physical-health >= 4 [set physical-health physical-health + 1]
        household-income-score >= 4 [set household-income max (list 0 (household-income - 10))]
  ;;      dependants-score >= 4 [set (item random-attribute score-list) min (list 5 (item random-attribute score-list + 1) )]

        ;; if any of the attributes are >5 then reset to 5 and choose one random category to get worse
       ;; for the following attributes, would also want them to possibly decrease household-income with equal (1/5) probability
  ;;      accommodation > 5 [set accommodation 5 set item random-attribute score-list min (list 5 (item random-attribute score-list + 1) )]
  ;;      work-and-training > 5 [set work-and-training 5 set item random-attribute score-list min (list 5 (item random-attribute score-list + 1) )]
  ;;      mental-health > 5 [set mental-health 5 set item random-attribute score-list min (list 5 (item random-attribute score-list + 1) )]
  ;;      physical-health > 5 [set physical-health 5 set item random-attribute score-list min (list 5 (item random-attribute score-list + 1) )]
      )
    ]
    set need-score accommodation + work-and-training + mental-health + physical-health + household-income-score + dependants-score
  ]

  ;; set the next round's budget
  set round-budget social-service-budget
  ;; if applicants' need scores are above the threshold then there's a reduction in the next round's available budget
  ask applicants [
    if need-score > threshold [
      set round-budget max (list 0 (round-budget - penalty))
    ]
  ]

  ;; random change in fortunes
  ask applicants[
    let m random 50
    (ifelse
      ;; some applicants' situations get worse
      m = 0 [set accommodation min (list 5 (accommodation + 1))]
      m = 1 [set work-and-training min (list 5 (work-and-training + 1))]
      m = 2 [set mental-health min (list 5 (mental-health + 1))]
      m = 3 [set physical-health min (list 5 (physical-health + 1))]
      m = 4 [set household-income max (list 0 (household-income - need-score))]
      ;; some applicants' situations improve
      m = 5 [set accommodation max (list 0 (accommodation - 1))]
      m = 6 [set work-and-training max (list 0 (work-and-training - 1))]
      m = 7 [set mental-health max (list 0 (mental-health - 1))]
      m = 8 [set physical-health max (list 0 (physical-health - 1))]
      m = 9 [set household-income household-income + need-score])
    ]

  ;; show applicants in a column above their agent and in a colour according to their seen / budget-allocated status
  ask applicants [
    setxy ([ xcor ] of location) (4 + count applicants-on location)
    set color (ifelse-value unallocated = 0 [ green ] need-score > threshold [ red ] seen = 1 [ blue ] [ pink ])
  ]

  tick
end
@#$#@#$#@
GRAPHICS-WINDOW
257
100
835
539
-1
-1
13.90244
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
30
0
0
1
months
30.0

SLIDER
40
205
240
238
number-of-agnts
number-of-agnts
0
10
5.0
1
1
NIL
HORIZONTAL

BUTTON
40
35
106
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
195
35
258
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
40
250
240
283
number-of-applicants
number-of-applicants
0
50
20.0
1
1
NIL
HORIZONTAL

SLIDER
40
345
240
378
available-appointments
available-appointments
0
10
4.0
1
1
NIL
HORIZONTAL

PLOT
855
100
1055
250
Total need score
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
"default" 1.0 0 -16777216 true "" "plot sum [need-score] of applicants"

BUTTON
110
35
192
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
855
265
1055
415
Applicants with critical needs
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
"default" 1.0 0 -16777216 true "" "plot count applicants with [need-score >= threshold]"

TEXTBOX
305
35
560
66
The Spanish complex needs game
14
63.0
1

TEXTBOX
305
60
455
78
Version 0.9
14
0.0
1

SLIDER
40
120
240
153
number-of-rounds
number-of-rounds
0
100
100.0
1
1
NIL
HORIZONTAL

TEXTBOX
625
35
855
101
 Pink: not seen by an agent this round\n Blue: seen by an agent\n Green: allocated funds\n Red: critical needs
11
0.0
1

SLIDER
40
480
240
513
penalty
penalty
0
20
7.0
1
1
NIL
HORIZONTAL

SLIDER
40
390
240
423
social-service-budget
social-service-budget
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
40
435
240
468
threshold
threshold
0
30
23.0
1
1
NIL
HORIZONTAL

MONITOR
855
445
1055
490
Upcoming round's budget
round-budget
0
1
11

PLOT
1075
100
1275
250
Budget over time
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
"default" 1.0 0 -16777216 true "" "plot round-budget"

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

computer workstation
false
0
Rectangle -7500403 true true 60 45 240 180
Polygon -7500403 true true 90 180 105 195 135 195 135 210 165 210 165 195 195 195 210 180
Rectangle -16777216 true false 75 60 225 165
Rectangle -7500403 true true 45 210 255 255
Rectangle -10899396 true false 249 223 237 217
Line -16777216 false 60 225 120 225

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

orbit 6
true
0
Circle -7500403 true true 116 11 67
Circle -7500403 true true 26 176 67
Circle -7500403 true true 206 176 67
Circle -7500403 false true 45 45 210
Circle -7500403 true true 26 58 67
Circle -7500403 true true 206 58 67
Circle -7500403 true true 116 221 67

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
