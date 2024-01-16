; A comparison of the allocation of social benefits in Germany and Spain - An agent-based model


; Lars Helmstädter
; 30 November 2023


; This model is adapted from the work of Prof. Dr. Nigel Gilbert's and Dr. Martha Bicket's "An Agent-Based Model for the Spanish Case Study" (Version 0.9) - https://github.com/micrology/AIFORA/blob/main/Games/Spain/Spain%20v0.9.nlogo


; ########## Contents ##########

; 1. Globals

; 2. Creating the Agents
;   2.1 Properties
;   2.2 Property Assignment

; 3. Switches

; 4. Buttons & Factors

; 5. Creation & Distribution
;  5.1 Creation
;  5.2 Distribution

; 6. Appointments

; 7. Funds

; 8. Social Needs

; 9. Reset

; 10. Setup

; 11. Go



;########## 1. Globals ##########

globals [
  ; Applicants_German (slider) - number of German applicants.
  ; Applicants_Spanish (slider) - number of Spanish applicants

  ; Worker_German (slider) - number of German clerks
  ; Worker_Spanish (slider) - number of Spanish clerks

  ; Budgetslider - Initial budget
  ; Roundbudget (slider) - increase of the budget each round
  ; Roundbudget_calculation (switch) - If on: Budget increases every round by round budget, if off: Budget goes back to original value after each round
  Budget_Spain; Amount of the total available budget spain
  Budget_Germany; Amount of the total available budget germany

  ; Appointment_coupling (switch) - If on: Set of appointments is coupled to each other (e.g. German always 3 appointments more), if off: Appointments can be set freely
  ; Applicant_linkage (switch) - If on: Number of requesters is linked and always the same.

  ; Penalty (slider) - Sets the penalty thats subtracted from the budget for applicants who receive funds with a Total_Score of > Threshold

  ; Threshold (slider) - Specifies the limit from which grants are awarded

  ; Applicant Insertion (button) - Adds applicants in the amount of the slider of the given country
  ; Funds_Spain (slider) - Determines how many Spanish applicants get funding
  ; Worker_Funds (switch) - If on: Spanish applicants with highest Total Score at the position of workers get funding; if off: Spanish applicants with generally highest Total Score get funding

  ; Rounds (slider) - Represents the maximum number of rounds before the simulation ends
]



;########## 2. 2. Creating the Agents ##########
; Create the agents of the model

breed [Applicant_Gs Applicant_G]
breed [Applicant_Ss Applicant_S]
breed [Workers Worker]
breed [End_of_Simulations End_of_Simulation]



; ########## 2.1 Properties ##########
; Create the different properties the applicants of the two countries as well as the workers own
; Properties are based on the work of Gilbert and Bicket


Workers-own [
  Location
  open_Appointment
  Assigned_Appointments
  Applicants
]

Applicant_Gs-own [
  Location
  appointment
  Funds
  Total_Score
  Needsscorerank
  Condition
  Income
  Incomescore
  Accommodation
  Work
  Mental_Health
  Physical_Health
  Dependants
  Dependantsscore
]

Applicant_Ss-own [
  Location
  appointment
  Funds
  Total_Score
  Needsscorerank
  Needsscorerank_Appointment
  Condition
  Income
  Incomescore
  Accommodation
  Work
  Mental_Health
  Physical_Health
  Dependants
  Dependantsscore
]



;########## 2.2 Property Assignment ##########
; Assign and calculate the values of the properties of the applicants and workers


to Worker_Properties
  ask Workers with [label = "German"][
    set open_Appointment German_Appointments
  ]
  ask Workers with [label = "Spanish"][
    set open_Appointment Spanish_Appointments
  ]
end

to Applicant_Properties
  ask Applicant_Gs [
    set Income norm 50
    set Accommodation 1 + random 5
    set Work 1 + random 5
    set Mental_Health 1 + random 5
    set Physical_Health 1 + random 5
    set Dependants random 5
  ]
  ask Applicant_Gs [
    let Income_Sorting sort-on [(- Income)] Applicant_Gs
    let Dependency_Sorting sort-on [Dependants] Applicant_Gs
    set Incomescore ((position self Income_Sorting / count Applicant_Gs) * 4) + 1
    set Dependantsscore ((position self Dependency_Sorting / count Applicant_Gs) * 4) + 1
    set Total_Score Incomescore + Accommodation + Work + Mental_Health + Physical_Health + Dependantsscore
  ]

  ask Applicant_Ss [
    set Income norm 50
    set Accommodation 1 + random 5
    set Work 1 + random 5
    set Mental_Health 1 + random 5
    set Physical_Health 1 + random 5
    set Dependants random 5
  ]
  ask Applicant_Ss [
    let Income_Sorting sort-on [(- Income)] Applicant_Ss
    let Dependency_Sorting sort-on [Dependants] Applicant_Ss
    set Incomescore ((position self Income_Sorting / count Applicant_Ss) * 4) + 1
    set Dependantsscore ((position self Dependency_Sorting / count Applicant_Ss) * 4) + 1
    set Total_Score Incomescore + Accommodation + Work + Mental_Health + Physical_Health + Dependantsscore
  ]
end

to needsranking
  ask Applicant_Gs [
    let Needsscore_Sorting sort-on [(- Total_Score)] Applicant_Gs
    set needsscorerank (position self Needsscore_Sorting + 1)
  ]
  ask Applicant_Ss [
    let Needsscore_Sorting sort-on [(- Total_Score)] Applicant_Ss
    set needsscorerank (position self Needsscore_Sorting + 1)
  ]
end

; Code cited from "An Agent-Based Model for the Spanish Case Study" (Gilbert & Bicket 2023)
to-report norm [ the-mean ]
  let val random-normal the-mean the-mean / 2
  report  ifelse-value (val < 0) [0] [val]
end


;########## 3. Switches ##########
; Create several switches


to toggle-Roundbudget_Calculation
  set Roundbudget_Calculation not Roundbudget_Calculation
end

to Roundbudget_Switch
  if not Roundbudget_Calculation [
    if ticks mod 1 = 0 [
      set Budget_Spain Budgetslider
      set Budget_Germany Budgetslider
    ]
  ]
  if Roundbudget_Calculation [
    if ticks mod 1 = 0 [
      set Budget_Spain Budget_Spain + Roundbudget
      set Budget_Germany Budget_Germany + Roundbudget
    ]
  ]
end


to toggle-Appointment_Linkage
  set Appointment_Linkage not Appointment_Linkage
end

to Appointment_Linkage_Switch
  if Appointment_Linkage [
    set German_Appointments appointments + 1
    set Spanish_Appointments appointments
  ]
end


to toggle-Applicants_Linkage
  set Applicants_Linkage not Applicants_Linkage
end

to Applicants_Linkage_Switch
  if Applicants_Linkage [
    set Applicants_Spanish Applicants_German
  ]
end

to toggle-Worker_Funds
  set Worker_Funds not Worker_Funds
end



;########## 4. Buttons & Factors ##########

;########## Applicant Insertion #########
; Applicant Insertion allows the user to implement Applicants into an alerady running model

to Applicant_Insertion
  ask Workers with [label = "Home"] [
    hatch-Applicant_Gs Applicants_German [
      set color green
      set shape "person"
      set label "G"
      set size 1.1
      set Location one-of Workers with [label = "Home"]

      set Income norm 50
      set Accommodation 1 + random 5
      set Work 1 + random 5
      set Mental_Health 1 + random 5
      set Physical_Health 1 + random 5
      set Dependants random 5
    ]
    ask Applicant_Gs [
      let Income_Sorting sort-on [(- Income)] Applicant_Gs
      let Dependency_Sorting sort-on [Dependants] Applicant_Gs
      set Incomescore ((position self Income_Sorting / count Applicant_Gs) * 4) + 1
      set Dependantsscore ((position self Dependency_Sorting / count Applicant_Gs) * 4) + 1
      set Total_Score Incomescore + Accommodation + Work + Mental_Health + Physical_Health + Dependantsscore
    ]

    hatch-Applicant_Ss Applicants_Spanish [
      set color green
      set shape "person"
      set label "S"
      set size 1.1
      set Location one-of Workers with [label = "Home"]

      set Income norm 50
      set Accommodation 1 + random 5
      set Work 1 + random 5
      set Mental_Health 1 + random 5
      set Physical_Health 1 + random 5
      set Dependants random 5
    ]
    ask Applicant_Ss [
      let Income_Sorting sort-on [(- Income)] Applicant_Ss
      let Dependency_Sorting sort-on [Dependants] Applicant_Ss
      set Incomescore ((position self Income_Sorting / count Applicant_Ss) * 4) + 1
      set Dependantsscore ((position self Dependency_Sorting / count Applicant_Ss) * 4) + 1
      set Total_Score Incomescore + Accommodation + Work + Mental_Health + Physical_Health + Dependantsscore
    ]
  ]
  ask Applicant_Gs with [Location = one-of Workers with [label = "Home"]] [
    setxy ([xcor] of Location ) (1 + count turtles-on Location)
  ]
  ask Applicant_Ss with [Location = one-of Workers with [label = "Home"]] [
    setxy ([xcor] of Location ) (1 + count turtles-on Location)
  ]
end



;########## 5. Creation & Distribution ##########

;########## 5.1 Creation ##########
; Creating the workers and applicants


to Workerdistribution
  let Workers_Total Spanish_Workers + German_Workers + 1
  let x-step (max-pxcor - min-pxcor) / Workers_Total

  create-Workers Workers_Total [
    ifelse who = 0 [
      set color green
      set shape "house"
      set size 2
      set heading 0
      set label "Home"
    ] [
      ifelse who > German_Workers [
        set color red
        set shape "computer workstation"
        set size 2
        set heading 0
        set label "Spanish"
        set Location self
        set open_Appointment Spanish_Appointments
      ] [
        set color yellow
        set shape "computer workstation"
        set size 2
        set heading 0
        set label "German"
        set Location self
        set open_Appointment German_Appointments
      ]
    ]
    setxy (min-pxcor + (who * x-step + x-step / 2)) (min-pycor + 1)
  ]
end

to Applicant_Creation
  ask Workers with [label = "Home"] [
    hatch-Applicant_Gs Applicants_German [
      set ycor ycor + 1 + who - (German_Workers + Spanish_Workers)
      set color green
      set shape "person"
      set label "G"
      set size 1.1
      set Location "Home"
    ]
    hatch-Applicant_Ss Applicants_Spanish [
      set ycor ycor + 1 + who - (German_Workers + Spanish_Workers)
      set color green
      set shape "person"
      set label "S"
      set size 1.1
      set Location "Home"
    ]
  ]
end



;########## 5.2 Distribution ##########
; Distribute the applicants and insert an average distribution


to Average_Distribution
  ask Workers with [Label = "German"][
    let Average mean [Applicants] of Workers with [label = "German"]
  ]
  ask Workers with [Label = "Spanish"][
    let Average mean [Applicants] of Workers with [label = "German"]
  ]
end

to Distribution
  ask Applicant_Gs with [Total_Score > 12][
    let Suitable_Worker_G Workers with [label = "German" and open_Appointment > count Applicant_Gs-on Location]
    if Suitable_Worker_G != nobody [
      set Location one-of Suitable_Worker_G
    ]
      ask Applicant_Gs with [Total_Score > 12 and Location = nobody][
      set Location one-of Workers with [label = "German"]
      ]
    setxy [xcor] of Location [ycor] of Location
  ]
  ask Applicant_Gs with [Total_Score <= 12][
    set Location one-of Workers with [label = "Home"]
    setxy [xcor] of Location [ycor] of Location
  ]

  ask Applicant_Ss with [Total_Score > 12][
    let geeigneter_Arbeiter_S Workers with [label = "Spanish" and open_Appointment > count Applicant_Ss-on Location]
    if geeigneter_Arbeiter_S != nobody [
      set Location one-of geeigneter_Arbeiter_S
    ]
      ask Applicant_Ss with [Total_Score > 12 and Location = nobody][
      set Location one-of Workers with [label = "Spanish"]
      ]
    setxy [xcor] of Location [ycor] of Location
  ]
  ask Applicant_Ss with [Total_Score <= 12][
    set Location one-of Workers with [label = "Home"]
    setxy [xcor] of Location [ycor] of Location
  ]
  ask Applicant_Gs [
    setxy ([xcor] of Location ) (1 + count Turtles-on Location)
  ]
  ask Applicant_Ss [
    setxy ([xcor] of Location ) (1 + count Turtles-on Location)
  ]
end



;########## 6. Appointments ##########
; Assigning appointments to applicants who are with a worker


to Appointment_Allocation
  ask Applicant_Gs with [Location != "Home"][
    if [Assigned_Appointments] of Location < German_Appointments [
      set appointment 1
      set color blue
      ask Workers with [Location != "Home"] with [[Location] of myself = self][
        set Assigned_Appointments Assigned_Appointments + 1
        set open_Appointment open_Appointment - 1
        set Applicants count Applicant_Gs with [Location = myself]
      ]
    ]
  ]
  ask Applicant_Ss with [Location != "Home"][
    if [Assigned_Appointments] of Location < Spanish_Appointments [
      set appointment 1
      set color blue
      ask Workers with [Location != "Home"] with [[Location] of myself = self][
        set Assigned_Appointments Assigned_Appointments + 1
        set open_Appointment open_Appointment - 1
        set Applicants count Applicant_Ss with [Location = myself]
      ]
    ]
  ]
end



;########## 7. Funds ##########
; Allocationg funds to applicants whose Needs-score is high enough


; Universal allocation of social services in Germany means that all people in critical condition are helped
; In Spain, the people with the highest score are helped
to Allocated_Funds
  ask Applicant_Gs with [Total_Score > Threshold] with [appointment = 1] [
    let Random_Funds_Property 0
    let condition_met false
    while [not condition_met] [
      set Random_Funds_Property random 4 + 1
      ifelse Random_Funds_Property = 1 and Accommodation > 1 [
        set Accommodation Accommodation - 1
        set color pink
        set funds 1
        set condition_met true
      ] [
        ifelse Random_Funds_Property = 2 and Work > 1 [
          set Work Work - 1
          set color pink
          set funds 1
          set condition_met true
        ] [
          ifelse Random_Funds_Property = 3 and Mental_Health > 1 [
            set Mental_Health Mental_Health - 1
            set color pink
            set funds 1
            set condition_met true
          ] [
            if Random_Funds_Property = 4 and Physical_Health > 1 [
              set Physical_Health Physical_Health - 1
              set color pink
              set funds 1
              set condition_met true
            ]
          ]
        ]
      ]
      if Physical_Health >= 1 and Mental_Health >= 1 and Work >= 1 and Accommodation >= 1 [
        set color pink
        set funds 1
        set condition_met true
      ]
    ]
  ]
  set Budget_Germany Budget_Germany - ((2 * count Applicant_Gs with [condition = "critical"] with [appointment = 1]) +
    (penalty * count Applicant_Gs with [appointment = 1] with [condition = "critical"]))

; Sorting applicants based on their total-needs
; Worker Funds changes the way funds are allocated in the spanish system - Funds are allocated based on the needs-score of the applicants at the same worker
  if Worker_Funds [
    ask Applicant_Ss with [appointment = 1] [
      let sameLocationApplicants Applicant_Ss with [location = [location] of myself]
      if count sameLocationApplicants > 1 [
        let Needsscorerank_Appointment_Sorting sort-on [(- Total_Score)] sameLocationApplicants
        set Needsscorerank_Appointment ((position self Needsscorerank_Appointment_Sorting) + 1)
      ]
        if Needsscorerank_Appointment != 1 [
          set Needsscorerank_Appointment 0
      ]
        if count samelocationApplicants = 1 [
          set Needsscorerank_Appointment 1
        ]
      ]
    ]

    if not Worker_Funds [
      ask Applicant_Ss with [Appointment = 1] [
        let Needsscorerank_Appointment_Sorting sort-on [(- Total_Score)] Applicant_Ss with [appointment = 1]
          set Needsscorerank_Appointment (position self Needsscorerank_Appointment_Sorting + 1)
      ]
    ]

  ask Applicant_Ss with [Needsscorerank_Appointment <= Funds_Spain and Needsscorerank_Appointment > 0 and location != (worker 0)] [
    repeat 2 [
      let Random_Funds_Property 0
      let condition_met false
      while [not condition_met] [
        set Random_Funds_Property random 4
        ifelse Random_Funds_Property = 0 and Accommodation > 1 [
          set Accommodation Accommodation - 1
          set color pink
          set funds 1
          set condition_met true
        ] [
          ifelse Random_Funds_Property = 1 and Work > 1 [
            set Work Work - 1
            set color pink
            set funds 1
            set condition_met true
          ] [
            ifelse Random_Funds_Property = 2 and Mental_Health > 1 [
              set Mental_Health Mental_Health - 1
              set color pink
              set funds 1
              set condition_met true
            ] [
              if Random_Funds_Property = 3 and Physical_Health > 1 [
                set Physical_Health Physical_Health - 1
                set color pink
                set funds 1
                set condition_met true
              ]
            ]
          ]
        ]
        if Physical_Health <= 1 and Mental_Health <= 1 and Work <= 1 and Accommodation <= 1 [
          set color pink
          set funds 1
          set condition_met true
        ]
      ]
    ]
  ]

  let Total_Score_Critical sum [Total_Score] of Applicant_Ss with [Needsscorerank_Appointment <= Funds_Spain and Needsscorerank_Appointment > 0] with [condition = "critical"]
  let threshold_critical threshold * count Applicant_Ss with [Needsscorerank_Appointment <= Funds_Spain and Needsscorerank_Appointment > 0] with [condition = "critical"]
  let difference_critical (Total_Score_Critical - threshold_critical) * 2
  let Penalty_Critical (penalty * 2) * count Applicant_Ss with [Needsscorerank_Appointment <= Funds_Spain and Needsscorerank_Appointment > 0] with [condition = "critical"]
  let Budget_Okay 2 * count Applicant_Ss with [Needsscorerank_Appointment <= Funds_Spain and Needsscorerank_Appointment > 0] with [condition = "Okay"]
  let Budget_Critical Difference_Critical + Penalty_Critical

  set Budget_Spain Budget_Spain - Budget_Critical - Budget_Okay
end

; Sorting Applicants with regards to their Total_Score
to Sorting_Applicants
  ask Applicant_Gs [
    let Applicant_Sortet sort-by [t -> [Total_Score] of t] Applicant_Gs
    let Applicants_Critical filter [t -> [Total_Score] of t > Threshold] Applicant_Sortet
  ]
end



;########## 8. Social Needs ##########
; Calculate the Needs-score, change in Needs-score and consequences of the assignment


to needs-score-bad
; Higher Needsscore --> Worse Condition
  ask Applicant_Gs with [Total_Score > 12][
    set Condition "Okay"
    set color yellow
  ]
  ask Applicant_Gs with [Total_Score > Threshold][
    set color red
    set Condition "critical"
  ]
  ask Applicant_Ss with [Total_Score > 12][
    set Condition "Okay"
    set color yellow
  ]
  ask Applicant_Ss with [Total_Score > Threshold][
    set color red
    set Condition "critical"
  ]
end

to needs-score-good
  ask Applicant_Gs with [Location = one-of Workers with [label = "Home"]][
    set color green
    set appointment 0
    set funds 0
    set Condition "Very Good"
  ]

  ask Applicant_Ss with [Location = one-of Workers with [label = "Home"]][
    set color green
    set appointment 0
    set funds 0
    set needsscorerank_appointment 0
    set Condition "Very Good"
  ]
end

to Roundchange
  ask Applicant_Gs with [funds = 0] [
    if count Applicant_Gs with [funds = 0] > 0 [
      let Random_Property random 4 + 1
      if Random_Property = 1 [
        set Accommodation Accommodation + 1
      ]
      if Random_Property = 2 [
        set Work Work + 1
      ]
      if Random_Property = 3 [
        set Mental_Health Mental_Health + 1
      ]
      if Random_Property = 4 [
        set Physical_Health Physical_Health + 1
      ]
    ]
  ]

  ask Applicant_Gs [
    let Random_Change random 9 + 1
    if Random_Change = 1 [
      if random-float 1 < 0.1 [
        set Accommodation Accommodation + 1
      ]
    ]
    if Random_Change = 2 [
      if random-float 1 < 0.1 [
        set Accommodation Accommodation - 1
      ]
    ]
    if Random_Change = 3 [
      if random-float 1 < 0.1 [
        set Work Work + 1
      ]
    ]
    if Random_Change = 4 [
      if random-float 1 < 0.1 [
        set Work Work - 1
      ]
    ]
    if Random_Change = 5 [
      if random-float 1 < 0.1 [
        set Mental_Health Mental_Health + 1
      ]
    ]
    if Random_Change = 6 [
      if random-float 1 < 0.1 [
        set Mental_Health Mental_Health - 1
      ]
    ]
    if Random_Change = 7 [
      if random-float 1 < 0.1 [
        set Physical_Health Physical_Health + 1
      ]
    ]
    if Random_Change = 8 [
      if random-float 1 < 0.1 [
        set Physical_Health Physical_Health - 1
      ]
    ]
    if Random_Change = 9 [
      if random-float 1 < 0.05 [
        set Income norm 50
      ]
    ]
  ]

  ask Applicant_Gs [
    let Income_Sorting sort-on [(- Income)] Applicant_Gs
    set Incomescore ((position self Income_Sorting / count Applicant_Gs) * 4) + 1
    set Total_Score Incomescore + Accommodation + Work + Mental_Health + Physical_Health + Dependantsscore
  ]

  ask Applicant_Ss with [funds = 0] [
    if count Applicant_Ss with [funds = 0] > 0 [
      let Random_Property random 4 + 1
      if Random_Property = 1 [
        set Accommodation Accommodation + 1
      ]
      if Random_Property = 2 [
        set Work Work + 1
      ]
      if Random_Property = 3 [
        set Mental_Health Mental_Health + 1
      ]
      if Random_Property = 4 [
        set Physical_Health Physical_Health + 1
      ]
    ]
  ]

  ask Applicant_Ss [
    let Random_Change random 9 + 1
    if Random_Change = 1 [
      if random-float 1 < 0.1 [
        set Accommodation Accommodation + 1
      ]
    ]
    if Random_Change = 2 [
      if random-float 1 < 0.1 [
        set Accommodation Accommodation - 1
      ]
    ]
    if Random_Change = 3 [
      if random-float 1 < 0.1 [
        set Work Work + 1
      ]
    ]
    if Random_Change = 4 [
      if random-float 1 < 0.1 [
        set Work Work - 1
      ]
    ]
    if Random_Change = 5 [
      if random-float 1 < 0.1 [
        set Mental_Health Mental_Health + 1
      ]
    ]
    if Random_Change = 6 [
      if random-float 1 < 0.1 [
        set Mental_Health Mental_Health - 1
      ]
    ]
    if Random_Change = 7 [
      if random-float 1 < 0.1 [
        set Physical_Health Physical_Health + 1
      ]
    ]
    if Random_Change = 8 [
      if random-float 1 < 0.1 [
        set Physical_Health Physical_Health - 1
      ]
    ]
    if Random_Change = 9 [
      if random-float 1 < 0.05 [
        set Income norm 50
      ]
    ]
  ]

  ask Applicant_Ss [
    let Income_Sorting sort-on [(- Income)] Applicant_Ss
    set Incomescore ((position self Income_Sorting / count Applicant_Ss) * 4) + 1
    set Total_Score (Incomescore + Accommodation + Work + Mental_Health + Physical_Health + Dependantsscore)
  ]
end



;########## Total Score ##########
; Calculation of the Total-Needs-Score

to TNS
  ask Applicant_Gs [
    set Total_score Incomescore + Accommodation + Work + Mental_Health + Physical_Health + Dependantsscore
  ]
  ask Applicant_Ss [
    set Total_Score Incomescore + Accommodation + Work + Mental_Health + Physical_Health + Dependantsscore
  ]
  end



;########## 9. Reset ##########
; Resetting the turn-based property values


to Reset
  set Budget_Spain Budgetslider
  set Budget_Germany Budgetslider

  ask Workers with [label = "German"] [
    set open_Appointment German_Appointments
    set Assigned_Appointments 0
    set Applicants 0
  ]
  ask Workers with [label = "Spanish"] [
    set open_Appointment Spanish_Appointments
    set Assigned_Appointments 0
    set Applicants 0
  ]
  ask Applicant_Gs [
    set appointment 0
    set color green
    set funds 0
  ]
  ask Applicant_Ss [
    set appointment 0
    set color green
    set funds 0
    set needsscorerank 0
    set needsscorerank_appointment 0
  ]
end



;########## 10. Setup ##########
; Setup of the model with the assigned values


to setup
  clear-all
  reset-ticks
  set Budget_Spain Budgetslider
  set Budget_Germany Budgetslider

  Appointment_Linkage_Switch
  Applicants_Linkage_Switch

  Workerdistribution
  Applicant_Creation

  Applicant_Properties

  Needsranking
end



;########## 11. Go ##########
; Execution of the model according to the previously set commands


to go
  tick
  if Budget_Germany <= 0 and Budget_Spain <= 0 [
    create-End_of_Simulations 1 [
      set color green
      set size 0
      set label "Insufficient Budget of Both Countries"
      setxy 31 2
    ]
    print "Insufficient Budget"
    stop
  ]
  if Budget_Germany <= 0 [
    create-End_of_Simulations 1 [
      set color green
      set size 0
      set label "Insufficient Budget Germany"
      setxy 31 25
    ]
    print "Insufficient Budget"
    stop
  ]
  if Budget_Spain <= 0 [
    create-End_of_Simulations 1 [
      set color green
      set size 0
      set label "Insufficient Budget Spain"
      setxy 31 25
    ]
    print "Insufficient Budget"
    stop
  ]
  if ticks = Rounds [
    create-End_of_Simulations 1 [
      set color black
      set size 0
      set label "Roundlimit Reached"
      setxy 28 25
    ]
    print "Roundlimit"
    stop
  ]

  Roundbudget_Switch
  Appointment_Linkage_Switch

  Roundchange
  Reset
  Distribution
  needs-score-bad
  Appointment_Allocation
  Allocated_Funds
  needs-score-good
  needsranking
end
@#$#@#$#@
GRAPHICS-WINDOW
428
10
1099
448
-1
-1
13.0
1
10
1
1
1
0
1
1
1
0
50
0
32
0
0
1
Monat
30.0

SLIDER
1124
38
1300
71
Applicants_German
Applicants_German
0
20
12.0
1
1
NIL
HORIZONTAL

SLIDER
1365
36
1542
69
Applicants_Spanish
Applicants_Spanish
0
20
12.0
1
1
NIL
HORIZONTAL

SLIDER
1365
81
1542
114
Spanish_Workers
Spanish_Workers
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
1123
82
1302
115
German_Workers
German_Workers
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
15
204
184
237
Budgetslider
Budgetslider
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
226
329
398
362
Roundbudget
Roundbudget
0
100
54.0
1
1
NIL
HORIZONTAL

SLIDER
1123
125
1303
158
German_Appointments
German_Appointments
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
1366
121
1544
154
Spanish_Appointments
Spanish_Appointments
0
10
3.0
1
1
NIL
HORIZONTAL

MONITOR
1401
168
1504
213
Budget S.
round (Budget_Spain * 100) / 100
17
1
11

BUTTON
92
20
156
53
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
167
20
261
53
Go (once)
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

SWITCH
12
329
215
362
Roundbudget_Calculation
Roundbudget_Calculation
1
1
-1000

SWITCH
12
370
215
403
Appointment_Linkage
Appointment_Linkage
0
1
-1000

BUTTON
270
20
333
53
Go
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

MONITOR
1403
459
1506
504
Appointments G.
count Applicant_Ss with [appointment = 1]
17
1
11

MONITOR
1166
459
1277
504
Appointments G.
count Applicant_Gs with [appointment = 1]
17
1
11

SWITCH
12
409
215
442
Applicants_Linkage
Applicants_Linkage
0
1
-1000

PLOT
439
457
639
607
Budget
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"German_Budget" 1.0 0 -1184463 true "" "plot Budget_Germany"
"" 1.0 0 -2674135 true "" "plot Budget_Spain"

MONITOR
1164
168
1275
213
Budget G.
round (Budget_Germany * 100) / 100
17
1
11

PLOT
888
459
1088
609
Total Needs
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
"default" 1.0 0 -1184463 true "" "plot sum [Total_Score] of Applicant_Gs"
"pen-1" 1.0 0 -2674135 true "" "plot sum [Total_Score] of Applicant_Ss"

BUTTON
15
501
188
534
Applicant Insertion
Applicant_Insertion
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1166
315
1277
360
Critical G.
count Applicant_Gs with [Total_Score > Threshold]
17
1
11

MONITOR
1403
315
1505
360
Critical S.
count Applicant_Ss with [Total_Score > Threshold]
17
1
11

SLIDER
226
370
398
403
Appointments
Appointments
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
221
205
393
238
Rounds
Rounds
0
20
20.0
1
1
NIL
HORIZONTAL

MONITOR
1403
411
1505
456
Applicants S.
count Applicant_Ss
17
1
11

MONITOR
1166
411
1277
456
Applicants G.
count Applicant_Gs
17
1
11

MONITOR
1165
218
1276
263
Av. Needs. G.
round (sum [Total_Score] of Applicant_Gs / count Applicant_Gs * 100) / 100
17
1
11

MONITOR
1402
218
1504
263
Av. Needs. S.
round (sum [Total_Score] of Applicant_Ss / count Applicant_Ss * 100) / 100
17
1
11

TEXTBOX
187
183
337
201
General
11
0.0
1

TEXTBOX
1152
16
1302
34
Properties Germany
11
0.0
1

TEXTBOX
1393
14
1543
32
Propoerties Spain
11
0.0
1

TEXTBOX
192
303
342
321
Switches
11
0.0
1

TEXTBOX
182
471
332
489
Scenario
11
0.0
1

PLOT
663
459
863
609
Number of Critical Applicants
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
"default" 1.0 0 -1184463 true "" "plot count Applicant_Gs with [Total_Score > Threshold]"
"pen-1" 1.0 0 -2674135 true "" "plot count Applicant_Ss with [Total_Score > Threshold]"

SLIDER
15
239
183
272
Threshold
Threshold
0
30
18.0
1
1
NIL
HORIZONTAL

SLIDER
221
240
393
273
Penalty
Penalty
0
10
5.0
1
1
NIL
HORIZONTAL

SWITCH
220
499
356
532
Worker_Funds
Worker_Funds
1
1
-1000

TEXTBOX
20
72
170
184
Countries:\nYellow - German\nRed - Spanish\n\nNeedscore:\nGreen - Very Good\nYellow - Okay\nRed - Critical
11
0.0
1

TEXTBOX
228
74
378
144
Appointments:\nAppointment - Blue\n\nFunds:\nAllocated Funds - Pink
11
0.0
1

MONITOR
1164
266
1276
311
Total Needs G.
round (sum [Total_Score] of Applicant_Gs * 100) / 100
17
1
11

MONITOR
1403
266
1504
311
Total Needs S.
round (sum [Total_Score] of Applicant_Ss * 100) / 100
17
1
11

SLIDER
16
544
188
577
Funds_Spain
Funds_Spain
0
10
4.0
1
1
NIL
HORIZONTAL

PLOT
663
616
863
766
% of Critical Applicants
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"German" 1.0 0 -1184463 true "" "if count Applicant_Gs with [Total_Score > Threshold] >= 1 \n  [plot (count Applicant_Gs with [Total_Score > Threshold] / count Applicant_Gs) * 100]"
"Spain" 1.0 0 -2674135 true "" "if count Applicant_Ss with [Total_Score > Threshold] >= 1 \n  [plot (count Applicant_Ss with [Total_Score > Threshold] / count Applicant_Ss) * 100]"

MONITOR
1164
363
1277
408
Critical G. %
round ((count Applicant_Gs with [Total_Score > Threshold] / count Applicant_Gs) * 100 * 100) / 100
17
1
11

MONITOR
1403
363
1505
408
Critical S. %
round ((count Applicant_Ss with [Total_Score > Threshold] / count Applicant_Ss) * 100 * 100) / 100
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
NetLogo 6.4.0
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
0
@#$#@#$#@
