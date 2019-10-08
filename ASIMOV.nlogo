;;=====================================================================================================================================================================================
;;=====================================================================================================================================================================================
;;;
;;;                                          A        S        I         M         O         V
;;;
;;=====================================================================================================================================================================================
;;=====================================================================================================================================================================================



;;==========================================================================================================================================
;;                                                   V A R I A B L E    I N I T I A L I Z A T I O N
;;==========================================================================================================================================

;;Create animats and assign qualities
globals [Addiction_Cycle_Phase switch_time stop_time hermi-color flab-color drug-color]

breed [Cslugs Cslug]
breed [nociceptors nociceptor] ;ie the pain receptors
breed [probos proboscis]
breed [flabs flab]
breed [hermis hermi]
breed [fauxflabs fauxflab]
breed [drugs drug]


Cslugs-own [ Somatic_Map App_State App_State_Switch ExpReward_pos ExpReward_neg Incentive Nutrition Satiation speed turn-angle
             sns_betaine sns_betaine_left sns_betaine_right sns-pain-left sns-pain-right sns-pain-caud sns-pain spontaneous-pain pain pain-switch
             Vh sns_hermi sns_hermi_left sns_hermi_right alpha_hermi beta_hermi lambda_hermi  delta_Vh hermcount hermcount_history
             Vf sns_flab sns_flab_left sns_flab_right alpha_flab beta_flab lambda_flab delta_Vf flabcount fauxflabcount flabcount_history
             Vd sns_drug sns_drug_left sns_drug_right delta_Vd alpha_drug beta_drug lambda_drug drugcount drugcount_history drug_reward
             R R_hermi R_flab R_drug RewardExperience deg_sensitization IN M M0 W1 W2 W3 dW3 W4 W5 RewardExperience_history
            ]
nociceptors-own [parent painval id hit]
probos-own [parent phase]
patches-own [odor_flab odor_hermi odor_betaine odor_drug]

to startup
  setup

end



;-----------------------------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------------------


;;==========================================================================================================================================
;;                                                   S E T U P    P R O C E D U R E S
;;==========================================================================================================================================

to setup
  clear-all

  ; sets up first phase of Addiction Cycle, to be used when Addiction_Cycle is turned ON
  set Addiction_Cycle_Phase "No Drug"
  ;set switch_time 15000
  set switch_time 15000
  set stop_time 60000; was 150000
  set hermi-color 85
  set flab-color 135
  set drug-color 45

;---------------------------------------------------------------------------------
;           Spawns a Cslug
;---------------------------------------------------------------------------------
  create-Cslugs 1 [

    ;sets shape, size, color, and position of pleuro
    set shape "Cslug" ; "octo"
    set color orange - 2
    set size 16
    set heading 0

    ;creates 7 sensors for pain detection
    repeat 7 [
      hatch-nociceptors 1 [
        set hidden? true
        set shape "dot"
        set size 3
        set parent myself
        let idnum ((count nociceptors) mod 7)
        let labels ["snsrOL" "snsrOR" "snsrUL" "snsrUR" "snsrBL" "snsrBR" "snsrBM"]
        set id (item idnum labels)

        if id = "snsrOL"[
          set hidden? false
        ]
        if id = "snsrOR"[
          set hidden? false
        ]
      ]
    ]
    ;updates pain sensor position
    update-nociceptor-position

    ;sets baseline spontaneous pain activity
    set spontaneous-pain 2

    ;sets initial values for feeding network variables (nutrition, incentive salience, somatic map, and satiation)
    set Nutrition 0.5
    set Incentive 0
    set Somatic_Map 0
    set Satiation 0.5

    ;sets initial habituation/sensitization parameters for Homeostatic Reward Circuit (HRC):
    set W1 1
    set W2 0.2
    set W3 1
    set W4 0.1
    set W5 0.1
    set M0 10 ;Baseline activity for M
    ;set R_drug 0.5
    set drug_reward 30

    set drugcount_history [];
    set hermcount_history [];
    set flabcount_history [];
    set RewardExperience_history [];

    ;Preliminary Rescorla-Wagner parameters for learning Hermi & Flab odors. V is learned value of an odor, alpha is the salience
    ;(or noticeability) of an odor, beta is the learning rate, and lambda sets the maximum value of learning (between 0 and 1).
    set Vf 0
    set Vh 0
    set Vd 0
    set alpha_hermi 0.5
    set beta_hermi 1
    set lambda_hermi 1
    set alpha_flab 0.5
    set beta_flab 1
    set lambda_flab 1
    set alpha_drug 0.5
    set beta_drug 1
    set lambda_drug 1

    ;Give Cslug a feeding apparatus for decorative effect
    hatch-probos 1 [
      set shape "airplane"
      set size size / 2
      set parent myself
    ]

   ;Track Cslug's path
    pen-down
  ]


;---------------------------------------------------------------------------------
;           Spawns Prey and Drug
;---------------------------------------------------------------------------------
if Presentation-Mode = false[

 ;creates flabellina prey
 create-flabs flab-populate [
    set shape "circle"
    set size 1
    set color flab-color
    setxy random-xcor random-ycor
  ]
 ;creates hermissenda prey
 create-hermis hermi-populate [
    set shape "circle"
    set size 1
    set color hermi-color
    setxy random-xcor random-ycor
  ]
 ;creates faux flabellina prey
; create-fauxflabs fauxflab-populate [
;    set shape "circle"
;    set size 1
;    set color blue
;    setxy random-xcor random-ycor
;  ]

 ;creates drug
 create-drugs drug-populate [
    set shape "circle"
    set size 1
    set color drug-color
    setxy random-xcor random-ycor
  ]
]



 reset-ticks

end



;-----------------------------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------------------


;;==========================================================================================================================================
;;                                                   U P D A T E    P R O C E D U R E S
;;==========================================================================================================================================


to go

;---------------------------------------------------------------------------------
;           User Actions
;---------------------------------------------------------------------------------

  ;; allow user to drag things around
  if mouse-down? [
    ask Cslugs [
      if distancexy mouse-xcor mouse-ycor < 3 [setxy mouse-xcor mouse-ycor]
    ]
    ask flabs [
      if distancexy mouse-xcor mouse-ycor < 3 [setxy mouse-xcor mouse-ycor]
    ]
    ask hermis [
      if distancexy mouse-xcor mouse-ycor < 3 [setxy mouse-xcor mouse-ycor]
    ]
    ask drugs [
      if distancexy mouse-xcor mouse-ycor < 3 [setxy mouse-xcor mouse-ycor]
    ]
  ]


;---------------------------------------------------------------------------------
;           Updates Prey and Drug Populations
;---------------------------------------------------------------------------------
ifelse Presentation-Mode[
  if count hermis > 1 [ask hermis [die]]
  if count flabs > 1 [ask flabs [die]]
  if count fauxflabs > 1 [ask fauxflabs[die]]
  if count drugs > 1 [ask drugs [die]]][


 create-flabs flab-populate - count flabs [
    set shape "circle"
    set size 1
    set color flab-color
    setxy random-xcor random-ycor
  ]
  if flab-populate < count flabs [
    ask n-of (count flabs - flab-populate) flabs [die]
  ]


 create-hermis hermi-populate - count hermis [
    set shape "circle"
    set size 1
    set color hermi-color
    setxy random-xcor random-ycor
  ]
  if hermi-populate < count hermis [
    ask n-of (count hermis - hermi-populate) hermis [die]
  ]

; create-fauxflabs fauxflab-populate - count fauxflabs [
;    set shape "circle"
;    set size 1
;    set color blue
;    setxy random-xcor random-ycor
;  ]

if not Addiction_Cycle[
 create-drugs drug-populate - count drugs [
    set shape "circle"
    set size 1
    set color drug-color
    setxy random-xcor random-ycor
  ]
  if drug-populate < count drugs [
    ask n-of (count drugs - drug-populate) drugs [die]
  ]
]

]

;---------------------------------------------------------------------------------
;           Updates Odors
;---------------------------------------------------------------------------------

; Initialize, deposit, diffuse, and evaporate odors
  ask hermis [set odor_hermi 0.5]
  ask hermis [set odor_betaine 0.5]
  ask flabs [set odor_flab 0.5]
  ask flabs [set odor_betaine 0.5]
  ask fauxflabs [set odor_flab 0.5]
  ask fauxflabs [set odor_betaine 0.5]
  ask drugs [set odor_drug 0.5]

;; diffuse odors
  diffuse odor_hermi 0.5
  diffuse odor_flab 0.5
  diffuse odor_betaine 0.5
  diffuse odor_drug 0.5

;; evaporate odors
  ask patches [
    set odor_hermi 0.95 * odor_hermi
    set odor_flab 0.95 * odor_flab ; changed from 0.98 to 0.95
    set odor_betaine 0.95 * odor_betaine
    set odor_drug 0.95 * odor_drug
    recolor-patches
  ]



;=================================================================================
;
;           Cslug Actions (update of Cslug variables)
;
;=================================================================================

  ask Cslugs [

    ;updates sensor values and locations, see function below for details
    update-sensors

    ;updates proboscis and pleurobranchaea movement
    update-proboscis
    set speed 0.3
    set turn-angle -1 + random-float 2


    ;------------------------------------------------------;
    ;           Sets values for odor and pain sensation
    ;------------------------------------------------------;
    ;; Detecting prey and pain
    set sns_hermi (sns_hermi_left + sns_hermi_right ) / 2
    set sns_betaine (sns_betaine_left + sns_betaine_right) / 2
    set sns_flab (sns_flab_left + sns_flab_right ) / 2
    set sns_drug (sns_drug_left + sns_drug_right ) / 2
    set sns-pain ((sns-pain-left + sns-pain-right ) / 2)

;    ;if the drug type is heroin, if consumed, it will directly suppress pain sensation.
;    ;(This suppression effect will decay over time after the drug consumption.)
;    set h-suppression 0.99 * h-suppression

    ;sets an upper limit for pain (via logistic function).
    ; Note that for creating the pain response map, apply_pain was set at 10
    set pain 10 / (1 + e ^ (- 2 * (sns-pain + spontaneous-pain) + 10 ))


    ;pain-switch will switch the signs of Reward State and Pain in Appetitive State when Pain is greater than Reward State
    ; (Effectively, this causes pain and reward state to be in reciprocal inhibition.
    ;  In the presence of enough pain, positive rewards will ease the effect of pain,
    ;  while negative rewards will exacerbate it.)
    ;set pain-switch 2 / (1 + e ^ (- 7 * (RewardExperience - pain + 2.1))) - 1;
    ;set pain-switch 1 - 2 / (1 + e ^ (- 10 * (sns-pain - 0.7)));
    set pain-switch 1 - 2 / (1 + e ^ (- 10 * (sns-pain - 0.2)));

  ;-----------------------------------------------------------------;
  ;           APPROACH AND AVOIDANCE BEHAVIORS
  ;=================================================================;

    ;------------------------------------------------------;
    ;        Sets positive and negative expected reward
    ;------------------------------------------------------;
    ;sets positive expected reward (based on satiation, sns-betaine, sns-hermi, sns-drug, associative value for sns-hermi, and associative value for sns-drug)
    set ExpReward_pos sns_betaine / (1 + (0.05 * Vh * sns_hermi) - 0.006 / Satiation) + 3.0 * Vh * sns_hermi + 8.0 * Vd * sns_drug;
    ;set ExpReward_pos sns_betaine / (4 - 0.032 / Satiation) + 0.33 * Vh * sns_hermi + 2.5 * Vd * sns_drug;

    ;sets negative reward (based on pain, sns-flab, and associative value for for sns-flab)
    ;set ExpReward_neg 0.33 * Vf * sns_flab; + pain; R-
    set ExpReward_neg 0.59 * Vf * sns_flab; + pain; R-


    ; sets decrease in nutrition, satiation (based only on nutrition), and incentive salience (based on positive and negative reward)
    set Nutrition Nutrition - 0.0005 * Nutrition ; Nutritional state declines with time
    ifelse Fix-var1: [set Satiation fix-satiation] [set Satiation 1 / ((1 + 0.7 * exp(-4 * Nutrition + 2)) ^ (2))]
    set Incentive ExpReward_pos - ExpReward_neg;

    ;------------------------------------------------------;
    ;           Sets Somatic Map
    ;------------------------------------------------------;
    ;; Exponent variables for somatic map, which basically determine interaction and "priority" of the sensations
    let H (sns_hermi - sns_flab - sns_drug - sns-pain)
    let F (sns_flab - sns_hermi - sns_drug - sns-pain)
    let D (sns_drug - sns_hermi - sns_flab - sns-pain)
    let P (pain)

   ;; sets somatic map variable, transforms sensory input into a "place code"
    set Somatic_Map (- ((sns_flab_left - sns_flab_right) / (1 + exp (-50 * F)) + (sns_hermi_left - sns_hermi_right) / (1 + exp (-50 * H)) + (sns_drug_left - sns_drug_right) / (1 + exp (-50 * D))+(sns-pain-left - sns-pain-right)/(1 + exp (-50 * P))))



    ;------------------------------------------------------;
    ;           Sets Appetitive State
    ;------------------------------------------------------;
    ; sets Appetitve State
    ;set App_State 0.01 + (1 / (1 + exp(- (0.75 * Incentive - 9 * satiation  - 1.8 * pain-switch * (RewardExperience - Pain) ))) + 0.1 * ((App_State_Switch - 1) * 0.5)); + 0.25
    set App_State 0.01 + (1 / (1 + exp(- (0.75 * Incentive - 9 * satiation -  1.8 * Pain - 1.8 * pain-switch * RewardExperience))) + 0.1 * ((App_State_Switch - 1) * 0.5)); + 0.25

    ; The switch for approach-avoidance
    set App_State_Switch (((-2 / (1 + exp(-100 * (App_State - 0.245)))) + 1))

    ;;set speed
    set speed 0.3

    ;set the turning angle based on appetitive state switch and somatic map
    set turn-angle (2 * App_State_Switch) / (1 + exp (3 * Somatic_Map)) - App_State_Switch
    rt turn-angle


    ; if the immobilize switch is ON, then fixes Cslug in place, but it can still turn (for testing purposes)
    ifelse immobilize = true[
      fd 0
    ][
      fd speed
    ]


  ;-----------------------------------------------------------------;
  ;                 PREY AND DRUG CONSUMPTION
  ;=================================================================;

    ;----------------------------------------------------------
    ;consumption of hermissenda
    let hermitarget other (turtle-set hermis) in-cone (0.4 * size) 45
    if any? hermitarget [
      set Nutrition Nutrition + count hermitarget * 0.3
      set R_hermi R_hermi + count hermitarget  ;sets (positive) reward input to HRC based on number of hermissenda consumed
      set hermcount hermcount + 1
      ifelse Presentation-Mode [ask hermitarget [die]][ask hermitarget [setxy random-xcor random-ycor]]
      set delta_Vh alpha_hermi * beta_hermi * (lambda_hermi - Vh)
      set Vh Vh + delta_Vh ; The Rescorla-Wagner Learning Algorithm
      ;output-print word "Hermi eaten at tick " ticks
      set hermcount_history lput ticks hermcount_history
    ]

    ;----------------------------------------------------------
    ;consumption of flabellina
    let flabtarget other (turtle-set flabs) in-cone (0.4 * size) 45
    if any? flabtarget [
      set Nutrition Nutrition + count flabtarget * 0.3
      set R_flab R_flab - count flabtarget   ;sets (negative) reward input to HRC based on number of flabellina consumed
      set flabcount flabcount + 1
      ifelse Presentation-Mode [ask flabtarget [die]][ask flabtarget [setxy random-xcor random-ycor]]
      set delta_Vf alpha_flab * beta_flab * (lambda_flab - Vf)
      set Vf Vf + delta_Vf ; The Rescorla-Wagner Learning Algorithm
      ;output-print word "Flab eaten at tick " ticks
      set flabcount_history lput ticks flabcount_history
    ]

    ;----------------------------------------------------------
    ;consumption of flabellina
    let fauxflabtarget other (turtle-set fauxflabs) in-cone (0.4 * size) 45
    if any? fauxflabtarget [
      set Nutrition Nutrition + count fauxflabtarget * 0.3
      set R_flab R_flab + count flabtarget   ;sets (positive) reward input to HRC based on number of faux flabellina consumed
      set fauxflabcount fauxflabcount + 1
      ifelse Presentation-Mode [ask fauxflabtarget [die]][ask fauxflabtarget [setxy random-xcor random-ycor]]
      set delta_Vf alpha_flab * beta_flab * (0 - Vf)
      set Vf Vf + delta_Vf; Odor_flab is linked to ExpReward, a virtual extinction mechanism
    ]

    ;----------------------------------------------------------
    ;consumption of drug
    let drugtarget other (turtle-set drugs) in-cone (0.4 * size) 45
    ifelse any? drugtarget [
      set Nutrition Nutrition + count drugtarget * 0
      set R_drug R_drug + drug_reward * count drugtarget ;::::::::::   Half-dose = 1.5,  Full-dose = 3
      set drugcount drugcount + 1
      ifelse Presentation-Mode [ask drugtarget [die]][ask drugtarget [setxy random-xcor random-ycor]]

      ifelse drug_reward > 0
      [set delta_Vd alpha_drug * beta_drug * (lambda_drug - Vd)]
      [set delta_Vd alpha_drug * 0.5 * (0 - Vd)]

      set Vd Vd + delta_Vd;
;      ;if the drug type is heroin, if consumed, it will directly suppress pain sensation.
;      ifelse Drug-Type = "Heroin" [set h-suppression 5] [set h-suppression 0]
     ; output-print word "Drug eaten at tick " ticks
      set drugcount_history lput ticks drugcount_history
    ][

    ]
    ;update procedure for calculating input and output of HRC (see function details below)
    update-RewardExperience
  ]


;---------------------------------------------------------------------------------------------------
;     Prey and Drug Control (updates movement of flabellina, hermissenda, faux flabellina, and drug)
;---------------------------------------------------------------------------------------------------
  ask flabs [
    rt -1 + random-float 2
    ifelse immobilize = true [ fd 0][fd 0.02]
  ]

  ask hermis [
    rt -1 + random-float 2
    ifelse immobilize = true [ fd 0][fd 0.02]
  ]

  ask fauxflabs [
    rt -1 + random-float 2
    ifelse immobilize = true [ fd 0][fd 0.02]
  ]

  ask drugs [
    rt -1 + random-float 2
    ifelse immobilize = true [ fd 0][fd 0.02]
  ]


;---------------------------------------------------------------------------------------------------
;                     Updates ticks (units of time) and timers
;---------------------------------------------------------------------------------------------------
  tick
  if ticks = stop_time [stop] ; definite end of an epoch of play
  if ticks > 0 and (ticks mod switch_time) = 0 [switch_on]
end



;-----------------------------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------------------

;;==========================================================================================================================================
;;                                         O T H E R  U P D A T E   F U N C T I O N S
;;==========================================================================================================================================


;---------------------------------------------------------------------------------
;           Updates proboscis movement (Function)
;=================================================================================
to update-proboscis
 ask probos [
    set heading [heading] of parent
    setxy ([xcor] of parent) ([ycor] of parent)
    ifelse ([sns_betaine_left] of parent > 5.5) or ([sns_betaine_right] of parent > 5.5)
      [set phase (phase + 1) mod 20]
      [set phase 0]
    fd (0.15 * size) + (0.1 * phase)
  ]
end


;---------------------------------------------------------------------------------
;           Updates values and positions of sensors (Function)
;=================================================================================
to update-sensors

    let me self

  update-nociceptor-position

  ;colors pain sensors to show pain value
  ask nociceptors [
    if id = "snsrOL"[
      set color scale-color red [sns-pain-left] of parent 0 0.8
    ]
    if id = "snsrOR"[
      set color scale-color red [sns-pain-right] of parent 0 0.8
    ]
  ]

  ;----------------------------------------------------------
  ;sensation of flabellina odor on left side
  let odor_flab_left [odor_flab] of patch-left-and-ahead 40 (0.4 * size)
  ifelse odor_flab_left > 1e-7
    [set sns_flab_left 7 + (log odor_flab_left 10)]
    [set sns_flab_left 0]

  ;sensation of flabellina odor on right side
  let odor_flab_right [odor_flab] of patch-right-and-ahead 40 (0.4 * size)
  ifelse odor_flab_right > 1e-7
    [set sns_flab_right 7 + (log odor_flab_right 10)]
    [set sns_flab_right 0]

  ;----------------------------------------------------------
  ;sensation of hermissenda odor on left side
  let odor_hermi_left [odor_hermi] of patch-left-and-ahead 40 (0.4 * size)
  ifelse odor_hermi_left > 1e-7
    [set sns_hermi_left 7 + (log odor_hermi_left 10)]
    [set sns_hermi_left 0]

  ;sensation of hermissenda odor on right side
  let odor_hermi_right [odor_hermi] of patch-right-and-ahead 40 (0.4 * size)
  ifelse odor_hermi_right > 1e-7
    [set sns_hermi_right 7 + (log odor_hermi_right 10)]
    [set sns_hermi_right 0]

  ;----------------------------------------------------------
  ;sensation of betaine odor on left side
  let odor_betaine_left [odor_betaine] of patch-left-and-ahead 40 (0.4 * size)
  ifelse odor_betaine_left > 1e-7
    [set sns_betaine_left 7 + (log odor_betaine_left 10)]
    [set sns_betaine_left 0]

  ;sensation of betaine odor on right side
  let odor_betaine_right [odor_betaine] of patch-right-and-ahead 40 (0.4 * size)
  ifelse odor_betaine_right > 1e-7
    [set sns_betaine_right 7 + (log odor_betaine_right 10)]
    [set sns_betaine_right 0]

  ;----------------------------------------------------------
  ;sensation of drug odor on left side
  let odor_drug_left [odor_drug] of patch-left-and-ahead 40 (0.4 * size)
  ifelse odor_drug_left > 1e-7
    [set sns_drug_left 7 + (log odor_drug_left 10)]
    [set sns_drug_left 0]

  ;sensation of drug odor on right side
  let odor_drug_right [odor_drug] of patch-right-and-ahead 40 (0.4 * size)
  ifelse odor_drug_right > 1e-7
    [set sns_drug_right 7 + (log odor_drug_right 10)]
    [set sns_drug_right 0]

  ;----------------------------------------------------------

  ;set sns-pain-left sum [painval] of (nociceptors with [id = one-of["snsrOL" "snsrUL" "snsrBL"] and parent = me])
  ;set sns-pain-right sum [painval] of (nociceptors with [id = one-of["snsrOR" "snsrUR" "snsrBR"] and parent = me])
  ;set sns-pain-caud  sum [painval] of (nociceptors with [id = one-of["snsrBL" "snsrBR" "snsrBM"] and parent = me])

  ;sensation of pain
  set sns-pain-left 0.9 * sns-pain-left
  set sns-pain-right 0.9 * sns-pain-right

end



;---------------------------------------------------------------------------------
;           Updates positions of pain sensors (Function)
;=================================================================================

to update-nociceptor-position
  ask nociceptors[
      let x-par [xcor] of parent
      let y-par [ycor] of parent
      let hd-par [heading] of parent
      let sz-par [size] of parent

      ifelse id = "snsrOL"[
        setxy (x-par + 0.4 * sz-par * sin (hd-par - 40)) (y-par + 0.4 * sz-par * cos (hd-par - 40))
      ][
       ifelse id = "snsrOR"[
         setxy (x-par + 0.4 * sz-par * sin (hd-par + 40)) (y-par + 0.4 * sz-par * cos (hd-par + 40))
       ][
        ifelse id = "snsrUL"[
          setxy (x-par + 0.3 * sz-par * sin (hd-par - 100)) (y-par + 0.3 * sz-par * cos (hd-par - 100))
        ][
         ifelse id = "snsrUR"[
           setxy (x-par + 0.3 * sz-par * sin (hd-par + 100)) (y-par + 0.3 * sz-par * cos (hd-par + 100))
         ][
          ifelse id = "snsrBL"[
            setxy (x-par + 0.35 * sz-par * sin (hd-par - 150)) (y-par + 0.35 * sz-par * cos (hd-par - 150))
          ][
           ifelse id = "snsrBR"[
             setxy (x-par + 0.35 * sz-par * sin (hd-par + 150)) (y-par + 0.35 * sz-par * cos (hd-par + 150))
           ][
             setxy (x-par + 0.46 * sz-par * sin (hd-par - 180)) (y-par + 0.46 * sz-par * cos (hd-par - 180))
           ]
          ]
         ]
        ]
       ]
      ]
    ]

end



;---------------------------------------------------------------------------------
;           Homeostatic Reward Circuit Calculations (Function)
;=================================================================================
to update-RewardExperience

  ;sets reward inputs, which decay over time
  set R_hermi 0.98 * R_hermi
  set R_flab 0.98 * R_flab
  set R_drug 0.999 * R_drug

  ;Sets Reward Input to neuron M, with a baseline activity of M0
  set R (W1 * R_drug + W2 * IN + W4 * R_hermi + W5 * R_flab + M0)

  ;Positive feedback loop for reward input
  set IN (W2 * R)

  ;Response of neuron M, based on the dynamic synaptic weight W3 and Reward Input (R)
  set M (W3 * R)


  ;Change in W3 depends on neuron M activity, neuron M baseline (M0), and Reward Input (R)
  set dW3 ((M0 - M) / R) / 1000
  set W3 (W3 + dW3)


  ;sets Reward State value as a logistic function of neuron M activity minus its baseline. Basically indicates how much neuron M activity differs from baseline.
  ; (Reward State is the output of the Homeostatic Reward Circuit (HRC) to Appetitive State)
  ifelse Fix-var2: [set RewardExperience Fix-RewardExp][set RewardExperience -15 + 30 / (1 + e ^ (-(M - M0)))]
  set RewardExperience_history lput RewardExperience RewardExperience_history

end


;---------------------------------------------------------------------------------
;           Recolors patches based on odor value (Function)
;=================================================================================
to recolor-patches
    ifelse odor_flab > odor_hermi [
      set pcolor scale-color flab-color odor_flab 0 0.6
    ][
      set pcolor scale-color hermi-color odor_hermi 0 0.9
    ]
end





;-----------------------------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------------------

;;==========================================================================================================================================
;;                                         U S E R     A P P L I C A T I O N     F U N C T I O N S
;;==========================================================================================================================================



;=======================================================================================================
;             Functions for Presentation Mode
;=======================================================================================================
;
;                  Presentation Mode:
;                  ---------------------------
;                      Cslug is immobilized, except for turning, and can be fed or
;                      presented with prey or drug, to monitor approach-avoidance turns
;                      To start, set Presentation-Mode to ON, and click on the Present button.

;                      Note that Satiation can be fixed via the Fix-var1 switch and fix-satiation slider,
;                      and Reward Experience can be fixed via the Fix-var2 switch and fix-RewardExperience slider.
;-------------------------------------------------------------------------------------------------------

    ;---------------------------------------------------------------------------------------------------
    ;                     Presentation of Prey or Drug (function)
    ;---------------------------------------------------------------------------------------------------
    ;                        INSTRUCTIONS: Choose prey or drug type from the Presentation-Choice
    ;                        drop-down menu, then click on the Present button. This will spawn the
    ;                        selected item on the left side of Cslug, which may cause it to turn away
    ;                        or towards the item.
    ;---------------------------------------------------------------------------------------------------
    to Present
      if Presentation-Mode = true[
        ;ask Cslugs[set pain 0]

        clear-patches
        let xc 9
        let yc 13
        set immobilize true
        ask hermis [die]
        ask flabs [die]
        ask fauxflabs[die]
        ask drugs [die]
        ask Cslugs [
          set heading 0
          setxy 0 0
        ]

        if Presentation-Choice = "Drug vs Hermi"[
          create-drugs 1 [set odor_drug 0.5 set shape "circle" set size 1 set color drug-color setxy (- xc) yc]
          create-hermis 1 [set odor_hermi 0.5 set odor_betaine 0.5 set shape "circle" set size 1 set color hermi-color setxy xc yc]
        ]
        if Presentation-Choice = "Drug vs Flab"[
          create-drugs 1 [set odor_drug 0.5 set shape "circle" set size 1 set color drug-color setxy (- xc) yc]
          create-flabs 1 [set odor_flab 0.5 set odor_betaine 0.5 set shape "circle" set size 1 set color flab-color setxy xc yc]
        ]
        if Presentation-Choice = "Hermi vs Flab"[
          create-hermis 1 [set odor_hermi 0.5 set odor_betaine 0.5 set shape "circle" set size 1 set color hermi-color setxy (- xc) yc]
          create-flabs 1 [set odor_flab 0.5 set odor_betaine 0.5 set shape "circle" set size 1 set color flab-color setxy xc yc]
        ]
        if Presentation-Choice = "None"[
        ]
        if Presentation-Choice = "Hermi"[
          create-hermis 1 [set odor_hermi 0.5 set odor_betaine 0.5 set shape "circle" set size 1 set color hermi-color setxy (- xc) yc]
        ]
        if Presentation-Choice = "Drug"[
          create-drugs 1 [set odor_drug 0.5 set shape "circle" set size 1 set color drug-color setxy (- xc) yc]
        ]
        if Presentation-Choice = "Flab"[
          create-flabs 1 [set odor_flab 0.5 set odor_betaine 0.5 set shape "circle" set size 1 set color flab-color setxy (- xc) yc]
        ]
      ]
     end


    ;---------------------------------------------------------------------------------------------------
    ;                     Feeding Cslug with Prey or Drug (function)
    ;---------------------------------------------------------------------------------------------------
    ;                        INSTRUCTIONS: Choose prey or drug type from the Feeding-Choice
    ;                        drop-down menu, then click on the Feed button. This will spawn the
    ;                        selected item right at the mouth of Cslug, effectively feeding it the item.
    ;---------------------------------------------------------------------------------------------------
    to Feed
      if Presentation-Mode = true[
        clear-patches
        let xc 0
        let yc 6
        set immobilize true
        ask hermis [die]
        ask flabs [die]
        ask fauxflabs[die]
        ask drugs [die]
        ask Cslugs [
          set heading 0
          setxy 0 0
        ]
        if Feeding-Choice = "Hermi"[
          create-hermis 1 [set odor_hermi 0.5 set odor_betaine 0.5 set shape "circle" set size 1 set color hermi-color setxy xc yc]
        ]
        if Feeding-Choice = "Drug"[
          create-drugs 1 [set odor_drug 0.5 set shape "circle" set size 1 set color drug-color setxy xc yc]
        ]
        if Feeding-Choice = "Odorless Drug"[
          create-drugs 1 [set odor_drug 0 set shape "circle" set size 1 set color drug-color setxy xc yc]
        ]
        if Feeding-Choice = "Flab"[
          create-flabs 1 [set odor_flab 0.5 set odor_betaine 0.5 set shape "circle" set size 1 set color flab-color setxy xc yc]
        ]
      ]
     end



;=======================================================================================================
;             Pain Application (Functions)
;=======================================================================================================
;                  To apply a painful stimulus to Cslug, set the pain value via the apply_pain slider,
;                  and click on the Poke-Left or Poke-Right button.
;-------------------------------------------------------------------------------------------------------
  to Poke-Left
    ask Cslugs[
      set sns-pain-left sns-pain-left + Apply_Pain
    ]
  end

  to Poke-Right
    ask Cslugs[
      ;set pain 1
      set sns-pain-right sns-pain-right + apply_pain
    ]
  end



;=============================================================================================================
;             Addiction Cycle
;=============================================================================================================
;                  The Addiction Cycle mode will start off a series of events that causes the forager to go
;                  through the phases of addiction over the course of 60000 ticks.
;
;                  Before enabling the Addiction Cycle Mode, make sure Presentation_Mode and Immobilize are both OFF.
;                  The easiest way to to make sure everything is ready and set up for the Addiction Cycleis is to click on the Reset to Default Settings button at the bottom center of the interface. The populations of hermi and flab can be set beforehand to the desired amounts. These will not change throughout the addiction cycle. If desired, variables can also be fixed beforehand, but it is recommended to observe the Addiction Cycle without the fixation of any variables.
;
;                  Once everything is ready and set as desired, set the Addiction_Cycle switch to ON. It is recommended to not alter any controls during this process in order to observe a fully-functioning Addiction Cycle, but above all, please refrain
;                  from altering the drug population sliders during this process. Take note of the graph of the total prey and drug consumed and how it changes throughout the phases of the Addiction Cycle.The Addiction Cycle
;                  takes a total of 60000 ticks to complete.
;
;                  The forager will start out in an environment with no drugs and only prey in the No-Drug phase.
;                  In the Drug Introduced phase, 5 drugs are spawned, allowing the forager to start
;                  drug consumption. Note that drug consumption typically occurs when the forager is
;                  very hungry, or by accident, if it was trying to consume another prey nearby.
;                  The next phase is the Drug Removed phase, where all drugs are removed. During this phase
;                  The forager is likely to undergo withdrawal and recovery from withdrawal.
;                  Following this is the Drug Without Reward phase, where 6 drug items are spawned again, emitting the same drug odor,
;                  but providing no reward instead of a high positive reward.
;                  In the beginning of this phase the forager is likely to demonstrate cravings by resuming drug consumption
;                  to some extent, due to the strong learned association, but over time as associative strength goes down, its
;                  drug consumption rate should be lower than before in the phase where the drug was first introduced.
;-------------------------------------------------------------------------------------------------------------

  to switch_on

    ifelse Addiction_Cycle_Phase = "No Drug" [
      if any? drugs[ask drugs [die]]
      create-drugs 6[
        set shape "circle"
        set size 1
        set color drug-color
        setxy random-xcor random-ycor
        set odor_drug 0.5
      ]
      set Addiction_Cycle_Phase "Drug Introduced"
    ][

    ifelse Addiction_Cycle_Phase = "Drug Introduced" [
      if any? drugs[ask drugs [die]]
      set Addiction_Cycle_Phase "Drug Removed"
    ][

    if Addiction_Cycle_Phase = "Drug Removed" [
      if any? drugs[ask drugs [die]]
      create-drugs 6[
        set shape "circle"
        set size 1
        set color drug-color
        setxy random-xcor random-ycor
        set odor_drug 0.5
      ]
      ask Cslugs [ set drug_reward 0]
      set Addiction_Cycle_Phase "Drug Without Reward"
;    ][
;
;    if Addiction_Cycle_Phase = "Drug Removed" [
;      ask Cslugs [ set drug_reward 0]
;      set Addiction_Cycle_Phase "Drug Without Reward"
;    ]
   ]
  ]
 ]


end


;=======================================================================================================
;             Reset to Default Settings (Function)
;=======================================================================================================
;                All adjustable sliders and controls on the interface are reset to their default setting.
;-------------------------------------------------------------------------------------------------------
  to reset
    Setup
    set Presentation-Mode false
    set immobilize false
    set Presentation-Choice "None"
    set Feeding-Choice "Drug"
    set fix-RewardExp 0
    set fix-satiation 0.5
    set fix-Incentive 3
    set Fix-var1: false
    set Fix-var2: false
    set Fix-var3: false
    set Addiction_Cycle false
    set hermi-populate 4
    set flab-populate 4
    set drug-populate 4
    set apply_pain 10

  end
@#$#@#$#@
GRAPHICS-WINDOW
211
10
732
543
51
50
4.9703
1
10
1
1
1
0
1
1
1
-51
51
-50
50
1
1
1
ticks
30.0

BUTTON
5
10
72
43
SETUP
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
136
10
199
43
GO
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
71
10
137
43
STEP
Go
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
841
102
919
147
Nutrition
[nutrition] of Cslug 0
2
1
11

MONITOR
816
56
889
101
Satiation
[satiation] of Cslug 0
2
1
11

MONITOR
1135
296
1191
341
sns_betn
[sns_betaine] of Cslug 0
2
1
11

MONITOR
978
341
1078
386
V_Hermi (Learning)
[Vh] of Cslug 0
3
1
11

MONITOR
739
56
814
101
Incentive
[Incentive] of Cslug 0
2
1
11

MONITOR
1031
107
1147
152
Somatic Map
[Somatic_Map] of Cslug 0
2
1
11

MONITOR
978
384
1078
429
V_Flab (Learning)
[Vf] of Cslug 0
3
1
11

MONITOR
1027
10
1146
55
App. State Switch
[App_State_Switch] of Cslug 0
2
1
11

MONITOR
738
102
788
147
Exp R+
[ExpReward_pos] of Cslug 0
2
1
11

MONITOR
1082
341
1138
386
sns_hermi
[sns_hermi] of Cslug 0
2
1
11

MONITOR
789
102
840
147
Exp R-
[ExpReward_neg] of Cslug 0
2
1
11

SLIDER
19
104
191
137
flab-populate
flab-populate
0
15
4
1
1
NIL
HORIZONTAL

SLIDER
19
71
191
104
hermi-populate
hermi-populate
0
15
4
1
1
NIL
HORIZONTAL

MONITOR
1081
386
1138
431
sns_flab
[sns_flab] of Cslug 0
2
1
11

SLIDER
19
136
191
169
drug-populate
drug-populate
0
15
4
1
1
NIL
HORIZONTAL

MONITOR
1082
296
1135
341
sns_drug
[sns_drug] of Cslug 0
4
1
11

MONITOR
978
297
1078
342
V_Drug (Learning)
[Vd] of Cslug 0
3
1
11

PLOT
742
435
963
567
Reward Experience
NIL
NIL
0.0
10.0
0.0
10.5
true
false
"" ""
PENS
"pen-1" 1.0 0 -13345367 true "" "plot [RewardExperience] of Cslug 0;plot [ExpReward + ExpReward_neg] of Cslug 0"
"pen-2" 1.0 0 -5298144 true "" ";plot [M - M0] of Cslug 0"

PLOT
742
293
963
427
Incentive (Expected Reward)
NIL
NIL
0.0
10.0
0.0
10.5
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot [Incentive] of Cslug 0"

MONITOR
977
478
1076
523
R_Hermi
[R_hermi] of Cslug 0
3
1
11

MONITOR
1079
448
1185
493
Reward Input
[R] of Cslug 0
3
1
11

MONITOR
890
56
1000
101
Reward Experience
[RewardExperience] of Cslug 0
3
1
11

SWITCH
9
548
204
581
Immobilize
Immobilize
1
1
-1000

MONITOR
739
10
1025
55
Appetitve State
[App_State] of Cslug 0
4
1
11

SWITCH
8
413
204
446
Presentation-Mode
Presentation-Mode
1
1
-1000

CHOOSER
66
447
204
492
Presentation-Choice
Presentation-Choice
"None" "Drug" "Hermi" "Flab" "Drug vs Hermi" "Drug vs Flab" "Hermi vs Flab"
0

BUTTON
8
446
64
492
NIL
Present
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
97
289
207
322
fix-Satiation
fix-Satiation
0.1
1
0.5
0.1
1
NIL
HORIZONTAL

CHOOSER
66
501
204
546
Feeding-Choice
Feeding-Choice
"Drug" "Hermi" "Flab"
0

BUTTON
9
501
64
546
NIL
Feed
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
2
289
97
322
Fix-var1:
Fix-var1:
1
1
-1000

SLIDER
97
321
207
354
fix-RewardExp
fix-RewardExp
-20
20
0
1
1
NIL
HORIZONTAL

SWITCH
2
321
97
354
Fix-var2:
Fix-var2:
1
1
-1000

BUTTON
12
194
101
227
NIL
Poke-Left
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
102
194
190
227
NIL
Poke-Right
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
1137
341
1191
386
sns-pain
[sns-pain] of Cslug 0
4
1
11

SLIDER
12
227
190
260
Apply_Pain
Apply_Pain
0
30
10
5
1
NIL
HORIZONTAL

MONITOR
1076
56
1145
101
Pain
[pain] of Cslug 0
4
1
11

MONITOR
977
434
1075
479
R_Drug
[R_drug] of Cslug 0
4
1
11

MONITOR
1000
56
1075
101
Pain Switch
[pain-switch] of Cslug 0
3
1
11

MONITOR
977
522
1076
567
R_Flab
[R_flab] of Cslug 0
3
1
11

MONITOR
1079
495
1185
540
Neuron M Response
[M] of Cslug 0
3
1
11

TEXTBOX
21
54
194
72
Prey and Drug Population Controls:\n
11
23.0
1

TEXTBOX
41
179
181
197
Pain Application Controls:
11
23.0
1

TEXTBOX
51
275
171
293
Fixation of Variables:
11
23.0
1

TEXTBOX
35
399
198
417
Presentation Mode Controls:
11
23.0
1

SWITCH
1
353
97
386
Fix-var3:
Fix-var3:
1
1
-1000

SLIDER
97
353
207
386
fix-Incentive
fix-Incentive
0
10
3
1
1
NIL
HORIZONTAL

SWITCH
1020
178
1147
211
Addiction_Cycle
Addiction_Cycle
1
1
-1000

MONITOR
1020
211
1147
256
NIL
Addiction_Cycle_Phase
3
1
11

PLOT
741
156
1005
283
Total Prey and Drug Eaten
NIL
NIL
0.0
10.0
0.0
10.5
true
true
"" ""
PENS
"Drug" 1.0 0 -4079321 true "" "plot [drugcount] of Cslug 0"
"Hermi" 1.0 0 -11221820 true "" "plot [hermcount] of Cslug 0"
"Flab" 1.0 0 -4757638 true "" "plot [flabcount] of Cslug 0"

TEXTBOX
1015
161
1136
179
Addiction Cycle Controls:
11
23.0
1

BUTTON
212
546
732
583
RESET TO DEFAULT SETTINGS
Reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1
488
215
506
------------------------------------------
13
122.0
1

@#$#@#$#@
## WHAT IS IT?

ASIMOV (Algorithm of Selectivity based on Incentive, Motivation and Optimized Valuation) is built onto the Cyberslug™ software, which reproduces the decisions for approach or avoidance in the predatory sea-slug _Pleurobranchaea californica_. It applies relations discovered in the nervous system of the real animal that underlie decisions in foraging for prey. The approach-avoidance decision is basic to foraging as well as most other economic behavior. ASIMOV adds specific algorithms for homeostatic plasticity in a neural circuit of reward experience and for pain to recreate the hallmark processes of addiction: dependency, desensitization, withdrawal, resensitization and prolonged cravings. Fluctuations in reward experience, caused by drug consumption and homeostatic plasticity, cause significant changes in nutritional intake and pain sensitivity throughout the addiction process.

Homeostatic plasticity is the quality that gives addictive processes their pathological increases in reward seeking and makes withdrawal an unpleasant process. In the neural circuits of the brain, homeostatic plasticity is the tendency for the circuits' excitation states to slowly return to a certain range of values when altered for periods of hours to days. This causes reward effects of drug consumption to decrease over time.

A useful reference:

Brown JW, Caetano-Anollés D, Catanho M, Gribkova E, Ryckman N, Tian K, Voloshin M, and Gillette R. (2018). Implementing goal-directed foraging decisions of a simpler nervous system in simulation. _eNeuro_, ENEURO-0400.



## HOW IT WORKS

Approach-avoidance choice is organized around how the ASIMOV agent feels in terms of hunger, positive and negative rewards that it receives, the savory qualities of prey odor, pain sensation, and what it remembers about earlier experience with that prey. The agent adds up sensation, motivation (satiation/hunger), reward experience, pain, and memory from moment-to-moment into its appetitive state. Appetitive state controls the switch for approach vs. avoidance turn responses to prey.

In ASIMOV, reward experience integrates rewards from prey and drug consumption, and via homeostatic plasticity mechanisms, mediates habituation to rewarding cues, which is the basis of effects of drug desensitization and withdrawal. Reward experience is in in reciprocal inhibition with pain, and both pain and reward experience separately provide negative feedback to appetitive state.


The difference of odor sensation at two sensors on the ASIMOV agent's head is used to calculate the probable location of prey or drug for the turn response. The sensors respond to betaine, an odor representing the energy value of the prey (like the taste of sugar to the human tongue), and to the learned identifying odors of Hermi, Flab, and drug. Likewise, two similarly placed pain sensors are used to calculate the probable location of painful stimuli that are applied to the agent.


As ASIMOV's forager consumes prey and drug, it learns to associate the odor of the specific item with the positive or negative reward that it receives. For instance, Hermis, pictured as green dots on the graphic interface, are valuable prey that give a positive reward when consumed, Flabs, pictured as red dots, are a noxious prey that give the forager a negative reward when consumed, while the drug, pictured as yellow dots, is an addictive substance that gives the forager a much larger positive reward. Therefore, through ongoing prey consumption, the agent can learn to associate Hermi odor and drug odor with positive reward and Flab odor with negative reward. These associations are established via Rescorla-Wagner learning rules, and play a part in setting the forager's appetitive state, which also relies on the forager's satiation and sensation of prey odor.


Note that at very high satiation, ASIMOV's agent does not approach prey, as would be expected in a natural setting. Also, even when the agent has learned that the prey, Hermis and Flabs, are respectively valuable and noxious, it can still approach and consume the normally undesirable Flabs at very low satiation, since it is in dire need of nutrition.


Drug effects on reward experience alter sensitivity to pain. In drug withdrawal, which manifests as a negative reward experience, the effects of pain and hunger are increased, causing ASIMOV's forager to show less selectivity towards prey of different values. In high reward experiences, which often immediately follow drug consumption, pain sensitivity is reduced and nutritional needs are ignored in favor of drug ingestion, leading to increased selectivity for high reward prey, and particularly for drug. While fluctuations in reward experience contribute to increased drug consumption, the primary driver for high drug selectivity is the learned association between drug odor signature and high reward. This creates strong preference, or cravings, for the drug, so that even after recovery from withdrawal, cravings remain, and the forager unsurprisingly resumes drug consumption when available.

## HOW TO USE IT

The program is set to run for 60,000 software cycles (ticks). This can be changed in the code.


In the middle environment panel, the user can manipulate the positions of the forager and prey by clicking and dragging them around the environment. Everything, including the forager, and all prey and drugs, can also be immobilized by clicking on the Immobilize switch at the bottom left side. This option still allows for the agent's turning behavior, but not forward movement. There is a button at the bottom middle, which resets all variables to their default values.


On the right hand side, there are several tabs and graphs that monitor the values of certain ASIMOV variables. Progress of learning is shown in the interface tabs V_drug, V_hermi, and V_flab. Other tabs show important quantities used in calculating the decision: the nutritional and satiation states, summed appetitive state (App_State), and the positive and negative rewards and incentives sensed for prey. There is also a tab for the ASIMOV agent's estimate of the odor source (Somatic_Map). Other tabs on the left show the averaged strengths of the three odors sensed at the two sensors (sns_odor). There is one control, for the Addiction Cycle:

**Addiction Cycle Control**
Addiction Cycle Mode, when enabled, allows the user to observe ASIMOV's forager as it freely forages and experiences different phases of the addiction processes of desensitization, withdrawal, and cravings in a dynamic environment. In this mode the availability of drug changes over time, starting with an environment with only prey and no drug, and then adding and removing the drug, causing the forager to go through desensitization, withdrawal, and cravings. In the last phase, the drug is present with the drug odor signature but does not provide any reward on consumption. In this phase, drug consumption decreases significantly (see Fig. 4 for results from the Addiction Cycle Mode). The Addiction Cycle Mode lasts to 60000 software cycles (ticks), in which each phase lasts for 15000 ticks. During an initial “No Drug” phase, the environment contains only the prey Hermissenda and/or Flabellina, letting the predator forage and learn the corresponding associations. In the second “Drug Introduced” phase, the drug is introduced for the first time with a high reward on consumption. The third phase is “Drug Removed” and the fourth and last phase is “Drug without Reward”, where the drug is reintroduced with the same odor signature, but provides no reward on consumption, causing the forager to decrease its associative strength for the drug via a Rescorla-Wagner algorithm for extinction.



_There are several types of controls on the left-hand side that the user can manipulate:_

**Prey and Drug Population Controls**
The user can occupy the world with valuable and dangerous prey, Hermis and Flabs respectively, as well as a non-nutritious high-reward Drug with the Prey and Drug Population Control slider bars. In the beginning the simulation starts with 4 Drugs, 4 Hermis and 4 Flabs. ASIMOV's forager learns to prefer or avoid the different prey and may learn an increased preference towards the drug.

**Pain Application Controls**
Using the Pain Application Controls, the user can also apply a painful poke at the top-left or top-right of the forager via the Poke-Left or Poke-Right buttons. The severity of this painful stimulus can be controlled via the apply_pain slider.

**Fixation of Variables**
To observe the behavior of ASIMOV's forager when certain variables are held constant, the user can use the Fixation of Variables controls. The three variables that can be fixed are Satiation, Reward Experience, and Incentive. To fix the desired variable at a certain value, turn the corresponding Fix-var switch to ON, and set the corresponding slider to the desired value.

**Presentation Mode**
Presentation Mode is a useful tool to control the forager's food and drug intake, as well as to monitor its approach and avoidance behavior with certain prey and drug. Presentation Mode was used in combination with Fixation of variables to construct prey and drug response maps, as well as the pain response map as seen in the ASIMOV paper.
In Presentation Mode, ASIMOV's forager is immobilized, except for turning, and can be fed or
presented with prey or drug, to monitor approach-avoidance turns. To start using presentation mode, set Presentation-Mode to ON, and click on the Present button.


To present prey or drug, choose prey or drug type from the Presentation-Choice drop-down menu, then click on the Present button. This will spawn the selected item on the left side of the forager, which may cause it to turn away or towards the item.

To feed ASIMOV's forager, choose the desired prey or drug type from the Feeding-Choice drop-down menu, then click on the Feed button. This will spawn the selected item right at its mouth, effectively feeding it the item.





## THINGS TO NOTICE

What happens to approach-avoidance decision when the forager is not hungry? What happens to decision about the noxious Flab prey when the forager is very hungry?
What happens if the Addiction Cycle is started with 6 Flabellina and no Hermissenda, versus when it is started with no Flabellina and 6 Hermissenda? How does drug consumption change in these cases?



## THINGS TO TRY

What can explain the different susceptibilities to addiction in different individuals? Some individuals are significantly more likely than others to develop severe addictions, and experience more or less difficulties in withdrawal and long-term cravings. You may test effects of changing values of reward and pain, of their interactions and their effects on appetitive state. You may also change values of the homeostatic algorithm, and any others. The results may show you the relative importance of the variables, and how complicated the individual differences may be. In real human populations the ranges of the different variables' values may be influenced by either heredity or experience, or both. Perhaps treatments for addiction be customized for individuals based on the ranges of the variables. How could that be approached clinically?



## A CHALLENGE!
Here we pose a challenge to the user: can you incorporate homeostatic plasticity into the pain system, just as we did with Reward Experience and the Homeostatic Reward Circuit? What would be the consequences? How would consumption of certain types of drug, say heroin, which directly suppresses the pain system, and cocaine, which does not, affect Reward Experience? Would you expect heroin to produce a more severe and painful withdrawal than cocaine?


Recommended Soundtracks: Jane Says, by Jane's Addiction; Heroin, by Velvet Underground; Sympathy for the Devil, by the Rolling Stones
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

cslug
true
0
Polygon -7500403 true true 135 285 165 285 210 240 240 165 225 105 210 90 195 75 105 75 90 90 75 105 60 165 90 240
Polygon -7500403 true true 150 60 240 60 210 105 90 105 60 60
Polygon -7500403 true true 195 120 255 90 195 90
Polygon -7500403 true true 105 120 45 90 105 90

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

monster
false
0
Polygon -7500403 true true 75 150 90 195 210 195 225 150 255 120 255 45 180 0 120 0 45 45 45 120
Circle -16777216 true false 165 60 60
Circle -16777216 true false 75 60 60
Polygon -7500403 true true 225 150 285 195 285 285 255 300 255 210 180 165
Polygon -7500403 true true 75 150 15 195 15 285 45 300 45 210 120 165
Polygon -7500403 true true 210 210 225 285 195 285 165 165
Polygon -7500403 true true 90 210 75 285 105 285 135 165
Rectangle -7500403 true true 135 165 165 270

octo
true
0
Polygon -7500403 true true 75 165 90 105 210 105 225 165 255 180 255 255 180 300 120 300 45 255 45 180
Polygon -7500403 true true 225 150 285 105 300 30 285 15 270 90 180 135
Polygon -7500403 true true 90 120 45 90 45 30 60 0 60 75 135 120
Polygon -7500403 true true 210 120 255 90 255 30 240 0 240 75 165 120
Polygon -7500403 true true 75 150 15 105 0 30 15 15 30 90 120 135
Circle -11221820 true false 75 180 60
Rectangle -16777216 true false 90 210 120 225
Circle -11221820 true false 165 180 60
Rectangle -16777216 true false 180 210 210 225
Polygon -7500403 true true 90 120 75 75 75 15 90 0 90 60 120 120
Polygon -7500403 true true 120 120 105 75 105 15 120 0 120 60 150 120
Polygon -7500403 true true 210 120 225 75 225 15 210 0 210 60 180 120
Polygon -7500403 true true 180 120 195 75 195 15 180 0 180 60 150 120

octopus
true
0
Polygon -7500403 true true 75 150 90 195 210 195 225 150 255 120 255 45 180 0 120 0 45 45 45 120
Circle -2064490 true false 165 60 60
Circle -2064490 true false 75 60 60
Polygon -7500403 true true 225 150 285 195 285 285 270 300 270 210 180 165
Rectangle -16777216 true false 90 75 120 90
Rectangle -16777216 true false 180 75 210 90
Polygon -7500403 true true 90 180 45 210 45 270 60 285 60 225 135 180
Polygon -7500403 true true 210 180 255 210 255 270 240 285 240 225 165 180
Polygon -7500403 true true 75 150 15 195 15 285 30 300 30 210 120 165
Polygon -7500403 true true 150 195 180 285 195 270 180 225 180 180
Polygon -7500403 true true 210 195 225 300 210 285 195 225 165 180
Polygon -7500403 true true 150 195 120 285 105 270 120 225 120 180
Polygon -7500403 true true 90 195 75 300 90 285 105 225 135 180

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

pleuro
true
0
Polygon -7500403 true true 135 285 165 285 210 240 240 165 225 105 210 90 195 75 105 75 90 90 75 105 60 165 90 240
Polygon -7500403 true true 150 60 240 60 210 105 90 105 60 60
Polygon -7500403 true true 195 120 255 90 195 90
Polygon -7500403 true true 105 120 45 90 105 90

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
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
