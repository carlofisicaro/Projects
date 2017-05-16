
breed [rdFirms rdFirm]
breed [productFirms productFirm]
breed [laborForce laborForceUnit]
breed [banks bank]
breed [centralBanks centralBank]
undirected-link-breed [workRelations workRelation]
undirected-link-breed [supplyRelations supplyRelation]
undirected-link-breed [liquidAssetRelations liquidAssetRelation]
undirected-link-breed [centralBankRelations centralBankRelation]

globals [I J N P nu zeta alpha beta gamma mu s omega governmentAssets fi delta eta1 eta2 eta3 eta4 varpi re_0 numberOfInstallments n_loans deposit_savings_in_aBank loanGuaranteesRequired totalProduction]
rdFirms-own [technology liquidAssets inventories  pastSales unitProductionCost workersNeed production debt loan satisfaction]
productFirms-own [liquidAssets machines tickOfMachines  goodsList pastSales workersNeed production debt loan satisfaction]
laborForce-own [employed liquidAssets previousStepLiquidAssets expenditure debt loan satisfaction]
banks-own [liquidAssets deposits debt centralBankDebt loan lead_bank centralBankLoan]
centralBanks-own [deposits]

to setup

clear-all
random-seed Seed


set I HowManyRdFirms
set J HowManyProductFirms
set N HowManyWorkersConsumers
set P HowManyBanks
set nu InvestmentInRD ;fraction of liquid assets in R&D
set zeta ChanceNewDiscovery ;chance of a new discovery. The highest it is a new discovery is more likely.
set alpha 1.2 ;technological development
set beta 1 / TechnologicalDevelopment ;technological development
set gamma 0.5 ;rate of imperfect information. gamma = 1 best information
set mu Markup ;markup of both R&D and product Firms
set s SavingRate ;saving rate of consumers
set omega FlexibilityLabour ;flexibility of labour market. If 1 workers can be freely fired
set governmentAssets 0
set fi TaxesLevel ;level of taxation
set delta SpendingInUnemploymentAid ;how much of governmentAssets is spent in unemployment subsidies. 1 - delta is spent in buying goods.
set eta1 interestRateRestitution ;interest rate fixed by commercial banks if debitors have to return a loan to a bank
set eta2 CBRateRestitution ;interest rate fixed by centralBank if debitors banks have to return a loan to the centralBank
set eta3 interestRateDeposit ;interest rate fixed by commercial banks if laborForce want to deposit money to a bank
set eta4 CBRateDeposit ;;interest rate fixed by centralBank if banks want to deposit money to the centralBank
set varpi savings_in_aBank_probability
set re_0 bankReserveRatio ;complusory reserve coefficient. Infact we approximate re = re_0 + reL as re = re_0, which implies that in our simulation reL = 0
set numberOfInstallments 10
set n_loans 0
set totalProduction 0

create-rdFirms I [set shape "building store"
                   set color white
                   setxy random-pxcor random-pycor
                   set technology [1 1]
                   set liquidAssets 100
                   set inventories 0
                   set pastSales 0
                   set unitProductionCost 1
                   set workersNeed 1
                   set production 0
                   set debt 0
                   set loan 0
                   set satisfaction 0
                   ]

create-productFirms J [  set shape "factory"
                         set color yellow
                         setxy random-pxcor random-pycor
                         repeat 3 [create-supplyRelation-with one-of rdFirms [set color blue
                                                                              set hidden? true]]
                         set liquidAssets 100
                         set machines []
                         set tickOfMachines []
                         set workersNeed 1
                         set pastSales 0
                         set goodsList n-values 10 [1]
                         set production 0
                         set debt 0
                         set loan 0
                         set satisfaction 0
                      ]

create-laborForce N [set shape "person"
                     setxy random-pxcor random-pycor
                     set employed false
                     set liquidAssets 100
                     set previousStepLiquidAssets 0
                     set expenditure 0
                     set debt 0
                     set loan 0
                     set satisfaction 0
                    ]

create-banks P [set shape "house"
                set color pink
                setxy random-pxcor random-pycor
                set liquidAssets 0
                set deposits 0
                set debt 0
                set loan 0
                set centralBankLoan 0
                set centralBankDebt 0
                set lead_bank false
                set size 2
               ]

create-centralBanks 1 [set shape "house"
                       set color brown
                       set size 5
                       setxy min-pxcor + 3 max-pycor - 3
                       set deposits 0
                      ]

initial-hire

reset-ticks
end



to go
  if ticks >= 1000 [stop]
  if ticks > 10 and totalProduction < 2 [stop]
  performResearch
  advertiseMachines
  hireAndFire
  produceMachines
  buyMachines
  produceGoods
  buyGoods
  publicSpend
  grantLoan
  depositMoneyToCentralBank
  obtainMoneyByCentralBank
  if turbulence? [move-customers]
  if followLeadBankStrategy? [move-banks]
  ;print totalProduction
  tick
end



;Procedures
;___________________________________________________________________________________________________________________________________
;___________________________________________________________________________________________________________________________________
;___________________________________________________________________________________________________________________________________


to initial-hire
ask (turtle-set rdFirms productFirms) [let aWorker one-of laborForce with [employed = false]
                                       create-workRelation-with aWorker [set hidden? true]
                                       ask aWorker [set employed true set color green]
                                       ]
ask laborForce with [employed = false] [set color  red]
end





to performResearch
  ask rdFirms [let rdfunds nu * liquidAssets
               set liquidAssets liquidAssets - rdfunds
               let workers count workRelation-neighbors
               ask workRelation-neighbors [set liquidAssets (liquidAssets + rdfunds / workers )]
               let binomialparameter 1 - e ^(- zeta * rdfunds)
               if random-float 1 < binomialparameter [
                   set technology replace-item 0 technology (item 0 technology * (1 + random-gamma alpha beta))
                   set technology replace-item 1 technology (item 1 technology * (1 + random-gamma alpha beta))
                   set unitProductionCost 1 / (item 1 technology)
                 ]

    ]
end



to advertiseMachines
  ask rdFirms [repeat (gamma * (count supplyRelation-neighbors) + 1) [
           let newProductFirms (complement supplyRelation-neighbors productFirms)
           if any? newProductFirms [create-supplyRelation-with one-of newProductFirms [set color blue
                                                                                       set hidden? true]]
      ]
     ]
end



to hireAndFire

  let meanworkersNeed mean ([workersNeed] of (turtle-set rdFirms productFirms))
  ask (turtle-set rdFirms productFirms) [
                                                 let normalizedWorkersNeed floor (workersNeed / meanworkersNeed * (N / (I + J) - 2))
                                                 if (workersNeed < 3 or meanworkersNeed < 1) [set normalizedWorkersNeed floor workersNeed]

                                                 if count workRelation-neighbors < normalizedWorkersNeed [
                                                      repeat (normalizedWorkersNeed - count workRelation-neighbors) [
                                                         if any? laborForce with [employed = false] [
                                                           let aWorker one-of laborForce with [employed = false]
                                                           create-workRelation-with aWorker [set hidden? true]
                                                           ask aWorker [set employed true set color green]]]]

                                                 if count workRelation-neighbors > normalizedWorkersNeed [
                                                      repeat (count workRelation-neighbors - normalizedWorkersNeed) [
                                                        if random-float 1 < omega and count workRelation-neighbors > 1 [
                                                           let aWorker one-of workRelation-neighbors
                                                           ask my-workRelations with [end2 = aWorker] [die]
                                                           ask aWorker [set employed false set color red]]]]

                                         ]
end


to produceMachines
  ask rdFirms [
               ifelse (ticks = 0) [let costInitialProduction (J / I) * unitProductionCost
                                   set liquidAssets liquidAssets - costInitialProduction
                                   ask workRelation-neighbors [set liquidAssets (liquidAssets + (costInitialProduction / (count workRelation-neighbors))*(1 - fi) )
                                                               set governmentAssets governmentAssets + fi * costInitialProduction]
                                   set inventories inventories + floor(J / I)]

                                [let desiredProduction max list (pastSales + 5) ceiling (pastSales * (1 + 0.05))
                                 ifelse liquidAssets > desiredProduction * unitProductionCost
                                                                       [ set production desiredProduction]
                                                                       [ set production floor (liquidAssets / unitProductionCost)]
                                 set workersNeed production * unitProductionCost
                                 let costProduction production * unitProductionCost

                                  set liquidAssets liquidAssets - costProduction
                                  set inventories inventories + production
                                  let workers count workRelation-neighbors
                                  ask workRelation-neighbors [set liquidAssets (liquidAssets + (costProduction / workers)*(1 - fi) )]
                                  set governmentAssets (governmentAssets + fi * costProduction)]

               set pastSales 0


  ]
end







to buyMachines
   ask productFirms [if (any? supplyRelation-neighbors with [inventories > 0]) [ ;this check prevents an error occurring if productFirm does not have any link with any rdFirm, which may occur if it did not receive any piece of advertisement

                                               let k max list (pastSales + 5) ceiling (pastSales * (1 + 0.05)) ;the investment of the productFirm depends on its past sales
                                               let bestrdFirms sort-by [[1 / (item 0 technology) + unitProductionCost] of ?1 < [1 / (item 0 technology) + unitProductionCost] of ?2 ] supplyRelation-neighbors with [inventories > 0] ;il primo elemento sarà la rdFirm con la tecnologia migliore
                                               let sellingrdFirms []
                                               let investment liquidAssets * mu / 2 ;if investment decisions are not correlated to markup, the economy soon collapses
                                               let m 0
                                               let mincost [unitProductionCost] of min-one-of supplyRelation-neighbors with [inventories > 0] [unitProductionCost]

                                               while [k > m and (investment > (1 + mu) * mincost) and (any? supplyRelation-neighbors with [inventories > 0])] [
                                                         set mincost [unitProductionCost] of min-one-of supplyRelation-neighbors with [inventories > 0] [unitProductionCost]
                                                         let listposition floor random-gamma 1.2 0.66 ; CDF: 0 (0.39) 1 (0.66) 2 (0.81) 3 (0.90). It may not choose the best rdFirm.
                                                         ;It is reasonable to increase the best rdFirm probability by adding up the cases when the draw from the gamma distribution is out of the list
                                                         if listposition >= (length bestrdFirms) [set listposition 0]

                                                         ;the machine sold by the chosen rdFirm is added to the list, provided that rdFirm has enough machines
                                                         if ([inventories] of (item listposition bestrdFirms) > 0) and
                                                            (investment > (1 + mu) * ([unitProductionCost] of (item listposition bestrdFirms))) [
                                                             set machines lput ([item 0 technology] of (item listposition bestrdFirms)) machines
                                                             set tickOfMachines lput ticks tickOfMachines ;this variable is to track when machines were bought
                                                             set liquidAssets liquidAssets - (1 + mu) * ([unitProductionCost] of (item listposition bestrdFirms))
                                                             set investment investment - (1 + mu) * ([unitProductionCost] of (item listposition bestrdFirms))
                                                             set m m + 1
                                                             ;the rdFirm which sold the machine is added to the list of the firms which sold at least one machine
                                                             set sellingrdFirms lput (item listposition bestrdFirms) sellingrdFirms
                                                             ask (item listposition bestrdFirms) [set inventories inventories - 1
                                                                                                  set pastSales pastSales + 1
                                                                                                  set liquidAssets liquidAssets + unitProductionCost + (1 - fi ) * mu * unitProductionCost
                                                                                                  set governmentAssets governmentAssets + fi * mu * unitProductionCost]
                                                             ]
                                                         ]
                                               ;if there was a link between the productFirm which is running and an rdFirm it did not buy any machine from, it is deleted

                                               let sellingrdFirmsAgentset turtle-set sellingrdFirms
                                               ask my-supplyRelations with [not(member? end1 sellingrdFirmsAgentset or member? end2 sellingrdFirmsAgentset)] [die]
   ]

   ;remove outdated machines
   while [(not empty? tickOfMachines) and (ticks - (first tickOfMachines) > 20)] [ set tickOfMachines but-first tickOfMachines
                                                                                   set machines but-first machines]
   ]



end



to produceGoods
  ask productFirms [
         let desiredQuantity (10 + pastSales + pastSales / 10 - length goodsList)
         let actualQuantity 0
         set workersNeed 0
         set production 0
         foreach machines [if ((liquidAssets > 1 / ? ) and (actualQuantity < desiredQuantity)) [set production production + 1
                                                                                                set liquidAssets liquidAssets - 1 / ?
                                                                                                set goodsList lput ( 1 / ? ) goodsList
                                                                                                set workersNeed workersNeed +  1 / ?
                                                                                                let workers count workRelation-neighbors
                                                                                                ask workRelation-neighbors [set liquidAssets (liquidAssets + 1 / ? / workers * (1 - fi) )]
                                                                                                set governmentAssets (governmentAssets + fi * 1 / ?) ]]
         set pastSales 0
  ]
  set totalProduction sum [production] of productFirms
end



;There was a simple way to build this procedure: to let buyers buy as much as they can, given their expenditure during that time step, while updating their liquid assets and the features
;of the firms they deal with, for any good they buy. When buyers run out of their expenditure, they stop buying. However I found out this procedure terribly slowed down the program.
;My solution was that first buyers understand how many goods they can buy, and then they buy them all at once. This way the program substantially sped up.
to buyGoods
  ask laborForce [ifelse employed = true [let salary liquidAssets - previousStepLiquidAssets
                                          set expenditure (1 - s) * salary
                                          if random-float 1 < varpi [let nearestBank min-one-of banks [distance myself]     ;probability that aWorker deposits money in aBank
                                                                  let aBank one-of banks
                                                                  every ticks [set liquidAssets liquidAssets + (s * salary) * eta3] ;interest given by aBank if a laborForceUnit deposit money
                                                                  ifelse random 10 < customersRationalityLevel [ask nearestBank [set liquidAssets liquidAssets + s * salary
                                                                                                                                 set deposits deposits + s * salary
                                                                                                                                 every ticks [set liquidAssets liquidAssets - (s * salary) * eta3
                                                                                                                                              set governmentAssets governmentAssets + (s * salary) * eta3 * fi ;taxation on financial return
                                                                                                                                              set liquidAssets liquidAssets - (s * salary) * eta3 * fi]]]
                                                                                                               [ask aBank [set liquidAssets liquidAssets + s * salary
                                                                                                                           set deposits deposits + s * salary
                                                                                                                           every ticks [set liquidAssets liquidAssets - (s * salary) * eta3
                                                                                                                                        set governmentAssets governmentAssets + (s * salary) * eta3 * fi
                                                                                                                                        set liquidAssets liquidAssets - (s * salary) * eta3 * fi]]]]]
                                         [set expenditure liquidAssets / 2 ]

                   let knownProductFirms n-of (J / 10) productFirms

                   while [any? (knownProductFirms with [not empty? goodsList])] [

                     let chosenProductFirm max-one-of (knownProductFirms with [not empty? goodsList]) [mean goodsList] ;this is the best mean price
                     let localgoodsList sort ([goodsList] of chosenProductFirm)

                     ifelse (1 + mu)*(sum localgoodsList) <= expenditure [;if this condition is satisfied, all goods of the productFirm are bought all at once
                       set liquidAssets liquidAssets - (1 + mu) * (sum localgoodsList)
                       set expenditure expenditure - (1 + mu) * (sum localgoodsList)
                       ask chosenProductFirm [
                         set pastSales pastSales + length goodsList
                         set liquidAssets liquidAssets + sum goodsList + (1 - fi)* mu * (sum goodsList)
                         set governmentAssets governmentAssets + fi * mu * (sum goodsList)
                         set goodsList []   ]]

                     [let temp 0 let m 0 ;in this case the consumer has to find out how many goods he can afford
                       while [expenditure > temp and
                         not empty? localgoodsList] [set temp temp + (1 + mu) * (first localgoodsList)
                         set localgoodsList but-first localgoodsList
                         set m m + 1]
                       if m = 1 or m = 0 [stop] ;it means the consumer has no money to buy the cheapest good of the productFirm with the lowest mean price
                                                ;once the number of goods the consumer can afford is established, m cheapest goods are bought
                       set liquidAssets liquidAssets - (1 + mu) * (sum (sublist (sort([goodsList] of chosenProductFirm)) 0 (m - 1)))
                       set expenditure expenditure - (1 + mu) * (sum (sublist (sort([goodsList] of chosenProductFirm)) 0 (m - 1)))
                       ask chosenProductFirm [
                         set pastSales pastSales + m
                         set liquidAssets liquidAssets + (sum (sublist (sort([goodsList] of chosenProductFirm)) 0 (m - 1))) + (1 - fi)* mu * (sum (sublist (sort([goodsList] of chosenProductFirm)) 0 (m - 1)))
                         set governmentAssets governmentAssets + fi * mu *(sum (sublist (sort([goodsList] of chosenProductFirm)) 0 (m - 1)))
                         set goodsList sublist (sort(goodsList)) m (length goodsList)
                       ]]

                   ]


      set previousStepLiquidAssets liquidAssets
      ]
end




to publicSpend
  let unemploymentSubsidies delta * governmentAssets
  let maximumSpendingInGoods (1 - delta) * governmentAssets

  set governmentAssets governmentAssets - unemploymentSubsidies
  let unemployedPeople count laborForce with [employed = false]
  ask laborForce with [employed = false] [set liquidAssets liquidAssets + unemploymentSubsidies / unemployedPeople]

  while [any? (productFirms with [not empty? goodsList])] [
      let chosenProductFirm one-of productFirms with [not empty? goodsList]
      let localgoodsList sort ([goodsList] of chosenProductFirm)
      ifelse maximumSpendingInGoods < (1 + mu) * (first localgoodsList) [stop]
                                                                        [set maximumSpendingInGoods maximumSpendingInGoods - (1 + mu) * (first localgoodsList)
                                                                         set governmentAssets governmentAssets -  (1 + mu) * (first localgoodsList)
                                                                         ask chosenProductFirm [
                                                                                             set pastSales pastSales + 1
                                                                                             set liquidAssets liquidAssets + (1 + mu) * (first localgoodsList)
                                                                                             set goodsList but-first goodsList
                                                                          ]]
  ]



end

to grantLoan
if ticks > 10 [
  ask (turtle-set rdFirms productFirms laborForce banks) [
    set loan 0.1 * liquidAssets
    let rdFirmsNeedersOfLoan n-of floor((count rdFirms with [(liquidAssets < mean ([liquidAssets] of rdFirms)) and debt = 0 and liquidAssets > loan + eta1 * loan]) * 0.5) rdFirms with [(liquidAssets < mean ([liquidAssets] of rdFirms)) and debt = 0 and liquidAssets > loan + eta1 * loan]
    let productFirmsNeedersOfLoan n-of floor((count productFirms with [(liquidAssets < mean ([liquidAssets] of productFirms)) and debt = 0 and liquidAssets > loan + eta1 * loan]) * 0.5) productFirms with [(liquidAssets < mean ([liquidAssets] of productFirms)) and debt = 0 and liquidAssets > loan + eta1 * loan]
    let laborForceNeedersOfLoan n-of floor((count laborForce with [(liquidAssets < mean ([liquidAssets] of laborForce)) and debt = 0 and liquidAssets > loan + eta1 * loan and employed = true]) * 0.5) laborForce with [(liquidAssets < mean ([liquidAssets] of laborForce)) and debt = 0 and liquidAssets > loan + eta1 * loan and employed = true]
    let banksNeedersOfLoan n-of floor((count banks with [(liquidAssets < mean ([liquidAssets] of banks)) and debt = 0 and liquidAssets > loan + eta1 * loan]) * 0.5) banks with [(liquidAssets < mean ([liquidAssets] of banks)) and debt = 0 and liquidAssets > loan + eta1 * loan]
    let needersOfLoan (union rdFirmsNeedersOfLoan productFirmsNeedersOfLoan laborForceNeedersOfLoan banksNeedersOfLoan)
        if any? needersOfLoan[
              ask needersOfLoan [ifelse random 10 < customersRationalityLevel [create-liquidAssetRelation-with min-one-of other banks [distance myself] [set color pink
                                                                                                                                                         set hidden? true]]
                                                                              [create-liquidAssetRelation-with one-of other banks [set color pink
                                                                                                                                   set hidden? true]]
                                 if breed != banks and satisfaction < satisfactionCutOff [ask my-liquidAssetRelations [die]
                                                                                          create-liquidAssetRelation-with min-one-of banks [distance myself] [set color pink
                                                                                                                                                              set hidden? true]]]
              let creditorBanks liquidAssetRelation-neighbors with [breed = banks]
              ask creditorBanks  [let bankReserve re_0 * deposits
                                  let pDebtorsOfaBank liquidAssetRelation-neighbors ;pDebtorsOfaBank = possible debitor of aBank
                                  if liquidAssets - bankReserve < sum [loan] of pDebtorsOfaBank and any? pDebtorsOfaBank
                                     [while [liquidAssets - bankReserve < sum [loan] of pDebtorsOfaBank]
                                            [ask one-of pDebtorsOfaBank [ask my-liquidAssetRelations[die]
                                                                         set pDebtorsOfaBank pDebtorsOfaBank with [self != myself]]]]
                                  let debtorsOfaBank pDebtorsOfaBank     ;these are the true debtors
                                  ask my-liquidAssetRelations [set hidden? false]
                                  set liquidAssets liquidAssets - sum [loan] of debtorsOfaBank
                                  ask debtorsOfaBank  [set liquidAssets liquidAssets + loan
                                                       set debt loan
                                                       every ticks [ifelse debt > 0[set liquidAssets liquidAssets - loan / numberOfInstallments - (loan / numberOfInstallments) * eta1
                                                                                    set debt debt - loan / numberOfInstallments]
                                                                                   [ask my-liquidAssetRelations [die]
                                                                                    set debtorsOfaBank debtorsOfaBank with [self != myself]]]]
                                  every ticks [set liquidAssets (liquidAssets + (sum [loan] of debtorsOfaBank) / numberOfInstallments + eta1 * (sum [loan] of debtorsOfaBank) / numberOfInstallments)]]]
    ]
   ask (turtle-set rdFirms productFirms laborForce) [ifelse any? liquidAssetRelation-neighbors [let neighborBankDistance distance one-of liquidAssetRelation-neighbors
                                                                                                ifelse neighborBankDistance > 0 [set satisfaction 1 / neighborBankDistance]
                                                                                                                                [set satisfaction 1]]
                                                                                               [set satisfaction 0]]]
end

to depositMoneyToCentralBank
   let RE sum [deposits] of banks
   ask banks [set deposits (1 - re_0) * deposits
              every ticks [set liquidAssets liquidAssets + deposits * re_0 * eta4
                           set governmentAssets governmentAssets + deposits * re_0 * eta4 * fi ;taxation on financial return
                           set liquidAssets liquidAssets - deposits * re_0 * eta4 * fi]]
   ask centralBanks [set deposits deposits + RE * re_0]
end

to obtainMoneyByCentralBank
  if ticks > 10 [
  ask centralBanks [let banksNeedersOfLoanByCentralBank n-of ceiling((count banks with [debt > 0 and liquidAssets > centralBankLoan + eta2 * centralBankLoan]) * 0.25) banks with [debt > 0 and liquidAssets > centralBankLoan + eta2 * centralBankLoan]
                    if random-float 1 < quantitativeEasing [
                       ask banksNeedersOfLoanByCentralBank [create-centralBankRelation-with one-of centralBanks [set color brown]]
                       let debtorsOfCentralBank centralBankRelation-neighbors with [breed = banks]
                       ask debtorsOfCentralBank  [set centralBankLoan liquidAssets * 0.1
                                                  set liquidAssets liquidAssets + centralBankLoan
                                                  set centralBankDebt centralBankDebt + centralBankLoan
                                                  every ticks [ifelse centralBankDebt > 0[set liquidAssets liquidAssets - centralBankLoan / numberOfInstallments - (centralBankLoan / numberOfInstallments) * eta2
                                                                                          set centralBankDebt centralBankDebt - centralBankLoan / numberOfInstallments]
                                                                                         [ask my-centralBankRelations [die]
                                                                                          set debtorsOfCentralBank debtorsOfCentralBank with [self != myself]]]]
                 ]]]
end

to move-customers
  ask (turtle-set rdFirms productFirms laborForce) [right random 360
                                                    forward 1]
end

to move-banks
  ask banks [ifelse liquidAssets + deposits = max[liquidAssets + deposits] of banks [set lead_bank true
                                                                                     set color blue]
                                                                                    [set lead_bank false
                                                                                     set color pink]]
  let leaderBank banks with [lead_bank = true]
  ask leaderBank [let leaderBankNearPatches patches in-radius 5
                  ask other banks [move-to one-of leaderBankNearPatches]]
end

to-report complement [A B] ;A,B must be agentsets. It returns the complement of A with respect to B, i.e. all elements of B which are not part of A
  let list-A sort A
  let list-B sort B
  report turtle-set filter [not member? ? list-A] list-B
end

to-report union [A B C D]  ;A,B,C,D must be agentsets. It returns the union of A,B,C,D
  let list-A sort A
  let list-B sort B
  let list-C sort C
  let list-D sort D
  report (turtle-set sentence list-A list-B list-C list-D)
end
@#$#@#$#@
GRAPHICS-WINDOW
226
10
737
542
17
17
14.33333333333334
1
10
1
1
1
0
0
0
1
-17
17
-17
17
1
1
1
ticks
30.0

BUTTON
35
15
98
48
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
122
13
185
46
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
733
10
1218
238
liquidAssets of firms and population
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"rdfirms" 1.0 0 -16777216 true "" "if ticks > 0 [plot mean ([liquidAssets] of rdFirms)]"
"productfirms" 1.0 0 -13840069 true "" "if ticks > 0 [plot mean ([liquidAssets] of productFirms)]"
"laborforce" 1.0 0 -2674135 true "" "if ticks > 0 [plot mean ([liquidAssets] of laborForce)]"
"mean" 1.0 0 -13791810 true "" "if ticks > 0 [plot mean ([liquidAssets] of (turtle-set rdFirms productFirms laborForce))]"

PLOT
733
241
1218
509
liquidAssets of banks
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
"banks" 1.0 0 -2064490 true "" "if ticks > 0 [plot mean ([liquidAssets] of banks)]"

PLOT
734
508
1228
777
Production
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Production of machines" 1.0 0 -16777216 true "" "if ticks > 0 [plot sum [production] of rdFirms]"
"Production of goods" 1.0 0 -2674135 true "" "if ticks > 0 [plot sum [production] of productFirms]"

SLIDER
23
60
195
93
HowManyRdFirms
HowManyRdFirms
2
15
5
1
1
NIL
HORIZONTAL

SLIDER
21
99
196
132
HowManyProductFirms
HowManyProductFirms
10
100
40
5
1
NIL
HORIZONTAL

SLIDER
5
139
216
172
HowManyWorkersConsumers
HowManyWorkersConsumers
50
500
120
10
1
NIL
HORIZONTAL

SLIDER
226
681
398
714
InvestmentInRD
InvestmentInRD
0
0.2
0.1
0.02
1
NIL
HORIZONTAL

SLIDER
226
552
398
585
ChanceNewDiscovery
ChanceNewDiscovery
0
3
2.6
0.1
1
NIL
HORIZONTAL

SLIDER
413
553
609
586
TechnologicalDevelopment
TechnologicalDevelopment
0.001
0.05
0.027
0.001
1
NIL
HORIZONTAL

SLIDER
226
595
398
628
Markup
Markup
0
1
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
417
682
589
715
SavingRate
SavingRate
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
414
595
586
628
FlexibilityLabour
FlexibilityLabour
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
226
637
398
670
TaxesLevel
TaxesLevel
0
1
0.75
0.05
1
NIL
HORIZONTAL

SLIDER
416
638
625
671
SpendingInUnemploymentAid
SpendingInUnemploymentAid
0
1
0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
233
764
383
802
The rest of spending is in Keynesian policies of demand
11
0.0
1

INPUTBOX
228
804
383
864
Seed
3
1
0
Number

SLIDER
21
178
193
211
HowManyBanks
HowManyBanks
2
50
3
1
1
NIL
HORIZONTAL

SLIDER
24
215
193
248
interestRateRestitution
interestRateRestitution
0
0.25
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
18
559
208
592
BankReserveRatio
BankReserveRatio
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
6
292
216
325
savings_in_aBank_probability
savings_in_aBank_probability
0
1
0.8
0.1
1
NIL
HORIZONTAL

SLIDER
26
599
198
632
quantitativeEasing
quantitativeEasing
0
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
6
479
218
512
CBRateRestitution
CBRateRestitution
- 0.1
0.2
0
0.01
1
NIL
HORIZONTAL

TEXTBOX
33
459
183
477
Central Bank monetary policy
11
0.0
1

SLIDER
23
255
195
288
interestRateDeposit
interestRateDeposit
0
0.2
0
0.01
1
NIL
HORIZONTAL

SLIDER
7
518
218
551
CBRateDeposit
CBRateDeposit
-0.1
0.2
-0.03
0.01
1
NIL
HORIZONTAL

SWITCH
-2
333
101
366
turbulence?
turbulence?
1
1
-1000

SLIDER
6
415
200
448
customersRationalityLevel
customersRationalityLevel
0
10
8
1
1
NIL
HORIZONTAL

SWITCH
7
374
186
407
followLeadBankStrategy?
followLeadBankStrategy?
1
1
-1000

SLIDER
99
333
227
366
satisfactionCutOff
satisfactionCutOff
0
0.7
0.1
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
#WHAT IS IT?

It's a macroeconomic model with a turbulent environment, i.e. the customers of banks either stay still or take a random walk along the space. Patches rather than a physical space, represent a strategy space. Thanks to this model is possibile to simulate several macroeconomic policies and two differents banking stragies.

#HOW IT WORKS?

There are five kind of agents. R&D firms produce capital-goods (machinery) and
perform R&D in order to improve the performance of the goods they sell
and to cut their production costs. Product firms make final goods using the
capital-goods they bought from R&D firms and sell them to their customers.
Workers work in the firms; they consume only a fraction of their wage and
deposit the other fraction in a bank with a certain probability, obtaining a
percentage of the deposited money, fixed by the respective bank. Banks pay
a fraction of their deposits to the Central bank obtaining from it a percentage
of the deposited money, and grant loans to R&D firms, product firms and
workers fixing the interest rate that each customer has to pay towards the
respective bank. Central bank grant loans to banks, deciding the interest
rate that each debtor bank has to pay, and fixes several parameters that
correspond to monetary policy choices I will discuss below. I also impose the
Central bank to occupy a stationary position in the strategy space. R&D
firms, product firms and workers/consumers also have a satisfaction level that
I have set inversely proportional to the distance between the customer and
the bank in the strategy space. If satisfaction level < satisfaction cut-off,
customers search for a new bank. Finally, if customers rationality level = 10,
all potential customers will choose the nearest bank; otherwise if customers
rationality level = 0, they will choose a random one. In order to introduce
turbulence into the model, I assume the customers either stay still or take
a random walk along the space, i.e. within a turbulent environment.

#Notes

Have a look to the essay in this same web page for an in-depth explanation.
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

building store
false
0
Rectangle -7500403 true true 30 45 45 240
Rectangle -16777216 false false 30 45 45 165
Rectangle -7500403 true true 15 165 285 255
Rectangle -16777216 true false 120 195 180 255
Line -7500403 true 150 195 150 255
Rectangle -16777216 true false 30 180 105 240
Rectangle -16777216 true false 195 180 270 240
Line -16777216 false 0 165 300 165
Polygon -7500403 true true 0 165 45 135 60 90 240 90 255 135 300 165
Rectangle -7500403 true true 0 0 75 45
Rectangle -16777216 false false 0 0 75 45

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
NetLogo 5.2.1
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
