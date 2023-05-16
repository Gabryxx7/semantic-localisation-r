## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## library(testthat)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# usethis::use_test("name")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tripsSubset  <- new.tripsDT[1:10000]
program <- start_trace(tripsSubset) %>%
  add_state(agents(aSet("a1","a2"), aSet("b1", "b2"), aGroup("a3","a4"), range(2, 8)), rooms(rSet("r1","r2"), rGroup("r3","r4")))

invisible(print_states(program)) # I just want to avoid the second print showing the return of the print function


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_data <- new.tripsDT[timeSlotID == 11206,][["18"]][[1]]
print(test_data)

cat("Query?: RESULT [EXPECTED RESULT]\n\n")
res <- check_room_data(test_data, agents(aSet("365", "574")))
cat("Is aSet(365,574) in the room?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(aSet("365")))
cat("Is aSet(365) in the room?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(aSet("123")))
cat("Is aSet(123) in the room?: \n\t",res, "   [FALSE]\n\n")

res <- check_room_data(test_data, agents(aSet("365", "574", "600")))
cat("Is aSet(365,574,600) in the room?: \n\t",res, "   [FALSE]\n\n")

res <- check_room_data(test_data, agents(aGroup("365", "574")))
cat("Is aGroup(365,574) in the room?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(aGroup("365", "574", "600")))
cat("Is aGroup(365,574,600) in the room?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(range(1,2)))
cat("Is the room in the range(1,2)?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(range(4,9)))
cat("Is the room in the range(4,9)?: \n\t",res, "   [FALSE]\n\n")

# res <- check_room_data(new.tripsDT[timeSlotID == 11206,][["18"]], agents(range(c(1,2))))
# cat("Is the room in the range(4,9)?: \n\t",res, "\n")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_data <- new.tripsDT[timeSlotID == 47295,][["14"]]
print(test_data)

cat("Query?: RESULT [EXPECTED RESULT]\n\n")
res <- check_room_data(test_data, agents(aSet("334", "540")))
cat("Is aSet(334,540) in the room?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(set(334)))
cat("Is aSet(334) in the room?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(aSet("123")))
cat("Is aSet(123) in the room?: \n\t",res, "   [FALSE]\n\n")

res <- check_room_data(test_data, agents(aSet("334", "540", "600")))
cat("Is aSet(334,540,600) in the room?: \n\t",res, "   [FALSE]\n\n")

res <- check_room_data(test_data, agents(aGroup("334", "540")))
cat("Is aGroup(334,540) in the room?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(aGroup("334", "540", "600")))
cat("Is aGroup(334,540,600) in the room?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(range(1,2)))
cat("Is the room in the range(1,2)?: \n\t",res, "   [TRUE]\n\n")

res <- check_room_data(test_data, agents(range(4,9)))
cat("Is the room in the range(4,9)?: \n\t",res, "   [FALSE]\n\n")

# res <- check_room_data(new.tripsDT[timeSlotID == 11206,][["18"]], agents(range(c(1,2))))
# cat("Is the room in the range(4,9)?: \n\t",res, "\n")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tripsSubset <- new.tripsDT[1:10000]
queryTest1 <- start_trace(tripsSubset) %>%
  add_state(agents(set(334)), rooms(set(14))) %>%
  apply_state() %>%
  end_trace()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tripsSubset <- new.tripsDT[1:100000]
queryTest2 <- start_trace(tripsSubset) %>%
  add_state(agents(set(334), range(1,1)), rooms(rSet("14","24"))) %>%
  apply_state() %>%
  end_trace()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tripsSubset <- new.tripsDT[1:10000]
queryTest3 <- start_trace(tripsSubset) %>%
  add_state(agents(aSet("334", "432")), rooms(set(14,24))) %>%
  end_trace()
View(queryTest3$traces)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tripsSubset <- new.tripsDT
queryTest4 <- start_trace(tripsSubset) %>%
  add_state(agents(aSet("334", "432"), range(2,5)), rooms(set(14,24))) %>%
  apply_state() %>%
  end_trace() %>%
  peek(10)

View(queryTest4$tracesList[[1]])
invisible(get_agent_journey(queryTest4$tracesList[[1]], "334"))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
View(get_trace(queryTest4, 1, all.rooms = FALSE, all.agents = FALSE))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tripsSubset <- new.tripsDT[1:100000]
queryTestNeg <- start_trace(tripsSubset, parallel = FALSE) %>%
  add_state(agents(set(334)), rooms(rSet("14", "24", neg=TRUE))) %>%
  apply_state() %>%
  end_trace()
cat("\n")
queryTestNeg <- start_trace(tripsSubset, parallel = TRUE) %>%
  add_state(agents(set(334)), rooms(rSet("14", "24", neg=TRUE))) %>%
  apply_state() %>%
  end_trace()
View(queryTestNeg$tracesList[[2]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Checking first
# coupleRes <- check_room_data(new.tripsDT[timeSlotID == 11206,][["18"]], aSet("365", "574"))
# coupleResList <- check_room_series(new.tripsDT[["18"]], c("365", "574"))

# Ok this is now working!!
programTest3 <- start_trace(new.tripsDT) %>%
  add_state(agents(aSet("365", "574")), rooms(rSet("18"))) %>%
  apply_state(1) %>%
  end_trace()
  # expand_trace(10) 

programTest2 <- start_trace(new.tripsDT) %>%
  add_state(agents(aSet("334", "432"),range(2, 3)), rooms(set(14))) %>%
  print_states() %>%
  apply_state(1) %>%
  end_trace()
  # expand_trace(10) 

View(programTest2)
head(programTest2$jumps, 3)
View(programTest2$tracesList[[1]])
View(programTest2$tracesList[[2]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
randTrace1000 <- readRDS("randTrace1000.rds")
randTrace100 <- readRDS("randomTrace100.rds")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
queryTestRand <- start_trace(randTrace100) %>%
     add_state(agents(set(a1)), rooms(rSet("r1"))) %>%
     apply_state() %>%
     end_trace()
View(queryTestRand$tracesList[[1]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
queryTestRand <- start_trace(randTrace100) %>%
     add_state(agents(set(a1), range(0,4)), rooms(rSet("r1"))) %>%
     apply_state() %>%
     end_trace()
View(queryTestRand$tracesList[[1]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
queryTestRand <- start_trace(randTrace100) %>%
     add_state(agents(set(a1), range(0,3)), rooms(rSet("r1"))) %>%
     apply_state() %>%
     end_trace()
View(queryTestRand$tracesList[[1]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
queryTestRand <- start_trace(randTrace100) %>%
     add_state(agents(aSet("a1", "a9")), rooms(rSet("r1"))) %>%
     apply_state() %>%
     end_trace()
View(queryTestRand$tracesList[[1]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
queryTestRand <- start_trace(randTrace1000) %>%
     add_state(agents(set(a1)), rooms(rSet("r1", "r2"))) %>%
     apply_state() %>%
     end_trace()

cat("Trace1 :")
invisible(get_agent_journey(queryTestRand$tracesList[[1]], "a1"))
# View(queryTestRand$tracesList[[1]])
cat("\n\nTrace2 :")
invisible(get_agent_journey(queryTestRand$tracesList[[2]], "a1"))
# View(queryTestRand$tracesList[[2]])
cat("\n\nTrace3 :")
invisible(get_agent_journey(queryTestRand$tracesList[[3]], "a1"))
# View(queryTestRand$tracesList[[3]])
cat("\n\nTrace4 :")
invisible(get_agent_journey(queryTestRand$tracesList[[4]], "a1"))
# View(queryTestRand$tracesList[[3]])






## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
programTestTransition <- start_trace(new.tripsDT[1:15000]) %>%
  add_state(agents(aSet("334", "432")), rooms(set(14))) %>%
  apply_state() %>%
  add_state(agents(set(334), range(2, 4)), rooms(set(24))) %>%
  apply_state()
  print_states() %>%

# View(programTestTransition$states[[1]]$tracesList); View(programTestTransition$states[[2]]$tracesList)
# View(programTestTransition$states[[1]]$jumps); View(programTestTransition$states[[2]]$jumps); View(testTransitionRes$jumps)

# Start from the last state 1 result before state2
rangeDT <- extract_start_end_traces(programTestTransition$data$data, programTestTransition$states, extended=FALSE);
View(rangeDT$res[[2]])
# Start from the first state 1 result before the first state2
rangeDTEx <- extract_start_end_traces(programTestTransition$data$data, programTestTransition$states, extended=TRUE);
View(rangeDTEx$res[[2]])



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testTransitionRes <- end_trace(programTestTransition)

testTransitionResSummary <- print_trace(testTransitionRes, 1)
View(testTransitionResSummary)
state1Res <- testTransitionRes$states[[1]]$tracesList$jumps
state2Res <- testTransitionRes$states[[2]]$tracesList$jumps

# Let's keep track of the timeSlots of each state's result
state1Res$timeSlotState1 <- state1Res$timeSlotID
state2Res$timeSlotState2 <- state2Res$timeSlotID

# Now, which state1 results are in between the results of state2?
state1InState2 <- which(state1Res$timeSlotID >= min(state2Res$timeSlotID) & state1Res$timeSlotID <= max(state2Res$timeSlotID))
# Since State1 comes before state2, we should expand by one row to get the index of where state1 was true BEFORE the first state2 was true
# state1InState2 <- c( state1InState2)
state1InState2 <- c(state1InState2[[1]]-1, state1InState2)
# Let's build a comparison table
resComparison <- state1Res[state1InState2, timeSlotState2 := state2Res$timeSlotState2][state1InState2,]
View(resComparison)

# Now that we know from where to subset the dataset to where, we can apply the subsetting
resFinal1 <- apply(resComparison, 1, function(x) new.tripsDT[timeSlotID >= x[["timeSlotState1"]] & timeSlotID <= x[["timeSlotState2"]], ])
# Technically this resFinal1 should have all the traces between state1 and state2, excluding everything in between
View(resFinal1)
View(resFinal1[[1]])
View(resFinal1[[2]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
invisible(print_states(res))
View(resFinal1[[1]][, c("timeSlotID", "14", "24")])
View(resFinal1[[2]][, c("timeSlotID", "14", "24")])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tripsSubset <- new.tripsDT[1:100000]
programTestTransition2 <- start_trace(tripsSubset) %>%
  add_state(agents(set(390), range(1,1)), rooms(set(15))) %>%
  add_state(agents(set(390), range(2, 4)), rooms(set(15))) %>%
  print_states() %>% 
  end_trace()

# Start from the last state 1 result before state2
rangeDT2 <- extract_start_end_traces(programTestTransition2, programTestTransition2, extended=FALSE);
View(rangeDT2$range)

# Start from the first state 1 result before the first state2
rangeDTEx2 <- extract_start_end_traces(programTestTransition2, programTestTransition2, extended=TRUE);
View(rangeDTEx2$res[[2]])



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tripsSubset <- new.tripsDT[1:100000]
programTestTransitionNeg <- start_trace(tripsSubset) %>%
  add_state(agents(set(334)), rooms(set(14))) %>%
  apply_state() %>%
  add_state(agents(set(334), range(1,3)), rooms(rSet("14", "24", neg=TRUE))) %>%
  apply_state() %>%
  add_state(agents(set(334)), rooms(rSet("14", "24", neg=TRUE))) %>%
  apply_state() %>%
  print_states() %>%
  end_trace()

invisible(get_agent_journey(programTestTransitionNeg$tracesList[[1]], "334"))
View(programTestTransitionNeg$tracesList[[1]])
# interRes <- programTestTransitionNeg$states[[2]]$intermediate
# traceRes <- programTestTransitionNeg$jumps
View(interRes)
View(traceRes)
trace_contains_state(traceRes, interRes)
# Start from the last state 1 result before state2

# View(programTestTransitionNeg$tracesList[[1]])
# View(programTestTransitionNeg$tracesList[[2]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tripsSubset <- new.tripsDT
programTestTransition3 <- start_trace(tripsSubset) %>%
  add_state(agents(set(334)), rooms(set(14))) %>%
  add_state(agents(range(0,0)), rooms(set(14))) %>%
  add_state(agents(set(334)), rooms(set(14,24))) %>%
  end_trace()

programTestTransition3_4 <- copy(programTestTransition3) %>%
  add_state(agents(set(334), range(1,4)), rooms(set(14,24))) %>%
  apply_state() %>%
  # print_states() %>%
  end_trace()

invisible(get_agent_journey(programTestTransition3$tracesList[[3]], "334"))
View((programTestTransition3%>% peek(10))$tracesList[[1]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
programTestTransition4 <- start_trace( new.tripsDT, parallel=FALSE, use.cpp=FALSE) %>%
     add_state(agents(set(390), range(1,1)), rooms(set(15))) %~~>%
     add_state(agents(set(390)), rooms(~set(15))) %~~>%
     add_state(agents(set(390), range(2,5)), rooms(~set(15))) %~~>%
     end_trace()
programTestTransition4cpp <- start_trace( new.tripsDT, parallel=FALSE, use.cpp=TRUE) %>%
    add_state(agents(set(390), range(1,1)), rooms(set(15))) %~~>%
    add_state(agents(set(390)), rooms(~set(15))) %~~>%
    add_state(agents(set(390), range(2,5)), rooms(~set(15))) %~~>%
    end_trace()
# view_trace(programTestTransition4, 1, title="pTT4")
# view_trace(programTestTransition4cpp, 1, title="pTT4Cpp")
print(programTestTransition4)
print(programTestTransition4cpp)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestSynth <- start_trace(randTrace1000, parallel=FALSE) %>%
  add_state(agents(set(a1)), rooms(set(r4))) %~~>%
  add_state(agents(set(a1)), rooms(set(r5))) %~~>%
  add_state(agents(set(a1)), rooms(set(r3))) %~~>%
  end_trace() %>%
  peek(10)

View(get_all_traces(transitionTestSynth))

tracesLengths <- unlist(lapply(transitionTestSynth$tracesList, nrow))
print(tracesLengths)
longerTraces <- which(tracesLengths >= 10)
View(transitionTestSynth$tracesList[[longerTraces[[2]]]])
# View(transitionTestSynth$states[[1]]$ranges)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
View((transitionTestSynth %>% peek(10))$tracesList[[1]])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
roomOccupancy1 <- start_trace(new.tripsDT) %>%
    add_state(agents(range(4,Inf)), rooms(set(14))) %~~>%
    add_state(agents(range(5,Inf)), rooms(set(14))) %~~>%
    add_state(agents(range(3,Inf)), rooms(set(14))) %~~>%
    end_trace(verbose=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
roomOccupancy2_single <- start_trace(randTrace1000) %>%
    add_state(a(range(1,Inf)), r(set(r4))) %>%
    end_trace(verbose=FALSE)
View(roomOccupancy2_single[[1]])
occ2s_rate <- nrow(roomOccupancy2_single[[-1]])/nrow(randTrace1000)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
roomOccupancy2_multiple<- start_trace(randTrace1000) %>%
    add_state(a(range(1,Inf)), r(set(r1,r4))) %>%
    end_trace(verbose=FALSE)
# View(roomOccupancy2_multiple[[1]])
occ2m_rate <- nrow(roomOccupancy2_multiple[[-1]])/nrow(randTrace1000)
print(occ2m_rate)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
roomOccupancy2 <- start_trace( new.tripsDT, parallel=TRUE) %>%
    add_state(agents(range(1,Inf)), rooms(set(14))) %~~>%
    end_trace(verbose=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
programTestTransition2 <- start_trace( new.tripsDT, parallel=TRUE) %>%
  add_state(agents(set(334), range(1,1)), rooms(set(14))) %~~>%
  add_state(agents(set(334), range(2,3)), rooms(set(14))) %~~>%
  add_state(agents(set(334), range(3,5)), rooms(set(14))) %~~>%
  end_trace(verbose=FALSE)
view_trace(programTestTransition2, 1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
programTestTransition3 <- start_trace( new.tripsDT, parallel=FALSE) %>%
  add_state(agents(set(334), range(1,1)), rooms(set(14))) %~~>%
  add_state(agents(set(334), range(3,5)), rooms(set(14))) %~~>%
  add_state(agents(set(334), range(1,1)), rooms(set(14))) %~~>%
  end_trace(verbose=FALSE)
view_trace(programTestTransition3, 1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
programTestTransition3TRange <- start_trace( new.tripsDT, parallel=FALSE) %>%
    add_state(agents(set(334), range(1,1)), rooms(set(14)), tRange(50, Inf)) %~~>%
    add_state(agents(set(334), range(2,Inf)), rooms(set(14)), tRange(50, Inf)) %~~>%
    end_trace(verbose=FALSE)
view_trace(programTestTransition3TRange, 1)
programTestTransition3TRange


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
programTestTransition4 <- start_trace( new.tripsDT, parallel=FALSE) %>%
  add_state(agents(set(334)), rooms(set(14))) %~~>% # non-fuzzy
  add_state(agents(set(334)), rooms(set(24))) %~~>% # non-fuzzy
  end_trace(verbose=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
programTestTransition4 <- start_trace( new.tripsDT, parallel=FALSE) %>%
  add_state(agents(set(334)), rooms(set(14))) %-->% # non-fuzzy
  add_state(agents(set(334)), rooms(set(24))) %-->% # non-fuzzy
  end_trace(verbose=FALSE)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestSynth2 <- start_trace(randTrace1000) %>%
  add_state(agents(set(a1)), rooms(set(r4))) %~~>%
  add_state(agents(set(a1)), rooms(set(r5))) %~~>%
  add_state(agents(set(a1)), rooms(set(r3))) %~~>%
  end_trace(verbose=FALSE) %>%
  peek(2)
view_trace(transitionTestSynth2, 1, title="transitionTestSynth2")
# view_all_traces(transitionTestSynth, keep.peek=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestSynth2 <- start_trace(randTrace1000) %>%
    add_state(agents(set(a1)), rooms(set(r4))) %-->%
    add_state(agents(set(a1)), rooms(set(r5))) %-->%
    add_state(agents(set(a1)), rooms(set(r3))) %-->%
    end_trace(verbose=FALSE) %>%
    peek(3)
view_trace(transitionTestSynth2, 1, title="transitionTestSynth2")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestSynt4 <- start_trace(randTrace1000) %>%
    add_state(a(set(a1)), r(set(r4))) %~>%
    add_state(a(set(a1)), r(set(r5))) %~>%
    add_state(a(set(a1)), r(set(r2))) %~>%
    add_state(a(set(a1)), r(set(r3))) %~>%
    end_trace(verbose=FALSE) %>%
    peek(10)
view_trace(transitionTestSynt4, 1, title="4States")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestSynt4 <- start_trace(randTrace1000) %>%
    add_state(a(set(a1)), r(set(r4))) %->%
    add_state(a(set(a1)), r(set(r5))) %->%
    add_state(a(set(a1)), r(set(r2))) %->%
    add_state(a(set(a1)), r(set(r3))) %->%
    end_trace(verbose=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestSynt4 <- start_trace(randTrace1000) %>%
    add_state(a(set(a1)), r(set(r4))) %->%
    add_state(a(set(a1)), r(set(r5))) %->%
    add_state(a(set(a1)), r(set(r2))) %~>%
    add_state(a(set(a1)), r(set(r3))) %->%
    end_trace(verbose=FALSE)
view_trace(transitionTestSynt4, 1, title="transitionTestSynt4")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
lookBehindTransition <- start_trace(randTrace1000) %>%
    add_state(a(set(a1)), r(set(r1,r2))) %~>%
    add_state(a(set(a1)), r(set(.prev))) %~>%
    end_trace(verbose=FALSE)
view_trace(lookAheadTransition, 1, title="lookBehind")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# State 1 should last for at least 2 timeslots
timeRangeTest1 <- start_trace(randTrace1000) %>%
  add_state(agents(set(a1)), rooms(set(r4)), tRange(2,Inf)) %~>%
  add_state(agents(set(a1)), rooms(set(r5))) %~>%
  add_state(agents(set(a1)), rooms(set(r2))) %~>%
  add_state(agents(set(a1)), rooms(set(r3))) %~>%
  end_trace(verbose=FALSE) %>%
  peek(10)
view_trace(timeRangeTest1, 1, title="timeRangeTest1")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# State 1 should last for at least 2 timeslots
timeRangeTest2 <- start_trace(customTrace1) %>%
  add_state(agents(set(a2)), rooms(set(r4)), tRange(5,Inf)) %~>%
  add_state(agents(set(a2)), rooms(set(r4)), tRange(1,2)) %~>%
  end_trace(verbose=FALSE) %>%
  peek(3)
view_trace(timeRangeTest2, 1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
timeRangeTest2Direct <- start_trace(customTrace1, parallel=FALSE) %>%
    add_state(agents(set(a2)), rooms(set(r4)), tRange(4,Inf)) %->% # non-fuzzy
    add_state(agents(set(a2)), rooms(set(r3)), tRange(3,Inf)) %->% # non-fuzzy
    end_trace(verbose=TRUE) %>%
    peek(3)
view_trace(timeRangeTest2Direct, 1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
timeRangeTest3 <- start_trace(randTrace1000, parallel=FALSE) %>%
    add_state(agents(set(a3, a1)), rooms(set(r4, r3)), tRange(3,Inf)) %-->% # non-fuzzy
    end_trace(verbose=TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
timeRangeTest4 <- start_trace(randTrace1000, parallel=FALSE) %>%
    add_state(agents(group(a2, a9)), rooms(set(r4, r3)), tRange(3,Inf)) %-->% # non-fuzzy
    end_trace(verbose=TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
timeRangeTest4 <- start_trace(randTrace1000, parallel=FALSE) %>%
    add_state(agents(group(a2, a9)), rooms(set(r4, r3)), tRange(1,Inf)) %-->% # non-fuzzy
    end_trace(verbose=TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestSynth4Direct <- start_trace(randTrace1000) %>%
    add_state(agents(set(a1)), rooms(set(r4))) %-->% # non-fuzzy
    add_state(agents(set(a1)), rooms(set(r5))) %-->% # non-fuzzy
    add_state(agents(set(a1)), rooms(set(r2))) %-->% # non-fuzzy
    add_state(agents(set(a1)), rooms(set(r3))) %-->% # non-fuzzy
    end_trace()
view_trace(transitionTestSynth4Direct, 1, title="4StatesDirect")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestReturn1 <-  start_trace(new.tripsDT) %>%
    add_state(agents(set(334)), rooms(set(14))) %~~>% # Fuzzy
    add_state(agents(set(334)), rooms(set(24))) %~~>% # Fuzzy
    end_trace()


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestReturn <- start_trace(new.tripsDT) %>%
    add_state(agents(set(334)), rooms(set(24))) %~~>% # Fuzzy
    add_state(agents(set(334)), rooms(set(28))) %~~>% # Fuzzy
    add_state(agents(set(334)), rooms(set(24))) %~~>% # Fuzzy
    end_trace()

# I know there is an instance here. 24 -> 28 -> 24 (fuzzy)
View(new.tripsDT[11790:11796, ])


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transitionTestSynth2 <- start_trace(randTrace1000) %>%
  add_state(agents(set(a1)), rooms(set(.any))) %~~>% # fuzzy
  end_trace(verbose=FALSE)
view_trace(transitionTestSynth2, 1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nowhereTest1 <- start_trace(new.tripsDT) %>%
  add_state(agents(set(334)), rooms(!set(.any))) %~~>% # Fuzzy
  end_trace(verbose=FALSE)
view_all_traces(nowhereTest1)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nowhereTest2 <- start_trace(new.tripsDT) %>%
    add_state(agents(set(390)), rooms(!set(.any))) %~~>% # Fuzzy
    end_trace(verbose=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contactTracing <- start_trace( new.tripsDT, parallel=FALSE) %>%
   add_state(agents(set(390), range(2,Inf)), rooms(set(.any))) %~~>%
   end_trace()
View(contactTracing[[1]], title='Contact Tracing 1')

unique(unlist(roomOccupancy2_single[]$r4))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contactTracing2 <- start_trace( new.tripsDT, parallel=FALSE) %>%
   add_state(agents(set(390), range(1,1)), rooms(set(.any))) %~~>%
   add_state(agents(set(390), range(2,Inf)), rooms(set(.any))) %~~>%
   end_trace()
# contactTracing2 <- contactTracing2 %>% peek(5)
View(contactTracing2[[1]], title="Contact Tracing 2")



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contactTracing3 <- start_trace(randTrace1000) %>%
    add_state(a(set(a1), range(2,Inf)), r(set(.any))) %>%
    end_trace(verbose=FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Contact Tracing Summary
found_agents <- contactTracing3$result$agents
ct3_res <- lapply(found_agents[found_agents != "a1"], function(x){
    contacts <- contactTracing3[c("a1", x)]
    res <- list(title=paste0("a1 + ", x))
    contact_times <- !is.na(contacts$agents_room)
    consecutive <- rle(contact_times)
    consecutive.true <- consecutive$lengths[which(consecutive$values == TRUE)]
    res["n_encounters"] <- length(consecutive.true)
    res["max_duration"] <- max(consecutive.true)
    res["total_duration"] <- nrow(contacts[contact_times])
    return(res)
 })
ct3_res <- do.call(rbind, lapply(ct3_res, data.frame))
View(ct3_res)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
source("R/generated/RandomTraceGeneration.R")
generate_test_traces <- function(n_times=NULL, n_rooms=NULL, start_threshold=800, distribution='perlin'){
  if(is.null(n_times)){
    n_times <- c(10^2, 10^3, 10^4, 10^5, 10^6)
  }
  else if(is.numeric(n_times) && length(n_times) <= 1){
    n_times <- 10^seq(n_times)
  }
  if(is.null(n_rooms)){
    n_rooms <- c(5, 10, 20, 30, 40, 50)
  }
  else if(is.numeric(n_rooms) && length(n_rooms) <= 1){
    n_rooms <- 10*seq(n_rooms)
  }
  tests_data <- data.frame(
    index = seq(length(n_times) * length(n_rooms)),
    n_rooms = rep(n_rooms, each = length(n_times)),
    n_times = n_times)
  cat("No. of Time slots:", paste0(n_times), "\nNo. of Rooms:", paste0(n_rooms), "\n\nTests")
  cat(paste0("\n", tests_data$index,  ".\t", tests_data$n_times, " x ", tests_data$n_rooms))
  test_traces <- apply(tests_data, 1, function(x){
    cat("\nGenerating trace ", x[['index']], "/", nrow(tests_data), ":\tTime Slots: ", x[['n_times']], "\t\tRooms:", x[['n_rooms']], sep="")
    res <- generate_trace(distribution=distribution, start_threshold=start_threshold, ntimes=x[['n_times']], nrooms=x[['n_rooms']])
    print("")
    return(res)
  })
  return(list(configs=tests_data, traces=test_traces))
}



## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## if(!exists("scalability_test_traces")){
##   scalability_test_traces <- readRDS("data/scalability_test_traces.rds")
## }
## if(!exists("scalability_test_traces")){
##   # n_times <- c(10^2, 10^3, 10^4, 10^5, 10^6)
##   # n_rooms <- c(5, 10, 20, 30, 40, 50)
##   n_times <- c(10^2, 10^3, 10^4, 10^5)
##   n_rooms <- c(5, 10, 20, 30, 40)
##   scalability_test_traces <- generate_test_traces(n_times, n_rooms)
##   # saveRDS(scalability_test_traces, "scalability_test_traces.rds")
## }


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tictoc)
library(microbenchmark)
run_trace <- function(test_trace, b_times=5){
  n_rooms <- ncol(test_trace$data)-2
  n_timeslots <- nrow(test_trace$data) # Remove tSId and tag
  cat(paste0("\n************ Running Trace ", test_trace$idx, "( ", n_rooms, " x ", n_timeslots, " ) for ", b_times, " times ************\n"))
  tic(paste0("Trace_Test", n_rooms, "_", n_timeslots))
  micro_b <- microbenchmark("trace_test" = {
    .GlobalEnv[['test_trace_data']] <- copy(test_trace$data)
    res_trace <- start_trace(.GlobalEnv[['test_trace_data']]) %>%
      add_state(agents(set(a1)), rooms(set(.any))) %~~>% # non-fuzzy
      add_state(agents(set(a2)), rooms(set(.any))) %~~>% # non-fuzzy
      add_state(agents(set(a3)), rooms(set(.any))) %~~>% # non-fuzzy
      end_trace()
  }, times=b_times)

  timing_toc <- toc()
  occupancy <- attr(res_trace, "occupancy")
  avg_occ <- mean(unlist(unname(lapply(attr(res_trace, "non.empty.data"), length)))  / nrow(res_trace$data))
  mean_occ <- mean(unlist(occupancy))
  return(list(occ=occupancy, mean_occ=mean_occ, timing=timing_toc, n_rooms=n_rooms, n_timeslots=n_timeslots, micro_b=micro_b))
}




## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_scalability_results <- function(test_output_df){
  test_output_df_plot <- test_output_df
  test_output_df_plot$n_rooms <- as.factor(test_output_df_plot$n_rooms)
  return(ggplot() +
   geom_line(data=test_output_df_plot, aes(x=n_timeslots, y=time, color=n_rooms)) +
   xlab("No. of Time Slots") +
   ylab("Completion Time")
  )
}



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# n_times <- seq(from=1000,to=100000,by=1000)
# n_rooms <- seq(from=5,to=100,by=5)
# test_traces <- invisible(generate_test_traces(n_times,n_rooms))
# test_output_bak <- test_output
test_output <- list()
for(i in 1:nrow(test_traces$configs)){
  trace_to_test <- test_traces$traces[[i]]
  test_output <- append(test_output, list(run_trace(list(data=test_traces$traces[[i]], idx=i), b_times=10)))
}
# saveRDS(test_traces, "data/test_traces.rds")
# saveRDS(test_output, "data/test_traces_output.rds")
test_output_filtered <- lapply(test_output, function(x) list(time=(median(x$micro_b$time)/1000000), time_median=(mean(x$micro_b$time)/1000000), mean_occ=x$mean_occ, n_rooms=x$n_rooms, n_timeslots=x$n_timeslots))
test_output_filtered <- data.frame(apply(do.call("rbind", test_output_filtered), 2, unlist))
test_output_filtered$time_s <- test_output_filtered$time/1000000
test_output_df <- test_output_filtered[order(test_output_filtered$n_timeslots),]
plot_scalability_results(test_output_df)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_output_df_plot <- test_output_df
test_output_df_plot$n_rooms <- as.factor(test_output_df_plot$n_rooms)
scalability_plot <- ggplot() +
 geom_line(data=test_output_df_plot, aes(x=n_timeslots, y=time, color=n_rooms)) +
 xlab("No. of Time Slots") +
 ylab("Completion Time")
scalability_plot


