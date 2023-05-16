x <- start_trace(randTrace1000) %>% add_state(agents(aSet("a1")), rooms(rSet("r1"))) %>% add_state(agents(aSet("a1")), rooms(rSet("r2"))) %>% add_state(agents(aSet("a1")), rooms(rSet("r3")))
xEndNew <- xNew %>% end_trace()
xEnd <- end_trace(x) %>% peek(10)

x2 <- start_trace(randTrace1000) %>% add_state(agents(aSet("a1"), aGroup("a2", "a3"), aRange(1,3)), rooms(rSet("r1")))

res <- apply_state(x$data, x$states[[1]])

state_res <- apply_state(xEnd$result$traces[[1]], xNew$states[[2]], mclapply, "")

xEnd:
〈 {a1}, {r1} 〉 -> 〈 {a1}, {r3} 〉

xNew:
〈 {a1}, {r1} 〉 -> 〈 {a1}, {r2} 〉 -> 〈 {a1}, {r3} 〉


state_res <- apply_state(xEnd$result$traces[[4]], xNew$states[[2]], mclapply, "")

queryTest3New <- start_trace(new.tripsDT[1:10000]) %>%
     add_state(agents(aSet("334", "432")), rooms(rSet("14", "24"))) %>%
     end_trace()

     queryTest4New <- start_trace(new.tripsDT) %>%
  add_state(agents(aSet("334", "432"), aRange(2,5)), rooms(rSet("14", "24"))) %>%
  end_trace() %>%
  peek(10)

  queryTestNegNew <- start_trace(new.tripsDT[1:100000]) %>% add_state(agents(aSet("334")), rooms(rSet("14", "24", neg=TRUE))) %>% end_trace()

  xTest <- start_trace(new.tripsDT[1:100000]) %>% add_state(agents(aSet("334")), rooms(rSet("14", "24", neg=TRUE)))


  programTestTransition2 <- start_trace( new.tripsDT, parallel=FALSE) %>%
     add_state(agents(aSet("390"), aRange(1,1)), rooms(rSet("15"))) %>%
     add_state(agents(aSet("390"), aRange(2, 4)), rooms(rSet("15"))) %>%
     end_trace()

   programTestTransition3 <- start_trace( new.tripsDT, parallel=TRUE) %>%
     add_state(agents(aSet("390"), aRange(1,1)), rooms(rSet("15"))) %>%
     add_state(agents(aSet("390")), rooms(rSet("15", exclude = TRUE))) %>%
     end_trace()

      programTestTransition4 <- start_trace( new.tripsDT, parallel=TRUE) %>%
     add_state(agents(aSet("390"), aRange(1,1)), rooms(rSet("15"))) %>%
     add_state(agents(aSet("390")), rooms(rSet("15", exclude = TRUE))) %>%
     add_state(agents(aSet("390"), aRange(2,5)), rooms(rSet("15", exclude = TRUE))) %>%
     end_trace()

     programTestTransition2 <- start_trace( new.tripsDT, parallel=FALSE) %>%
      add_state(agents(aSet("334"), aRange(1,1)), rooms(rSet("14"))) %>%
      add_state(agents(aSet("334"), aRange(2,2)), rooms(rSet("14"))) %>%
      add_state(agents(aSet("334"), aRange(3,5)), rooms(rSet("14"))) %>%
      end_trace()

     View(get_all_traces(programTestTransition2))


      programTestTransition2Test <- start_trace( new.tripsDT[1:10], parallel=FALSE) %>%
              add_state(agents(aSet("334"), aRange(2,10)), rooms(rSet("14"))) %>%
              end_trace()

      programTestTransition2Test <- start_trace( new.tripsDT[1:10], parallel=FALSE) %>%
     add_state(agents(aSet("334"), aRange(2,10)), rooms(rSet("14"))) %>%
     end_trace()


     transitionTestSynth <- start_trace(randTrace1000) %>%
  add_state(agents(aSet("a1")), rooms(rSet("r4"))) %>%
  add_state(agents(aSet("a1")), rooms(rSet("r5"))) %>%
  add_state(agents(aSet("a1")), rooms(rSet("r3"))) %>%
  end_trace() %>%
  peek(10)




x2 <- start_trace(randTrace1000[1:10], parallel=TRUE) %>% add_state(agents(aSet("a1", not = TRUE)), rooms(rSet("r1", not.in = TRUE)))
x2End <- x2 %>% end_trace() %>% peek(3)
View(x2End$states[[1]]$results)

xRooms <- get_def_by_attr(rooms(rSet("r1", "r2"), rSet("r1", not.in=TRUE)), "modifiers", "not.in", `%in%`)


programTestTransition4 <- start_trace( new.tripsDT, parallel=FALSE) %>%
                   add_state(agents(aSet("390"), aRange(1,1)), rooms(rSet("15"))) %>%
                   add_state(agents(aSet("390")), rooms(rSet("15", exclude = TRUE))) %>%
                   add_state(agents(aSet("390"), aRange(2,5)), rooms(rSet("15", exclude = TRUE))) %>%
                   end_trace()
View(programTestTransition4$states[[1]]$ranges)
View(programTestTransition4$states[[3]]$ranges)
View(programTestTransition4$result$jumps)