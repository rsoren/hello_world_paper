; initialize simulant characteristics variables
turtles-own[ prob sex age year simulant_id cost]

to setup
  clear-all
  set-default-shape turtles "person"
  create-turtles 10000 [ setxy random-xcor random-ycor ]
  reset-ticks
end

; check-death determines if simulant dies during timestep
to check-death
  random-seed 123
  ask turtles [
    if random-float 1 <= prob [ die ]
  ]
end

; progress-time moves age and year forward each timestep
to progress-time
   ask turtles [
   set age age + 1
   set year year + 1
   ]
end

to go
  check-death
  progress-time
  tick
end