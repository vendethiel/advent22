set left(A) rock
set left(B) paper
set left(C) scissors

set weight(rock) 1
set weight(paper) 2
set weight(scissors) 3

set loses(rock) scissors
set loses(paper) rock
set loses(scissors) paper

set wins(scissors) rock
set wins(rock) paper
set wins(paper) scissors

set total 0
set line 1
while {![eof stdin]} {
  gets stdin cmd
  if {$cmd eq ""} {
    break
  }
  set parts [split $cmd " "]
  set len [llength $parts]
  if {$len != 2} {
    puts "Wrong length $len at line $line ($cmd)"
    exit
  }

  set lft [lindex $parts 0]
  if {![info exists left($lft)]} {
    puts "Invalid left $lft at line $line"
    exit
  }
  set their $left($lft)

  set rgt [lindex $parts 1]
  set hand ""
  set score 0
  if {$rgt eq "X"} {
    # lose
    set hand $loses($their)
  } elseif {$rgt eq "Y"} {
    # draw
    set hand $their
    set score 3
  } elseif {$rgt eq "Z"} {
    # win
    set hand $wins($their)
    set score 6
  } else {
    puts "Invalid right $rgt on line $line"
  }
  set w $weight($hand)

  incr total [expr $score+$w]
  incr line
}
puts "Total: $total"
