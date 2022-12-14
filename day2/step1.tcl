set left(A) rock
set left(B) paper
set left(C) scissors
set right(X) rock
set right(Y) paper
set right(Z) scissors

set weight(rock) 1
set weight(paper) 2
set weight(scissors) 3

set wins(rock) scissors
set wins(paper) rock
set wins(scissors) paper

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

  set rgt [lindex $parts 1]
  if {![info exists right($rgt)]} {
    puts "Invalid right $rgt at line $line"
    exit
  }

  set their $left($lft)
  set mine $right($rgt)
  set winning $wins($mine)
  set myweight $weight($mine)

  set score 0
  if {$their eq $mine} {
    set score 3
  } elseif {$winning eq $their} {
    set score 6
  }
  incr total [expr $score+$myweight]
  incr line
}
puts "Total: $total"
