open util/ordering[Time]

sig Time{}

sig User {
  follows:set User,
  blocks:set User,
} {
  no blocks & this
  no follows & this
  no blocks & follows
}

sig Tweet {
  from: User,
  replyTo: lone Tweet,
  favorite: set User,
  created:Time,
  number:Int
} {
  no replyTo & this
  no favorite & from
}

fact replyNotCyclic { no t:Tweet | t in t.^replyTo }

run {
  
}  for 2 but 1 Int
