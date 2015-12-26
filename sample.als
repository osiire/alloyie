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
  favorite: set User
} {
  no replyTo & this
  no favorite & from
}

fact replyNotCyclic { no t:Tweet | t in t.^replyTo }

run {
  
} for 2
