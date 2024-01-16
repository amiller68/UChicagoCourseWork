allp ps a = and (map ($ a) ps)
= and (flip map ps ($ a))
= and (flip map ps (flip ($) a))
= (and . flip map ps . flip ($)) a
= (and . flip map ps . flip ($))
