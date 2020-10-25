xgcd a b = realxgcd a b 1 0 0 1



realxgcd a 0 u v _ _ = (a,u,v)
realxgcd a b u v s t = realxgcd b (a-q*b) s t (u-q*s) (v-q*t)
    where q = a `div` b