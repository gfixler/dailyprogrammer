import Data.Maybe

kshift = mapMaybe $ flip lookup m
    where ks = [ "=-0987654321`", "+_)(*&^%$#@!~"
               , "\\][poiuytrewq", "|}{POIUYTREWQ"
               , "';lkjhgfdsa", "';LKJHGFDSA"
               , "/.,mnbvcxz  ", "?><MNBVCXZ  " ]
          m = concatMap (\xs -> zip xs $ tail xs) ks

