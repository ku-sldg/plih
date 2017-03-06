module Do where

f x = if x > 3 then Just x else Nothing
g x = if x < 4 then Nothing else Just x

test1 = do {
  a <- f 4 ;
  b <- g a ;
  return (a + b)
  }

test2 = do {
  a <- f 2 ;
  b <- g a ;
  return (a + b)
  }

