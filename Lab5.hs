module Lab5 where
 thirdLast x = head(drop (length x -3) x)
 
 everyOther [] = []
 everyOther x = [head x] ++ (everyOther (drop 2 x))
 
 sumPosList [] = 0
 sumPosList x = do
  let first = (head x)
  if first > 0 then first + sumPosList(tail x)
  else sumPosList(tail x)
  