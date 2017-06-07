fizzbuzz  xs = [ 
  if x `mod` 5 == 0 && x `mod` 3 == 0 
    then "FizzBuzz" 
      else 
        if x `mod` 5 == 0 
          then "Buzz" 
        else 
          if x `mod` 3 == 0 
            then "Fizz" 
          else show x | x <- xs ]