coordSnap size coord = rndAdv size coord

clickSnap :: Integer -> (Integer, Integer) -> (Integer, Integer)
clickSnap size (xCoord, yCoord) = ((coordSnap size (toInteger xCoord)), (coordSnap size (toInteger yCoord)))

rndAdv :: Integer -> Integer -> Integer
rndAdv target input
    | input >= 0 = rnd target input
    | input < 0 = (-1) * (rnd target (input * (-1)))

rnd :: Integer -> Integer -> Integer 
rnd target input = do
    let temp = rem input target
    if (temp < div target 2) 
        then (input - temp)
        else (input + 50 - temp)
