-- 1
let x = 1
-- x = _

-- 2
let x = ['1']
-- x = "1"

-- 3
let x = [1]
-- x = []

-- 4
let x = 1 :: Int
-- x = 1

-- 5
let f = \x -> x
let x = f 1
-- x = _

-- 6
let f :: Int -> Int; f = \x -> x
let x = f 1
-- x = _
