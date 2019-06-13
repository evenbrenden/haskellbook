-- 1
const 1 undefined
\a b -> a
\a _ -> a
a
1

-- 2
const undefined 1
\a b -> a
\a _ -> a
a
undefined

-- 3
flip const undefined 1
\a b -> const b a
\a b -> (\b a -> b)
\_ b -> (\b _ -> b)
\_ b -> b
b
1

--4
flip const 1 undefined
\a b -> const b a
\a b -> (\b a -> b)
\_ b -> (\b _ -> b)
\_ b -> b
b
undefined

--5
const undefined undefined
\a b -> a
\a _ -> a
a
undefined

--6
foldr const 'z' ['a'..'e']
go "abcde"
    where
        go [] = 'z'
        go ('a':"bcde") = const 'a' (go "bcde")
const 'a' (go "bcde")
'a'

--7
foldr (flip const) 'z' ['a'..'e']
go "abcde"
    where
        go [] = 'z'
        go ('a':"bcde") = (flip const) 'a' (go "bcde")
(flip const) 'a' (go "bcde")
go "bcde"
(flip const) 'b' (go "cde")
go "cde"
(flip const) 'c' (go "de")
go "de"
(flip const) 'd' (go "e")
go "e"
(flip const) 'e' (go "")
go ""
'z'
