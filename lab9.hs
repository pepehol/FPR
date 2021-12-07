data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char
        deriving (Eq)

-- 1 + 2
test1 :: Expr
test1 = Add (Num 1) (Num 2)

-- 1 + 2 * 3
test2 :: Expr
test2 = Add (Num 1) (Mul (Num 2) (Num 3))

-- (1 + 2) * 3
test3 :: Expr
test3 = Mul (Add (Num 1) (Num 2)) (Num 3)

-- 2x * (x - 1)
test4 :: Expr
test4 = Mul (Mul (Num 2) (Var 'x')) (Sub (Var 'x') (Num 1))

-- Funkce, ktere vyhodnocuji vyrazy.
eval :: Expr -> Int
eval (Num x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

-- Funkce zobrazi vyhodnocujici vyrazi jako String.
data Operation = Low | Hi | No | OpDiv | OpSub deriving Eq

showExpr :: Expr -> String
showExpr x = showExpr' x No

showExpr' :: Expr -> Operation -> String
showExpr' (Num x) _ = show x
showExpr' (Add x y) op = let
    result = showExpr' x Low ++ "+" ++ showExpr' y Low
    in if op == Hi || op == OpDiv || op == OpSub then "(" ++ result ++ ")" else result
showExpr' (Sub x y) op = let
    result = showExpr' x Low ++ "-" ++ showExpr' y OpSub
    in if op == Hi || op == OpDiv || op == OpSub then "(" ++ result ++ ")" else result
showExpr' (Mul x y) op = let
    result = showExpr' x Hi ++ "*" ++ showExpr' y Hi
    in if op == OpDiv then "(" ++ result ++ ")" else result
showExpr' (Div x y) op = let
    result = showExpr' x Hi ++ "/" ++ showExpr' y OpDiv
    in if op == OpDiv then "(" ++ result ++ ")" else result
showExpr' (Var x) _ = [x]

-- Zobrazeni vytvorenych vyrazu pomoci tridu Show.
instance (Show Expr) where
    show x = showExpr x

-- Funkce ktera symbolicky vypise derivaci
-- deriv (Add (Num 1) (Num 2)) 'x'
    -- 0+0

-- deriv (Mul (Num 2) (Mul (Var 'x') (Var 'x'))) 'x'
    -- 0*x*x+2*(1*x+x*1)

-- deriv (Mul (Num 2) (Mul (Var 'x') (Var 'x'))) 'x'
    -- 0*x*x+2*(1*x+x*1)

deriv :: Expr -> Char -> Expr
deriv (Num _) _ = (Num 0)
deriv (Var x) y | x == y = (Num 1)
                | otherwise = (Num 0)
deriv (Add l r) x = Add (deriv l x) (deriv r x)
deriv (Sub l r) x = Sub (deriv l x) (deriv r x)
deriv (Mul l r) x = Add (Mul (deriv l x) r) (Mul l (deriv r x))
deriv (Div l r) x =
    Div
        (Sub (Mul (deriv l x) r) (Mul l (deriv r x)))
        (Mul r r)
