-- [] is MonadFail
l :: [Int]
l = do
    x <- [1,2,3]
    if x == 2 then fail "2 is evil" else [x,2*x]

l' :: [String]
l' = do
    x <- ["lol","rofl"]
    if x == "lol" then fail "lol no good" else [x]

l'' :: [a]
l'' = do
    fail "nope"

-- Maybe is MonadFail
m :: Maybe Int
m = do
    x <- Just 5
    fail "what"
    return 7

-- IO is MonadFail
io :: IO Int
io = do
    input <- getLine
    if input == "lol" then fail "lol noooooooez" else putStrLn "whatever dude"
    return 5

-- ^ this seems to be the only instance that actually uses the error String =/
