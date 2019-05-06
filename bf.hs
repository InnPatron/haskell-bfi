data VM = Exec (State (Maybe Instruction)) (State Integer)
          End State Integer

data State t = State [t] t [t]
    deriving Show

data Instruction = IncDP
                 | DecDP
                 | IncB
                 | DecB
                 | Output
                 | Input
                 | JmpFwd
                 | JmpBack
    deriving Show

initState :: String -> Int -> (State Instruction, State Integer)
initState string size = 
    let instructionState(instrs) = case instrs of
                               []           -> State [] Output []
                               (first:rest) -> State [] first rest
        dataState(size) = State [] 0 (replicate size 0)
      in (instructionState (map translate string), dataState size)

translate :: Char -> Instruction
translate '>' = IncDP
translate '<' = DecDP
translate '+' = IncB
translate '-' = DecB
translate '.' = Output
translate ',' = Input
translate '[' = JmpFwd
translate ']' = JmpBack

-- bfExecute :: (State Instruction, State Integer) -> (State Instruction, State Integer)  
-- bfExecute (instrs, data) 

state = initState "><+-.,[]" 50
main = putStrLn (show state)
