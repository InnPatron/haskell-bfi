data VM = Exec (State (Maybe Instruction)) (State Integer)
        | End (State Integer)

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

shiftR :: State a -> State a
shiftR (State left current (rightFirst:rightRest)) =
    State (current : left) rightFirst rightRest 
shiftR (State left current []) =
    error "Shifted right beyond the tape"

shiftL (State (leftFirst: leftRest) current right ) =
    State leftRest leftFirst (current : right)
shiftL (State [] current right) =
    error "Shifted left beyond the tape"

stateApply :: (a -> a) -> (State a) -> (State a)
stateApply f (State left current right) = 
    State left (f current) right

current :: State a -> a
current (State _ current _) = current

jumpForward :: State Instruction -> State Instruction
jumpForward state =
    let nextState = shiftR state

     in case (current nextState) of 
          JmpBack   -> nextState
          otherwise -> jumpForward nextState

jumpBackward :: State Instruction -> State Instruction
jumpBackward state =
    let nextState = shiftL state

     in case (current nextState) of
          JmpFwd    -> nextState
          otherwise -> jumpBackward nextState


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
