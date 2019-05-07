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

step :: VM -> VM
step (Exec instrState@(State _ (Just instr) _) dataState) =
    Exec (instrHandler instrState instr) (dataHandler dataState instr)

step (Exec (State _ Nothing _) dataState) =
    End dataState
step (End state) =
    End state

instrHandler :: State (Maybe Instruction) -> Instruction -> State (Maybe Instruction)
instrHandler instrState IncDP = shiftR instrState
instrHandler instrState DecDP = shiftR instrState
instrHandler instrState IncB = shiftR instrState
instrHandler instrState DecB = shiftR instrState
instrHandler instrState Input = error "Input"
instrHandler instrState Output = error "Output"
instrHandler instrState JmpFwd = shiftR (jumpForward instrState)
instrHandler instrState JmpBack = shiftR (jumpBackward instrState)
 

dataHandler :: State Integer -> Instruction -> State Integer
dataHandler dataState IncDP = shiftR dataState
dataHandler dataState DecDP = shiftL dataState
dataHandler dataState IncB = stateApply (\x -> x + 1) dataState
dataHandler dataState DecB = stateApply (\x -> x - 1) dataState
dataHandler dataState _ = dataState

      
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

jumpForward :: (State (Maybe Instruction)) -> (State (Maybe Instruction))
jumpForward state =
    let nextState = shiftR state

     in case (current nextState) of 
          (Just JmpBack)   -> nextState
          otherwise -> jumpForward nextState

jumpBackward :: (State (Maybe Instruction)) -> (State (Maybe Instruction))
jumpBackward state =
    let nextState = shiftL state

     in case (current nextState) of
          (Just JmpFwd)    -> nextState
          otherwise        -> jumpBackward nextState


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
