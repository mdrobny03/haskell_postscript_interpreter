{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Interpreter (interpret) where
import Language hiding (Result)
import Graphics.Rendering.Cairo hiding (x)

-- Define the State of the interpreter
data State = State {
    execStack :: [PSExpr],
    dataStack :: [PSExpr],
    dictStack :: [Dictionary] -- System dict
} deriving (Show)

-- Result type: either an error message or the final state
type Result s = Either (String, s) s

-- Main interpreter function
interpret :: PSExpr -> Render (Result State)
interpret psExpr = case psExpr of
    PSProcedure body -> execute (State body [] [sysdict])
    _                -> execute (State [psExpr] [] [sysdict])

-- Helper function to resolve names
resolveName :: String -> [Dictionary] -> Maybe PSExpr
resolveName name [] = Nothing
resolveName name (x:xs) = case lookup name x of
    Just val -> Just val
    Nothing  -> resolveName name xs

-- Execution
execute :: State -> Render (Result State)
execute state@(State [] _ _) = do
    -- liftIO $ putStrLn "Execution complete."
    return $ Right state -- Finished execution
execute state@(State (expr:rest) stack dict) = do
    -- Debugging Log the current state
    -- liftIO $ putStrLn $ "\n--- Processing Next Expression ---"
    -- liftIO $ putStrLn $ "Current execStack: " ++ show (expr:rest)
    -- liftIO $ putStrLn $ "Current dataStack: " ++ show stack
    -- liftIO $ putStrLn $ "Processing expression: " ++ show expr

    case expr of
        -- Push values onto the data stack
        PSInt n          -> nextState (PSInt n : stack)
        PSReal r         -> nextState (PSReal r : stack)
        PSBoolean b      -> nextState (PSBoolean b : stack)
        PSArray arr      -> nextState (PSArray arr : stack)
        PSLiteralName name -> nextState (PSLiteralName name : stack)
        PSProcedure body -> nextState (PSProcedure body : stack) -- Push procedure onto data stack

        -- Execute a named command
        PSExecutableName name -> case resolveName name dict of
            Just (PSProcedure body) -> do
                -- liftIO $ putStrLn $ "Resolved name '" ++ name ++ "' to procedure."
                nextExec (body ++ rest) -- Execute procedure body
            Just val -> do
                -- liftIO $ putStrLn $ "Resolved name '" ++ name ++ "' to: " ++ show val
                nextExec (val : rest) -- Push resolved command onto execStack
            Nothing  -> errorOut ("Undefined name: " ++ name)

        -- Execute built-in operator
        PSOp op -> do
            -- liftIO $ putStrLn $ "Executing operator: " ++ show op
            evalOperator op rest

  where
    -- Advance state with updated dataStack
    nextState newStack = do
        -- liftIO $ putStrLn $ "Updated dataStack: " ++ show newStack
        execute $ state { dataStack = newStack, execStack = rest }

    -- Expand procedure into the execStack
    nextExec newExec = do
        -- liftIO $ putStrLn $ "Expanded execStack with: " ++ show newExec
        execute $ state { execStack = newExec }

    -- Handle errors
    errorOut msg = do
        -- liftIO $ putStrLn $ "Error encountered: " ++ msg
        return $ Left (msg, state)

    -- Evaluate a built-in operator
    evalOperator op remainingExec = do
        let stateWithoutOp = state { execStack = remainingExec }
        result <- evalBuiltin op remainingExec stateWithoutOp
        case result of
            Left err -> return $ Left err
            Right newState -> do
                -- Debugging: Log after operator execution
                -- liftIO $ putStrLn $ "Operator executed successfully: " ++ show op
                -- liftIO $ putStrLn $ "Remaining execStack: " ++ show (execStack newState)
                execute newState



-- Evaluate built-in operators
evalBuiltin :: PSBuiltin -> [PSExpr] -> State -> Render (Result State)

-- Addition
evalBuiltin PSadd _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "add: current dataStack: " ++ show stack
    case stack of
        (y : x : xs) -> do
            let result = case (x, y) of
                    (PSInt xi, PSInt yi)     -> PSInt (xi + yi)
                    (PSReal xr, PSReal yr)   -> PSReal (xr + yr)
                    (PSInt xi, PSReal yr)    -> PSReal (fromIntegral xi + yr)
                    (PSReal xr, PSInt yi)    -> PSReal (xr + fromIntegral yi)
                    _ -> error "add: type mismatch"
            -- liftIO $ putStrLn $ "add: result is " ++ show result
            return $ Right (State es (result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "add: stack underflow, requires two numbers."
            return $ Left ("Invalid operation or stack underflow: PSadd", State es stack dict)

-- Subtraction
evalBuiltin PSsub _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "sub: current dataStack: " ++ show stack
    case stack of
        (y : x : xs) -> do
            let result = case (x, y) of
                    (PSInt xi, PSInt yi)     -> PSInt (xi - yi)
                    (PSReal xr, PSReal yr)   -> PSReal (xr - yr)
                    (PSInt xi, PSReal yr)    -> PSReal (fromIntegral xi - yr)
                    (PSReal xr, PSInt yi)    -> PSReal (xr - fromIntegral yi)
                    _ -> error "sub: type mismatch"
            -- liftIO $ putStrLn $ "sub: result is " ++ show result
            return $ Right (State es (result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "sub: stack underflow, requires two numbers."
            return $ Left ("Invalid operation or stack underflow: PSsub", State es stack dict)

-- Multiplication
evalBuiltin PSmul _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "mul: current dataStack: " ++ show stack
    case stack of
        (y : x : xs) -> do
            let result = case (x, y) of
                    (PSInt xi, PSInt yi)     -> PSInt (xi * yi)
                    (PSReal xr, PSReal yr)   -> PSReal (xr * yr)
                    (PSInt xi, PSReal yr)    -> PSReal (fromIntegral xi * yr)
                    (PSReal xr, PSInt yi)    -> PSReal (xr * fromIntegral yi)
                    _ -> error "mul: type mismatch"
            -- liftIO $ putStrLn $ "mul: result is " ++ show result
            return $ Right (State es (result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "mul: stack underflow, requires two numbers."
            return $ Left ("Invalid operation or stack underflow: PSmul", State es stack dict)

-- Division
evalBuiltin PSdiv _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "div: current dataStack: " ++ show stack
    case stack of
        (y : x : xs) -> do
            let toDouble e = case e of
                    PSInt i -> fromIntegral i
                    PSReal r -> r
                    _ -> error "div: type mismatch"
            let divisor = toDouble y
            if divisor == 0 then do
                -- liftIO $ putStrLn "div: division by zero."
                return $ Left ("Division by zero", State es stack dict)
            else do
                let result = toDouble x / divisor
                -- liftIO $ putStrLn $ "div: result is " ++ show result
                return $ Right (State es (PSReal result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "div: stack underflow, requires two numbers."
            return $ Left ("Invalid operation or stack underflow: PSdiv", State es stack dict)

-- Negation
evalBuiltin PSneg _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "neg: current dataStack: " ++ show stack
    case stack of
        (x : xs) -> do
            let result = case x of
                    PSInt xi    -> PSInt (-xi)
                    PSReal xr   -> PSReal (-xr)
                    _ -> error "neg: type mismatch"
            -- liftIO $ putStrLn $ "neg: result is " ++ show result
            return $ Right (State es (result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "neg: stack underflow, requires one number."
            return $ Left ("Invalid operation or stack underflow: PSneg", State es stack dict)

-- Equality
evalBuiltin PSeq _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "eq: current dataStack: " ++ show stack
    case stack of
        (y : x : xs) -> do
            let result = x == y
            -- liftIO $ putStrLn $ "eq: comparing " ++ show x ++ " and " ++ show y ++ " = " ++ show result
            return $ Right (State es (PSBoolean result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "eq: stack underflow, requires two elements."
            return $ Left ("Invalid operation or stack underflow: PSeq", State es stack dict)

-- Conditional Execution
evalBuiltin PSifelse remainingExec (State _ stack dict) =
    case stack of
        (PSProcedure elseProc : PSProcedure thenProc : PSBoolean cond : xs) -> do
            -- liftIO $ putStrLn $ "ifelse: condition is " ++ show cond
            let procToExecute = if cond then thenProc else elseProc
            execute $ State (procToExecute ++ remainingExec) xs dict
        _ -> do
            -- liftIO $ putStrLn "ifelse: expects a boolean and two procedures on the stack."
            return $ Left ("Invalid operation or stack underflow: PSifelse", State [] stack dict)

-- Stack manipulation
evalBuiltin PSdup _ (State es (x:xs) dict) = do
    -- liftIO $ putStrLn $ "dup: duplicating " ++ show x
    return $ Right (State es (x:x:xs) dict)

evalBuiltin PSpop _ (State es (_:xs) dict) = do
    -- liftIO $ putStrLn "pop: removing top of stack"
    return $ Right (State es xs dict)

evalBuiltin PSexch _ (State es (x:y:xs) dict) = do
    -- liftIO $ putStrLn $ "exch: swapping " ++ show x ++ " and " ++ show y
    return $ Right (State es (y:x:xs) dict)

-- Graphics operations
evalBuiltin PSmoveto _ (State es stack dict) =
    case stack of
        (y : x : xs) -> do
            let toDouble e = case e of
                    PSInt i  -> fromIntegral i
                    PSReal r -> r
                    _        -> error "moveto: type mismatch"
                x' = toDouble x
                y' = toDouble y
            -- liftIO $ putStrLn $ "moveto: moving to (" ++ show x' ++ ", " ++ show y' ++ ")"
            moveTo x' y'
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "moveto: stack underflow, requires two numbers."
            return $ Left ("Stack underflow: moveto requires two numbers", State es stack dict)

evalBuiltin PSlineto _ (State es stack dict) =
    case stack of
        (y : x : xs) -> do
            let toDouble e = case e of
                    PSInt i  -> fromIntegral i
                    PSReal r -> r
                    _        -> error "lineto: type mismatch"
                x' = toDouble x
                y' = toDouble y
            -- liftIO $ putStrLn $ "lineto: drawing line to (" ++ show x' ++ ", " ++ show y' ++ ")"
            lineTo x' y'
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "lineto: stack underflow, requires two numbers."
            return $ Left ("Stack underflow: lineto requires two numbers", State es stack dict)


evalBuiltin PSstroke _ state = do
    -- liftIO $ putStrLn "stroke: rendering the current path."
    stroke
    return $ Right state

evalBuiltin PSnewpath _ state = do
    -- liftIO $ putStrLn "newpath: starting a new path."
    newPath
    return $ Right state

evalBuiltin PSclosepath _ state = do
    -- liftIO $ putStrLn "closepath: closing the current path."
    closePath
    return $ Right state

evalBuiltin PSsetlinewidth _ (State es (w : xs) dict) = do
    -- liftIO $ putStrLn $ "setlinewidth: current dataStack: " ++ show (w:xs)
    let width = case w of
            PSInt wint  -> fromIntegral wint
            PSReal wreal -> wreal
            _ -> error "setlinewidth: type mismatch/error"
    -- liftIO $ putStrLn $ "setlinewidth: setting line width to " ++ show width
    setLineWidth width
    return $ Right (State es xs dict)

evalBuiltin PSsetrgbcolor _ (State es stack dict) =
    case stack of
        (PSReal b : PSReal g : PSReal r : xs) -> do
            -- liftIO $ putStrLn $ "setrgbcolor: setting color to (" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"
            setSourceRGB r g b
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "setrgbcolor: stack underflow or type mismatch, requires three real numbers."
            return $ Left ("Invalid operation or stack underflow: PSsetrgbcolor", State es stack dict)

-- Looping
evalBuiltin PSrepeat remainingExec (State _ stack dict) =
    case stack of
        (PSProcedure proc : PSInt n : xs) -> do
            -- liftIO $ putStrLn $ "repeat: executing procedure " ++ show n ++ " times."
            if n < 0 then
                return $ Left ("repeat: negative repeat count", State [] stack dict)
            else
                executeRepeat n proc remainingExec (State [] xs dict)
        _ -> return $ Left ("repeat: expects an integer and a procedure on the stack", State [] stack dict)

evalBuiltin PSdict _ (State es stack dictStack) = do
  case stack of
      (PSInt _ : xs) -> do
          -- liftIO $ putStrLn "dict: pushing new empty dictionary onto the data stack."
          return $ Right (State es (PSDict []: xs) dictStack)
      _ -> do
          -- liftIO $ putStrLn "dict: stack underflow, requires an integer."
          return $ Left ("Invalid operation or stack underflow: PSdict", State es stack dictStack)

evalBuiltin PSbegin _ (State es stack dictStack) = do
    case stack of
        (PSDict d : xs) -> do
            -- liftIO $ putStrLn $ "begin: moving dictionary to the dictionary stack: " ++ show d
            return $ Right (State es xs (d : dictStack))
        _ -> do
            -- liftIO $ putStrLn "begin: stack underflow or type mismatch, requires a dictionary."
            return $ Left ("Invalid operation or stack underflow: PSbegin", State es stack dictStack)

evalBuiltin PSend _ (State es stack dictStack) = do
    case dictStack of
        (_ : xs) -> do
            -- liftIO $ putStrLn "end: removing the top dictionary from the dictionary stack."
            return $ Right (State es stack xs)
        _ -> do
            -- liftIO $ putStrLn "end: dictionary stack underflow."
            return $ Left ("Invalid operation: dictionary stack underflow in end", State es stack [])

evalBuiltin PSdef _ (State es stack (top : rest)) = do
    case stack of
        (val : PSLiteralName name : xs) -> do
            -- liftIO $ putStrLn $ "def: defining '" ++ name ++ "' as " ++ show val
            let newDict = (name, val) : top
            return $ Right (State es xs (newDict : rest))
        _ -> do
            -- liftIO $ putStrLn "def: stack underflow or incorrect type, need a name and a val."
            return $ Left ("Invalid operation or stack underflow: PSdef", State es stack (top : rest))
            
evalBuiltin PSsin _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "sin: current dataStack: " ++ show stack
    case stack of
        (x : xs) -> do
            let toDouble e = case e of
                    PSInt xint  -> fromIntegral xint
                    PSReal xreal -> xreal
                    _         -> error "sin: type mismatch"
                angle   = toDouble x
                radians = angle * pi / 180
                result  = sin radians
            -- liftIO $ putStrLn $ "sin: result is " ++ show result
            return $ Right (State es (PSReal result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "sin: stack underflow, requires one number."
            return $ Left ("Invalid operation or stack underflow: PSSin", State es stack dict)

evalBuiltin PScos _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "cos: current dataStack: " ++ show stack
    case stack of
        (x : xs) -> do
            let toDouble e = case e of
                    PSInt xint  -> fromIntegral xint
                    PSReal xreal -> xreal
                    _         -> error "cos: type mismatch"
                angle   = toDouble x
                radians = angle * pi / 180
                result  = cos radians
            -- liftIO $ putStrLn $ "cos: result is " ++ show result
            return $ Right (State es (PSReal result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "cos: stack underflow, requires one number."
            return $ Left ("Invalid operation or stack underflow: PSCos", State es stack dict)

evalBuiltin PSmod _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "mod: current dataStack: " ++ show stack
    case stack of
        (PSInt y : PSInt x : xs) -> do
            if y == 0 then do
                -- liftIO $ putStrLn "mod: division by zero."
                return $ Left ("Division by zero in mod", State es stack dict)
            else do
                let result = x `mod` y
                -- liftIO $ putStrLn $ "mod: result is " ++ show result
                return $ Right (State es (PSInt result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "mod: requires two integers on the stack."
            return $ Left ("Type mismatch or stack underflow in mod", State es stack dict)

evalBuiltin PStruncate _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "truncate: current dataStack: " ++ show stack
    case stack of
        (x : xs) -> do
            let result = case x of
                    PSInt xint  -> PSInt xint
                    PSReal xreal -> PSInt (truncate xreal)
                    _         -> error "truncate: type mismatch"
            -- liftIO $ putStrLn $ "truncate: result is " ++ show result
            return $ Right (State es (result : xs) dict)
        _ -> do
            -- liftIO $ putStrLn "truncate: stack underflow, requires one number."
            return $ Left ("Invalid operation or stack underflow: PSTruncate", State es stack dict)

evalBuiltin PSfill _ state = do
    -- liftIO $ putStrLn "fill: filling the current path."
    fill
    return $ Right state

evalBuiltin PStranslate _ (State es stack dict) =
    case stack of
        (dy : dx : xs) -> do
            let toDouble e = case e of
                    PSInt i  -> fromIntegral i
                    PSReal r -> r
                    _        -> error "translate: type mismatch"
                dx' = toDouble dx
                dy' = toDouble dy
            -- liftIO $ putStrLn $ "translate: translating by (" ++ show dx' ++ ", " ++ show dy' ++ ")"
            translate dx' dy'
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "translate: requires two numbers on the stack."
            return $ Left ("Invalid operation or stack underflow: PSTranslate", State es stack dict)

evalBuiltin PSscale _ (State es stack dict) =
    case stack of
        (sy : sx : xs) -> do
            let toDouble e = case e of
                    PSInt i  -> fromIntegral i
                    PSReal r -> r
                    _        -> error "scale: type mismatch"
                sx' = toDouble sx
                sy' = toDouble sy
            -- liftIO $ putStrLn $ "scale: scaling by (" ++ show sx' ++ ", " ++ show sy' ++ ")"
            scale sx' sy'
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "scale: requires two numbers on the stack."
            return $ Left ("Invalid operation or stack underflow: PSScale", State es stack dict)

evalBuiltin PSrotate _ (State es stack dict) =
    case stack of
        (angle : xs) -> do
            let toDouble e = case e of
                    PSInt i  -> fromIntegral i
                    PSReal r -> r
                    _        -> error "rotate: type mismatch"
                angle'  = toDouble angle
                radians = angle' * pi / 180
            -- liftIO $ putStrLn $ "rotate: rotating by " ++ show angle' ++ " degrees"
            rotate radians
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "rotate: requires one number on the stack."
            return $ Left ("Invalid operation or stack underflow: PSRotate", State es stack dict)


evalBuiltin PSgsave _ (State es stack dict) = do
    -- liftIO $ putStrLn "gsave: saving the current graphic state."
    save
    return $ Right (State es stack dict)

evalBuiltin PSgrestore _ (State es stack dict) = do
    -- liftIO $ putStrLn "grestore: restoring the previous graphic state."
    restore
    return $ Right (State es stack dict)    

-- NEW FUNCTIONS

evalBuiltin PSsetlinecap _ (State es (cap : xs) dict) = do
    let capStyle = case cap of
            PSInt 0 -> LineCapButt
            PSInt 1 -> LineCapRound
            PSInt 2 -> LineCapSquare
            _ -> error "setlinecap: invalid cap style"
    -- liftIO $ putStrLn $ "setlinecap: setting line cap to " ++ show capStyle
    setLineCap capStyle
    return $ Right (State es xs dict)

evalBuiltin PSsetlinejoin _ (State es (join : xs) dict) = do
    let joinStyle = case join of
            PSInt 0 -> LineJoinMiter
            PSInt 1 -> LineJoinRound
            PSInt 2 -> LineJoinBevel
            _ -> error "setlinejoin: invalid join style"
    -- liftIO $ putStrLn $ "setlinejoin: setting line join to " ++ show joinStyle
    setLineJoin joinStyle
    return $ Right (State es xs dict)

evalBuiltin PSsetdash _ (State es stack dict) =
    case stack of
        (PSInt phase : PSArray offsets : xs) -> do
            let toDouble e = case e of
                    PSInt i  -> fromIntegral i
                    PSReal r -> r
                    _        -> error "setdash: type mismatch"
            let dashOffsets = map toDouble offsets
                phase' = fromIntegral phase
            -- liftIO $ putStrLn $ "setdash: setting dash pattern to " ++ show dashOffsets ++ ", phase = " ++ show phase'
            setDash dashOffsets phase'
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "setdash: stack underflow or type mismatch, requires an array and a number."
            return $ Left ("Invalid operation or stack underflow: PSsetdash", State es stack dict)


evalBuiltin PSarc _ (State es stack dict) =
    case stack of
        (angle2 : angle1 : radius : y : x : xs) -> do
            let toDouble e = case e of
                    PSInt i  -> fromIntegral i
                    PSReal r -> r
                    _        -> error "arc: type mismatch"
            let x' = toDouble x
                y' = toDouble y
                r' = toDouble radius
                a1' = toDouble angle1
                a2' = toDouble angle2
            -- liftIO $ putStrLn $ "arc: drawing counterclockwise arc at (" ++ show x' ++ ", " ++ show y' ++
            --                    ") with radius " ++ show r' ++ " from " ++ show a1' ++ " to " ++ show a2'
            arc x' y' r' (a1' * pi / 180) (a2' * pi / 180)
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "arc: stack underflow or type mismatch, requires five numbers."
            return $ Left ("Invalid operation or stack underflow: PSarc", State es stack dict)


evalBuiltin PSarcn _ (State es stack dict) =
    case stack of
        (angle2 : angle1 : radius : y : x : xs) -> do
            let toDouble e = case e of
                    PSInt i  -> fromIntegral i
                    PSReal r -> r
                    _        -> error "arcn: type mismatch"
            let x' = toDouble x
                y' = toDouble y
                r' = toDouble radius
                a1' = toDouble angle1
                a2' = toDouble angle2
            -- liftIO $ putStrLn $ "arcn: drawing clockwise arc at (" ++ show x' ++ ", " ++ show y' ++
              --                  ") with radius " ++ show r' ++ " from " ++ show a1' ++ " to " ++ show a2'
            arcNegative x' y' r' (a1' * pi / 180) (a2' * pi / 180)
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "arcn: stack underflow or type mismatch, requires five numbers."
            return $ Left ("Invalid operation or stack underflow: PSarcn", State es stack dict)


evalBuiltin PScurveto _ (State es stack dict) =
    case stack of
        (y3 : x3 : y2 : x2 : y1 : x1 : xs) -> do
            let toDouble e = case e of
                    PSInt i  -> fromIntegral i
                    PSReal r -> r
                    _        -> error "curveto: type mismatch"
            let x1' = toDouble x1
                y1' = toDouble y1
                x2' = toDouble x2
                y2' = toDouble y2
                x3' = toDouble x3
                y3' = toDouble y3
            -- liftIO $ putStrLn $ "curveto: drawing Bézier curve to (" ++ show x3' ++ ", " ++ show y3' ++
             --                   ") with control points (" ++ show x1' ++ ", " ++ show y1' ++
             --                    ") and (" ++ show x2' ++ ", " ++ show y2' ++ ")"
            curveTo x1' y1' x2' y2' x3' y3'
            return $ Right (State es xs dict)
        _ -> do
            -- liftIO $ putStrLn "curveto: stack underflow or type mismatch, requires six numbers."
            return $ Left ("Invalid operation or stack underflow: PScurveto", State es stack dict)


-- Error handler for invalid operators or stack underflow
evalBuiltin op _ (State es stack dict) = do
    -- liftIO $ putStrLn $ "Invalid operation or stack underflow for operator: " ++ show op
    return $ Left ("Invalid operation or stack underflow: " ++ show op, State es stack dict)

-- Helper function to execute the procedure N times
executeRepeat :: Int -> [PSExpr] -> [PSExpr] -> State -> Render (Result State)
executeRepeat 0 _ remainingExec state = return $ Right (state { execStack = remainingExec })
executeRepeat n proc remainingExec state = do
    -- Push the procedure onto the execStack
    let newExecStack = proc
    executeResult <- execute (state { execStack = newExecStack })
    case executeResult of
        Left err -> return $ Left err
        Right newState -> executeRepeat (n - 1) proc remainingExec newState