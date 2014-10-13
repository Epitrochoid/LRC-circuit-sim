{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import Eulerdefs (timeConstant, rcCircuit)

-- List of command line options
data Options = Options {
             resistance :: Double,
             capacitance :: Double,
             initial_charge :: Double,
             dt :: Double
             } deriving (Data, Typeable, Show, Eq)

-- Default option settings and help information
optSettings :: Options
optSettings = Options {
        resistance = def &= help "Resistance of the RC circuit",
        capacitance = def &= help "Capacitance of the RC circuit",
        initial_charge = def &= help "Initial charge of the RC circuit",
        dt = def &= help "Time increment for ODE solution"
        }

-- Sets up the help page options
getOpts :: IO Options
getOpts = cmdArgs $ optSettings
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "RC Circuit Simulation"
_PROGRAM_ABOUT = "Simulates an RC circuit using Euler's method and outputs the time constant. " ++
    "Numerical values may be entered in exponential notation, e.g. 22e-6."

main :: IO ()
main = do
        args <- getArgs
        opts <- (if null args then withArgs ["--help"] else id) getOpts
        optionHandler opts

-- Deal with incorrect user input
optionHandler :: Options -> IO ()
optionHandler opts@Options{..} = do -- RecordWildCards fills in with 'resistance=resistance', etc
    when (resistance == 0.0) $ putStrLn "Must enter a value for resistance." >> exitWith (ExitFailure 1)
    when (capacitance == 0.0) $ putStrLn "Must enter a value for capacitance." >> exitWith (ExitFailure 1)
    when (initial_charge == 0.0) $ putStrLn "Must enter an initial charge." >> exitWith (ExitFailure 1)
    when (dt == 0.0) $ putStrLn "Must enter a time increment dt." >> exitWith (ExitFailure 1)
    when (resistance < 0) $ putStrLn "Resistance must not be negative." >> exitWith (ExitFailure 1)
    when (capacitance < 0) $ putStrLn "Capacitance must not be negative." >> exitWith (ExitFailure 1)
    when (initial_charge < 0) $ putStrLn "Initial charge must not be negative." >> exitWith (ExitFailure 1)
    when (dt < 0) $ putStrLn "Time increment must not be negative." >> exitWith (ExitFailure 1)
    exec opts

-- Function that recieves the sanitized args
--   and outputs the time constant
exec :: Options -> IO ()
exec opts@Options{..} = do
    putStrLn $ "tau = " ++ (show timeconst) ++ "s"
    where
        -- Normally it would be inefficient to produce a list
        --  then discard most of it and take the length.
        --  Haskell, however, is a lazy evaluated language
        --  and doesn't build the list until the 'takeWhile'
        --  call in 'timeConstant'. Most of the list 
        --  is not calculated at all because the compiler
        --  recognizes that 'takeWhile' is going to 
        --  discard it.
        timeconst = timeConstant dt $ rcCircuit resistance capacitance initial_charge dt
