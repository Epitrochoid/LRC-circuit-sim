{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import Eulerdefs (lrcCircuit)
import qualified Graphics.Rendering.Chart.Easy as C
import Graphics.Rendering.Chart.Backend.Cairo as B
import Control.Parallel (pseq)

data Options = Options {
             resistance :: Double,
             capacitance :: Double,
             inductance :: Double,
             initial_charge :: Double,
             dt :: Double,
             output_file :: String,
             csv_file :: String
             } deriving (Data, Typeable, Show, Eq)

optSettings :: Options
optSettings = Options {
                      resistance = def &= help "Resistance of the LRC circuit",
                      capacitance = def &= name "c" &= help "Capacitance of the LRC circuit",
                      inductance = def &= name "l" &= help "Inductance of the LRC circuit",
                      initial_charge = def &= name "q" &= help "Initial charge of the RC circuit",
                      dt = def &= help "Time increment for ODE solution",
                      output_file = def &= help "Filepath for output graph",
                      csv_file = def &= help "Filepath for CSV to plot alongside solution"
                      }

getOpts :: IO Options
getOpts = cmdArgs $ optSettings
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "LRC Circuit Simulation"
_PROGRAM_ABOUT = "Simulates an LRC circuit using Euler's method and plots the simulation." ++
    "Numerical values may be entered in exponential notation, e.g. '10e-6'." ++
    "Output file must be .png, .svg, or .ps"


main :: IO ()
main = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) getOpts
    optionHandler opts

optionHandler :: Options -> IO ()
optionHandler opts@Options{..} = do -- RecordWildCards fills in with 'resistance=resistance', etc
    when (resistance == 0.0) $ putStrLn "Must enter a value for resistance." >> exitWith (ExitFailure 1)
    when (capacitance == 0.0) $ putStrLn "Must enter a value for capacitance." >> exitWith (ExitFailure 1)
    when (initial_charge == 0.0) $ putStrLn "Must enter an initial charge." >> exitWith (ExitFailure 1)
    when (inductance == 0.0) $ putStrLn "Must enter a value for inductance." >> exitWith (ExitFailure 1)
    when (dt == 0.0) $ putStrLn "Must enter a time increment dt." >> exitWith (ExitFailure 1)
    when (resistance < 0) $ putStrLn "Resistance must not be negative." >> exitWith (ExitFailure 1)
    when (capacitance < 0) $ putStrLn "Capacitance must not be negative." >> exitWith (ExitFailure 1)
    when (inductance < 0) $ putStrLn "Inductance must not be negative." >> exitWith (ExitFailure 1)
    when (dt < 0) $ putStrLn "Time increment must not be negative." >> exitWith (ExitFailure 1)
    when (null output_file) $ putStrLn "Must include an output file location." >> exitWith (ExitFailure 1)
    exec opts

exec :: Options -> IO ()
exec opts@Options{..} = B.toFile C.def output_file $ do
    C.layout_title C..= "Voltage vs Time"
    C.layout_x_axis . C.laxis_title C..= "Time, (s)"
    C.layout_y_axis . C.laxis_title C..= "Voltage, (V)"
    C.plot $ C.line "LRC Circuit" [lrcPlot]
    where
        lrcVolts = filter (not . isNaN) $ lrcCircuit resistance capacitance inductance initial_charge 0.0 dt
        lrcPlot = zip (map (*dt) [1..800]) lrcVolts

